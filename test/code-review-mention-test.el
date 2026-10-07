;;; code-review-mention-test.el --- ERT tests for @mention user completion -*- lexical-binding: t; -*-
;;
;; This file is part of code-review.

(require 'ert)
(require 'dash)
(require 'a)
(require 'code-review-comment)
(require 'code-review-github)
(require 'code-review-db)
(require 'code-review-test-helpers)

(ert-deftest code-review-mention-test/org-members-two-pages ()
  "Mention candidates are the repository organization's members,
all pages of them, in ghub 5.1's synchronous return shape (the
single (data CONTENTS) pair); cached per PR afterwards."
  (code-review-test--with-db
    (let* ((calls 0)
           (pr (code-review-github-repo
                :owner "WriterInternal" :repo "writer-data-platform" :number 1))
           (old-ghub (symbol-function 'ghub-query)))
      (fset 'ghub-query
            (lambda (query variables &rest _)
              (setq calls (1+ calls))
              (should (string-match-p "membersWithRole" query))
              (if (= calls 1)
                  (progn
                    (should (equal (cdr (assq 'org variables))
                                   "WriterInternal"))
                    '(data (organization
                            (membersWithRole
                             (nodes ((login . "bob") (name . "B")))
                             (pageInfo (hasNextPage . t)
                                       (endCursor . "c1"))))))
                (progn
                  (should (equal (cdr (assq 'cursor variables)) "c1"))
                  '(data (organization
                          (membersWithRole
                           (nodes ((login . "alice")))
                           (pageInfo (hasNextPage . nil)))))))))
      (unwind-protect
          (progn
            (code-review-db--pullreq-create pr)
            (let ((users (code-review-get-mentionable-users pr)))
              (should (equal (-map (lambda (u) (a-get u 'login)) users)
                             '("bob" "alice")))
              (should (= calls 2))
              ;; second call: served from the RAW-INFOS cache, no API call
              (code-review-get-mentionable-users pr)
              (should (= calls 2))))
        (fset 'ghub-query old-ghub)))))

(ert-deftest code-review-mention-test/owner-not-org-falls-back-to-assignable ()
  "A repository whose owner is no organization (a null
`organization' in the GraphQL data) falls back to the assignable
users; both results are cached, so a second lookup makes no API
call."
  (code-review-test--with-db
    (let* ((calls 0)
           (pr (code-review-github-repo
                :owner "ag91" :repo "sandbox" :number 1))
           (old-ghub (symbol-function 'ghub-query)))
      (fset 'ghub-query
            (lambda (query _variables &rest _)
              (setq calls (1+ calls))
              (if (string-match-p "membersWithRole" query)
                  '(data (organization . nil))
                '(data (repository
                        (assignableUsers
                         (nodes ((login . "alice") (name . "A")))
                         (pageInfo (hasNextPage . nil))))))))
      (unwind-protect
          (progn
            (code-review-db--pullreq-create pr)
            (let ((users (code-review-get-mentionable-users pr)))
              (should (equal (-map (lambda (u) (a-get u 'login)) users)
                             '("alice")))
              ;; the org members query plus one assignable page
              (should (= calls 2))
              ;; BOTH cached now: no more API calls
              (code-review-get-mentionable-users pr)
              (should (= calls 2))
              (should (a-get (oref pr raw-infos) 'mentionable-users))
              (should (a-get (oref pr raw-infos) 'assignable-users)))
        (fset 'ghub-query old-ghub))))))

(ert-deftest code-review-mention-test/command-inserts-selected-login ()
  "C-c @ in the comment buffer completes over the SORTED org
member logins and inserts @LOGIN of the selected one."
  (code-review-test--with-db
    (let* ((pr (code-review-github-repo
                :owner "WriterInternal" :repo "writer-data-platform" :number 1))
           (old-ghub (symbol-function 'ghub-query))
           (old-complete (symbol-function 'completing-read)))
      (fset 'ghub-query
            (lambda (_query _variables &rest _)
              '(data (organization
                      (membersWithRole
                       (nodes ((login . "bob") (name . "B"))
                              ((login . "alice")))
                       (pageInfo (hasNextPage . nil)))))))
      (fset 'completing-read
            (lambda (_prompt collection &rest _)
              (should (equal collection '("alice" "bob")))
              "bob"))
      (unwind-protect
          (progn
            (code-review-db--pullreq-create pr)
            (with-temp-buffer
              (code-review-input-mention-user-at-point)
              (should (equal (buffer-string) "@bob "))))
        (progn
          (fset 'ghub-query old-ghub)
          (fset 'completing-read old-complete))))))

(ert-deftest code-review-mention-test/no-users-user-error ()
  "With no org members and no assignable users the command
user-errors instead of opening an empty require-match completion
(a batch completing-read would hang on stdin)."
  (code-review-test--with-db
    (let* ((pr (code-review-github-repo
                :owner "WriterInternal" :repo "writer-data-platform" :number 1))
           (old-ghub (symbol-function 'ghub-query))
           (old-complete (symbol-function 'completing-read)))
      (fset 'ghub-query
            (lambda (query _variables &rest _)
              (if (string-match-p "membersWithRole" query)
                  '(data (organization . nil))
                '(data (repository
                        (assignableUsers
                         (nodes)
                         (pageInfo (hasNextPage . nil))))))))
      (fset 'completing-read
            (lambda (&rest _) (ert-fail "completing-read must not run")))
      (unwind-protect
          (progn
            (code-review-db--pullreq-create pr)
            (should-error (code-review-input-mention-user-at-point)
                          :type 'user-error))
        (progn
          (fset 'ghub-query old-ghub)
          (fset 'completing-read old-complete))))))
