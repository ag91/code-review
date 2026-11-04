;;; code-review-draft-test.el --- Draft persistence tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'code-review)
(require 'code-review-test-helpers)

;;; req-2026-11-03-005: reopening-preserves-pending-comments
;; covers: code-review-db.el
;; covers-fn: code-review-db--pullreq-create
;; enforcement: test
(ert-deftest code-review-draft/req-2026-11-03-005-reopen-keeps-comments ()
  "WHEN a reviewer reopens a PR THE SYSTEM SHALL preserve pending comments across refreshes."
  (code-review-test--with-db
    (let* ((make-pr (lambda ()
                      (code-review-github-repo :owner "test" :repo "test" :number "1")))
           (draft '((state . "LOCAL COMMENT")
                    (comments (nodes ((internal-id . "draft")
                                      (bodyText . "Keep this comment")
                                      (path . "lib.el") (line . 1)
                                      (side . "RIGHT") (local? . t)))))))
      (code-review-db--pullreq-create (funcall make-pr))
      (code-review-db--pullreq-raw-comments-update draft)
      (dotimes (_ 3)
        (let ((old-id code-review-db--pullreq-id))
          (code-review-db--pullreq-create (funcall make-pr))
          (should-not (equal old-id code-review-db--pullreq-id))
          (code-review-db--pullreq-raw-infos-update
           '((state . "OPEN") (reviews (nodes))))
          (should (equal (code-review-db--pullreq-raw-comments) (list draft))))))))

(ert-deftest code-review-draft/different-local-range-does-not-inherit-comments ()
  (code-review-test--with-db
    (let ((make-pr (lambda (range)
                     (let ((pr (code-review-local-diff
                                :owner "local" :repo "repo" :number "1")))
                       (oset pr state "LOCAL")
                       (oset pr base-ref-name range)
                       pr))))
      (code-review-db--pullreq-create (funcall make-pr "HEAD~1"))
      (code-review-db--pullreq-raw-comments-update '((state . "LOCAL COMMENT")))
      (code-review-db--pullreq-create (funcall make-pr "HEAD~2"))
      (should-not (code-review-db--pullreq-raw-comments)))))

(ert-deftest code-review-draft/refresh-keeps-pending-replies ()
  (code-review-test--with-db
    (code-review-db--pullreq-create
     (code-review-github-repo :owner "test" :repo "test" :number "1"))
    (let ((draft '((state . "COMMENTED")
                   (comments (nodes ((internal-id . "reply")
                                     (bodyText . "my reply")
                                     (databaseId . 42) (reply? . t)))))))
      (code-review-db--pullreq-raw-comments-update draft)
      (dotimes (_ 2)
        (code-review-db--pullreq-raw-infos-update '((reviews (nodes))))
        (should (equal (code-review-db--pullreq-raw-comments) (list draft)))))))

(ert-deftest code-review-draft/delete-only-the-matching-node ()
  (code-review-test--with-db
    (code-review-db--pullreq-create
     (code-review-github-repo :owner "test" :repo "test" :number "1"))
    (let ((entry '((state . "COMMENTED")
                   (comments (nodes ((databaseId . "42"))
                                     ((databaseId . 43)))))))
      (code-review-db--pullreq-raw-comments-update entry)
      (code-review-db-delete-raw-comment 42)
      (should (equal (a-get-in (car (code-review-db--pullreq-raw-comments))
                              '(comments nodes))
                     '(((databaseId . 43)))))
      (should (= (length (a-get-in entry '(comments nodes))) 2)))))

(ert-deftest code-review-draft/retry-replaces-draft-without-duplicates ()
  (code-review-test--with-db
    (code-review-db--pullreq-create
     (code-review-github-repo :owner "test" :repo "test" :number "1"))
    (let ((obj (code-review-local-comment-section))
          (entry '((state . "LOCAL COMMENT")
                   (comments (nodes ((internal-id . "draft")
                                     (bodyText . "my draft")))))))
      (code-review-comment--save-draft obj entry)
      (code-review-comment--save-draft obj entry)
      (should (equal (code-review-db--pullreq-raw-comments) (list entry)))
      (cl-letf (((symbol-function 'code-review-db--pullreq-raw-comments-update)
                 (lambda (&rest _) (error "Injected database failure"))))
        (should-error (code-review-comment--save-draft obj entry)))
      (should (equal (code-review-db--pullreq-raw-comments) (list entry))))))

(defmacro code-review-draft-test--with-composer-context (&rest body)
  "Run BODY with two reviews and an isolated comment composer."
  (declare (indent 0) (debug t))
  `(code-review-test--with-db
     (let ((origin nil) (other nil) (first-id nil) (second-id nil)
           (code-review-comment-buffer-name "*Code Review context test composer*")
           (code-review-comment-uncommitted
            (code-review-local-comment-section
             :state "LOCAL COMMENT" :author "me" :path "lib.el"
             :position 1 :line 1 :side "RIGHT" :line-type "ADDED" :send? nil))
           (code-review-comment-description? nil)
           (code-review-comment-title? nil)
           (code-review-comment-feedback? nil)
           (code-review-promote-comment-to-issue? nil)
           (code-review-comment-send? nil)
           (code-review-comment-single-comment? nil)
           (code-review-comment-suggestion? nil)
           (code-review-comment-commit-buffer? nil))
       (unwind-protect
           (progn
             (code-review-db--pullreq-create
              (code-review-github-repo :owner "test" :repo "test" :number "1"))
             (setq first-id code-review-db--pullreq-id
                   origin (get-buffer-create (code-review-pr-buffer-name)))
             (with-current-buffer origin
               (setq-local code-review-review-buffer-pr-id first-id))
             (code-review-db--pullreq-create
              (code-review-github-repo :owner "test" :repo "test" :number "2"))
             (setq second-id code-review-db--pullreq-id
                   other (get-buffer-create (code-review-pr-buffer-name)))
             (with-current-buffer other
               (setq-local code-review-review-buffer-pr-id second-id))
             ,@body)
         (--each (list origin other (get-buffer code-review-comment-buffer-name))
           (when (buffer-live-p it)
             (with-current-buffer it (set-buffer-modified-p nil))
             (kill-buffer it)))))))

;;; req-2026-11-04-002: composer-review-isolation
;; covers: code-review-comment.el
;; covers-fn: code-review-comment-add code-review-comment-commit
;; enforcement: test
(ert-deftest code-review-draft/req-2026-11-04-002-open-from-actual-review ()
  "WHEN another PR becomes current THE SYSTEM SHALL keep the comment attached to its originating review."
  (code-review-draft-test--with-composer-context
    (cl-letf (((symbol-function 'switch-to-buffer-other-window) #'ignore))
      (with-current-buffer origin
        (code-review-comment-add "Draft for first PR")))
    (with-current-buffer code-review-comment-buffer-name
      (should (eq code-review-comment-review-buffer origin)))))

(ert-deftest code-review-draft/commit-after-another-pr-becomes-current ()
  (code-review-draft-test--with-composer-context
    (setq code-review-db--pullreq-id first-id)
    (cl-letf (((symbol-function 'switch-to-buffer-other-window) #'ignore))
      (with-current-buffer origin
        (code-review-comment-add "Draft for first PR")))
    (setq code-review-db--pullreq-id second-id)
    (let ((rebuilt nil))
      (cl-letf (((symbol-function 'code-review--build-buffer)
                 (lambda (&rest _)
                   (setq rebuilt code-review-db--pullreq-id))))
        (with-current-buffer code-review-comment-buffer-name
          (code-review-comment-commit)))
      (should (equal rebuilt first-id)))
    (should (equal code-review-db--pullreq-id second-id))
    (should-not (code-review-db--pullreq-raw-comments))
    (let ((code-review-db--pullreq-id first-id))
      (should (equal (a-get (car (a-get-in
                                 (car (code-review-db--pullreq-raw-comments))
                                 '(comments nodes)))
                            'bodyText)
                     "Draft for first PR")))))

;;; req-2026-11-04-003: empty-comment-preserves-composer
;; covers: code-review-comment.el
;; covers-fn: code-review-comment-handler-commit code-review-comment-commit
;; enforcement: test
(ert-deftest code-review-draft/req-2026-11-04-003-empty-comment-keeps-composer ()
  "WHEN a comment contains no authored text THE SYSTEM SHALL retain its composer without saving or rendering."
  (code-review-draft-test--with-composer-context
    (setq code-review-db--pullreq-id first-id)
    (cl-letf (((symbol-function 'switch-to-buffer-other-window) #'ignore)
              ((symbol-function 'code-review--build-buffer)
               (lambda (&rest _) (ert-fail "Empty comment must not render"))))
      (with-current-buffer origin
        (code-review-comment-add))
      (with-current-buffer code-review-comment-buffer-name
        (should-error (code-review-comment-commit) :type 'user-error)
        (should (buffer-live-p (current-buffer)))))
    (should-not (code-review-db--pullreq-raw-comments))))

(ert-deftest code-review-draft/closed-origin-never-falls-back-to-another-pr ()
  (code-review-draft-test--with-composer-context
    (setq code-review-db--pullreq-id first-id)
    (cl-letf (((symbol-function 'switch-to-buffer-other-window) #'ignore))
      (with-current-buffer origin
        (code-review-comment-add "Draft for first PR")))
    (kill-buffer origin)
    (setq code-review-db--pullreq-id second-id)
    (with-current-buffer code-review-comment-buffer-name
      (should-error (code-review-comment-commit) :type 'user-error)
      (should (buffer-live-p (current-buffer))))
    (should-not (code-review-db--pullreq-raw-comments))))

(ert-deftest code-review-draft/edit-retains-actual-review-context ()
  (code-review-draft-test--with-composer-context
    (oset code-review-comment-uncommitted msg "Previously saved draft")
    (let ((section (magit-section)))
      (oset section value code-review-comment-uncommitted)
      (cl-letf (((symbol-function 'magit-current-section) (lambda () section))
                ((symbol-function 'switch-to-buffer-other-window) #'set-buffer))
        (with-current-buffer origin
          (code-review-comment-edit))))
    (with-current-buffer code-review-comment-buffer-name
      (should (eq code-review-comment-review-buffer origin)))))

(ert-deftest code-review-draft/async-comment-refresh-stays-with-origin ()
  (code-review-draft-test--with-composer-context
    (let ((callback nil) (rebuilt nil)
          (code-review-db--pullreq-id first-id)
          (code-review-comment-single-comment? t))
      (cl-letf (((symbol-function 'code-review-new-issue-comment)
                 (lambda (_pr _msg cb) (setq callback cb)))
                ((symbol-function 'code-review--build-buffer)
                 (lambda (&rest _) (setq rebuilt code-review-db--pullreq-id))))
        (code-review-comment--commit-text
         (code-review-db-get-pullreq) "Issue comment for first PR")
        (setq code-review-db--pullreq-id second-id)
        (funcall callback)
        (should (equal rebuilt first-id))))))

;;; code-review-draft-test.el ends here
