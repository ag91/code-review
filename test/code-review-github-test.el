;;; code-review-github-test.el --- ERT tests for GitHub API plumbing -*- lexical-binding: t; -*-

;; Phase 6 (edit submitted comments / PR lifecycle): the endpoint
;; helper is pure, so it is tested offline.  The actual ghub calls
;; are exercised live only.

(require 'ert)
(require 'code-review-github)

(ert-deftest code-review-github-test/update-comment-endpoint-review-comment ()
  "Diff comments go to the review comment PATCH endpoint."
  (should (equal (code-review-github--update-comment-endpoint
                  "review-comment" "octocat" "repo" 5 123)
                 (cons "/repos/octocat/repo/pulls/comments/123" #'ghub-patch))))

(ert-deftest code-review-github-test/update-comment-endpoint-issue-comment ()
  "Conversation comments go to the issue comment PATCH endpoint."
  (should (equal (code-review-github--update-comment-endpoint
                  "issue-comment" "octocat" "repo" 5 456)
                 (cons "/repos/octocat/repo/issues/comments/456" #'ghub-patch))))

(ert-deftest code-review-github-test/update-comment-endpoint-review-summary ()
  "Submitted review summaries go to the PUT reviews endpoint."
  (should (equal (code-review-github--update-comment-endpoint
                  "review-summary" "octocat" "repo" 5 789)
                 (cons "/repos/octocat/repo/pulls/5/reviews/789" #'ghub-put))))

(ert-deftest code-review-github-test/graphql-queries-fetch-raw-body ()
  "Both GraphQL queries must fetch `body' (raw markdown) on review
nodes, review comment nodes and top-level comment nodes: editing a
submitted comment needs the body as written, not the rendered
bodyHTML."
  (dolist (query (list code-review-github-graphql-fallback
                       code-review-github-graphql-complete))
    ;; review nodes carry bodyHTML and body
    (should (string-match-p "bodyHTML\n\\s-*body\n\\s-*state" query))
    ;; review comment nodes carry bodyHTML and body
    (should (string-match-p "bodyHTML\n\\s-*body\n\\s-*originalPosition" query))
    ;; top-level comment nodes carry databaseId, bodyHTML and body
    (should (string-match-p "databaseId\n\\s-*bodyHTML\n\\s-*body\n\\s-*createdAt" query))))

(defun code-review-github-test--files-response (nodes next cursor)
  "Return a ghub response with file NODES and page NEXT/CURSOR."
  (cons 'data
        (a-assoc-in
         '((repository (pullRequest (id . "PR_NODE"))))
         '(repository pullRequest files)
         `((nodes . ,nodes)
           (pageInfo (hasNextPage . ,next) (endCursor . ,cursor))))))

;;; req-2026-10-16-003:
;; covers: code-review-github-query.el
;; covers-fn: code-review-pullreq-infos, code-review-github--continue-files
;; enforcement: test
(ert-deftest code-review-github-test/req-2026-10-16-003-viewed-state-all-pages ()
  "WHEN a change has many files THE SYSTEM SHALL reconcile review progress beyond the first page."
  (let ((original (symbol-function 'ghub-query))
        (code-review-auth-login-marker 'test)
        (calls 0)
        result)
    (unwind-protect
        (progn
          (fset 'ghub-query
                (lambda (query vars &rest options)
                  (setq calls (1+ calls))
                  (should-not (plist-get options :synchronous))
                  (should (string-match-p "viewerViewedState" query))
                  (if (= calls 1)
                      (progn
                        (should-not vars)
                        (funcall
                         (plist-get options :callback)
                         (code-review-github-test--files-response
                          '(((path . "first.el") (viewerViewedState . "VIEWED")))
                          t "cursor-1")))
                    (should (equal (a-get vars 'id) "PR_NODE"))
                    (should (equal (a-get vars 'cursor) "cursor-1"))
                    (funcall
                     (plist-get options :callback)
                     '(data (node
                             (files
                              (nodes ((path . "last.el")
                                      (viewerViewedState . "UNVIEWED")))
                              (pageInfo (hasNextPage . nil)))))))))
          (code-review-pullreq-infos
           (code-review-github-repo :owner "owner" :repo "repo" :number 1)
           nil (lambda (res) (setq result res)))
          (should (= calls 2))
          (should (equal
                   (mapcar (lambda (file) (a-get file 'path))
                           (a-get-in (code-review-github--graphql-data result)
                                     '(repository pullRequest files nodes)))
                   '("first.el" "last.el"))))
      (fset 'ghub-query original))))

(ert-deftest code-review-github-test/viewed-page-error-keeps-fetched-subset ()
  "A continuation error preserves fetched states rather than clearing unseen files."
  (let ((original (symbol-function 'ghub-query))
        (code-review-auth-login-marker 'test)
        (response (code-review-github-test--files-response
                   '(((path . "first.el") (viewerViewedState . "VIEWED")))
                   t "cursor-1"))
        result)
    (unwind-protect
        (progn
          (fset 'ghub-query
                (lambda (_query _vars &rest options)
                  (funcall (plist-get options :errorback) 'offline)))
          (code-review-github--continue-files
           response (lambda (res) (setq result res)))
          (should (equal result response)))
      (fset 'ghub-query original))))

(ert-deftest code-review-github-test/viewed-page-repeated-cursor-stops ()
  "Pagination cannot loop forever if the server repeats a cursor."
  (let* ((original (symbol-function 'ghub-query))
         (code-review-auth-login-marker 'test)
         (response (code-review-github-test--files-response nil t "same"))
         (calls 0)
         result)
    (unwind-protect
        (progn
          (fset 'ghub-query
                (lambda (_query _vars &rest options)
                  (setq calls (1+ calls))
                  (funcall
                   (plist-get options :callback)
                   '(data (node (files (nodes)
                                       (pageInfo (hasNextPage . t)
                                                 (endCursor . "same"))))))))
          (code-review-github--continue-files
           response (lambda (res) (setq result res)))
          (should result)
          (should (= calls 1)))
      (fset 'ghub-query original))))

(ert-deftest code-review-github-test/viewed-query-fields-and-mutations ()
  "Queries use the file-node enum, and mutations use the PR node id and path."
  (dolist (template (list code-review-github-graphql-complete
                          code-review-github-graphql-fallback))
    (let ((query (format template "repo" "owner" 42)))
      (should (string-match-p "viewerViewedState" query))
      (should (string-match-p "pageInfo" query))
      (should-not (string-match-p "viewedEdges\\|;;\\|%s" query))))
  (let ((original (symbol-function 'ghub-query))
        (code-review-auth-login-marker 'test)
        (pr (code-review-github-repo :owner "owner" :repo "repo" :number 42))
        calls)
    (oset pr raw-infos '((id . "PR_NODE")))
    (unwind-protect
        (progn
          (fset 'ghub-query
                (lambda (query vars &rest options)
                  (push query calls)
                  (should (equal (a-get-in vars '(input pullRequestId))
                                 "PR_NODE"))
                  (should (equal (a-get-in vars '(input path)) "src/app.el"))
                  (should (eq (plist-get options :callback) #'ignore))
                  (should (eq (plist-get options :errorback) #'ignore))))
          (code-review-toggle-file-viewed pr "src/app.el" t #'ignore #'ignore)
          (code-review-toggle-file-viewed pr "src/app.el" nil #'ignore #'ignore)
          (should (string-match-p "unmarkFileAsViewed" (car calls)))
          (should (string-match-p "markFileAsViewed" (cadr calls))))
      (fset 'ghub-query original))))

;;; code-review-github-test.el ends here
