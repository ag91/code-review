;;; code-review-actions-test.el --- ERT tests for interactive actions -*- lexical-binding: t; -*-

;; Phase 6 (PR lifecycle): the local-review read-only guards must
;; fire before any DB or network access.

(require 'ert)
(require 'code-review-actions)
(require 'code-review-comment)

(ert-deftest code-review-actions-test/local-review-lifecycle-guards ()
  "Reopen, draft toggle and remote comment edit refuse local reviews."
  (let ((orig (symbol-function 'code-review-db-local-pr-p)))
    (unwind-protect
        (progn
          (fset 'code-review-db-local-pr-p (lambda () t))
          (should-error (code-review-reopen-pr) :type 'user-error)
          (should-error (code-review-toggle-pr-draft) :type 'user-error)
          (should-error (code-review-edit-remote-comment-at-point) :type 'user-error))
      (fset 'code-review-db-local-pr-p orig))))

;;; code-review-actions-test.el ends here
