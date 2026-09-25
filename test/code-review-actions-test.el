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

(ert-deftest code-review-actions-test/kill-comments-numbered-list ()
  "Comments become a numbered list, oldest first, with inline context."
  (let* ((alice (a-alist 'author (a-alist 'login "alice")
                         'body "top-level one\nsecond line"
                         'createdAt "2026-09-23T10:00:00Z"))
         (inline (a-alist 'body "inline note"
                         'bodyHTML "<p>i</p>"
                         'path "models/marts/_marts.yml"
                         'line 42
                         'outdated t
                         'createdAt "2026-09-23T11:30:00Z"))
         (resolved-note (a-alist 'body "this one is resolved"
                                 'bodyHTML "<p>r</p>"
                                 'path "models/marts/_marts.yml"
                                 'line 7
                                 'threadId "T_1"
                                 'isResolved t
                                 'createdAt "2026-09-23T12:00:00Z"))
         (bob (a-alist 'author (a-alist 'login "bob")
                       'body ""
                       'bodyHTML ""
                       'createdAt "2026-09-23T11:00:00Z"
                       'comments (a-alist 'nodes (list inline resolved-note))))
         (carol (a-alist 'author (a-alist 'login "carol")
                         'body "review summary"
                         'bodyHTML "<p>s</p>"
                         'createdAt "2026-09-24T09:00:00Z"
                         'comments (a-alist 'nodes nil)))
         (infos (a-alist 'title "Backfill pepper"
                         'comments (a-alist 'nodes (list alice))
                         'reviews (a-alist 'nodes (list bob carol))))
         (reviews (a-get-in infos '(reviews nodes)))
         (res (code-review-comments--numbered-list infos reviews))
         (text (car res))
         (open-res (code-review-comments--numbered-list infos reviews t))
         (open-text (car open-res)))
    ;; top-level + 2 inline + review summary; empty-bodied review
    ;; summaries are excluded (same filter as the conversation wash)
    (should (equal (cdr res) 4))
    ;; oldest first: alice, bob (inline), bob (resolved), carol
    (should (string-match-p "\\`1\\. alice (" text))
    (should (string-match-p
             "2\\. bob (.*) on models/marts/_marts\\.yml:42 \\[outdated\\]:\n   inline note"
             text))
    (should (string-match-p
             "3\\. bob (.*) on models/marts/_marts\\.yml:7:\n   this one is resolved"
             text))
    (should (string-match-p "4\\. carol (" text))
    ;; multiline body lines indent under the entry
    (should (string-match-p "1\\. alice (.*:\n   top-level one\n   second line" text))
    ;; open-only (C-u): resolved threads are skipped, everything
    ;; that is not a thread (top-level, summaries) stays
    (should (equal (cdr open-res) 3))
    (should-not (string-match-p "this one is resolved" open-text))
    (should (string-match-p "\\`1\\. alice (" open-text))
    (should (string-match-p "3\\. carol (" open-text))
    ;; raw-comments nil falls back to the reviews in raw-infos
    (should (equal (cdr (code-review-comments--numbered-list infos nil))
                   4))
    ;; no comments at all
    (should-not (code-review-comments--numbered-list
                 (a-alist 'comments (a-alist 'nodes nil)
                          'reviews (a-alist 'nodes nil))
                 nil))))

;;; code-review-actions-test.el ends here
