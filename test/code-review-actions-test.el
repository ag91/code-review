;;; code-review-actions-test.el --- ERT tests for interactive actions -*- lexical-binding: t; -*-

;; Phase 6 (PR lifecycle): the local-review read-only guards must
;; fire before any DB or network access.

(require 'ert)
(require 'code-review-actions)
(require 'code-review-comment)
(require 'code-review-db)
(require 'code-review-test-helpers)

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

(ert-deftest code-review-actions-test/pr-description-insert ()
  "The popup content renders the PR title plus the body: plain
text inserted, HTML rendered, empty bodies get the fallback."
  (require 'code-review-local)
  (let ((pr (code-review-github-repo
             :owner "foo" :repo "bar" :number 1)))
    (oset pr state "OPEN")
    ;; plain text body
    (oset pr title "Fix pepper backfill")
    (oset pr raw-infos (a-alist 'bodyText "The backfill was wrong."))
    (with-temp-buffer
      (code-review-pr-description-insert pr)
      (should (string-match-p "Fix pepper backfill" (buffer-string)))
      (should (string-match-p "The backfill was wrong." (buffer-string))))
    ;; empty body: fallback message
    (oset pr raw-infos (a-alist 'bodyText "" 'bodyHTML ""))
    (with-temp-buffer
      (code-review-pr-description-insert pr)
      (should (string-match-p "No description provided."
                              (buffer-string))))
    ;; HTML body: rendered, not inserted raw (shr wraps at
    ;; code-review-fill-column, so allow line breaks)
    (oset pr raw-infos (a-alist 'bodyHTML "<p>Rendered from html</p>"))
    (with-temp-buffer
      (code-review-pr-description-insert pr)
      (should (string-match-p "Rendered[[:space:]\n]*from" (buffer-string)))
      (should-not (string-match-p "<p>" (buffer-string))))
    ;; local review: no forge description, the title is the intent
    (let ((local (code-review-local-diff
                  :owner "local" :repo "myrepo" :number 0 :url nil)))
      (oset local state "LOCAL")
      (oset local title "Commit 5dc7268 (add pepper)")
      (with-temp-buffer
        (code-review-pr-description-insert local)
        (should (string-match-p "Commit 5dc7268" (buffer-string)))
        (should (string-match-p "Local review: no forge description"
                                (buffer-string)))))))

(ert-deftest code-review-actions-test/popup-pr-description ()
  "The popup command renders the current PR's description and a
RECREATE refreshes the content for a new PR; with no PR at all
it user-errors instead of exploding."
  (code-review-test--with-db
    ;; no PR yet: friendly error, no signal
    (should-error (code-review-popup-pr-description)
                  :type 'user-error)
    ;; create a PR: popup shows its title and description
    (let ((pr (code-review-github-repo
               :owner "foo" :repo "bar" :number 1)))
      (oset pr state "OPEN")
      (oset pr title "Fix pepper backfill")
      (oset pr raw-infos (a-alist 'bodyText "The backfill was wrong."))
      (code-review-db--pullreq-create pr))
    (code-review-popup-pr-description)
    (should (string-match-p
             "Fix pepper backfill"
             (with-current-buffer code-review-pr-description-buffer-name
               (buffer-string))))
    (should (string-match-p
             "The backfill was wrong."
             (with-current-buffer code-review-pr-description-buffer-name
               (buffer-string))))
    ;; a new current PR: recreate (dismiss first, however it is
    ;; displayed) refreshes the content for it
    (let ((pr2 (code-review-github-repo
                :owner "foo" :repo "bar" :number 2)))
      (oset pr2 state "OPEN")
      (oset pr2 title "Second PR")
      (oset pr2 raw-infos (a-alist 'bodyText "Other intent."))
      (code-review-db--pullreq-create pr2))
    (let ((win (get-buffer-window code-review-pr-description-buffer-name)))
      (when win (quit-window nil win)))
    (code-review-popup-pr-description)
    (should (string-match-p
             "Other intent."
             (with-current-buffer code-review-pr-description-buffer-name
               (buffer-string))))
    (should-not (string-match-p
                 "The backfill was wrong."
                 (with-current-buffer code-review-pr-description-buffer-name
                   (buffer-string))))
    ;; toggle: with the popup's window visible the command DISMISSES it
    (let ((win (get-buffer-window code-review-pr-description-buffer-name)))
      (when win
        (code-review-popup-pr-description)
        (should-not (get-buffer-window
                     code-review-pr-description-buffer-name))
        ;; and a further call recreates it, content intact
        (code-review-popup-pr-description)
        (should (string-match-p
                 "Other intent."
                 (with-current-buffer code-review-pr-description-buffer-name
                   (buffer-string))))))
    (kill-buffer code-review-pr-description-buffer-name)))

;;; code-review-actions-test.el ends here
