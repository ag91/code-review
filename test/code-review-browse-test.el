;;; code-review-browse-test.el --- ERT tests for code-review-browse -*- lexical-binding: t; -*-

(require 'ert)
(require 'a)
(require 'browse-url)
(require 'code-review-db)
(require 'code-review-github)
(require 'code-review-section)
(require 'code-review-browse)
(require 'code-review-test-helpers)

(defconst code-review-browse-test--diff-text
  (concat
   "diff --git a/src/app.py b/src/app.py\n"
   "index 111..222 100644\n"
   "--- a/src/app.py\n"
   "+++ b/src/app.py\n"
   "@@ -1,4 +1,4 @@\n"
   " keep one\n"
   "-removed old\n"
   "+added new\n"
   " keep two\n"
   "diff --git a/README.md b/README.md\n"
   "index 333..444 100644\n"
   "--- a/README.md\n"
   "+++ b/README.md\n"
   "@@ -1,3 +1,3 @@\n"
   " doc one\n"
   "-doc old\n"
   "+doc new\n")
  "Two-file diff with hunks of known line ranges for anchor tests.
app.py: old lines 1-4 (keep, removed, keep, and one missing),
new lines 1-4 (keep, added, keep, and one missing).")

(defun code-review-browse-test--sample-pr-obj ()
  "Return a fresh sample pullreq object (see section-test file)."
  (code-review-github-repo
   :owner "owner"
   :repo "repo"
   :number "num"))

(defmacro code-review-browse-test--with-section-env (&rest body)
  "Run BODY with a fresh test db and deterministic section variables."
  (declare (indent 0))
  `(code-review-test--with-db
     (code-review-db--pullreq-create
      (code-review-browse-test--sample-pr-obj))
     ,@body))

(defun code-review-browse-test--wash-into-buffer (diff-text)
  "Wash DIFF-TEXT into sections in the current temp buffer.
The buffer is prepared with `magit-section-mode' and a root
section, mirroring the real render structure.  The caller wraps
this in a temp buffer and the section env."
  (let ((inhibit-read-only t))
    (insert diff-text)
    (goto-char (point-min))
    (magit-insert-section (code-review--root-section)
      (magit-insert-section (code-review-files-chnged)
        (save-restriction
          (narrow-to-region (point) (point-max))
          (magit-wash-sequence #'code-review-wash-diff))))))

(defun code-review-browse-test--hash (path)
  "Convenience: the GitHub anchor hash for PATH."
  (secure-hash 'sha256 path))

;;; URL parsing

(ert-deftest code-review-browse-test/parse-fragment ()
  "GitHub anchor fragments become jump plists."
  (should (equal (code-review-browse--parse-fragment
                  "https://github.com/o/r/pull/1")
                 nil))
  (should (equal (code-review-browse--parse-fragment
                  "https://github.com/o/r/pull/1#issue-1")
                 nil))
  (should (equal (code-review-browse--parse-fragment
                  "https://github.com/o/r/pull/1#partial-timer")
                 nil))
  ;; file-only diff anchor
  (should (equal (code-review-browse--parse-fragment
                  "https://github.com/o/r/pull/1#diff-abc123")
                 '(:kind diff :hash "abc123")))
  ;; old-side line
  (should (equal (code-review-browse--parse-fragment
                  "https://github.com/o/r/pull/1#diff-abc123L42")
                 '(:kind diff :hash "abc123" :line 42 :side left)))
  ;; new-side line
  (should (equal (code-review-browse--parse-fragment
                  "https://github.com/o/r/pull/1#diff-abc123R50")
                 '(:kind diff :hash "abc123" :line 50 :side right)))
  ;; range anchor: the new side (R) is the useful target
  (should (equal (code-review-browse--parse-fragment
                  "https://github.com/o/r/pull/1#diff-abc123L42-R50")
                 '(:kind diff :hash "abc123" :line 50 :side right)))
  ;; comment anchors
  (should (equal (code-review-browse--parse-fragment
                  "https://github.com/o/r/pull/1#discussion_r987654")
                 '(:kind comment :id 987654)))
  (should (equal (code-review-browse--parse-fragment
                  "https://github.com/o/r/pull/1#issuecomment-99")
                 '(:kind comment :id 99)))
  (should (equal (code-review-browse--parse-fragment
                  "https://github.com/o/r/pull/1#pullrequestreview-77")
                 '(:kind comment :id 77))))

(ert-deftest code-review-browse-test/canonical-url ()
  "Fragments, queries and view suffixes are stripped."
  (should (equal (code-review-browse--canonical-url
                  "https://github.com/o/r/pull/762")
                 "https://github.com/o/r/pull/762"))
  (should (equal (code-review-browse--canonical-url
                  "https://github.com/o/r/pull/762#discussion_r1")
                 "https://github.com/o/r/pull/762"))
  (should (equal (code-review-browse--canonical-url
                  "https://github.com/o/r/pull/762/files#diff-abcL2")
                 "https://github.com/o/r/pull/762"))
  (should (equal (code-review-browse--canonical-url
                  "https://github.com/o/r/pull/762/commits/abc")
                 "https://github.com/o/r/pull/762"))
  (should (equal (code-review-browse--canonical-url
                  "https://gitlab.com/g/p/-/merge_requests/5/diffs")
                 "https://gitlab.com/g/p/-/merge_requests/5")))

(ert-deftest code-review-browse-test/buffer-name ()
  "Buffer names mirror `code-review-pr-buffer-name' derivation."
  (should (equal (code-review-browse--buffer-name
                  (code-review-utils-pr-from-url
                   "https://github.com/owner/repo/pull/762"))
                 "*Code Review: owner/repo#762*"))
  ;; gitlab nested subgroups are %2F-escaped in the alist
  (should (equal (code-review-browse--buffer-name
                  (code-review-utils-pr-from-url
                   "https://gitlab.com/owner/grp/sub/proj/-/merge_requests/1"))
                 "*Code Review: owner/grp/sub/proj#1*")))

(ert-deftest code-review-browse-test/handler-regexp ()
  "The registered handler regexp accepts PR links, rejects others."
  (dolist (url '("https://github.com/owner/repo/pull/1"
                 "http://github.com/owner/repo/pull/12"
                 "https://github.com/owner/repo/pull/762/files"
                 "https://github.com/owner/repo/pull/762#diff-abcL2"
                 "https://github.com/o/r/pull/42#discussion_r9"))
    (should (string-match-p code-review-browse-url-regexp url)))
  (dolist (url '("https://github.com/owner/repo/issues/5"
                 "https://github.com/owner/repo/pulls/5"
                 "https://gitlab.com/owner/repo/-/merge_requests/1"
                 "https://github.com/owner/pull/1"
                 "https://example.com/owner/repo/pull/1"))
    (should-not (string-match-p code-review-browse-url-regexp url))))

;;; Handler registration

(ert-deftest code-review-browse-test/registration ()
  "The browse handler is registered, idempotently, and removable."
  ;; the file registered itself when loaded
  (should (rassoc 'code-review-browse-url browse-url-default-handlers))
  ;; installing again must not duplicate the entry
  (code-review-browse-url-install)
  (should (= 1 (length (seq-filter
                        (lambda (e) (eq (cdr e) 'code-review-browse-url))
                        browse-url-default-handlers))))
  ;; uninstall removes every variant (autoloads vs load-time entry)
  (code-review-browse-url-uninstall)
  (should-not (rassoc 'code-review-browse-url browse-url-default-handlers))
  ;; leave the suite in the installed state
  (code-review-browse-url-install)
  (should (rassoc 'code-review-browse-url browse-url-default-handlers)))

;;; Anchor jumps (need a rendered section tree)

(ert-deftest code-review-browse-test/jump-to-file-anchor ()
  "A #diff-<hash> anchor lands on the matching file section."
  (code-review-browse-test--with-section-env
    (with-temp-buffer
      (magit-section-mode)
      (code-review-browse-test--wash-into-buffer
       code-review-browse-test--diff-text)
      (goto-char (point-min))
      (code-review-browse--jump-to-diff-anchor
       (list :kind 'diff :hash (code-review-browse-test--hash "src/app.py")))
      (should (string-match-p "src/app.py"
                              (thing-at-point 'line t)))
      ;; the other file is reachable through its own hash
      (code-review-browse--jump-to-diff-anchor
       (list :kind 'diff :hash (code-review-browse-test--hash "README.md")))
      (should (string-match-p "README.md" (thing-at-point 'line t)))
      ;; unknown hash: stay at top, no error
      (goto-char (point-min))
      (code-review-browse--jump-to-diff-anchor
       (list :kind 'diff :hash "deadbeef"))
      (should (bobp)))))

(ert-deftest code-review-browse-test/jump-to-truncated-hash ()
  "GitHub may truncate the path digest: prefix matching still works."
  (code-review-browse-test--with-section-env
    (with-temp-buffer
      (magit-section-mode)
      (code-review-browse-test--wash-into-buffer
       code-review-browse-test--diff-text)
      (let ((truncated (substring (code-review-browse-test--hash "src/app.py")
                                  0 40)))
        (code-review-browse--jump-to-diff-anchor
         (list :kind 'diff :hash truncated))
        (should (string-match-p "src/app.py" (thing-at-point 'line t)))))))

(ert-deftest code-review-browse-test/jump-to-line-anchor ()
  "#diff anchors with L/R lines land on the exact diff line."
  (code-review-browse-test--with-section-env
    (with-temp-buffer
      (magit-section-mode)
      (code-review-browse-test--wash-into-buffer
       code-review-browse-test--diff-text)
      ;; new side line 2: the added line
      (code-review-browse--jump-to-diff-anchor
       (list :kind 'diff
             :hash (code-review-browse-test--hash "src/app.py")
             :line 2 :side 'right))
      (should (string-match-p "^\\+added new" (thing-at-point 'line t)))
      ;; old side line 2: the removed line
      (code-review-browse--jump-to-diff-anchor
       (list :kind 'diff
             :hash (code-review-browse-test--hash "src/app.py")
             :line 2 :side 'left))
      (should (string-match-p "^-removed old" (thing-at-point 'line t)))
      ;; context line on the new side
      (code-review-browse--jump-to-diff-anchor
       (list :kind 'diff
             :hash (code-review-browse-test--hash "src/app.py")
             :line 1 :side 'right))
      (should (string-match-p "^ keep one" (thing-at-point 'line t))))))

(ert-deftest code-review-browse-test/jump-to-missing-line ()
  "A line inside the hunk range but not in the diff body falls back
to the nearest patch line (no error, no raw crash)."
  (code-review-browse-test--with-section-env
    (with-temp-buffer
      (magit-section-mode)
      (code-review-browse-test--wash-into-buffer
       code-review-browse-test--diff-text)
      ;; new-side line 4 is inside +1,4 but the sample hunk only
      ;; carries three content lines: fall back to first patch line
      (let ((pos (code-review-browse--line-position
                  (code-review-browse--find-file-section
                   (code-review-browse-test--hash "src/app.py"))
                  'right 4)))
        (should pos)
        (goto-char pos)
        (should (string-match-p "^ keep one" (thing-at-point 'line t))))
      ;; a line outside every hunk: no position at all
      (should-not (code-review-browse--line-position
                   (code-review-browse--find-file-section
                    (code-review-browse-test--hash "src/app.py"))
                   'right 99)))))

(ert-deftest code-review-browse-test/jump-to-comment-id ()
  "A #discussion_r/#issuecomment id lands on the comment section."
  (code-review-browse-test--with-section-env
    (with-temp-buffer
      (magit-section-mode)
      (let ((inhibit-read-only t))
        (code-review-db--pullreq-raw-infos-update
         `((comments (nodes ((author (login . "Code Review"))
                             (bodyHTML . "<p>Comment 1</p>")
                             (databaseId . 1234)
                             (createdAt . "2021-11-08T00:24:09Z"))))))
        (magit-insert-section (code-review--root-section)
          (magit-insert-section (code-review)
            (code-review-section-insert-top-level-comments))))
      (goto-char (point-min))
      (code-review-browse--jump-to-comment-id (list :kind 'comment :id 1234))
      ;; dual-role sections: the id lives on the value object
      (should (string-match-p "@Code Review" (thing-at-point 'line t)))
      (should (eq (code-review-browse--section-id (magit-current-section))
                  1234))
      ;; unknown id: stay at top, no error
      (goto-char (point-min))
      (code-review-browse--jump-to-comment-id (list :kind 'comment :id 4321))
      (should (bobp)))))

;;; Finding PR URLs in a buffer (email workflow)

(defconst code-review-browse-test--email-text
  (concat "From: CI <ci@corp.example>\n"
          "Subject: [widget] PR needs your review\n"
          "\n"
          "Please review "
          "https://github.com/acme/widget/pull/42#discussion_r123456 "
          "when you have a moment.\n"
          "Background: https://docs.example.com/intro\n")
  "One PR link buried in email prose, plus a non-PR link.")

(ert-deftest code-review-browse-test/urls-in-buffer ()
  "The email scan finds PR URLs and skips non-PR links."
  (with-temp-buffer
    (insert code-review-browse-test--email-text)
    ;; point is not on a URL: full-buffer scan finds the one PR link
    (goto-char (point-min))
    (should (equal (code-review-browse--url-in-buffer)
                   "https://github.com/acme/widget/pull/42#discussion_r123456"))
    ;; point on the URL: thing-at-point wins
    (goto-char (point-min))
    (search-forward "https://github.com/acme/widget/pull/42")
    (should (equal (code-review-browse--url-in-buffer)
                   "https://github.com/acme/widget/pull/42#discussion_r123456"))
    ;; the raw scan skips the docs link
    (should (equal (code-review-browse--urls-in-buffer)
                   '("https://github.com/acme/widget/pull/42#discussion_r123456")))))

(ert-deftest code-review-browse-test/urls-in-buffer-multiple ()
  "Several PR URLs are all collected, in order."
  (with-temp-buffer
    (insert "See https://github.com/acme/widget/pull/42 and "
            "https://gitlab.com/acme/grp/proj/-/merge_requests/7 "
            "and https://bitbucket.org/acme/team/pull-requests/9\n")
    (should (equal (code-review-browse--urls-in-buffer)
                   '("https://github.com/acme/widget/pull/42"
                     "https://gitlab.com/acme/grp/proj/-/merge_requests/7"
                     "https://bitbucket.org/acme/team/pull-requests/9")))))

(ert-deftest code-review-browse-test/no-url-error ()
  "`code-review-open-pr-at-point' fails loudly when there is no PR URL."
  (with-temp-buffer
    (insert "Just some prose, no links here.\n")
    (goto-char (point-min))
    (should-error (code-review-open-pr-at-point) :type 'user-error)))

;;; code-review-browse-test.el ends here
