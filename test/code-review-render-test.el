;;; code-review-render-test.el --- Review render safety tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'code-review)
(require 'code-review-render)
(require 'code-review-test-helpers)

(defconst code-review-render-test--diff
  "diff --git a/lib.el b/lib.el\n--- a/lib.el\n+++ b/lib.el\n@@ -1 +1 @@\n-old\n+new\n")

(defmacro code-review-render-test--with-review (&rest body)
  "Run BODY in a hermetic review with a disposable database."
  (declare (indent 0))
  `(code-review-test--with-db
     (let* ((name "*Code Review render safety test*")
            (code-review-history-enabled nil)
            (code-review-repo-enable nil)
            (code-review-post-hook nil)
            (code-review-sections-hook nil)
            (code-review-fold-hunks-by-default nil)
            (code-review-new-buffer-window-strategy #'ignore))
       (unwind-protect
           (progn
             (code-review-db--pullreq-create
              (code-review-github-repo :owner "test" :repo "test" :number "1"))
             (code-review-db--pullreq-raw-diff-update code-review-render-test--diff)
             ,@body)
         (when-let* ((buffer (get-buffer name)))
           (kill-buffer buffer))))))

;;; req-2026-11-03-002: comment-rerender-preserves-readable-review
;; covers: code-review-section.el
;; covers-fn: code-review--trigger-hooks
;; enforcement: test
(ert-deftest code-review-render/req-2026-11-03-002-comment-rerender ()
  "WHEN a reviewer saves a comment THE SYSTEM SHALL keep the refreshed review readable."
  (code-review-render-test--with-review
    (let* ((mode-calls 0)
           (code-review-mode-hook
            (list (lambda ()
                    (setq mode-calls (1+ mode-calls))
                    ;; A supported Emacs cleanup hook must not run on new text.
                    (add-hook 'change-major-mode-hook #'font-lock-defontify nil t)))))
      (code-review--trigger-hooks name)
      (code-review-db--pullreq-raw-comments-update
       '((state . "LOCAL COMMENT") (author (login . "me"))
         (comments (nodes ((internal-id . "draft")
                            (bodyText . "please check")
                            (path . "lib.el") (line . 1)
                            (side . "RIGHT") (local? . t))))))
      (dotimes (_ 3)
        (code-review--trigger-hooks name)
        (with-current-buffer name
          (goto-char (point-min))
          (search-forward "+new")
          (should (get-text-property (1- (point)) 'font-lock-face))
          (goto-char (oref magit-root-section end))
          (should (looking-at-p "[[:space:]]*\\'"))))
      (should (= mode-calls 1)))))

;;; req-2026-11-03-003: failed-render-keeps-previous-review
;; covers: code-review-render.el, code-review-section.el
;; covers-fn: code-review-render--call, code-review--trigger-hooks
;; enforcement: test
(ert-deftest code-review-render/req-2026-11-03-003-failed-render-keeps-review ()
  "WHEN a refresh fails THE SYSTEM SHALL retain the previous usable review."
  (code-review-render-test--with-review
    (code-review--trigger-hooks name)
    (with-current-buffer name
      (let* ((text (buffer-string))
             (root magit-root-section)
             (end (marker-position (oref root end)))
             (ov (make-overlay 1 4))
             (position 3))
        (overlay-put ov 'invisible t)
        (goto-char position)
        (cl-letf (((symbol-function 'code-review-section--fold-and-mark)
                   (lambda (&rest _) (error "Injected render failure"))))
          (should-error (code-review--trigger-hooks name)))
        (should (equal-including-properties text (buffer-string)))
        (should (eq root magit-root-section))
        (should (eq (marker-buffer (oref root end)) (current-buffer)))
        (should (= (marker-position (oref root end)) end))
        (should (= (overlay-start ov) 1))
        (should (= (overlay-end ov) 4))
        (should (= (point) position))
        ;; Inherited visibility must also survive a successful retry.
        (code-review--trigger-hooks name)
        (goto-char (oref magit-root-section end))
        (should (looking-at-p "[[:space:]]*\\'"))))))

(ert-deftest code-review-render/unwashed-tail-restores-review ()
  (code-review-render-test--with-review
    (code-review--trigger-hooks name)
    (let ((text (with-current-buffer name (buffer-string))))
      (cl-letf (((symbol-function 'code-review-wash-diff) (lambda () nil)))
        (should-error (code-review--trigger-hooks name)))
      (with-current-buffer name
        (should (equal-including-properties text (buffer-string)))))))

(ert-deftest code-review-render/folding-preserves-wash-position ()
  (with-temp-buffer
    (magit-section-mode)
    (let ((inhibit-read-only t)
          (child nil))
      (magit-insert-section (root)
        (setq child
              (magit-insert-section (child nil t)
                (magit-insert-heading "heading")
                (insert "body\n"))))
      (goto-char (1+ (marker-position (oref child content))))
      (let ((position (point)))
        (code-review-section--hide-if-hidden child)
        (should (= (point) position))
        (should (get-char-property position 'invisible))))))

(ert-deftest code-review-render/rollback-on-quit ()
  (with-temp-buffer
    (insert "previous")
    (should
     (eq (condition-case nil
             (code-review-render--call (lambda () (insert "partial") (signal 'quit nil)))
           (quit 'cancelled))
         'cancelled))
    (should (equal (buffer-string) "previous"))))

(ert-deftest code-review-render/stalled-washer-aborts ()
  (with-temp-buffer
    (insert "unconsumed\n")
    (goto-char (point-min))
    (should-error
     (code-review-render--wash-sequence (lambda () "incorrect success")))))

(ert-deftest code-review-render/wash-stop-is-not-an-error ()
  (with-temp-buffer
    (insert "unconsumed\n")
    (goto-char (point-min))
    (code-review-render--wash-sequence (lambda () nil))
    (should (= (point) (point-min)))))

(ert-deftest code-review-render/wash-can-consume-by-deletion ()
  (with-temp-buffer
    (insert "input")
    (goto-char (point-min))
    (code-review-render--wash-sequence
     (lambda () (delete-char 1) t))
    (should (eobp))))

(defun code-review-render-test--save-comment (name line)
  "Save a comment on LINE through the composer into review buffer NAME."
  (let* ((code-review-comment-buffer-name "*Code Review missing-anchor draft*")
         (buffer (get-buffer-create code-review-comment-buffer-name))
         (code-review-comment-description? nil)
         (code-review-comment-title? nil)
         (code-review-comment-feedback? nil)
         (code-review-promote-comment-to-issue? nil)
         (code-review-comment-send? nil)
         (code-review-comment-single-comment? nil)
         (code-review-comment-commit-buffer? nil)
         (code-review-comment-suggestion? nil)
         (code-review-comment-cursor-pos nil)
         (code-review-section-full-refresh? nil)
         (code-review-comment-uncommitted
          (code-review-local-comment-section
           :state "LOCAL COMMENT" :author "me" :path "lib.el"
           :line line :side "RIGHT" :line-type "ADDED" :send? nil)))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (insert "Keep this unanchored draft")
            (cl-letf (((symbol-function 'code-review-pr-buffer-name)
                       (lambda (&rest _) name)))
              (code-review-comment-commit)))
          (should-not (buffer-live-p buffer)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

;;; req-2026-11-03-007: missing-anchor-comment-save-keeps-review-usable
;; covers: code-review-section-comment.el, code-review-section-wash.el
;; covers-fn: code-review-section-insert-outdated-comment-missing
;; enforcement: test
(ert-deftest code-review-render/req-2026-11-03-007-save-missing-anchor ()
  "WHEN a saved comment no longer matches a diff line THE SYSTEM SHALL retain it without breaking the review."
  (code-review-render-test--with-review
    (code-review-db--pullreq-raw-diff-update
     (concat code-review-render-test--diff
             "diff --git a/next.el b/next.el\n"
             "--- a/next.el\n+++ b/next.el\n@@ -1 +1 @@\n-before\n+after\n"))
    (code-review--trigger-hooks name)
    (code-review-render-test--save-comment name 99)
    (code-review-render-test--save-comment name 100)
    (with-current-buffer name
      (let ((files nil))
        (magit-map-sections
         (lambda (section)
           (when (eq (oref section type) 'file)
             (push (oref section value) files))))
        (should (equal (nreverse files) '("b/lib.el" "b/next.el"))))
      (goto-char (point-min))
      (search-forward "Keep this unanchored draft")
      (search-forward "+after")
      (should (eq (get-text-property (1- (point)) 'font-lock-face)
                  'magit-diff-added)))
    (should (= (length (code-review-db--pullreq-raw-comments)) 2))))

(ert-deftest code-review-render/save-missing-anchor-at-end ()
  (code-review-render-test--with-review
    (code-review--trigger-hooks name)
    (code-review-render-test--save-comment name 99)
    (with-current-buffer name
      (goto-char (point-min))
      (search-forward "+new")
      (should (eq (get-text-property (1- (point)) 'font-lock-face)
                  'magit-diff-added))
      (search-forward "Keep this unanchored draft")
      (should-not (search-forward "diff --git" nil t)))
    (should (= (length (code-review-db--pullreq-raw-comments)) 1))))

(ert-deftest code-review-render/missing-outdated-hunk-cannot-wash-next-file ()
  (code-review-render-test--with-review
    (code-review-db--pullreq-raw-diff-update
     (concat code-review-render-test--diff
             "diff --git a/next.el b/next.el\n"
             "--- a/next.el\n+++ b/next.el\n@@ -1 +1 @@\n-before\n+after\n"))
    (code-review-db--pullreq-raw-comments-update
     '((state . "COMMENTED") (author (login . "reviewer"))
       (comments (nodes ((databaseId . 42) (path . "lib.el")
                         (originalPosition . 99) (outdated . t)
                         (bodyHTML . "<p>Historical review note</p>")
                         (body . "Historical review note")
                         ;; No trailing newline: the nested washer must not
                         ;; join this last line to the next diff header.
                         (diffHunk . "@@ -5 +5 @@\n-old context\n+obsolete"))))))
    (dotimes (_ 2)
      (code-review--trigger-hooks name)
      (with-current-buffer name
        (goto-char (point-min))
        (re-search-forward "Historical[[:space:]\n]+review[[:space:]\n]+note")
        (let ((section (magit-current-section)))
          (while (and section (not (eq (oref section type) 'file)))
            (setq section (oref section parent)))
          (should section)
          (should (equal (oref section value) "b/lib.el")))
        (search-forward "+after")
        (should (eq (get-text-property (1- (point)) 'font-lock-face)
                    'magit-diff-added))))))

;;; req-2026-11-03-004: failed-comment-save-keeps-draft
;; covers: code-review-comment.el
;; covers-fn: code-review-comment-commit
;; enforcement: test
(ert-deftest code-review-render/req-2026-11-03-004-failed-save-keeps-draft ()
  "WHEN saving a comment fails THE SYSTEM SHALL keep its text available for retry."
  (code-review-test--with-db
    (let* ((code-review-comment-buffer-name "*Code Review safety draft*")
           (buffer (get-buffer-create code-review-comment-buffer-name))
           (code-review-comment-description? nil)
           (code-review-comment-title? nil)
           (code-review-comment-feedback? nil)
           (code-review-promote-comment-to-issue? nil)
           (code-review-comment-send? nil)
           (code-review-comment-single-comment? nil)
           (code-review-comment-uncommitted (code-review-local-comment-section))
           (closed nil))
      (unwind-protect
          (progn
            (code-review-db--pullreq-create
             (code-review-github-repo :owner "test" :repo "test" :number "1"))
            (with-current-buffer buffer
              (insert "Do not lose this draft")
              (cl-letf (((symbol-function 'code-review-comment-handler-commit)
                         (lambda (&rest _) (error "Injected save failure")))
                        ((symbol-function 'delete-window)
                         (lambda (&rest _) (setq closed t)))
                        ((symbol-function 'delete-frame)
                         (lambda (&rest _) (setq closed t))))
                (should-error (code-review-comment-commit))))
            (should-not closed)
            (should (buffer-live-p buffer))
            (with-current-buffer buffer
              (should (equal (buffer-string) "Do not lose this draft"))))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

;;; req-2026-11-05-001: shr-render-failure-degrades-not-kills
;; covers: code-review-section-shared.el
;; covers-fn: code-review--insert-html
;; covers-fn: code-review--html-written-loc
;; enforcement: test
(ert-deftest code-review-render/req-2026-11-05-001-sub-at-buffer-start-renders ()
  "WHEN a comment body's first element is a <sub> THE SYSTEM SHALL render \
it without signaling: shr's own `shr-tag-sub' does (forward-char -1) at bol, \
which is (beginning-of-buffer) at the start of the narrowing \
(the 2026-11-05 litellm#50080 greptile-badge render kill)."
  (let ((body (concat "<p dir=\"auto\"><strong><sub><sub>"
                      "<a href=\"https://camo.example/badge\">"
                      "<img src=\"https://camo.example/badge.svg\" "
                      "alt=\"P2 Badge\"></a></sub></sub>  Some finding</strong></p>"
                      "<p dir=\"auto\">Useful? React with \U0001F44D.</p>")))
    (with-temp-buffer
      (code-review--insert-html body)
      (should (> (buffer-size) 10))
      ;; shr wraps the fill column between words (the AGENTS.md
      ;; shr-width gotcha): never assert on a phrase with a space
      (should (string-match-p
               "Some[[:space:]\n]*finding" (buffer-string)))))
  ;; the line estimator takes the same path and must not signal either
  (with-temp-buffer
    (should (numberp (code-review--html-written-loc
                      "<p><strong><sub><sub>x</sub></sub>y</strong></p>")))))

(ert-deftest code-review-render/req-2026-11-05-001-shr-crash-degrades-to-raw-text ()
  "WHEN shr signals on a comment body THE SYSTEM SHALL log the failure \
and insert the raw body as plain text instead of killing the render."
  (let ((body "<p>particular body</p>"))
    (with-temp-buffer
      (cl-letf (((symbol-function 'shr-insert-document)
                 (lambda (&rest _) (error "Beginning of buffer"))))
        (code-review--insert-html body)
        (should (string-match-p "particular body" (buffer-string)))))))

;;; req-2026-11-04-001: displayed-comment-save-preserves-review
;; covers: code-review-section-shared.el
;; covers-fn: code-review--html-written-loc
;; enforcement: test
(ert-deftest code-review-render/req-2026-11-04-001-html-estimate-preserves-point ()
  "WHEN a displayed review renders a comment THE SYSTEM SHALL preserve the surrounding review."
  (let ((review (generate-new-buffer "*Code Review HTML measurement test*"))
        (shr-use-fonts t))
    (unwind-protect
        (save-window-excursion
          (let ((window (split-window-right)))
            (set-window-buffer window review)
            (with-current-buffer review
              (insert (make-string 200 ?x))
              (set-window-point window 10)
              (goto-char 150)
              (code-review--html-written-loc
               "<table><tr><td>A comment table</td></tr></table>" 6)
              (should (= (point) 150)))))
      (kill-buffer review))))

(ert-deftest code-review-render/save-with-displayed-outdated-html ()
  (code-review-render-test--with-review
    (code-review--trigger-hooks name)
    (code-review-db--pullreq-raw-comments-update
     '((state . "COMMENTED") (author (login . "reviewer"))
       (comments (nodes ((databaseId . 42) (path . "lib.el")
                         (position . 1) (outdated . t)
                         (bodyHTML . "<table><tr><td>Historical table</td></tr></table>")
                         (body . "Historical table")
                         (diffHunk . "@@ -1 +1 @@\n-old\n+new\n"))))))
    (let ((shr-use-fonts t))
      (save-window-excursion
        (let ((window (split-window-right)))
          (set-window-buffer window (get-buffer name))
          (set-window-point window 1)
          (code-review-render-test--save-comment name 1)
          (with-current-buffer name
            (goto-char (point-min))
            (search-forward "Keep this unanchored draft")
            (should (search-backward "Historical" nil t))
            (goto-char (point-max))
            (search-backward "+new")
            (should (get-text-property (1+ (point)) 'font-lock-face))))))))

;;; code-review-render-test.el ends here
