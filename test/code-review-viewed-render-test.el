;;; code-review-viewed-render-test.el --- Read tracking UI tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'code-review)
(require 'code-review-test-helpers)

(defun code-review-viewed-render-test--diff ()
  "Return a two-file diff, with two hunks in the first file."
  (concat "diff --git app.el app.el\n"
          "index 111..222 100644\n--- app.el\n+++ app.el\n"
          "@@ -1,2 +1,2 @@\n context\n-old\n+new\n"
          "@@ -5,2 +5,3 @@\n context\n+added\n"
          "diff --git other.el other.el\n"
          "index 333..444 100644\n--- other.el\n+++ other.el\n"
          "@@ -1 +1 @@\n-before\n+after\n"))

(defmacro code-review-viewed-render-test--with-review (&rest body)
  "Run BODY in a temporary LOCAL review with no background work.
The hunk default-fold is bound nil so the read-tracking
expectations stay independent of it (tests for the default fold
rebind it locally)."
  (declare (indent 0))
  `(code-review-test--with-db
     (code-review-db--pullreq-create
      (code-review-local-diff :owner "local" :repo "test" :number 0 :url nil))
     (let ((pr (code-review-db-get-pullreq)))
       (oset pr state "LOCAL")
       (code-review-db-update pr))
     (code-review-db--pullreq-raw-diff-update
      (code-review-viewed-render-test--diff))
     (with-temp-buffer
       (code-review-mode)
       (setq code-review-review-buffer-pr-id code-review-db--pullreq-id)
       (let ((code-review-fold-hunks-by-default nil))
         ,@body))))

(defun code-review-viewed-render-test--render (&optional commit-focus? news)
  "Render the fixture with optional COMMIT-FOCUS? and snapshot NEWS."
  (let ((inhibit-read-only t)
        (code-review-history-enabled nil)
        (code-review-section--new-since-last-review news)
        (code-review-collapse-bot-comments nil)
        (code-review-comment-fringe-markers nil)
        (fresh? (not magit-root-section)))
    (code-review-section--insert-pr-text commit-focus?)
    (goto-char (point-min))
    (magit-insert-section (code-review--root-section)
      (magit-insert-section (code-review-files-report-section)
        (code-review-section-insert-files-changed)
        (magit-insert-section (code-review-files-chnged)
          (save-restriction
            (narrow-to-region (point) (point-max))
            (magit-wash-sequence #'code-review-wash-diff)))))
    (code-review-section--fold-and-mark fresh? commit-focus?)))

(defun code-review-viewed-render-test--sections (class)
  "Return all rendered sections matching CLASS."
  (let (sections)
    (magit-map-sections
     (lambda (section)
       (when (cl-typep section class) (push section sections))))
    (nreverse sections)))

(defun code-review-viewed-render-test--file (path)
  "Return the rendered file section for canonical PATH."
  (cl-find-if
   (lambda (section)
     (equal path (plist-get (code-review-viewed--target-at section) :path)))
   (code-review-viewed-render-test--sections 'magit-file-section)))

(defun code-review-viewed-render-test--noise-diff ()
  "Return source, documentation and generated changes."
  (concat
   (code-review-viewed-render-test--diff)
   "diff --git README.md README.md\n"
   "index 111..222 100644\n--- README.md\n+++ README.md\n"
   "@@ -1 +1 @@\n-before\n+after\n"
   "diff --git yarn.lock yarn.lock\n"
   "index 111..222 100644\n--- yarn.lock\n+++ yarn.lock\n"
   "@@ -1 +1 @@\n-before\n+after\n"))

;;; req-2026-10-16-005:
;; covers: code-review-diff.el, code-review-viewed.el, code-review-section-wash.el, code-review-section-shared.el
;; covers-fn: code-review-diff--mark-new-files, code-review-viewed--apply-visibility, code-review-section--file-collapse-p
;; enforcement: test
(ert-deftest code-review-viewed-render-test/req-2026-10-16-005-new-docs-keep-reading-policy ()
  "WHEN documentation changes since the last review THE SYSTEM SHALL preserve its low-noise treatment and existing review context."
  (code-review-viewed-render-test--with-review
    (code-review-db--pullreq-raw-diff-update
     (code-review-viewed-render-test--noise-diff))
    (let ((news '((files . ("README.md" "yarn.lock" "app.el"))))
          (code-review-section--just-refetched t))
      (code-review-viewed-render-test--render nil news)
      (dolist (entry '(("README.md" . "DOC") ("yarn.lock" . "GEN")))
        (let* ((file (code-review-viewed-render-test--file (car entry)))
               (heading (buffer-substring-no-properties
                         (oref file start) (oref file content))))
          (should (string-match-p
                   (regexp-quote (format "[%s]  [NEW]" (cdr entry)))
                   heading))
          (should (get-char-property (oref file content) 'invisible))))
      (should-not (get-char-property
                   (oref (code-review-viewed-render-test--file "app.el") content)
                   'invisible))
      ;; The default is not a command to refold deliberate expansion.
      (magit-section-show (code-review-viewed-render-test--file "README.md"))
      (let ((code-review-section--just-refetched nil))
        (code-review-viewed-render-test--render nil news))
      (should-not
       (get-char-property
        (oref (code-review-viewed-render-test--file "README.md") content)
        'invisible)))))

(ert-deftest code-review-viewed-render-test/new-source-reopens-after-fetch ()
  "Changed source still reopens after a fetch when it was previously folded."
  (code-review-viewed-render-test--with-review
    (code-review-viewed-render-test--render)
    (magit-section-hide (code-review-viewed-render-test--file "app.el"))
    (let ((code-review-section--just-refetched t))
      (code-review-viewed-render-test--render nil '((files . ("app.el")))))
    (should-not
     (get-char-property
      (oref (code-review-viewed-render-test--file "app.el") content)
      'invisible))))

(ert-deftest code-review-viewed-render-test/new-docs-with-comments-remain-readable ()
  "Changed documentation does not hide an attached review discussion."
  (code-review-viewed-render-test--with-review
    (code-review-db--pullreq-raw-diff-update
     (code-review-viewed-render-test--noise-diff))
    (code-review-db--pullreq-raw-infos-update '((author (login . "author"))))
    (let* ((comment (code-review-code-comment-section
                     :author "alice" :state "COMMENTED"
                     :msg "<p>Keep this discussion visible</p>"
                     :path "README.md" :position 1 :id 20261016005
                     :createdAt "2026-10-16T00:00:00Z" :reactions nil))
           (code-review-section-grouped-comments
            (list (cons (code-review-utils--comment-key "README.md" 1)
                        (list comment))))
           (code-review-section-hold-written-comment-count nil)
           (code-review-section-hold-written-comment-ids nil)
           (code-review-section--just-refetched t))
      (code-review-viewed-render-test--render nil '((files . ("README.md"))))
      (should-not
       (get-char-property
        (oref (code-review-viewed-render-test--file "README.md") content)
        'invisible))
      (goto-char (point-min))
      (should (search-forward "discussion" nil t))
      (should-not (get-char-property (1- (point)) 'invisible)))))

(ert-deftest code-review-viewed-render-test/new-preserves-heading-annotations ()
  "NEW adds to the heat, CI and incident annotations rather than replacing them."
  (code-review-viewed-render-test--with-review
    (let ((ci-tag (symbol-function 'code-review-testimpact--file-tag-for))
          (incident-tag (symbol-function 'code-review-registry--file-tag))
          (code-review-diff-file-order-rules
           '((:match "\\`app\\.el\\'" :tag "HOT"))))
      (unwind-protect
          (progn
            (fset 'code-review-testimpact--file-tag-for
                  (lambda (path) (equal path "app.el")))
            (fset 'code-review-registry--file-tag
                  (lambda (path) (when (equal path "app.el") "[2 incident(s)]")))
            (code-review-viewed-render-test--render nil '((files . ("app.el"))))
            (let ((file (code-review-viewed-render-test--file "app.el")))
              (should
               (string-match-p
                (regexp-quote "[HOT]  [NEW]  [CI-GAME]  [2 incident(s)]")
                (buffer-substring-no-properties
                 (oref file start) (oref file content))))))
        (fset 'code-review-testimpact--file-tag-for ci-tag)
        (fset 'code-review-registry--file-tag incident-tag)))))

;;; req-2026-10-23-001: hunks start folded
;; covers: code-review-section.el, code-review-folding.el
;; covers-fn: code-review-section--fold-hunks
;; enforcement: test
(ert-deftest code-review-viewed-render-test/req-2026-10-23-001-hunks-start-folded ()
  "WHEN a reviewer opens a review THE SYSTEM SHALL start the hunks folded so the affected files are scannable."
  (code-review-viewed-render-test--with-review
    (let ((code-review-fold-hunks-by-default t))
      (code-review-viewed-render-test--render))
    (let ((hunks (code-review-viewed-render-test--sections 'magit-hunk-section))
          (files (code-review-viewed-render-test--sections 'magit-file-section)))
      (should (= 2 (length files)))
      (should (= 3 (length hunks)))
      ;; every hunk folds, every file stays scannable
      (dolist (h hunks)
        (should (get-char-property (oref h content) 'invisible)))
      (dolist (f files)
        (should-not (oref f hidden)))
      ;; a deliberately expanded hunk survives a plain re-render: the
      ;; re-render runs with the fold ON (the real default) so the
      ;; folded hunks keep their invisible overlay while the expanded
      ;; one stays open
      (magit-section-show (car hunks))
      (let ((code-review-fold-hunks-by-default t))
        (code-review-viewed-render-test--render))
      (should-not (get-char-property
                   (oref (car (code-review-viewed-render-test--sections
                               'magit-hunk-section)) content)
                   'invisible))
      (should (get-char-property
               (oref (cadr (code-review-viewed-render-test--sections
                            'magit-hunk-section)) content)
               'invisible))
      ;; a refetch refolds the default state
      (let ((code-review-fold-hunks-by-default t)
            (code-review-section--just-refetched t))
        (code-review-viewed-render-test--render))
      (dolist (h (code-review-viewed-render-test--sections 'magit-hunk-section))
        (should (get-char-property (oref h content) 'invisible))))))

;;; req-2026-10-16-004:
;; covers: code-review-viewed.el, code-review-section.el, code-review.el
;; covers-fn: code-review-viewed-toggle, code-review-viewed--apply-visibility
;; enforcement: test
(ert-deftest code-review-viewed-render-test/req-2026-10-16-004-mark-and-unmark ()
  "WHEN a reviewer records progress THE SYSTEM SHALL retain it and distinguish unread changes."
  (code-review-viewed-render-test--with-review
    (let ((original (symbol-function 'code-review--build-buffer))
          (code-review-section-full-refresh? t)
          (renders 0))
      (unwind-protect
          (save-window-excursion
            (switch-to-buffer (current-buffer))
            (fset 'code-review--build-buffer
                  (lambda (name &rest _)
                    (should (equal name (buffer-name)))
                    (should-not code-review-section-full-refresh?)
                    (setq renders (1+ renders))
                    (code-review-viewed-render-test--render)))
            (code-review-viewed-render-test--render)
            (goto-char (oref (car (code-review-viewed-render-test--sections
                                  'magit-hunk-section)) start))
            (execute-kbd-macro (kbd "v"))
            (let* ((hunks (code-review-viewed-render-test--sections
                           'magit-hunk-section))
                   (files (code-review-viewed-render-test--sections
                           'magit-file-section)))
              (should (get-char-property (oref (car hunks) content) 'invisible))
              (should-not (get-char-property (oref (cadr hunks) content) 'invisible))
              (should-not (oref (car files) hidden)))
            ;; Completing the first file rolls it up and folds the whole file.
            (goto-char (oref (cadr (code-review-viewed-render-test--sections
                                   'magit-hunk-section)) start))
            (execute-kbd-macro (kbd "v"))
            (let ((file (car (code-review-viewed-render-test--sections
                              'magit-file-section))))
              (should (get-char-property (oref file content) 'invisible))
              (should (string-match-p "1/2 viewed" (buffer-string)))
              (goto-char (oref file start)))
            (execute-kbd-macro (kbd "C-u v"))
            (should-not (code-review-db--reviewed-rows))
            (should-not (oref (car (code-review-viewed-render-test--sections
                                   'magit-file-section)) hidden))
            (should (= renders 3)))
        (fset 'code-review--build-buffer original)))))

(ert-deftest code-review-viewed-render-test/partial-hunk-dim-and-fold ()
  "Only a viewed hunk dims and folds; the unread hunk stays visible."
  (code-review-viewed-render-test--with-review
    (code-review-db--reviewed-insert "app.el" "-1,2 +1,2")
    (code-review-viewed-render-test--render)
    (let ((hunks (code-review-viewed-render-test--sections 'magit-hunk-section)))
      (should (= 3 (length hunks)))
      (should (get-char-property (oref (car hunks) content) 'invisible))
      (should (eq (get-text-property (oref (car hunks) start) 'font-lock-face)
                  'code-review-dimmed))
      (should-not (get-char-property (oref (cadr hunks) content) 'invisible))
      (should-not (string-match-p "\\[VIEWED\\]" (buffer-string))))))

(ert-deftest code-review-viewed-render-test/hide-viewed-keeps-total-and-state ()
  "Hiding reviewed files keeps the original total, marks, and unread files."
  (code-review-viewed-render-test--with-review
    (code-review-db--reviewed-insert "app.el")
    (setq code-review-hide-viewed-files t)
    (code-review-viewed-render-test--render)
    (let ((files (code-review-viewed-render-test--sections 'magit-file-section)))
      (should (= 1 (length files)))
      (should (equal (oref (car files) value) "other.el"))
      (should (string-match-p "1/2 viewed" (buffer-string)))
      (should (string-match-p "viewed files hidden" (buffer-string)))
      (should (code-review-db--reviewed-file-p "app.el")))
    (setq code-review-hide-viewed-files nil)
    (code-review-viewed-render-test--render)
    (should (= 2 (length (code-review-viewed-render-test--sections
                         'magit-file-section))))
    ;; Hiding every file still leaves the heading and progress counter.
    (code-review-db--reviewed-insert "other.el")
    (setq code-review-hide-viewed-files t)
    (code-review-viewed-render-test--render)
    (should-not (code-review-viewed-render-test--sections 'magit-file-section))
    (should (string-match-p "2/2 viewed" (buffer-string)))))

(ert-deftest code-review-viewed-render-test/commit-focus-excludes-read-tracking ()
  "A commit view never inherits PR marks, hiding, or progress counters."
  (code-review-viewed-render-test--with-review
    (code-review-db--reviewed-insert "app.el")
    (setq code-review-hide-viewed-files t)
    (code-review-viewed-render-test--render t)
    (should (= 2 (length (code-review-viewed-render-test--sections
                         'magit-file-section))))
    (should-not code-review-section--viewed)
    (should-not (string-match-p "\\[VIEWED\\]\\|[0-9]/[0-9] viewed"
                                (buffer-string)))
    (let ((code-review-commit-minor-mode t))
      (should-error (code-review-viewed-toggle) :type 'user-error)
      (should-error (code-review-toggle-hide-viewed-files) :type 'user-error))))

(ert-deftest code-review-viewed-render-test/colliding-raw-and-canonical-paths ()
  "Raw prefixes never redirect tracking to a different canonical path."
  (code-review-viewed-render-test--with-review
    (code-review-db--pullreq-raw-diff-update
     (concat
      "diff --git a/app.el b/app.el\n"
      "index 111..222 100644\n--- a/app.el\n+++ b/app.el\n"
      "@@ -1 +1 @@\n-old\n+new\n"
      "diff --git a/b/app.el b/b/app.el\n"
      "index 333..444 100644\n--- a/b/app.el\n+++ b/b/app.el\n"
      "@@ -1 +1 @@\n-old\n+new\n"))
    (code-review-db--reviewed-insert "app.el")
    (code-review-viewed-render-test--render)
    (let ((files (code-review-viewed-render-test--sections 'magit-file-section)))
      (should (equal
               (mapcar (lambda (section)
                         (plist-get (code-review-viewed--target-at section) :path))
                       files)
               '("app.el" "b/app.el")))
      (should (get-char-property (oref (car files) content) 'invisible))
      (should-not (get-char-property (oref (cadr files) content) 'invisible)))))

(ert-deftest code-review-viewed-render-test/commit-paths-do-not-need-viewed-index ()
  "Canonical paths stay intact even when commit focus disables the viewed index."
  (code-review-viewed-render-test--with-review
    (code-review-db--pullreq-raw-diff-update
     "diff --git a/app.el a/app.el\nindex 111..222 100644\n--- a/app.el\n+++ a/app.el\n@@ -1 +1 @@\n-old\n+new\n")
    (code-review-viewed-render-test--render t)
    (let ((hunk (car (code-review-viewed-render-test--sections
                      'magit-hunk-section))))
      (should (equal (plist-get (code-review-viewed--target-at hunk) :path)
                     "a/app.el")))))

(ert-deftest code-review-viewed-render-test/viewed-file-expansion-survives-rerender ()
  "An ordinary rerender does not refold a deliberately expanded viewed file."
  (code-review-viewed-render-test--with-review
    (code-review-db--reviewed-insert "app.el")
    (code-review-viewed-render-test--render)
    (magit-section-show (car (code-review-viewed-render-test--sections
                              'magit-file-section)))
    (code-review-viewed-render-test--render)
    (should-not (get-char-property
                 (oref (car (code-review-viewed-render-test--sections
                             'magit-file-section)) content)
                 'invisible))))

;;; code-review-viewed-render-test.el ends here
