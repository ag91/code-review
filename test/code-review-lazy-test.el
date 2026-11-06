;;; code-review-lazy-test.el --- Lazy diff wash tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'code-review)
(require 'code-review-render)
(require 'code-review-lazy)
(require 'code-review-test-helpers)

(defconst code-review-lazy-test--small
  "diff --git a/lib.el b/lib.el\n--- a/lib.el\n+++ b/lib.el\n@@ -1 +1 @@\n-old\n+new\n")

(defun code-review-lazy-test--big-block (name hunks)
  "One raw diff block for NAME with HUNKS hunks, well above the lazy threshold."
  (concat (format "diff --git a/%s b/%s\n--- a/%s\n+++ b/%s\n" name name name name)
          (apply #'concat
                 (cl-loop for i below hunks
                          collect (format "@@ -%d +1 @@\n+%s\n"
                                          (1+ i) (make-string 40 ?x))))))

(defmacro code-review-lazy-test--with-review (&rest body)
  "Run BODY in a hermetic lazy review with a disposable database."
  (declare (indent 0))
  `(code-review-test--with-db
     (let* ((name "*Code Review lazy test*")
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
             (code-review-db--pullreq-raw-diff-update
              (concat code-review-lazy-test--small
                      (code-review-lazy-test--big-block "big.py" 400)))
             ,@body)
         (when-let* ((buffer (get-buffer name)))
           (kill-buffer buffer))))))

(defun code-review-lazy-test--drain (name)
  "Let the deferred chunk queue finish for review buffer NAME.
Batch emacs runs deferred callbacks only on timer ticks."
  (with-current-buffer name
    (let ((deadline (+ (float-time) 30)))
      (while (or code-review-lazy--queue deferred:queue)
        (when (> (float-time) deadline)
          (ert-fail "lazy chunks never drained"))
        (sit-for 0.05)))))

(defun code-review-lazy-test--counts ()
  "Return (FILES HUNKS) of the current buffer's section tree."
  (let ((files 0) (hunks 0))
    (magit-map-sections
     (lambda (section)
       (let ((type (oref section type)))
         (cond ((eq type 'file) (setq files (1+ files)))
               ((eq type 'hunk) (setq hunks (1+ hunks)))))))
    (list files hunks)))

;;; req-2026-11-05-001: opening-a-large-pr-shows-a-skeleton-instantly
;; covers: code-review-lazy.el
;; covers-fn: code-review-lazy--wash-files code-review-lazy--insert-placeholder
;; enforcement: test
(ert-deftest code-review-lazy/req-2026-11-05-001-skeleton-renders-without-big-hunks ()
  "WHEN a PR opens with a large file block THE SYSTEM SHALL render the skeleton without washing that file's hunks synchronously."
  (code-review-lazy-test--with-review
    (code-review--trigger-hooks name)
    (with-current-buffer name
      (goto-char (point-min))
      ;; the big file's placeholder heading is there, tagged and sized
      (should (search-forward "big.py" nil t))
      (goto-char (match-beginning 0))
      (should (get-text-property (point) 'code-review-lazy))
      (should (search-backward "modified" nil t))
      ;; the placeholder carries the canonical path provenance
      (goto-char (point-min))
      (search-forward "big.py")
      (should (get-text-property (match-beginning 0) 'code-review-path))
      ;; NO big hunk text was painted during the synchronous phase
      (goto-char (point-min))
      (should-not (search-forward "+xxxx" nil t))
      ;; the small file washed inline with painted faces
      (goto-char (point-min))
      (search-forward "+new")
      (should (eq (get-text-property (1- (point)) 'font-lock-face)
                  'magit-diff-added))
      ;; one placeholder queued for the deferred loader
      (should (= (length code-review-lazy--queue) 1)))))

(ert-deftest code-review-lazy/chunks-load-after-render ()
  (code-review-lazy-test--with-review
    (code-review--trigger-hooks name)
    (with-current-buffer name (code-review-lazy-test--drain name))
    (with-current-buffer name
      ;; placeholder replaced by the real file section: 2 files,
      ;; 400 big hunks + the small file's 1
      (should (equal (code-review-lazy-test--counts) (list 2 401)))
      (goto-char (point-min))
      (should (search-forward "+xxxx" nil t))
      (should (get-text-property (1- (point)) 'font-lock-face))
      (goto-char (point-min))
      (should-not (text-property-any
                   (point-min) (point-max) 'code-review-lazy t)))))

(ert-deftest code-review-lazy/toggle-loads-placeholder-on-demand ()
  (code-review-lazy-test--with-review
    (code-review--trigger-hooks name)
    ;; kill the background queue: only the TAB path may load
    (with-current-buffer name (code-review-lazy--invalidate))
    (with-current-buffer name
      (goto-char (point-min))
      (search-forward "big.py")
      (goto-char (match-beginning 0))
      (should (code-review-lazy-placeholder-section-p
               (magit-current-section)))
      (magit-section-toggle (magit-current-section))
      (goto-char (point-min))
      (should (search-forward "+xxxx" nil t))
      (should-not (text-property-any
                   (point-min) (point-max) 'code-review-lazy t))
      (should (equal (code-review-lazy-test--counts) (list 2 401))))))

;;; req-2026-11-05-002: failed-render-cancels-pending-lazy-chunks
;; covers: code-review-render.el, code-review-lazy.el
;; covers-fn: code-review-lazy--render-call
;; enforcement: test
(ert-deftest code-review-lazy/req-2026-11-05-002-rollback-cancels-pending-chunks ()
  "WHEN a re-render fails THE SYSTEM SHALL cancel pending lazy chunks so they never wash into the rolled-back buffer."
  (code-review-lazy-test--with-review
    (code-review--trigger-hooks name)
    (with-current-buffer name
      (let ((text (buffer-string)))
        (cl-letf (((symbol-function 'code-review-section--fold-and-mark)
                   (lambda (&rest _) (error "Injected render failure"))))
          (should-error (code-review--trigger-hooks name)))
        (code-review-lazy-test--drain name)
        ;; the rollback held AND no deferred chunk touched the text
        (should (equal-including-properties text (buffer-string)))))))

(ert-deftest code-review-lazy/comments-land-in-lazily-loaded-files ()
  (code-review-lazy-test--with-review
    (code-review--trigger-hooks name)
    (with-current-buffer name (code-review-lazy-test--drain name))
    (code-review-db--pullreq-raw-comments-update
     '((state . "LOCAL COMMENT") (author (login . "me"))
       (comments (nodes ((internal-id . "draft")
                          (bodyText . "please check the big file")
                          (path . "big.py") (line . 9999)
                          (side . "RIGHT") (local? . t))))))
    ;; a re-render re-queues placeholders; the comment inserts when the
    ;; big file's block loads (the missing-anchor fallback)
    (code-review--trigger-hooks name)
    (with-current-buffer name (code-review-lazy-test--drain name))
    (with-current-buffer name
      (goto-char (point-min))
      (search-forward "please check the big file")
      (should (equal (code-review-lazy-test--counts) (list 2 401))))))

(ert-deftest code-review-lazy/disabled-washes-everything-synchronously ()
  (code-review-lazy-test--with-review
    (let ((code-review-lazy-enabled nil))
      (code-review--trigger-hooks name)
      (with-current-buffer name
        (should (equal (code-review-lazy-test--counts) (list 2 401)))
        (goto-char (point-min))
        (should-not (text-property-any
                     (point-min) (point-max) 'code-review-lazy t))))))

(ert-deftest code-review-lazy/failed-load-restores-placeholder ()
  (code-review-lazy-test--with-review
    (code-review--trigger-hooks name)
    (with-current-buffer name (code-review-lazy--invalidate))
    (with-current-buffer name
      (goto-char (point-min))
      (search-forward "big.py")
      (let ((section (magit-current-section)))
        ;; inject a wash failure: the placeholder must come back intact
        (cl-letf (((symbol-function 'code-review-wash-hunk)
                   (lambda (&rest _) (error "Injected wash failure"))))
          (should-not (code-review-lazy--expand-at section)))
        (goto-char (point-min))
        (should (search-forward "big.py" nil t))
        (goto-char (match-beginning 0))
        (should (get-text-property (point) 'code-review-lazy))
        ;; the queue no longer holds it, but a TAB retry loads it
        (magit-section-toggle (magit-current-section))
        (goto-char (point-min))
        (should (search-forward "+xxxx" nil t))))))

;;; code-review-lazy-test.el ends here
