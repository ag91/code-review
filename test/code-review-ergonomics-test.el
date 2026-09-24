;;; code-review-ergonomics-test.el --- ERT tests for phase 9 review ergonomics -*- lexical-binding: t; -*-

;; Phase 9: eldoc context, global section folding, fringe thread
;; markers.  Pure-function and wash-environment tests; no forge
;; access.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'magit-section)
(require 'code-review-db)
(require 'code-review-github)
(require 'code-review-section)
(require 'code-review-actions)
(require 'code-review-test-helpers)

(defconst code-review-ergonomics-test--diff-text
  (concat
   "diff --git a/src/lib.py b/src/lib.py\n"
   "index 111..222 100644\n"
   "--- a/src/lib.py\n"
   "+++ b/src/lib.py\n"
   "@@ -1,3 +1,4 @@\n"
   " context line\n"
   "-removed line\n"
   "+added line\n")
  "One file, one hunk @@ -1,3 +1,4 @@ with one line of each kind.")

(defun code-review-ergonomics-test--sample-pr-obj ()
  "Return a fresh sample pullreq object.
Rebuilt per test: closql writes through `oset'."
  (code-review-github-repo
   :owner "owner"
   :repo "repo"
   :number "num"))

(defmacro code-review-ergonomics-test--with-washed-diff (&rest body)
  "Run BODY in a temp buffer with the sample diff fully washed.
The section tree is rooted the same way the real render roots it:
root > files-chnged > file > hunk."
  (declare (indent 0))
  `(code-review-test--with-db
     (code-review-db--pullreq-create
      (code-review-ergonomics-test--sample-pr-obj))
     (with-temp-buffer
       (magit-section-mode)
       (let ((inhibit-read-only t))
         (insert code-review-ergonomics-test--diff-text)
         (goto-char (point-min))
         (magit-insert-section (code-review--root-section)
           (magit-insert-section (code-review-files-chnged)
             (save-restriction
               (narrow-to-region (point) (point-max))
               (magit-wash-sequence #'code-review-wash-diff)))))
       ,@body)))

(defun code-review-ergonomics-test--find-section (type)
  "Find the first washed section of magit TYPE in the current buffer."
  (let ((found nil))
    (magit-map-sections
     (lambda (s)
       (when (and (not found) (eq (oref s type) type))
         (setq found s))))
    found))

;;; Eldoc context

(ert-deftest code-review-ergonomics/eldoc-hunk-lines ()
  "Eldoc reports file, new-side line and line kind inside hunks."
  (code-review-ergonomics-test--with-washed-diff
    (goto-char (point-min))
    (search-forward " context line")
    (should (equal (code-review-eldoc-context) "src/lib.py:1"))
    (goto-char (point-min))
    (search-forward "-removed line")
    (should (equal (code-review-eldoc-context) "src/lib.py:2 (removed)"))
    (goto-char (point-min))
    (search-forward "+added line")
    (should (equal (code-review-eldoc-context) "src/lib.py:2 (added)"))))

(ert-deftest code-review-ergonomics/eldoc-outside-hunks ()
  "Eldoc reports the file on the file header, nil above the file tree."
  (code-review-ergonomics-test--with-washed-diff
    ;; the file heading line itself: point's section IS the file
    (goto-char (point-min))
    (search-forward "src/lib.py")
    (should (equal (code-review-eldoc-context) "src/lib.py"))
    ;; no file section at or above the root section
    (should-not (code-review-eldoc--file-section magit-root-section))))

;;; Global section folding

(ert-deftest code-review-ergonomics/fold-levels ()
  "Fold levels hide/show files and hunks globally."
  (code-review-ergonomics-test--with-washed-diff
    (let ((file (code-review-ergonomics-test--find-section 'file))
          (hunk (code-review-ergonomics-test--find-section 'hunk)))
      (should file)
      (should hunk)
      ;; level 2: file list folded
      (code-review-fold-show-level 2)
      (should (oref file hidden))
      ;; level 3: files expanded, hunks folded
      (code-review-fold-show-level 3)
      (should-not (oref file hidden))
      (should (oref hunk hidden))
      ;; level 4: hunks expanded
      (code-review-fold-show-level 4)
      (should-not (oref hunk hidden))
      ;; the cycle commands move one level at a time
      (code-review-fold-less)
      (should (= code-review-fold-level 3))
      (should (oref hunk hidden))
      (code-review-fold-more)
      (should (= code-review-fold-level 4))
      (should-not (oref hunk hidden)))))

;;; Fringe thread markers

(ert-deftest code-review-ergonomics/fringe-thread-marker ()
  "Comment sections lay a clickable fringe marker on their anchor line.
The wash inserts each thread right after its anchor diff line, so
the marker scan walks backwards from the section start."
  (code-review-ergonomics-test--with-washed-diff
    (goto-char (point-min))
    (search-forward "+added line")
    (let* ((anchor (line-beginning-position))
           (thread-pos (min (1+ (line-end-position)) (point-max)))
           (hunk (code-review-ergonomics-test--find-section 'hunk))
           (comment (code-review-base-comment-section
                     :state "COMMENTED" :author "a" :msg "m")))
      (oset comment parent hunk)
      (oset comment start (copy-marker thread-pos))
      ;; magit-insert-section normally initializes these slots; the
      ;; hand-built object must match or magit-section-show explodes
      (oset comment end (copy-marker thread-pos))
      (oset comment hidden nil)
      ;; attach to the tree so the real entry point sees it
      (oset hunk children (append (oref hunk children) (list comment)))
      (code-review--mark-comment-lines)
      (let ((marker-ovs (cl-remove-if-not
                         (lambda (ov) (overlay-get ov 'cr-comment-marker))
                         (overlays-at anchor))))
        (should (= (length marker-ovs) 1))
        (let ((ov (car marker-ovs)))
          (should (eq (overlay-get ov 'cr-comment-marker) t))
          (should (= (marker-position (overlay-get ov 'cr-thread-start))
                      thread-pos))
          (should (equal (get-text-property 0 'display
                                           (overlay-get ov 'before-string))
                         '(left-fringe code-review-comment-marker
                                       code-review-fringe-comment-face)))))
      ;; idempotent: re-running the walk keeps exactly one marker
      (code-review--mark-comment-lines)
      (should (= (length
                  (cl-remove-if-not
                   (lambda (ov) (overlay-get ov 'cr-comment-marker))
                   (overlays-at anchor)))
                 1))
      ;; mouse-1 style jump lands on the thread and reveals it
      (goto-char anchor)
      (code-review-fringe-jump-to-thread)
      (should (= (point) thread-pos)))))

(ert-deftest code-review-ergonomics/fringe-marker-no-hunk-parent ()
  "A comment section not inside a hunk gets no marker (no crash)."
  (code-review-ergonomics-test--with-washed-diff
    (let ((comment (code-review-base-comment-section
                    :state "COMMENTED" :author "a" :msg "m")))
      (oset comment start (copy-marker (point-min)))
      (should-not (code-review--comment-anchor-position comment))
      (code-review--mark-comment-line comment)
      (should (= 0 (length (cl-remove-if-not
                            (lambda (ov) (overlay-get ov 'cr-comment-marker))
                            (overlays-in (point-min) (point-max)))))))))

(provide 'code-review-ergonomics-test)
;;; code-review-ergonomics-test.el ends here
