;;; code-review-folding.el --- section folding for the review buffer -*- lexical-binding: t; -*-

;; The fold commands, extracted from code-review-actions.el
;; (the file-size guideline: actions.el was past 800 lines and
;; growing).  One coherent concern: the global level folds (phase
;; 9) and the per-file fold (2026-10-09).

;;; Code:

(require 'magit-section)
(require 'cl-lib)

(declare-function code-review-eldoc--file-section "code-review-actions")

(defvar-local code-review-fold-level nil
  "Current global fold level, nil when unset.
1 shows only the top-level container, 2 collapses files, 3
collapses hunks, 4 expands everything.  Managed by
`code-review-fold-less'/`code-review-fold-more'.")

(defun code-review-fold-show-level (level)
  "Set the global fold LEVEL for this review buffer.
The scale is by section KIND, not raw nesting depth, so it is
stable across layouts (the real render wraps files in TWO
containers, files-report and files-chnged, while tests use one):
5 everything visible, 4 threads folded, 3 hunks folded,
2 files folded, 1 only the top-level containers.
Bypasses magit's visibility cache so a fold never gets remembered
across renders."
  (interactive "nCode-review fold level (1 folded ... 5 expanded): ")
  (let ((magit-section-cache-visibility nil))
    (magit-map-sections
     (lambda (s)
       (let ((type (oref s type)))
         (oset s hidden
               ;; a kind is folded AT its fold level and below:
               ;; threads at 4, hunks at 3, files at 2, the
               ;; top-level containers at 1
               (or (and (< level 5)
                        (cl-typep s 'code-review-base-comment-section))
                   (and (< level 4) (eq type 'hunk))
                   (and (< level 3) (eq type 'file))
                   (and (< level 2)
                        (not (eq s magit-root-section))
                        (slot-boundp s 'parent)
                        (eq (oref s parent) magit-root-section)))))))
    (magit-section-show magit-root-section)
    (setq code-review-fold-level level)))

(defun code-review-fold-less ()
  "Fold one level more (\\[code-review-fold-less]).
5 = everything visible, 4 = threads folded behind hunks,
3 = files expanded but hunks folded, 2 = files folded, 1 = only
the top-level containers."
  (interactive)
  (code-review-fold-show-level (max 1 (1- (or code-review-fold-level 5)))))

(defun code-review-fold-more ()
  "Expand one level more (\\[code-review-fold-more]).
See `code-review-fold-less' for the level scale."
  (interactive)
  (code-review-fold-show-level (min 5 (1+ (or code-review-fold-level 5)))))

(defun code-review-fold-file-at-point ()
  "Toggle the file section at or around point.
S-<tab> from inside a hunk folds the WHOLE file: it collapses to
its heading line, a quick way to mark a file as reviewed and move
on; pressing it again expands it, and each hunk keeps its own
fold state.  Works from anywhere inside the file (hunk,
comment thread, heading); with point outside any file it falls
back to magit's global section cycle
(`magit-section-cycle-global').  Unlike the level folds this
keeps magit's visibility cache ON, so a file fold survives
re-renders — that persistence is the point: a reviewed-and-folded
file stays folded after a reload."
  (interactive)
  (let ((file (code-review-eldoc--file-section (magit-current-section))))
    (if file
        (if (oref file hidden)
            (magit-section-show file)
          (magit-section-hide file))
      (magit-section-cycle-global))))

(provide 'code-review-folding)
;;; code-review-folding.el ends here
