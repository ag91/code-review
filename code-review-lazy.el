;;; code-review-lazy.el --- Wash the diff lazily, one file block at a time -*- lexical-binding: t; -*-

;;
;; Copyright (C) 2026 Andrea <andrea-dev@hotmail.com>
;;
;; Author: Andrea <andrea-dev@hotmail.com>
;; Keywords: git, tools, vc
;; Homepage: https://github.com/wandersoncferreira/code-review
;;
;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Phase 30: the diff wash used to be synchronous over the WHOLE raw
;; diff, so opening a large PR blocked Emacs.  The file block is the
;; lazy unit: blocks below `code-review-lazy-threshold' wash inline as
;; before; bigger blocks are replaced by a placeholder file section
;; (heading with classification tags and a size line, no hunks) whose
;; raw text is stored on the section.  A deferred chunk queue loads the
;; placeholders one file per tick after the render, and expanding a
;; placeholder (TAB) loads it immediately.  Loading runs through
;; `code-review-render--call' so a failed load restores the placeholder
;; instead of leaving a half-washed file.
;;
;; All buffer-locals here are `permanent-local': the deferred render
;; tail re-runs the mode body and `kill-all-local-variables' wipes every
;; non-permanent local mid-cycle (the 2026-10-30 counter bug).

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'deferred)
(require 'magit-section)
(require 'code-review-render)

(declare-function code-review-wash-diff "code-review-section-wash")
(declare-function code-review-wash--insert-file-tags "code-review-section-wash")
(declare-function code-review-section--fold-and-mark "code-review-section")
(declare-function code-review-section--fold-hunks "code-review-folding")
(declare-function code-review--mark-comment-lines "code-review-section")
(declare-function code-review-viewed--apply-visibility "code-review-viewed")

(defcustom code-review-lazy-enabled t
  "When non-nil, wash large file blocks lazily instead of all at once.
Small blocks always wash inline; see `code-review-lazy-threshold'."
  :group 'code-review
  :type 'boolean)

(defcustom code-review-lazy-threshold 8192
  "File blocks larger than this many characters wash lazily."
  :group 'code-review
  :type 'integer)

;; invalidate, never increment: chunks capture the epoch OBJECT and
;; compare with `eq', so a rollback that restores older buffer-locals
;; also invalidates them.
(defvar-local code-review-lazy--epoch nil
  "Token naming the current lazy state of this review buffer.")
(put 'code-review-lazy--epoch 'permanent-local t)

(defvar-local code-review-lazy--queue nil
  "Placeholder sections still to load, in wash order.")
(put 'code-review-lazy--queue 'permanent-local t)

(defvar-local code-review-lazy--commit-focus? nil
  "Whether the render that built the placeholders was commit-focus.")
(put 'code-review-lazy--commit-focus? 'permanent-local t)

(defvar-local code-review-lazy--fresh-render? nil
  "Whether the render that queued the wash is the buffer's first.
Captured at schedule time: the deferred drain runs long after the
render's dynamic bindings are gone.  Drives the fresh-render folds
at the drain.")
(put 'code-review-lazy--fresh-render? 'permanent-local t)

(defvar-local code-review-lazy--just-refetched? nil
  "Captured `code-review-section--just-refetched' for the drain.")
(put 'code-review-lazy--just-refetched? 'permanent-local t)

(defvar-local code-review-lazy--wash-pos nil
  "Marker at the next unwashed diff block, nil when nothing to wash.")
(put 'code-review-lazy--wash-pos 'permanent-local t)

(defvar-local code-review-lazy--wash-limit nil
  "Marker at the end of the wash region (the files-chnged body).")
(put 'code-review-lazy--wash-limit 'permanent-local t)

(defvar-local code-review-lazy--insert-parent nil
  "The section the render was inserting into when scheduled.
The deferred ticks create file sections: magit's insertion context
is dynamic, and after the render it is gone — a placeholder created
without it gets a nil parent AND silently REPLACES
`magit-root-section' (magit treats a parent-less insert as a new
root), corrupting the whole section tree.")
(put 'code-review-lazy--insert-parent 'permanent-local t)

(defvar-local code-review-lazy--end-fixups nil
  "Enclosing sections whose `end' markers the wash must extend.
The render finishes the files-chnged/report/root sections BEFORE
the deferred wash paints the hunks: their end markers (insertion
type nil) stay at the pre-wash position, pointing INTO the painted
content.  The drain moves them to the wash's true end.")
(put 'code-review-lazy--end-fixups 'permanent-local t)

(defvar code-review-lazy--wash-limit-dyn nil
  "Dynamic (non-buffer-local) wash limit for `--block-end' during a step.")

(defvar code-review-section--just-refetched)

(defclass code-review-lazy-placeholder-section (magit-section)
  ())

(defun code-review-lazy--invalidate ()
  "Drop every pending lazy chunk for the current buffer."
  (setq code-review-lazy--epoch (cons 'code-review-lazy nil)
        code-review-lazy--queue nil)
  ;; the wash stepper shares the epoch: drop its state too, or a
  ;; stale marker keeps a drain loop waiting forever
  (when (markerp code-review-lazy--wash-pos)
    (set-marker code-review-lazy--wash-pos nil))
  (when (markerp code-review-lazy--wash-limit)
    (set-marker code-review-lazy--wash-limit nil))
  (setq code-review-lazy--wash-pos nil
        code-review-lazy--wash-limit nil))

(defun code-review-lazy--render-call (origin &rest args)
  "ORIGIN is `code-review-render--call'; ARGS its arguments.
A failed render rolls the buffer text AND buffer-locals back, so
pending chunk callbacks would otherwise re-match their captured
epoch against the restored state.  Invalidate before the render
(kills chunks of older renders) and again after a failure (kills
the failed render's own chunks)."
  (code-review-lazy--invalidate)
  (let ((ok nil))
    (unwind-protect
        (prog1 (apply origin args)
          (setq ok t))
      (unless ok
        (code-review-lazy--invalidate)))))
(advice-add 'code-review-render--call :around
            #'code-review-lazy--render-call)

(defun code-review-lazy--toggle-around (origin section)
  "Load a placeholder SECTION instead of toggling its (empty) body.
ORIGIN is the advised `magit-section-toggle' or `magit-section-show'."
  (if (and code-review-lazy-enabled
           (code-review-lazy-placeholder-section-p section))
      (code-review-lazy--expand-at section)
    (funcall origin section)))
(advice-add 'magit-section-toggle :around #'code-review-lazy--toggle-around)
(advice-add 'magit-section-show :around #'code-review-lazy--toggle-around)

;;; Placeholder construction (the synchronous wash loop replaces
;;; large raw blocks with these)

(defun code-review-lazy--block-end ()
  "End position of the `diff --git' block starting at point.
Bounded by `code-review-lazy--wash-limit-dyn' when the deferred
stepper runs (the text after the wash region is other sections)."
  (save-excursion
    (forward-line 1)
    (if (re-search-forward "^diff --git " code-review-lazy--wash-limit-dyn 'move)
        (line-beginning-position)
      (or code-review-lazy--wash-limit-dyn (point-max)))))

(defun code-review-lazy--block-stats (text)
  "Return (HUNKS ADDED DELETED) counts for one raw diff block TEXT."
  (let ((stats (list 0 0 0)))
    (dolist (line (split-string text "\n" t))
      (cond
       ((string-prefix-p "@@ " line)
        (cl-incf (nth 0 stats)))
       ((string-prefix-p "+++" line))
       ((string-prefix-p "+" line)
        (cl-incf (nth 1 stats)))
       ((string-prefix-p "---" line))
       ((string-prefix-p "-" line)
        (cl-incf (nth 2 stats)))))
    stats))

(defun code-review-lazy--block-status (text)
  "Change status word for one raw diff block TEXT, as the washer prints it."
  (cond
   ((string-match-p "^Binary files" text) "binary")
   ((string-match-p "^new file" text) "new file")
   ((string-match-p "^deleted file" text) "deleted")
   ((string-match-p "^rename from" text) "renamed")
   (t "modified")))

(defun code-review-lazy--block-orig (text)
  "Original path of a renamed block TEXT, or nil."
  (when (string-match "^rename from \\(.+\\)$" text)
    (match-string 1 text)))

(defun code-review-lazy--insert-placeholder ()
  "Replace the raw block at point with a placeholder file section.
Point ends after the inserted section, ready for the next block."
  (let* ((beg (point))
         (end (code-review-lazy--block-end))
         (text (buffer-substring-no-properties beg end))
         (header (car (split-string text "\n" t)))
         (path (or (code-review--diff--extract-b-path header) ""))
         (orig (code-review-lazy--block-orig text))
         (status (code-review-lazy--block-status text))
         (stats (code-review-lazy--block-stats text))
         (info (and (hash-table-p code-review-section--file-classifications)
                    (gethash path
                             code-review-section--file-classifications)))
         (display (if (or (not orig) (equal orig path))
                      path
                    (format "%s -> %s" orig path)))
         (spec (list :path path :status status :text text :orig orig)))
    (delete-region beg end)
    (goto-char beg)
    (setq code-review-lazy--queue
          (nconc code-review-lazy--queue
                 (list (magit-insert-section
                           (code-review-lazy-placeholder-section spec nil)
                         (insert (propertize
                                  (format "%-10s %s" status display)
                                  'font-lock-face 'magit-diff-file-heading
                                  'code-review-path path
                                  'code-review-lazy t))
                         (code-review-wash--insert-file-tags path info)
                         (insert (propertize
                                  (format "  (%d hunk%s, +%d -%d - load with TAB)"
                                          (nth 0 stats)
                                          (if (= (nth 0 stats) 1) "" "s")
                                          (nth 1 stats)
                                          (nth 2 stats))
                                  'font-lock-face 'code-review-dimmed))
                         (magit-insert-heading)))))))

(defun code-review-lazy--wash-all ()
  "Wash the whole diff synchronously (the `code-review-lazy-enabled'
nil path): every block inline, large ones through the placeholder
machinery, no deferred ticks in between."
  (let ((code-review-lazy--wash-limit-dyn nil))
    (while (progn (skip-chars-forward " \t\r\n") (not (eobp)))
      (if (not (looking-at "^diff --git"))
          (error "Code review stopped before the end of the diff")
        (if (and code-review-lazy-enabled
                 (> (- (code-review-lazy--block-end) (point))
                    code-review-lazy-threshold))
            (code-review-lazy--insert-placeholder)
          (unless (code-review-wash-diff)
            (error "Code review stopped before the end of the diff")))))))

(defun code-review-lazy--precheck ()
  "Validate the inserted diff is a sequence of `diff --git' blocks.
Pure motion over the raw text (no washing): milliseconds instead of
the multi-second wash, so a malformed diff still fails the
synchronous render and `code-review-render--call' rolls it back.
The wash itself then runs through the deferred stepper."
  (save-excursion
    (while (progn (skip-chars-forward " \t\r\n") (not (eobp)))
      (if (not (looking-at "^diff --git"))
          (error "Code review stopped before the end of the diff")
        (goto-char (code-review-lazy--block-end))))))

(defun code-review-lazy--wash-block ()
  "Wash ONE file block at point: placeholder when large, inline else.
Return non-nil when a block was washed, nil when the raw text is
exhausted."
  (skip-chars-forward " \t\r\n")
  (if (eobp)
      nil
    (if (not (looking-at "^diff --git"))
        (error "Code review stopped before the end of the diff")
      (if (and code-review-lazy-enabled
               (> (- (code-review-lazy--block-end) (point))
                  code-review-lazy-threshold))
          (progn (code-review-lazy--insert-placeholder) t)
        (unless (code-review-wash-diff)
          (error "Code review stopped before the end of the diff"))
        t))))

(defun code-review-lazy--wash-step (buffer epoch)
  "Wash one file block in BUFFER per tick while EPOCH is current.
When the raw text is exhausted, hand over to the placeholder
expansion phase."
  (when (and (buffer-live-p buffer)
             (eq epoch (buffer-local-value 'code-review-lazy--epoch buffer)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t)
            (code-review-lazy--wash-limit-dyn code-review-lazy--wash-limit)
            done err)
        (if (null code-review-lazy--wash-pos)
            ;; wash phase already drained
            (code-review-lazy--expand-step buffer epoch)
          (save-restriction
            (narrow-to-region (point-min) code-review-lazy--wash-limit)
            (goto-char code-review-lazy--wash-pos)
            ;; re-install the render's magit insertion context: the
            ;; sections created here must become children of the
            ;; files-chnged section, not a new root
            (let ((magit-insert-section--parent code-review-lazy--insert-parent)
                  (magit-insert-section--current code-review-lazy--insert-parent)
                  (magit-insert-section--oldroot magit-root-section))
              (setq done
                    (condition-case e
                        (code-review-lazy--wash-block)
                      (error (setq err (error-message-string e)) nil)))
              (when done
                (set-marker code-review-lazy--wash-pos (point)))))
          (cond
           (err
            (code-review-lazy--invalidate)
            (message "Code review lazy wash failed: %s" err))
           ((not done)
            (setq code-review-lazy--wash-pos nil)
            (set-marker code-review-lazy--wash-limit nil)
            ;; hand over to the expansion phase on the NEXT tick (not
            ;; inline: a test or observer must be able to freeze the
            ;; buffer between the wash and the expansion phases)
            (deferred:nextc (deferred:next)
              (lambda (_)
                (code-review-lazy--expand-step buffer epoch))))
           (t
            (deferred:nextc (deferred:next)
              (lambda (_)
                (code-review-lazy--wash-step buffer epoch))))))))))

;;; Loading

(defun code-review-lazy--expand-at (section)
  "Replace the placeholder SECTION with its fully washed file section.
A failed wash rolls the placeholder back (text, properties and tree
slot) and returns nil.  This is deliberately NOT
`code-review-render--call': that swaps the whole buffer text to a
backup, which would re-point the placeholder's markers into the
backup buffer — it is a full-rebuild tool, not a partial-edit tool."
  (let* ((spec (oref section value))
         (parent (oref section parent))
         (text (plist-get spec :text))
         (inhibit-read-only t)
         (new-section nil))
    (save-excursion
      (let* ((beg (progn (goto-char (oref section start)) (point-marker)))
             (heading (buffer-substring (oref section start)
                                        (oref section end)))
             (old-children (oref parent children)))
        (oset parent children (delq section (oref parent children)))
        (delete-region beg (oref section end))
        (insert text)
        (let* ((end (copy-marker (point)))
               (magit-insert-section--parent parent))
          (goto-char beg)
          (save-restriction
            (narrow-to-region beg end)
            (if (condition-case err
                    (progn
                      (code-review-render--wash-sequence
                       #'code-review-wash-diff)
                      (skip-chars-forward " \t\r\n")
                      (unless (eobp)
                        (error "Code review stopped before the end of the diff"))
                      t)
                  (error
                   ;; roll the placeholder back: drop everything the
                   ;; wash inserted, restore its heading and its slot
                   (delete-region beg end)
                   (insert heading)
                   (oset parent children old-children)
                   (message "Code review lazy load failed: %s"
                            (error-message-string err))
                   nil))
                (setq new-section (car (last (oref parent children)))))))))
    (when (and new-section
               (not (code-review-lazy-placeholder-section-p new-section)))
      (when code-review-fold-hunks-by-default
        (code-review-section--fold-hunks new-section nil))
      (code-review-viewed--apply-visibility new-section nil)
      (when code-review-comment-fringe-markers
        (code-review--mark-comment-lines)))
    new-section))

(defun code-review-lazy--load-next ()
  "Load the first loadable placeholder from the queue.
Return non-nil when a file was loaded."
  (let ((inhibit-read-only t)
        (loaded nil))
    (while (and code-review-lazy--queue (not loaded))
      (let ((section (pop code-review-lazy--queue)))
        (setq loaded
              (and (object-of-class-p section 'magit-section)
                   (markerp (oref section start))
                   (marker-buffer (oref section start))
                   (code-review-lazy--expand-at section)))))
    loaded))

(defun code-review-lazy--expand-step (buffer epoch)
  "Expand queued placeholders in BUFFER, one per tick, while EPOCH
is current.  When the queue drains, run the whole-buffer visibility
passes (bot folds, viewed folds, fringe markers) that the render
could not apply: the hunks did not exist yet at render time."
  (when (and (buffer-live-p buffer)
             (eq epoch (buffer-local-value 'code-review-lazy--epoch buffer)))
    (with-current-buffer buffer
      (if (not (code-review-lazy--load-next))
          (let ((inhibit-read-only t)
                ;; the render's dynamic binding is long gone; the
                ;; drain re-installs the captured value so
                ;; fold-and-mark sees the render-time flags
                (code-review-section--just-refetched
                 code-review-lazy--just-refetched?))
            (dolist (sec code-review-lazy--end-fixups)
              (when (and (object-of-class-p sec 'magit-section)
                         (slot-boundp sec 'end)
                         (markerp (oref sec end)))
                (set-marker (oref sec end) (point-max))))
            (code-review-section--fold-and-mark
             code-review-lazy--fresh-render?
             code-review-lazy--commit-focus?))
        (deferred:nextc (deferred:next)
          (lambda (_)
            (code-review-lazy--expand-step buffer epoch)))))))

(defun code-review-lazy--schedule (commit-focus? fresh-render?)
  "Queue the deferred wash + chunk loader for this render.
FRESH-RENDER? is the render's own first-render flag, captured for
the drain.  Called inside the render's narrowing: capture the wash
region as markers and hand the per-block work to the deferred
queue so Emacs can process input between file blocks (the sync
wash blocked the UI for seconds on medium-file PRs — every block
under the lazy threshold washed inline in one pass)."
  (setq code-review-lazy--commit-focus? commit-focus?
        code-review-lazy--fresh-render? fresh-render?
        code-review-lazy--just-refetched?
        (and (boundp 'code-review-section--just-refetched)
             code-review-section--just-refetched)
        code-review-lazy--wash-pos (copy-marker (point))
        code-review-lazy--wash-limit (copy-marker (point-max))
        code-review-lazy--insert-parent magit-insert-section--parent
        code-review-lazy--end-fixups
        (let ((secs nil)
              (sec magit-insert-section--parent))
          (while sec
            (push sec secs)
            (setq sec (and (object-of-class-p sec 'magit-section)
                           (ignore-errors (oref sec parent)))))
          secs))
  (let ((buffer (current-buffer))
        (epoch code-review-lazy--epoch))
    (deferred:nextc (deferred:next)
      (lambda (_)
        (code-review-lazy--wash-step buffer epoch)))))

(provide 'code-review-lazy)
;;; code-review-lazy.el ends here
