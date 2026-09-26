;;; code-review-section-shared.el --- shared section-render primitives -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026 Andrea <andrea-dev@hotmail.com>
;;
;; Author: Wanderson Ferreira <https://github.com/wandersoncferreira>
;; Maintainer: Andrea <andrea-dev@hotmail.com>
;; Keywords: git, tools, vc
;; Homepage: https://github.com/wandersoncferreira/code-review
;;
;; Split from code-review-section.el (phase 15c, see Improvements.org):
;; the code is mostly Wanderson Ferreira's original
;; code-review-section.el, moved verbatim by the split; the fork and
;; this file are maintained by Andrea.

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; shared section-render primitives (part of the code-review section rendering, split from
;; code-review-section.el in phase 15c).

;;; Code:

(require 'magit-section)
(require 'magit-diff)
(require 'cl-lib)
(require 'a)
(require 'code-review-faces)
(require 'code-review-db)
(require 'code-review-utils)
(require 'shr)
(require 'dom)




(defcustom code-review-section-indent-width 1
  "Indent width for nested sections."
  :type 'integer
  :group 'code-review)
(defcustom code-review-section-image-scaling 0.8
  "Image scaling number used to resize images in buffer."
  :type 'float
  :group 'code-review)
(defcustom code-review-fill-column 80
  "Column number to wrap comments."
  :group 'code-review
  :type 'integer)
(defvar-local code-review-focus-mode nil
  "When non-nil, files auto-flagged as noise are hidden.
See `code-review-diff-noise-rules' and
`code-review-toggle-focus-mode'.")
(put 'code-review-focus-mode 'permanent-local t)
(defvar-local code-review-section--file-classifications nil
  "Hash table path -> plist (:tag :collapse :hide) for the diff.
Recomputed by `code-review--diff--classify-diff' on every render.
For internal usage only.")
(defcustom code-review-fold-header-sections t
  "When non-nil, low-signal top sections start folded.
\"Commits\" (with their CI checks), \"Description\", \"Your
Review Feedback\" and \"Conversation\" are collapsed to their
headings, so the diff is the first thing you see when the buffer
opens.  Press TAB on any of them to expand."
  :type 'boolean
  :group 'code-review)
(defcustom code-review-collapse-bot-comments t
  "When non-nil, comments authored by bots start folded.
AI-bot review comments (and bot-to-bot chatter in particular)
are noise for the human reviewer, so each is collapsed to its
one-line \"@author - date\" heading.  Threads where a human
replied stay fully expanded, and TAB unfolds any of them."
  :type 'boolean
  :group 'code-review)
(defcustom code-review-bot-author-regexp
  "\\`\\(github-actions\\|devin\\|cursor\\|copilot\\|gemini\\|codeium\\|coderabbit\\|codspeed\\|greptile\\|codecov\\|claassistant\\|renovate\\|dependabot\\|imgbot\\|semantic-release\\|all-contributors\\)\\|\\[bot\\]\\'"
  "Regexp matching comment authors considered bots.
Matched case-insensitively against the login, e.g.
\"devin-ai-integration\" or \"cursor[bot]\".  Extend this list
when a new automated reviewer shows up in your PRs.
See `code-review-collapse-bot-comments'."
  :type 'regexp
  :group 'code-review)
(defcustom code-review-comment-fringe-markers t
  "When non-nil, diff lines carrying a review thread get a fringe marker.
The marker (violet chevron in the left fringe) flags exactly which
diff lines have a conversation attached, including threads folded
away by `code-review-collapse-bot-comments' or collapsed hunks.
Mouse-1 on the marked line jumps to the thread.
Re-laid automatically on every render; turn off if the fringe
noise bothers you."
  :type 'boolean
  :group 'code-review)
(defun code-review--propertize-keyword (str)
  "Add property face to STR."
  (propertize str 'face
              (cond
               ((member str '("MERGED" "SUCCESS" "COMPLETED" "APPROVED" "REJECTED"))
                'code-review-success-state-face)
               ((member str '("FAILURE" "TIMED_OUT" "ERROR" "CHANGES_REQUESTED" "CLOSED" "CONFLICTING"))
                'code-review-error-state-face)
               ((member str '("RESOLVED" "OUTDATED"))
                'code-review-info-state-face)
               ((member str '("PENDING"))
                'code-review-pending-state-face)
               (t
                'code-review-state-face))))
(defvar code-review-section-full-refresh? nil
  "Indicate if we want to perform a complete restart.
For internal usage only.")
(defvar code-review-section-grouped-comments nil
  "Hold grouped comments to avoid computation on every hunk line.
For internal usage only.")
(defvar code-review-section-hold-written-comment-ids nil
  "List to hold written comments ids.
For internal usage only.")
(defvar code-review-section-hold-written-comment-count nil
  "List of number of lines of comments written in the buffer.
For internal usage only.")
(defvar code-review-section--display-all-comments t
  "Variable to define if we should display all comments or not.
For internal usage only.")
(defvar code-review-section--display-top-level-comments t
  "Variable to define if we should display top level comments or not.
For internal usage only.")
(defvar code-review-section--display-diff-comments t
  "Variable to define if we should display diff comments or not.
For internal usage only.")

;; utility functions

;; Phase 11b: code-review OWNS the diff wash.  Up to this point the
;; render went through magit-diff internals (`magit-diff-wash-diff',
;; `magit-diff-insert-file-section', `magit-diff-wash-hunk') via
;; :override advices, whose private contracts changed repeatedly
;; across magit 4.x (paint helpers renamed/removed, `:washer' became
;; a lazy-body inserter, return values silently drive the wash loop).
;; The wash primitives below read plain unified-diff text, whose
;; format git has kept stable for decades.  Only magit-SECTION is
;; used (the `magit-insert-section' macro, section classes,
;; visibility), which is the stable part of magit.

(defun code-review-wash--delete-line ()
  "Delete the current line, including its newline."
  (delete-region (line-beginning-position)
                 (min (point-max) (1+ (line-end-position)))))
(defun code-review-wash--paint-hunk (section)
  "Paint the body of hunk SECTION with the diff faces.
Our own base pass: the first character of each line selects the
face (`magit-diff-added', `magit-diff-removed' or
`magit-diff-context'), which guarantees red/green hunks on every
magit version.  When the stable `magit-section-paint' generic is
available, call it on top for the extras (whitespace and
refinement details)."
  (save-excursion
    (goto-char (oref section start))
    (forward-line)                       ; skip the hunk heading
    (let ((end (oref section end)))
      (while (< (point) end)
        (put-text-property (point) (line-end-position)
                           'font-lock-face
                           (cond ((eq (char-after) ?+) 'magit-diff-added)
                                 ((eq (char-after) ?-) 'magit-diff-removed)
                                 (t 'magit-diff-context)))
        (forward-line))))
  (when (fboundp 'magit-section-paint)
    (save-excursion
      (goto-char (oref section start))
      (magit-section-paint section nil))))
(defun code-review--html-written-loc (body &optional indent)
  "Compute how many lines the HTML BODY will have in the buffer.
INDENT is an optional."
  (let ((shr-indentation (* (or indent 0) (shr-string-pixel-width "-")))
        (image-scaling-factor code-review-section-image-scaling)
        (shr-width code-review-fill-column)
        start
        dom)
    (with-temp-buffer
      (insert body)
      (setq dom (libxml-parse-html-region (point-min) (point-max))))
    (-> (with-temp-buffer
          (setq start (point))
          (insert " ")
          (narrow-to-region start (1+ start))
          (goto-char start)
          (shr-insert-document dom)
          ;; delete the inserted " "
          (delete-char 1)
          (buffer-substring-no-properties (point-min) (point-max)))
        (split-string "\n")
        (length)
        (- 1))))
(defun code-review--dom-string (dom)
  "Return string of DOM."
  (mapconcat (lambda (sub)
               (if (stringp sub)
                   sub
                 (code-review--dom-string sub)))
             (dom-children dom) ""))
(defun code-review--shr-tag-div (dom)
  "Rendering div tag as DOM in shr, with special handle for suggested-changes."
  (if (not (string-match-p ".*suggested-changes.*" (or (dom-attr dom 'class) "")))
      (shr-tag-div dom)
    (let ((tbody (dom-by-tag dom 'tbody)))
      (let ((shr-current-font 'code-review-info-state-face))
        (shr-insert "* Suggested change:")
        (insert "\n"))
      (dolist (tr (dom-non-text-children tbody))
        (dolist (td (dom-non-text-children tr))
          (let ((classes (split-string (or (dom-attr td 'class) ""))))
            (cond
             ((member "blob-num" classes) t)
             ((member "blob-code-deletion" classes)
              (let ((shr-current-font 'diff-indicator-removed))
                (shr-insert "-"))
              (insert (propertize (concat (code-review--dom-string td)
                                          "\n")
                                  'face 'diff-removed)))
             ((member "blob-code-addition" classes)
              (let ((shr-current-font 'diff-indicator-added))
                (shr-insert "+"))
              (insert (propertize (concat (code-review--dom-string td)
                                          "\n")
                                  'face 'diff-added)))
             (t
              (shr-generic td)))))))))
(defun code-review--insert-html (body &optional indent)
  "Insert html content BODY.
INDENT is an optional number, if provided,
INDENT count of spaces are added at the start of every line."
  (let ((shr-indentation (* (or indent 0) (shr-string-pixel-width "-")))
        (image-scaling-factor code-review-section-image-scaling)
        (shr-external-rendering-functions '((div . code-review--shr-tag-div)))
        (shr-width code-review-fill-column)
        (start (point))
        end
        dom)
    (with-temp-buffer
      (insert body)
      (setq dom (libxml-parse-html-region (point-min) (point-max))))
    ;; narrow the buffer and insert dom. otherwise there would be an extra new line at start
    (save-restriction
      (insert " ")
      (narrow-to-region start (1+ start))
      (goto-char start)
      (shr-insert-document dom)
      ;; delete the inserted " "
      (delete-char 1)
      (setq end (point)))
    (when (> shr-indentation 0)
      ;; shr-indentation does not work for images and code block
      ;; let's fix it: prepend space for any lines that does not starts with a space
      ;; (but we still need to use shr-indentation because otherwise the line will be too long)
      (save-excursion
        (goto-char start)
        (while (< (point) end)
          (unless (or (looking-at-p "\n")
                      (eq 'space (car-safe (get-text-property (point) 'display))))
            (beginning-of-line)
            (insert (propertize " " 'display `(space :width (,shr-indentation)))))
          (forward-line))))))
(defun code-review--diff--file-has-comments-p (path)
  "Non-nil when PATH carries review comments."
  (let ((res nil)
        (groups code-review-section-grouped-comments))
    (while (and groups (not res))
      (let ((objs (cdr (pop groups))))
        (while (and objs (not res))
          (let ((c (pop objs)))
            (when (and (slot-boundp c 'path)
                       (string-equal (oref c path) path))
              (setq res t))))))
    res))
(defun code-review-section--hide-if-hidden (section)
  "Fold the body of SECTION when it was created hidden.
Magit computes the initial visibility of a section when it is
created, but the actual folding only happens while a buffer is
refreshed via `magit-refresh-buffer'.  Code Review renders its
buffers directly, so sections that should start collapsed
(outdated comments, commit CI details) must be folded here
explicitly."
  (when (and (oref section hidden)
             (not (eq section magit-root-section)))
    (magit-section-hide section)))
(provide 'code-review-section-shared)
;;; code-review-section-shared ends here
