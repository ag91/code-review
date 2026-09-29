;;; code-review-section-analysis.el --- the analysis and review-order sections -*- lexical-binding: t; -*-
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

;; the analysis and review-order sections (part of the code-review section rendering, split from
;; code-review-section.el in phase 15c).

;;; Code:

(require 'code-review-section-shared)
(require 'magit-section)
(require 'magit-diff)
(require 'cl-lib)
(require 'a)
(require 'code-review-faces)
(require 'code-review-db)
(require 'code-review-utils)
(require 'code-review-analysis)
(require 'code-review-testimpact)

(declare-function code-review-browse--reveal "code-review-browse")
(declare-function code-review-browse--strip-diff-prefix "code-review-browse")



(defclass code-review-analysis-section (magit-section)
  (()))
(defun code-review-section--insert-analysis-jump (worktree path line)
  "Insert a button jumping to PATH at LINE in WORKTREE."
  (insert-button (format "%s:%s" path line)
                 'face 'code-review-url-header-face
                 'follow-link t
                 'help-echo "Visit this location in the worktree"
                 'action (lambda (&rest _)
                           (find-file-other-window
                            (expand-file-name path worktree))
                           (goto-char (point-min))
                           (forward-line (1- line)))))
(defun code-review-section--insert-analysis-similar (worktree similar)
  "Insert the SIMILAR findings (each: diff-path total repo-path
covered lo hi), with a jump button into WORKTREE."
  (pcase-dolist (`(,diff-path ,total ,repo-path ,covered ,lo ,hi)
                 similar)
    (insert "  ")
    (insert (propertize "similar: " 'font-lock-face 'magit-dimmed))
    (insert (format "%s: %s/%s added lines also in "
                    diff-path covered total))
    (insert-button (format "%s:%s-%s" repo-path lo hi)
                   'face 'code-review-url-header-face
                   'follow-link t
                   'help-echo "Visit the similar code in the worktree"
                   'action (lambda (&rest _)
                             (find-file-other-window
                              (expand-file-name repo-path worktree))
                             (goto-char (point-min))
                             (forward-line (1- lo))))
    (insert ?\n)))
(defun code-review-section--insert-analysis-dead (dead)
  "Insert the DEAD findings (each: name path line)."
  (pcase-dolist (`(,name ,path ,line) dead)
    (insert "  ")
    (insert (propertize "possibly dead: " 'font-lock-face 'magit-dimmed))
    (insert (format "%s (added in %s:%s; no references found in the "
                    name path line))
    (insert "worktree)\n")))
(defun code-review-section--insert-analysis-dangling (worktree dangling)
  "Insert the DANGLING findings (each: name path refs), the first
three refs as jump buttons into WORKTREE."
  (pcase-dolist (`(,name ,path ,refs) dangling)
    (insert "  ")
    (insert (propertize "dangling: " 'font-lock-face 'magit-dimmed))
    (insert (format "%s (deleted in %s; still used at "
                    name path))
    (let ((first t))
      (pcase-dolist (`(,rpath ,rline ,_rtext)
                     (cl-subseq refs 0 (min (length refs) 3)))
        (unless first (insert ", "))
        (setq first nil)
        (code-review-section--insert-analysis-jump
         worktree rpath rline)))
    (insert ")\n")))
(defun code-review-section--insert-analysis-delicate (res)
  "Insert the delicate-hunk jump list from RES (phase 15): the
top-K entries at or above `code-review-analysis-delicacy-threshold',
hottest first, with in-buffer jump buttons."
  (let ((entries (cl-remove-if
                  (lambda (e)
                    (< (plist-get e :score)
                       code-review-analysis-delicacy-threshold))
                  (or (plist-get res :hunks) nil))))
    (setq entries (cl-subseq
                   entries 0 (min (length entries)
                                  code-review-analysis-delicacy-top-k)))
    (dolist (e entries)
      (let* ((path (plist-get e :path))
             (ranges (plist-get e :ranges))
             (reasons (string-join
                       (code-review-analysis--hunk-reasons e)
                       "; ")))
        (insert "  ")
        (insert (propertize "delicate: "
                            'font-lock-face 'magit-dimmed))
        (insert-button (format "%s %s" path ranges)
                       'face 'code-review-url-header-face
                       'follow-link t
                       'help-echo "Jump to this hunk in the review"
                       'action (lambda (&rest _)
                                 (code-review-section--goto-hunk-section
                                  path ranges)))
        (insert (format "  %.2f (%s)\n"
                        (plist-get e :score) reasons))))))
(defun code-review-section--insert-testimpact-ci (tres)
  "Insert the phase 17 CI-gaming findings from TRES (HOISTED to
the top of the Analysis section: skip markers, `|| true' in CI
commands, newly gated/removed workflow steps, lowered coverage
thresholds), each with a jump button into the worktree."
  (let ((worktree code-review-repo-worktree))
    (pcase-dolist (f (plist-get tres :ci))
      (insert "  ")
      (insert (propertize
               (format "ci-gaming: %s in "
                       (code-review-testimpact--kind-label
                        (plist-get f :kind)))
               'font-lock-face 'font-lock-warning-face))
      (code-review-section--insert-analysis-jump
       worktree (plist-get f :path) (plist-get f :line))
      (insert (format " — \"%s\"\n" (plist-get f :text))))))

(defun code-review-section--insert-testimpact-notest (tres)
  "Insert the phase 17 no-test-coverage findings from TRES: hunks
whose touched definitions are USED in the worktree but referenced
by no test file.  The button jumps inside the review buffer, like
the delicate-hunk list."
  (dolist (e (plist-get tres :notest))
    (let ((path (plist-get e :path))
          (ranges (plist-get e :ranges))
          (defs (plist-get e :defs)))
      (insert "  ")
      (insert (propertize "no test coverage: "
                          'font-lock-face 'font-lock-warning-face))
      (insert-button (format "%s %s" path ranges)
                     'face 'code-review-url-header-face
                     'follow-link t
                     'help-echo "Jump to this hunk in the review"
                     'action (lambda (&rest _)
                               (code-review-section--goto-hunk-section
                                path ranges)))
      (insert (format " — %s\n" (string-join defs ", "))))))

(defun code-review-section-insert-analysis ()
  "Insert the heuristic Analysis section (phase 5, 15, 17).
Phase 17 findings (CI-gaming, no test coverage) are HOISTED
above the phase 5 duplicate/dead/dangling findings and the phase
15 delicate-hunk jump list.  Toggle it with TAB like any other
section.  Nothing is inserted when neither analysis found
anything (or they are disabled, or there is no worktree)."
  (let* ((res (code-review-analysis-run))
         (tres (code-review-testimpact-run))
         (tres-findings (and tres (or (plist-get tres :ci)
                                      (plist-get tres :notest)))))
    (when (or res tres-findings)
      (let ((similar (and res (plist-get res :similar)))
            (dead (and res (plist-get res :dead)))
            (dangling (and res (plist-get res :dangling)))
            (worktree code-review-repo-worktree))
        (magit-insert-section (code-review-analysis-section)
          (insert (propertize "Analysis" 'font-lock-face 'magit-section-heading))
          (insert (propertize " (heuristic)" 'font-lock-face 'magit-dimmed))
          (magit-insert-heading)
          ;; phase 17 first: the CI-gaming findings are the hard tags
          (when tres-findings
            (code-review-section--insert-testimpact-ci tres)
            (code-review-section--insert-testimpact-notest tres))
          (when res
            (code-review-section--insert-analysis-similar worktree similar)
            (code-review-section--insert-analysis-dead dead)
            (code-review-section--insert-analysis-dangling worktree dangling)
            ;; phase 15: delicate hunks (top-K, hottest first).  The
            ;; buttons jump inside the review buffer: the section is
            ;; inserted there, so the button action runs there.
            (code-review-section--insert-analysis-delicate res))
          (insert ?\n))))))
(defclass code-review-review-order-section (magit-section)
  (()))
(defun code-review-section--goto-file-section (path)
  "Move point to the review buffer's file section for PATH.
Reveal its ancestors and recenter; no-op when the diff carries no
such file."
  (let ((target nil))
    (magit-map-sections
     (lambda (sec)
       (when (and (null target)
                  (magit-file-section-p sec)
                  (slot-boundp sec 'value)
                  (stringp (oref sec value))
                  (equal (code-review-browse--strip-diff-prefix
                          (substring-no-properties (oref sec value)))
                         path))
         (setq target sec))))
    (when target
      (code-review-browse--reveal target)
      (magit-section-goto target)
      (recenter))))
(defun code-review-section--find-hunk-section (path ranges)
  "The hunk section for PATH RANGES (nil when the diff has none).
RANGES is the raw ranges text of the @@ header (the hunk key, see
`code-review-analysis--split-hunks')."
  (let ((target nil))
    (magit-map-sections
     (lambda (sec)
       (when (and (null target)
                  (eq (eieio-object-class sec) 'magit-hunk-section)
                  (slot-boundp sec 'value)
                  (let ((v (oref sec value)))
                    (and (equal (cdr (assq 'path v)) path)
                         (equal (cdr (assq 'ranges v)) ranges))))
         (setq target sec))))
    target))
(defun code-review-section--goto-hunk-section (path ranges)
  "Move point to the review buffer's hunk section for PATH RANGES.
RANGES is the raw ranges text of the @@ header (the hunk key, see
`code-review-analysis--split-hunks').  Reveals the hunk's
ancestors (a collapsed file does not hide its delicate hunks);
no-op when the buffer carries no such hunk."
  (let ((target (code-review-section--find-hunk-section path ranges)))
    (when target
      (code-review-browse--reveal target)
      (magit-section-goto target)
      ;; `recenter' acts on the SELECTED window: only recenter when
      ;; that window actually displays this buffer (batch/async
      ;; callers run elsewhere — a bare `get-buffer-window' guard
      ;; still errors, the phase 15 daemon verification caught this)
      (when (eq (window-buffer (selected-window)) (current-buffer))
        (recenter)))))
(defun code-review-section-insert-review-order ()
  "Insert the phase 14 Review order section (heuristic).
The PR's changed files ranked by repository heat: churn
percentile x complexity x knowledge factors, hottest first,
each with a one-line reason and a button jumping to the file
section in this buffer.  Nothing is inserted while the heat data
is unavailable (disabled, code-compass missing, or the history
harvest still running: the sentinel re-renders when it lands)."
  (when code-review-history--order
    (magit-insert-section (code-review-review-order-section)
      (insert (propertize "Review order"
                          'font-lock-face 'magit-section-heading))
      (insert (propertize " (heuristic)" 'font-lock-face 'magit-dimmed))
      (magit-insert-heading)
      (let ((n 0))
        (dolist (e code-review-history--order)
          (setq n (1+ n))
          (let ((path (plist-get e :path))
                (reason (plist-get e :reason)))
            (insert "  ")
            (insert (format "%2d. " n))
            (insert (propertize (format "[%s]" (plist-get e :bucket))
                                'font-lock-face 'magit-dimmed))
            (insert " ")
            (insert-button path
                           'face 'code-review-url-header-face
                           'follow-link t
                           'help-echo "Jump to this file in the review"
                           'action (lambda (&rest _)
                                     (code-review-section--goto-file-section
                                      path)))
            (unless (string-empty-p reason)
              (insert (propertize (concat "  " reason)
                                  'font-lock-face 'magit-dimmed)))
            (insert ?\n)))))))
(provide 'code-review-section-analysis)
;;; code-review-section-analysis ends here
