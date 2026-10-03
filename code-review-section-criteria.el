;;; code-review-section-criteria.el --- the criteria checklist section (phase 23) -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026 Andrea <andrea-dev@hotmail.com>
;;
;; This file is part of code-review.
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the Free Software Foundation; either version 3,
;; or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;  the criteria checklist section (part of the code-review section
;;  rendering family, phase 23): the solo loop's enforcement surface.
;;  LOCAL reviews only: every requirement covering the changed files,
;;  each with a does-this-diff-satisfy-it state (RET cycles pending ->
;;  satisfied -> violated), rendered next to the diff, jump buttons
;;  into the criteria files.  warn mode: a warning banner when no
;;  criteria cover the change.  Forge reviews skip the section (phase
;;  24 builds their surface).

;;; Code:

(require 'code-review-section-shared)
(require 'magit-section)
(require 'cl-lib)
(require 'code-review-faces)
(require 'code-review-db)
(require 'code-review-utils)
(require 'code-review-criteria)

(defvar code-review-repo-worktree)         ; code-review-repo.el

(defclass code-review-criteria-section (magit-section)
  (()))

(defclass code-review-criteria-item-section (magit-section)
  ((keymap :initform 'code-review-criteria-item-section-map)
   (req    :initarg :req)))

(defvar code-review-criteria-item-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'code-review-criteria-toggle)
    map)
  "RET cycles the satisfied? state of the criteria item at point.")

(defvar-local code-review-criteria--states nil
  "Hash req-ID -> state of the current buffer's checklist.
States: `pending' (default), `satisfied', `violated'.  Buffer-local
on purpose for phase 23: the state is a reading aid, nothing is
submitted (phase 24 binds verdicts durably).")

(defun code-review-criteria--state (id)
  "The checklist state of requirement ID (`pending' by default)."
  (or (and code-review-criteria--states
           (gethash id code-review-criteria--states))
      'pending))

(defun code-review-criteria--state-marker (state)
  "The checklist marker text for STATE."
  (pcase state
    ('satisfied "[x]")
    ('violated "[!]")
    (_ "[ ]")))

(defun code-review-criteria--state-face (state)
  "The face for STATE's marker."
  (pcase state
    ('satisfied 'success)
    ('violated 'font-lock-warning-face)
    (_ 'magit-dimmed)))

(defun code-review-criteria-toggle ()
  "Cycle the does-this-diff-satisfy-it state of the item at point.
pending -> satisfied -> violated -> pending.  Rewrites the
item's marker in place; nothing is submitted anywhere (the
verdict is the reviewer's own reading aid)."
  (interactive)
  (let ((sec (magit-current-section)))
    (when (and sec
               (eq (eieio-object-class sec)
                   'code-review-criteria-item-section))
      (let* ((id (plist-get (oref sec req) :id))
             (next (pcase (code-review-criteria--state id)
                     ('pending 'satisfied)
                     ('satisfied 'violated)
                     (_ 'pending))))
        (unless code-review-criteria--states
          (setq code-review-criteria--states
                (make-hash-table :test #'equal)))
        (puthash id next code-review-criteria--states)
        (let ((inhibit-read-only t))
          (save-excursion
            (goto-char (oref sec start))
            (when (re-search-forward "\\[[ x!]\\]" (oref sec end) t)
              (let* ((beg (match-beginning 0))
                     ;; capture the marker's FULL property set
                     ;; FIRST: replace-match writes the new marker
                     ;; with NO properties — losing the magit-section
                     ;; matcher (magit-current-section then falls to
                     ;; the root and the toggle silently no-ops) AND
                     ;; the routing keymap
                     (props (text-properties-at beg)))
                (replace-match
                 (code-review-criteria--state-marker next) t t)
                ;; restore everything, then overlay the new state's
                ;; face (every marker is exactly 3 chars)
                (set-text-properties beg (+ beg 3) props)
                (put-text-property
                 beg (+ beg 3) 'font-lock-face
                 (code-review-criteria--state-face next))))))))))

(defun code-review-section--criteria-insert (text &rest props)
  "Insert TEXT carrying the criteria item keymap (plus PROPS).
The section class's `keymap' slot alone does NOT route keypresses
in this package's renders (magit 4.x): every keymap-carrying
section also propertizes its text — see
`code-review-section-header.el'.  The req ID button is the one
exception (insert-button sets its own keymap there)."
  (insert (apply #'propertize
                 ;; the VALUE must be the EVALUATED keymap object:
                 ;; a symbol-valued `keymap' text property is INERT
                 ;; (neither key-binding nor the command loop
                 ;; resolves it)
                 text 'keymap code-review-criteria-item-section-map
                 props)))

(defun code-review-section--insert-criteria-item (req covered)
  "Insert one checklist row for REQ with its COVERED paths.
State marker (RET cycles), the req ID (a jump button into the
criteria file), the EARS sentence, the covered paths."
  (let* ((id (plist-get req :id))
         (state (code-review-criteria--state id))
         (repo code-review-repo-worktree))
    (magit-insert-section
        (code-review-criteria-item-section id nil :req req)
      (code-review-section--criteria-insert "  ")
      (code-review-section--criteria-insert
       (code-review-criteria--state-marker state)
       'font-lock-face (code-review-criteria--state-face state))
      (code-review-section--criteria-insert " ")
      (insert-button (format "%s" id)
                     'face 'code-review-url-header-face
                     'follow-link t
                     'help-echo "Visit this criteria file in the worktree"
                     'action (lambda (&rest _)
                               (find-file-other-window
                                (expand-file-name
                                 (plist-get req :file) repo))))
      (code-review-section--criteria-insert
       (format " %s" (plist-get req :sentence)))
      (code-review-section--criteria-insert
       (format "  (%s)" (string-join covered ", "))
       'font-lock-face 'magit-dimmed)
      (code-review-section--criteria-insert "\n"))))

(defun code-review-section-insert-criteria ()
  "Insert the phase 23 criteria checklist (LOCAL reviews only).
Every requirement covering the changed files, with its
does-this-diff-satisfy-it state and a jump button into the
criteria file — the requirements NEXT TO THE DIFF, so the solo
reviewer judges conformance to intent.  `warn' mode with no
covering criteria: a warning banner instead.  Nothing in `nil'
mode, and nothing on forge reviews (phase 24 builds their
surface).  Never signals; the scan is one bounded `git ls-files'
pass with byte caps."
  (when (and (code-review-db-local-pr-p) code-review-repo-worktree)
    (let* ((diff (code-review-db--pullreq-raw-diff))
           (paths (and diff (code-review-criteria--changed-paths diff)))
           (criteria (and paths
                         (code-review-criteria--criteria
                          code-review-repo-worktree)))
           (matching (and criteria
                          (code-review-criteria--matching
                           criteria paths))))
      (when (or matching (eq code-review-criteria-required 'warn))
        (magit-insert-section (code-review-criteria-section)
          (insert (propertize "Criteria" 'font-lock-face 'magit-section-heading))
          (insert (propertize " (solo loop)" 'font-lock-face 'magit-dimmed))
          (magit-insert-heading)
          (cond
           (matching
            (pcase-dolist (`(:req ,req :paths ,covered) matching)
              (code-review-section--insert-criteria-item req covered)))
           (t
            (insert "  ")
            (insert (propertize
                     "no criteria cover the files in this change"
                     'font-lock-face 'font-lock-warning-face))
            (insert (propertize
                     "  — M-x code-review-criteria-insert-template drafts one"
                     'font-lock-face 'magit-dimmed))
            (insert ?\n)))
          (insert ?\n))))))

(provide 'code-review-section-criteria)
;;; code-review-section-criteria.el ends here
