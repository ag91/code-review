;;; code-review-criteria-brief.el --- station 1: the brief (phase 27) -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Andrea <andrea-dev@hotmail.com>
;;
;; This file is part of code-review.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Phase 27, STATION 1 — THE BRIEF.  Before reading the diff, the
;; reviewer owns a ranked list of what this change must do: EARS
;; criteria with a P1..P3 priority, evidence-linked into the diff,
;; EDITED before anything is judged.  The ownership step is the
;; point (Parasuraman/Sheridan/Wickens 2000: automate information
;; acquisition, never decision selection) — an unverifiable AI draft
;; would be outsourced comprehension, so the pane edits the LEDGER
;; itself: priorities live in the criteria files' front matter
;; (durable), rows are re-prioritized with `p' on the checklist,
;; and a P1 list that cannot be edited down under
;; `code-review-criteria-max-p1' is an UNDER-SPECIFIED change — the
;; bounce signal phase 25's budget consumes.
;;
;; The AI part is a DRAFTER HOOK (`code-review-criteria-brief-
;; drafter-function', nil by default — the phase 27 L0 is no AI at
;; all): called with the PR's intent (title, description, changed
;; paths, worktree), it drafts criteria FILES into the criteria
;; directory — untracked-but-not-ignored files count as criteria,
;; so a draft is instantly a first-class ledger member the reviewer
;; edits, drops or re-prioritizes.  The reference implementation is
;; `code-review-agents-shell.el'.

;;; Code:

(require 'cl-lib)
(require 'code-review-criteria)
(require 'code-review-criteria-home)
(require 'code-review-utils)

(defvar code-review-repo-worktree)         ; code-review-repo.el
(declare-function code-review-db--pullreq-raw-diff "code-review-db")
(declare-function code-review-section--criteria-slug
                  "code-review-section-criteria")

(defcustom code-review-criteria-max-p1 7
  "The P1 budget (phase 27 station 1, phase 25's input).
A P1 list that cannot be edited under this is an
UNDER-SPECIFIED change: the criteria must be split, merged or
re-prioritized, or the PR is too big to review honestly."
  :type 'natnum
  :group 'code-review)

(defcustom code-review-criteria-brief-drafter-function nil
  "The station-1 DRAFTER hook (phase 27; nil = no AI, hand-write).
Called with (TITLE BODY PATHS WORKTREE): the PR's title and
description (nil body for local reviews), the diff's changed
paths and the worktree directory.  The drafter DRAFTS THE
CRITERIA AS GUARD TESTS in the worktree's test suite (the
requirement id in the test name, the EARS sentence as the test's
docstring, the machine keys in the comment block above the test).
Never signals into callers (the command condition-cases);
returning nil means nothing was drafted.  The mode CALLS
this but never implements it."
  :type '(choice function (const nil))
  :group 'code-review)

;;; Priorities: the P1..P3 ledger state (durable, in the file)

(defun code-review-criteria-brief--next (priority)
  "The next priority after PRIORITY (nil = unset).  Pure.
Unset -> P1 -> P2 -> P3 -> unset."
  (pcase priority
    ('nil "P1")
    ("P1" "P2")
    ("P2" "P3")
    (_ nil)))

(defun code-review-criteria-brief--rewrite (text priority)
  "TEXT with its front-matter priority set to PRIORITY (nil
removes the line).  Pure: no front matter gains a minimal
`---/priority/---' block; an existing block gains or replaces
its `priority:' line.  The body is never touched."
  (let* ((close (and text (string-search "\n---\n" text)))
         (has-fm (and text (string-prefix-p "---\n" text) close)))
    (cond
     ((not priority)
      (if (not has-fm)
          text
        (let ((head (substring text 0 (+ close 5)))
              (tail (substring text (+ close 5))))
          (concat
           (with-temp-buffer
             (insert head)
             (goto-char (point-min))
             (while (re-search-forward "^priority:.*\n" nil t)
               (replace-match ""))
             (buffer-string))
           tail))))
     (has-fm
      (let ((head (substring text 0 (+ close 5)))
            (tail (substring text (+ close 5))))
        (if (string-match-p "^priority:" head)
            (with-temp-buffer
              (insert head)
              (goto-char (point-min))
              (re-search-forward "^priority:.*$")
              (replace-match (concat "priority: " priority))
              (concat (buffer-string) tail))
          ;; INSIDE the block: before the closing delimiter (an
          ;; append after "\n---\n" leaves the priority OUT of the
          ;; front matter — and out of the parser's reach, forever
          ;; re-appending on the next cycle).  The substring drops
          ;; the final "\n---\n" and the leading newline re-joins
          ;; the last front-matter line (it shared the delimiter's
          ;; newline)
          (concat (substring head 0 -5)
                  "\npriority: " priority "\n---\n" tail))))
     (t (concat "---\npriority: " priority "\n---\n\n" (or text ""))))))

(defun code-review-criteria-brief-set (req priority)
  "Set REQ's PRIORITY durably and return the updated plist.
Never signals.  The phase 29 ownership split: a test-block or
index criterion (the definition lives in the TEST, the tool
state in the LOCAL index) writes the INDEX — never test source;
a legacy spec-file criterion rewrites its FILE's front matter
in place.  A missing file (a criterion deleted under us) leaves
everything alone and returns nil."
  (cond
   ;; phase 29: the local index owns the priority of every
   ;; definition-in-test criterion
   ((or (plist-get req :index)
        (memq (plist-get req :source) '(test index)))
    (code-review-criteria-home-set-priority req priority))
   (t
    (let ((file (plist-get req :file)))
      (when (and file (file-exists-p file))
        (let ((text (with-temp-buffer
                      (insert-file-contents file)
                      (buffer-string))))
          (with-temp-file file
            (insert (code-review-criteria-brief--rewrite text priority)))
          (plist-put req :priority priority)))))))

(defun code-review-criteria-brief-cycle (req)
  "Cycle REQ's priority P1 -> P2 -> P3 -> unset, write the
criteria file, return the updated plist."
  (code-review-criteria-brief-set
   req (code-review-criteria-brief--next (plist-get req :priority))))

;;; The state machine (one next action, swift)

(defvar-local code-review-criteria-grill--run? nil
  "Whether the grill has run on this review buffer.
Defined here so the next-action surface needs no dependency on
the grill file; `code-review-criteria-grill' sets it.")

(defun code-review-criteria-brief--verdicts (states ids)
  "How many of IDS have a non-`pending' state in STATES (nil
states = 0).  Pure — the flat helper both pane and checklist call
\(the nested-count version of this was a closer-count trap twice)."
  (if (not states) 0
    (cl-count-if
     (lambda (id)
       (let ((st (gethash id states)))
         (and st (not (eq st 'pending)))))
     ids)))

(defun code-review-criteria-brief--next-action (criteria-count verdicts)
  "The ONE next action for the loop's current station.
CRITERIA-COUNT is the ledger size for this change, VERDICTS the
number of dispositioned checklist rows.  Pure."
  (cond
   ((zerop criteria-count)
    "station 1: draft the brief — M-x code-review-criteria-brief-draft, \
or hand-write specs/ files")
   ((zerop verdicts)
    (format "station 3: read the diff, then disposition the checklist (RET) — %d criteria"
            criteria-count))
   ((not code-review-criteria-grill--run?)
    (format "station 4: run the grill (g) — %d/%d verdicts recorded"
            verdicts criteria-count))
   (t
    (format "loop complete for this pass — %d/%d dispositioned, grill run; \
iterate or emit (C-c C-t)"
            verdicts criteria-count))))

;;; The pane (the `d' popup, phase 9's surface)

(defun code-review-criteria-brief--states ()
  "The review buffer's checklist states hash, or nil."
  (and (fboundp 'code-review-review-buffer)
       (let ((buf (ignore-errors (code-review-review-buffer))))
         (and buf (buffer-live-p buf)
              (buffer-local-value 'code-review-criteria--states buf)))))

(defun code-review-criteria-brief--state-marker (req)
  "The one-char state marker for REQ's checklist state."
  (let ((states (code-review-criteria-brief--states)))
    (pcase (and states (gethash (plist-get req :id) states))
      ('satisfied "[x]")
      ('violated "[!]")
      (_ "[ ]"))))

(defun code-review-criteria-brief--insert-pane (pr)
  "Insert the station-1 criteria pane for PR's review.
Called at the TOP of the `d' description popup (station 1: judge
the change's claim BEFORE the code): the ranked ledger with
evidence jump buttons, the P1 budget line, and the loop's ONE
next action.  Never signals: no worktree or an empty ledger
renders a one-line hint instead."
  (let* ((worktree (or (bound-and-true-p code-review-repo-worktree)
                       (and (fboundp 'code-review-registry--repo-dir)
                            (code-review-registry--repo-dir))))
         (criteria (and worktree
                        (code-review-criteria--criteria worktree))))
    (insert (propertize "The brief (phase 27 station 1)"
                        'font-lock-face 'magit-section-heading))
    (insert ?\n)
    (if (not criteria)
        (progn
          (insert (propertize
                   "  no criteria yet — M-x code-review-criteria-brief-draft \
drafts them from this PR's intent (or hand-write specs/ files)"
                   'font-lock-face 'magit-dimmed))
          (insert ?\n ?\n))
      (let* ((n-p1 (cl-count-if
                    (lambda (req)
                      (equal (plist-get req :priority) "P1"))
                    criteria))
             (over (> n-p1 code-review-criteria-max-p1)))
        (insert (propertize
                 (format "  %d criteria, %d P1 (budget %d)%s\n"
                         (length criteria) n-p1
                         code-review-criteria-max-p1
                         (if over
                             "  — OVER BUDGET: under-specified change, \
edit the P1 list down (p on checklist rows) or split the PR"
                           ""))
                 'font-lock-face
                 (if over 'font-lock-warning-face 'magit-dimmed)))
        (dolist (req criteria)
          (insert "  " (code-review-criteria-brief--state-marker req) " ")
          (insert-button (format "%s" (plist-get req :id))
                         'face 'code-review-url-header-face
                         'follow-link t
                         'help-echo "Open the criteria file"
                         'action (lambda (&rest _)
                                   (find-file-other-window
                                    (expand-file-name
                                     (plist-get req :file)
                                     (or worktree default-directory)))))
          (insert (propertize
                   (format " %s" (or (plist-get req :priority) "P-"))
                   'font-lock-face
                   (if (equal (plist-get req :priority) "P1")
                       'font-lock-warning-face
                     'magit-dimmed)))
          (insert (propertize
                   (format " %s" (code-review-section--criteria-slug req))
                   'font-lock-face 'magit-dimmed))
          (dolist (p (plist-get req :paths))
            (insert " ")
            (insert-button (file-name-nondirectory p)
                           'face 'code-review-url-header-face
                           'follow-link t
                           'help-echo "Jump to the criterion file"
                           'action (lambda (&rest _)
                                     (find-file-other-window
                                      (expand-file-name
                                       (plist-get req :file)
                                       (or worktree
                                           default-directory))))))
          (insert ?\n))
        (insert (propertize
                 (format "  next: %s\n"
                         (code-review-criteria-brief--next-action
                          (length criteria)
                          (code-review-criteria-brief--verdicts
                           (code-review-criteria-brief--states)
                           (mapcar (lambda (req)
                                     (plist-get req :id))
                                   criteria))))
                 'font-lock-face 'magit-dimmed))
        (insert ?\n)))))

;;;###autoload
(defun code-review-criteria-brief-draft ()
  "Draft the brief (station 1): hand this PR's intent to the
wired `code-review-criteria-brief-drafter-function'.  The
drafter writes EARS criteria files into the worktree's criteria
directory — untracked-but-not-ignored files count as criteria —
then the `d' pane and the review checklist render them for
EDITING (merge, drop, re-prioritize with `p') BEFORE the diff is
read.  With no drafter wired (the default): a friendly
user-error pointing at the hand-written path — the phase 27 L0."
  (interactive)
  (unless code-review-criteria-brief-drafter-function
    (user-error "No brief drafter wired (phase 27 L0): hand-write \
specs/ criteria files — M-x code-review-criteria-insert-template drafts one"))
  (let* ((pr (ignore-errors (code-review-db-get-pullreq)))
         (worktree (or code-review-repo-worktree
                       (and (fboundp 'code-review-registry--repo-dir)
                            (code-review-registry--repo-dir)))))
    (unless (and pr worktree)
      (user-error "No review with a worktree at point"))
    (let* ((infos (and (slot-boundp pr 'raw-infos) (oref pr raw-infos)))
           (body (and infos (cdr (assq 'bodyText infos))))
           (diff (ignore-errors (code-review-db--pullreq-raw-diff)))
           (paths (and diff (code-review-criteria--changed-paths diff))))
      (condition-case err
          (progn
            (funcall code-review-criteria-brief-drafter-function
                     (oref pr title) body paths worktree)
            (message "code-review: brief drafted — edit it (d pane, `p' \
re-prioritizes) BEFORE reading the diff"))
        (error
         (code-review-utils--log
          "code-review-criteria"
          (format "brief drafting failed: %S" err))
         (message "code-review: brief drafting failed (%s)"
                  (error-message-string err)))))))

(provide 'code-review-criteria-brief)
;;; code-review-criteria-brief.el ends here
