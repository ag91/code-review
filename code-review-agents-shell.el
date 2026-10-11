;;; code-review-agents-shell.el --- agent-shell drafter  -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: code-review contributors
;; Keywords: tools

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

;; Phase 27, the REFERENCE DRAFTER implementation.  The
;; architectural rule (the drafter/transport split, Improvements
;; phase 27): every AI capability is a DRAFTER HOOK the mode
;; CALLS but never implements (`code-review-criteria-drafter-
;; function'); transport stays the deterministic, AI-free
;; strategy chain.  This file is the ONE implementation the
;; package ships: it hands review criteria to the reviewer's
;; agent-shell session with a DRAFT-NOT-PUSH contract — the
;; regression suite is drafted as uncommitted changes in the
;; review worktree, the reviewer explores it (the T command runs
;; the mapped subset), and C-c C-g
;; (`code-review-criteria-request-regression') ships it as a
;; stacked PR the CONTRIBUTOR decides on.  Drafters for other
;; Emacs agents (gptel, ellama, ...) are contributor extensions
;; to the same hook contract.
;;
;; SOFT DEPENDENCY, the difftastic pattern: agent-shell is
;; probed, never required.  Without it this file wires nothing
;; and every station stays hand-written (the phase 27 L0: no AI
;; at all).

;;; Code:

(require 'cl-lib)
(require 'code-review-criteria)
(defvar code-review-repo-worktree)         ; code-review-repo.el
(declare-function code-review-registry--repo-dir "code-review-registry")
(declare-function agent-shell-prompt-queue "agent-shell-prompt-queue")

(defun code-review-agents-shell--worktree ()
  "The directory the drafter's agent should work in.
The review buffer's worktree (a detached checkout at the PR head
for forge PRs — the agent can trash it freely), the repository
root when no review is open, else the directory of the criterion
file itself (specs/ lives at the repo root)."
  (let ((fallback (and (fboundp 'code-review-registry--repo-dir)
                       (code-review-registry--repo-dir))))
    (file-name-as-directory
     (or (bound-and-true-p code-review-repo-worktree)
         (and fallback (not (string-empty-p fallback)) fallback)
         default-directory))))

(defun code-review-agents-shell-prompt (criterion worktree)
  "The DRAFT-NOT-PUSH handoff prompt for CRITERION in WORKTREE.
Pure: prompt text only, no transport.  The contract is the
point — the reviewer inspects the draft BEFORE anything ships
(decision selection stays with the human, Parasuraman/Sheridan/
Wickens 2000; the emit step is a separate explicit keypress)."
  (let ((paths (plist-get criterion :paths)))
    (format
     "Regression-test work item from a code review (criterion %s).

Working tree: %s
Draft HERE, and only here.

Draft the REGRESSION TEST for the requirement below as
UNCOMMITTED changes in that working tree.  Do NOT commit, do
NOT push, do NOT create or touch any branch: the reviewer
inspects the draft first and ships it with
M-x code-review-criteria-request-regression, which branches
and opens a pull request the contributor decides on.

Requirement (EARS): %s
Criteria file (already written by the reviewer): %s
Paths governed by the requirement: %s

Write a test that FAILS before the fix and PASSES after it.
Commit trailers will be added at ship time; keep the test
names greppable from the requirement id (%s).

The full criteria file:

%s"
     (plist-get criterion :id)
     worktree
     (plist-get criterion :sentence)
     (plist-get criterion :file)
     (string-join (or paths '("-")) " ")
     (plist-get criterion :id)
     (plist-get criterion :text))))

(defun code-review-agents-shell-drafter (criterion)
  "The `code-review-criteria-drafter-function' reference
implementation: hand CRITERION to the project's agent-shell
session.  The session is resolved by PROJECT of the review
worktree, so this finds the agent the reviewer started ON the
worktree/repository during the review.  Queues the prompt when
the agent is mid-run (agent-shell semantics), sends immediately
when idle, never steals focus from the review.  Returns t when
handed off, nil (with a message, never a signal) when agent-shell
is not available — the criterion stays ledger-only, nothing
evaporates."
  (if (not (fboundp 'agent-shell-prompt-queue))
      (progn
        (message "code-review: agent-shell not loaded; criterion %s stays ledger-only"
                 (plist-get criterion :id))
        nil)
    (let* ((worktree (code-review-agents-shell--worktree))
           ;; agent-shell resolves the session by CURRENT PROJECT,
           ;; so bind the directory: the comment buffer at C-c C-t
           ;; time is not the worktree.
           (default-directory worktree))
      (agent-shell-prompt-queue
       (code-review-agents-shell-prompt criterion worktree))
      (message "code-review: criterion %s handed to the agent session (draft-not-push)"
               (plist-get criterion :id))
      t)))

;; Wire the hook when agent-shell is present, the phase 27 file
;; plan: only fill the DEFAULT-EMPTY slot, never clobber a user's
;; explicit choice (a later init.el setq still wins).
(when (and (fboundp 'agent-shell-prompt-queue)
           (not code-review-criteria-drafter-function))
  (setq code-review-criteria-drafter-function
        #'code-review-agents-shell-drafter))

(provide 'code-review-agents-shell)
;;; code-review-agents-shell.el ends here
