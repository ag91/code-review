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
(require 'code-review-criteria-brief)
(require 'code-review-criteria-grill)
(require 'code-review-registry)
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

;;; Phase 27, the remaining stations' reference drafters.  Same
;;; shape everywhere: a PURE prompt builder (testable) + a thin
;;; handoff that binds `default-directory' to the worktree so
;;; agent-shell resolves the session the reviewer started there.
;;; Every hook stays nil without agent-shell — the L0 is no AI.

(defun code-review-agents-shell--handoff (prompt worktree &optional label)
  "The shared send: PROMPT into the worktree's agent session.
LABEL names the work item for the message.  Non-nil when handed
off; agent-shell absent returns nil with a message."
  (if (not (fboundp 'agent-shell-prompt-queue))
      (progn
        (message "code-review: agent-shell not loaded; %s runs hand-written"
                 (or label "this station"))
        nil)
    (let ((default-directory worktree))
      (agent-shell-prompt-queue prompt)
      (message "code-review: %s handed to the agent session" label)
      t)))

(defun code-review-agents-shell-brief-prompt (title body paths worktree)
  "The station-1 BRIEF drafting prompt (pure).
The agent writes EARS criteria FILES into the worktree's
criteria directory — untracked-but-not-ignored files count —
each with `paths:' front matter and a P1..P3 `priority:'
ranking.  Draft-not-push: uncommitted, the reviewer edits
BEFORE the diff is read."
  (format
   "Brief work item: draft the review criteria for a change.

Working tree: %s
Write each criterion as a file under %sspecs/ named
YYYY-MM-DD-NNN-<subject-slug>.md (NNN = one past the highest
existing number), content:

---
paths: [<files the requirement governs>]
priority: P1 | P2 | P3
---

WHEN <trigger> THE SYSTEM SHALL <verifiable behavior>.

Write at most 8 criteria, ranked: P1 = this change must not
ship without it.  They are DRAFTS for a reviewer who will edit
them before reading the diff: short, falsifiable, no interface
details (no keybindings or UI shapes — only behavior).  Do NOT
commit, do NOT push: leave them uncommitted.

PR title: %s
PR description:
%s
Changed paths:
%s"
   worktree worktree
   (or title "(no title)")
   (or body "(no description)")
   (string-join (or paths '("-")) "\n")))

(defun code-review-agents-shell-brief-drafter (title body paths worktree)
  "The `code-review-criteria-brief-drafter-function' reference
implementation (station 1): the agent drafts the criteria files."
  (code-review-agents-shell--handoff
   (code-review-agents-shell-brief-prompt title body paths worktree)
   worktree "the brief"))

(defun code-review-agents-shell-grill-prompt (findings criteria worktree)
  "The station-4 SEMANTIC grill prompt (pure): contradictions
between criteria, subtle unfalsifiability — what the mechanical
grill cannot see.  The answer lands in the agent session; the
reviewer dispositions by EDITING the ledger."
  (let ((lines nil))
    (dolist (req criteria)
      (setq lines
            (append lines
                    (list (format "- %s (P%s): %s"
                                  (plist-get req :id)
                                  (plist-get req :priority)
                                  (plist-get req :sentence))))))
    (format
     "Grill work item: cross-check these review criteria against \
each other — CONTRADICTIONS between them (one criterion's SHALL \
defeats another's), criteria that are subtly unfalsifiable, \
criteria that assume behavior the others deny.  Answer as a \
numbered list of concrete ledger edits (rewrite sentence X to \
..., merge A into B, split C); do NOT edit any file, do NOT \
commit: the reviewer dispositions every suggestion by hand.

Working tree: %s

The criteria:
%s

Mechanical findings already known (do not repeat them):
%s"
     worktree
     (string-join (or lines '("- none")) "\n")
     (if findings
         (string-join
          (mapcar (lambda (f)
                    (format "- %s: %s" (plist-get f :id)
                            (plist-get f :evidence)))
                  findings)
          "\n")
       "- none"))))

(defun code-review-agents-shell-grill-drafter (findings criteria worktree)
  "The `code-review-criteria-grill-drafter-function' reference
implementation (station 4)."
  (code-review-agents-shell--handoff
   (code-review-agents-shell-grill-prompt findings criteria worktree)
   worktree "the grill cross-check"))

(defun code-review-agents-shell-probe-prompt (criterion worktree)
  "The REVIEW-BY-EXECUTION probe prompt (pure): a test that
fails when the criterion is violated, as a standalone file the
phase 17 runner can execute at HEAD and BASE."
  (format
   "Probe work item: write ONE executable test for a review criterion.

Working tree: %s
Write the probe as %stest/%s-probe-test.el (elisp: a single
ert-deftest; use the project's own test framework and helpers
already in the worktree).  The test must FAIL when the criterion
below is VIOLATED and PASS when it is met — no mocks of the
behavior under test beyond what the worktree provides.

Requirement (EARS): %s
Paths governed: %s

Criterion file (context): %s

Do NOT commit, do NOT push: leave the probe uncommitted — the
runner executes it at HEAD and at the BASE rev and reads the
two outcomes.
"
   worktree worktree (plist-get criterion :id)
   (plist-get criterion :sentence)
   (string-join (or (plist-get criterion :paths) '("-")) ", ")
   (plist-get criterion :file)))

(defun code-review-agents-shell-probe-drafter (criterion worktree)
  "The `code-review-criteria-probe-drafter-function' reference
implementation (review-by-execution).  Returns the probe file's
path, or nil."
  (when (code-review-agents-shell--handoff
         (code-review-agents-shell-probe-prompt criterion worktree)
         worktree
         (format "the probe for %s" (plist-get criterion :id)))
    ;; the probe file path is PREDICTABLE from the prompt contract;
    ;; the caller checks existence after the agent has run
    (expand-file-name
     (format "test/%s-probe-test.el" (plist-get criterion :id))
     worktree)))

(defun code-review-agents-shell-incident-drafter (incident file repo)
  "The `code-review-registry-drafter-function' reference
implementation: the agent drafts the incident entry's HUMAN BODY
into FILE (local registry — the reviewer edits and commits it;
nothing is posted anywhere)."
  (code-review-agents-shell--handoff
   (format
    "Incident-report work item: fill the HUMAN BODY of the registry \
entry at %s (working tree %s).

Read the entry's front matter (id, paths, invariant, tests) and \
write below a `### Human notes' heading: what happened in plain \
narrative, what invariant it broke, what now guards it, and \
what a reviewer of the NEXT change to those paths should be \
suspicious of.  Base it ONLY on the entry metadata and the \
repository (read the paths, the tests, the history); mark \
anything you could not verify with TODO.

Do NOT touch the front matter, do NOT commit, do NOT push: the \
reviewer edits the draft and commits it with the fix.

Entry: %s"
    file repo (plist-get incident :id))
   repo
   (format "the incident draft for %s" (plist-get incident :id))))

;; Wire ALL the hooks when agent-shell is present: only fill the
;; DEFAULT-EMPTY slots, never clobber a user's explicit choice.
(when (fboundp 'agent-shell-prompt-queue)
  (unless code-review-criteria-drafter-function
    (setq code-review-criteria-drafter-function
          #'code-review-agents-shell-drafter))
  (unless (bound-and-true-p code-review-criteria-brief-drafter-function)
    (setq code-review-criteria-brief-drafter-function
          #'code-review-agents-shell-brief-drafter))
  (unless (bound-and-true-p code-review-criteria-grill-drafter-function)
    (setq code-review-criteria-grill-drafter-function
          #'code-review-agents-shell-grill-drafter))
  (unless (bound-and-true-p code-review-criteria-probe-drafter-function)
    (setq code-review-criteria-probe-drafter-function
          #'code-review-agents-shell-probe-drafter))
  (unless (bound-and-true-p code-review-registry-drafter-function)
    (setq code-review-registry-drafter-function
          #'code-review-agents-shell-incident-drafter)))

(provide 'code-review-agents-shell)
;;; code-review-agents-shell.el ends here
