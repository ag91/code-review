;;; code-review-criteria-grill.el --- station 4: the grill (phase 27) -*- lexical-binding: t; -*-

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

;; Phase 27, STATION 4 — THE GRILL and REVIEW-BY-EXECUTION.  The
;; order is the point: ATTEMPT FIRST, feedback after (self-
;; explanation before correction; the FAILED prediction is the
;; encoding moment) — the grill runs only AFTER verdicts are
;; recorded, and it validates the LEDGER, never the diff directly.
;; Every finding is a LEDGER SUGGESTION awaiting disposition in
;; this buffer — never a review comment (the forced disposition IS
;; the verification behavior that otherwise disappears).
;;
;; Mechanical core (bounded, no AI): the DISAGREEMENT set —
;; criteria the reviewer marked satisfied that the mechanical
;; evidence contradicts (a [x] whose spec the diff changes without
;; any test referencing it, or a [x] whose covered paths this diff
;; does not touch); vague/unfalsifiable EARS wording; P1 criteria
;; nothing covers.  Contradictions BETWEEN criteria are the AI
;; grill's job (`code-review-criteria-grill-drafter-function', nil
;; by default — the L0 is no AI at all).
;;
;; REVIEW-BY-EXECUTION (C-c C-p): a contested criterion converts
;; to an executable PROBE via the drafter hook, run at HEAD and at
;; the BASE rev on the phase 17 fake-fix substrate (detached
;; worktree, bounded).  Two crisp outcomes: the probe is red at
;; HEAD -> the code contradicts the criterion (flip the verdict,
;; emit through the phase 24 chain); it is green at BASE -> the
;; probe does not detect the criterion (a bad probe).

;;; Code:

(require 'cl-lib)
(require 'magit-section)
(require 'code-review-criteria)
(require 'code-review-criteria-brief)
(require 'code-review-utils)
(require 'code-review-testimpact)

(defvar code-review-repo-worktree)         ; code-review-repo.el
(declare-function code-review-criteria-regression--read-criterion
                  "code-review-criteria-regression")
(declare-function code-review-section--criteria-slug
                  "code-review-section-criteria")
(declare-function code-review-section--criteria-insert
                  "code-review-section-criteria")

(defcustom code-review-criteria-grill-vague-regexp
  (concat "\\(?:appropriately\\|properly\\|correctly\\|as needed\\|"
          "if necessary\\|reasonable\\|and so on\\|etc\\.\\|works? fine\\)")
  "Wordings the grill flags as vague/unfalsifiable in a
criterion's EARS sentence."
  :type 'regexp
  :group 'code-review)

(defcustom code-review-criteria-grill-drafter-function nil
  "The AI GRILL hook (phase 27; nil = mechanical grill only).
Called with (FINDINGS CRITERIA WORKTREE): FINDINGS is the
mechanical findings list, CRITERIA the requirement plists.  The
drafter cross-checks the LEDGER semantically (contradictions
between criteria, subtle unfalsifiability) — its answer lands in
the agent session for the reviewer to read and disposition by
EDITING the ledger, never as review comments.  The mode CALLS
this but never implements it."
  :type '(choice function (const nil))
  :group 'code-review)

(defcustom code-review-criteria-probe-drafter-function nil
  "The REVIEW-BY-EXECUTION drafter hook (phase 27; nil = no AI).
Called with (CRITERION WORKTREE): writes a PROBE test — a test
that FAILS when the criterion is violated and PASSES when met —
into the worktree, returning the probe file's path (or nil).
The mode CALLS this but never implements it; hand-write the
probe when the hook is nil."
  :type '(choice function (const nil))
  :group 'code-review)

;;; The mechanical grill (pure)

(defun code-review-criteria-grill--vague-p (sentence)
  "Non-nil when SENTENCE is unfalsifiable: no SHALL clause or a
vague-wording match.  Pure."
  (or (not (string-match-p "SHALL" (or sentence "")))
      (string-match-p code-review-criteria-grill-vague-regexp
                      (or sentence ""))))

(defun code-review-criteria-grill--findings (matching states diff-paths)
  "The mechanical grill findings.  Pure.
MATCHING is the checklist's ((:req REQ :paths COVERED)...) list,
STATES the id -> state hash (or nil: nothing dispositioned yet),
DIFF-PATHS the change's file paths.  Returns (:kind :id :req
:text :evidence) plists, kinds: `disagreement' (marked [x] but
the evidence contradicts it), `vague' (unfalsifiable wording),
`uncovered-p1' (the budget-carrying rank, nothing covers it)."
  (let ((res nil))
    (pcase-dolist (`(:req ,req :paths ,covered) matching)
      (let* ((id (plist-get req :id))
             (state (and states (gethash id states)))
             (covered? (cl-some
                        (lambda (p) (member p diff-paths))
                        (or covered (plist-get req :paths) '()))))
        (when (and (eq state 'satisfied) (not covered?))
          (push (list :kind 'disagreement :id id :req req
                      :text (plist-get req :sentence)
                      :evidence
                      (format "marked [x] but none of its covered paths (%s) \
is in this diff"
                              (string-join (or (plist-get req :paths)
                                               (list "<none>"))
                                           ", ")))
                res))
        (when (and (equal (plist-get req :priority) "P1") (not covered?))
          (push (list :kind 'uncovered-p1 :id id :req req
                      :text (plist-get req :sentence)
                      :evidence "P1: the budget-carrying rank, and this diff \
touches nothing it covers")
                res))
        (when (code-review-criteria-grill--vague-p
               (plist-get req :sentence))
          (push (list :kind 'vague :id id :req req
                      :text (plist-get req :sentence)
                      :evidence "unfalsifiable wording: no SHALL or a \
vague qualifier — tighten the EARS sentence")
                res))))
    (nreverse res)))

;;; The grill command (on demand, after verdicts)

(defun code-review-criteria-grill--criteria-section ()
  "The review buffer's criteria checklist section, or nil (one
bounded tree walk, results via side effect)."
  (let ((found nil))
    ;; no review buffer under point? (a temp/other buffer has no
    ;; section tree — magit-map-sections would walk nil)
    (when (and (boundp 'magit-root-section) magit-root-section)
      (magit-map-sections
       (lambda (sec)
         (when (eq (eieio-object-class sec)
                   'code-review-criteria-section)
           (setq found sec)))))
    found))

(defun code-review-criteria-grill--insert (findings)
  "Insert the grill findings section after the checklist.
Each row is a LEDGER SUGGESTION awaiting disposition: `d'
dismisses it (recorded, the row dims — the forced disposition IS
the verification behavior), the criterion button opens the file."
  (save-excursion
    ;; the suggestions live WITH the criteria: insert at the
    ;; checklist section's end (a top-level insert at eob would
    ;; nest under whatever section owns the buffer tail)
    (goto-char (or (let ((sec (code-review-criteria-grill--criteria-section)))
                     (and sec (oref sec end)))
                   (point-max)))
    (let ((inhibit-read-only t))
      (magit-insert-section (code-review-criteria-grill-section)
        (insert (propertize
                 (format "Grill — %d ledger suggestion%s (d dismisses, \
buttons jump)"
                         (length findings)
                         (if (cdr findings) "s" ""))
                 'font-lock-face 'magit-section-heading))
        (magit-insert-heading)
        (pcase-dolist (`(:kind ,kind :id ,id :req ,req
                               :evidence ,evidence)
                       findings)
          (magit-insert-section (code-review-criteria-grill-item-section
                                 (list :id id) t)
            (code-review-section--criteria-insert
             (format "  [%s] " (upcase (substring (symbol-name kind) 0 4))))
            (insert-button id
                           'face 'code-review-url-header-face
                           'follow-link t
                           'help-echo "Open the criteria file"
                           'action (lambda (&rest _)
                                     (find-file-other-window
                                      (plist-get req :file))))
            (insert (propertize (format " %s"
                                        (code-review-section--criteria-slug
                                         req))
                                'font-lock-face 'magit-dimmed))
            (insert (propertize (format " — %s" evidence)
                                'font-lock-face
                                'font-lock-warning-face))
            (magit-insert-heading)
            (code-review-section--criteria-insert
             (concat "      " (plist-get req :sentence))
             'font-lock-face 'magit-dimmed)
            (code-review-section--criteria-insert "\n")))
        (insert ?\n)))))

(defun code-review-criteria-grill-dismiss ()
  "Dismiss the grill suggestion at point (`d'): recorded for this
buffer, the row dims.  The suggestion is disposed of, not
silently forgotten — this keypress IS the disposition."
  (interactive)
  (let ((sec (magit-current-section)))
    (when (and sec
               (eq (eieio-object-class sec)
                   'code-review-criteria-grill-item-section))
      (cl-pushnew (oref sec value)
                  code-review-criteria-grill--dismissed
                  :test #'equal)
      (save-excursion
        (let ((inhibit-read-only t))
          (add-face-text-property
           (oref sec start) (oref sec end) 'magit-dimmed t))))))

(defclass code-review-criteria-grill-section (magit-section)
  (()))
(defclass code-review-criteria-grill-item-section (magit-section)
  ((keymap :initform 'code-review-criteria-grill-item-section-map)))
(defvar code-review-criteria-grill-item-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "d") 'code-review-criteria-grill-dismiss)
    map)
  "`d' dismisses the grill suggestion at point (its disposition).")

;;;###autoload
(defun code-review-criteria-grill ()
  "The grill (g, phase 27 station 4): validate the LEDGER, never
the diff directly — only AFTER verdicts are recorded (attempt
first, feedback after: the failed prediction is the encoding
moment).  Mechanical findings: disagreement ([x] vs the
mechanical evidence), vague/unfalsifiable EARS, P1 criteria
nothing covers.  With a grill drafter wired, the semantic
cross-check (contradictions) runs too — its answer lands in the
agent session to read and disposition by editing.  Output is a
ledger suggestions section in this buffer: `d' disposes of a
row, buttons jump.  Never review comments."
  (interactive)
  (unless (and (code-review-db-local-pr-p) code-review-repo-worktree)
    (user-error "The grill runs in a local review with a worktree"))
  (let* ((diff (code-review-db--pullreq-raw-diff))
         (paths (and diff (code-review-criteria--changed-paths diff)))
         (criteria (and paths
                        (code-review-criteria--criteria
                         code-review-repo-worktree)))
         (matching (and criteria
                        (code-review-criteria--matching
                         criteria paths))))
    (unless (and matching code-review-criteria--states
                 (cl-some (lambda (m)
                            (not (eq (gethash (plist-get
                                               (plist-get m :req) :id)
                                              code-review-criteria--states)
                                     'pending)))
                          matching))
      (user-error "Record verdicts first (RET on the checklist rows) — \
the grill runs after the attempt, not before"))
    (let ((findings (code-review-criteria-grill--findings
                     matching code-review-criteria--states paths)))
      (setq code-review-criteria-grill--run? t)
      (code-review-criteria-grill--insert findings)
      (when code-review-criteria-grill-drafter-function
        (condition-case err
            (funcall code-review-criteria-grill-drafter-function
                     findings criteria code-review-repo-worktree)
          (error
           (code-review-utils--log
            "code-review-criteria"
            (format "grill drafting failed: %S" err)))))
      (message "code-review: grill — %d mechanical suggestion%s%s"
               (length findings)
               (if (cdr findings) "s" "")
               (if code-review-criteria-grill-drafter-function
                   " (the semantic cross-check runs in the agent session)"
                 "")))))

;;; Review-by-execution (C-c C-p)

(defun code-review-criteria-grill--probe-interpret (head base)
  "The probe verdict for HEAD/BASE statuses.  Pure."
  (cond
   ((and (eq head 'pass) (eq base 'fail))
    "probe green at HEAD, red at BASE: the criterion is MET by this change")
   ((eq head 'fail)
    "probe RED at HEAD: the code CONTRADICTS the criterion — flip the \
verdict ([!] with RET) and/or emit the regression suite (C-c C-t, C-c C-g)")
   ((and (eq head 'pass) (eq base 'pass))
    "probe green at BASE: it does not DETECT the criterion — a bad probe, \
draft it again")
   (t "probe inconclusive (run timed out or no base rev)")))

;;;###autoload
(defun code-review-criteria-probe-run (criterion)
  "Convert CRITERION to an executable probe and run it (C-c C-p,
phase 27 review-by-execution).  The probe drafter hook writes a
test that fails when the criterion is violated; the phase 17
fake-fix substrate runs it at HEAD and at the BASE rev (detached
worktree, bounded).  Reading the two outcomes:
  HEAD pass + BASE fail -> the criterion is MET by this change;
  HEAD fail             -> the code CONTRADICTS the criterion —
                           flip the verdict and/or emit (C-c C-t
                           then C-c C-g);
  BASE pass             -> the probe does not detect the
                           criterion (draft again).
With no probe drafter wired (the default): hand-write the probe
test and run it with the T command — same substrate.  Interactive
CRITERION: the checklist row at point, else completing-read."
  (interactive
   (list (code-review-criteria-regression--read-criterion
          (or (bound-and-true-p code-review-repo-worktree)
              (code-review-registry--repo-dir)))))
  (unless (and code-review-repo-worktree
               code-review-criteria-probe-drafter-function)
    (user-error "No probe drafter wired (phase 27 L0): hand-write the \
probe test and run it with T"))
  (let ((probe (funcall code-review-criteria-probe-drafter-function
                        criterion code-review-repo-worktree)))
    (unless (and probe (file-exists-p probe))
      (message "code-review: no probe drafted for %s" (plist-get criterion :id)))
    (when (and probe (file-exists-p probe))
      (let ((cmd (code-review-testimpact--command-for
                  code-review-repo-worktree
                  (list (file-relative-name probe
                                            code-review-repo-worktree))
                  nil)))
        (unless cmd
          (user-error "No test command for this project — set \
code-review-testimpact-test-command-alist"))
        (message "code-review: probe for %s drafted, running at HEAD and BASE — \
HEAD pass+BASE fail = met; HEAD fail = contradicts (flip or emit); \
BASE pass = bad probe"
                 (plist-get criterion :id))
        (code-review-testimpact--fake-fix-run
         code-review-repo-worktree cmd
         (list (file-relative-name probe code-review-repo-worktree)))))))

(provide 'code-review-criteria-grill)
;;; code-review-criteria-grill.el ends here
