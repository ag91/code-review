;;; code-review-budget.el --- Review budget: risk-ranked attention, honest accounting -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Andrea
;; Keywords: git, tools, vc

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

;; Phase 25 (Improvements.org): the review budget.
;;
;; Attention is fixed and PRs arrive all day; today the rationing
;; happens hidden, performed badly by fatigue.  This makes it an
;; EXPLICIT allocation problem:
;; - RISK MODEL (composition of existing signals, no new harvest):
;;   phase 14 heat x phase 15 delicacy x phase 22 incident
;;   annotations x size/complexity, all read from caches the review
;;   renders already populate — the queue never harvests.  Cost
;;   estimation from hunk count/size is deterministic.  The core is
;;   pure: `code-review-budget--assess' takes the signals as a plist.
;;
;; - ADMISSION is a RATE with continuous admission, NOT a day-start
;;   batch: spent minutes are sunk (no clawback, ever); when the
;;   reviewer sits down the queue offers the current risk-ranked
;;   items within the remaining balance and the per-PR cap.  A
;;   later-arriving high-risk PR jumps the queue for free (ranking
;;   is pure and stateless: it re-ranks at the next sit-down).
;;
;; - OUTPUT LISTS over `code-review-db-all-unfinished': ADMIT (the
;;   ranked slice that fits the balance, deep review) and SAFE /
;;   APPROVE-ON-GATES (low heat, low blast radius, traceability
;;   green) with a ONE-KEY approve that leaves an explicit
;;   audit-trail comment.  A RANDOM AUDIT SLICE of the safe list
;;   still gets human minutes — the calibration check on the risk
;;   model.  Incident-touched PRs (phase 22) are NEVER sampleable:
;;   auto-elevated to mandatory human review.  PRs estimated over
;;   the bounce threshold leave the queue with a one-key
;;   "unreviewable, please split" comment.
;;
;; - SIGNALLING: the queue header reports risk backlog / review
;;   capacity ("2.3x daily budget") — team-level, never per-developer
;;   (Goodhart).
;;
;; Capability ladder (the cockpit constraint): L0 is bare git + the
;; local db.  Forge signals degrade to "unknown" and unknown risk is
;; honestly ranked as needing human eyes, never waved through.

;;; Code:

(require 'cl-lib)
(require 'format-spec)
(require 'tabulated-list)
(require 'subr-x)
(require 'code-review-diff)
(require 'code-review-criteria)
(require 'code-review-db)

(defvar code-review-repo-worktree)         ; code-review-repo.el
(defvar code-review-history--order-table)  ; code-review-history.el
(defvar code-review-analysis--cache)       ; code-review-analysis.el
;; valueless on purpose (AGENTS.md): the owning defcustom installs
;; the default when code-review-analysis loads
(defvar code-review-analysis-delicacy-threshold)

(declare-function code-review-registry--incidents "code-review-registry")
(declare-function code-review-registry--incident-paths "code-review-registry")
(declare-function code-review-new-issue-comment "code-review-interfaces")
(declare-function code-review-start "code-review")
(declare-function code-review-utils--log "code-review-utils")

;;; Configuration

(defgroup code-review-budget nil
  "Review budget: risk-ranked attention, honest accounting (phase 25)."
  :group 'code-review)

(defcustom code-review-review-budget-minutes 90
  "Deep-review capacity per team-day, in minutes.
nil disables the budget machinery entirely (the queue still
ranks; nothing is rationed).  Team-level, never per-developer."
  :group 'code-review-budget
  :type '(choice (const :tag "Disabled" nil) natnum))

(defcustom code-review-review-per-pr-minutes 15
  "Ideal deep-review allocation for ONE PR, in minutes.
Also a diagnostic: a PR whose realistic estimate exceeds this is
over-budget by construction (it cannot be SAFE-tier)."
  :group 'code-review-budget
  :type 'number)

(defcustom code-review-review-bounce-minutes 45
  "A PR estimated above this many minutes is BOUNCED.
It leaves the human queue with a one-key \"unreviewable, please
split\" template comment: size discipline matters MORE with AI
generation, not less."
  :group 'code-review-budget
  :type 'number)

(defcustom code-review-review-audit-rate 0.1
  "Fraction of SAFE (gate-approved) PRs randomly drawn for human audit.
What financial auditors do: the sample calibrates whether the
risk model is lying (catches the boring-looking hunk with a
subtle flaw)."
  :group 'code-review-budget
  :type 'number)

(defcustom code-review-review-cost-base 2.0
  "Fixed cost of reading the PR description and building context."
  :group 'code-review-budget
  :type 'number)

(defcustom code-review-review-cost-per-hunk 0.75
  "Minutes per diff hunk: the ORIENTATION cost, not the reading cost.
The model splits review time in two.  Reading the changed lines
themselves is carried by `code-review-review-cost-per-100-lines';
this term prices what happens AROUND the reading — locating the
hunk in the file, loading the surrounding code into your head,
checking what the neighbors do.  That overhead is roughly
constant per hunk regardless of hunk size (a 3-line hunk in a
file you know still costs the locate-and-orient trip), hence a
per-hunk constant.  0.75 min = ~45s per hunk: a calibration
guess against the author's own review practice, NOT a derived
number — tune it against minutes actually logged (the queue's
`s' command) and treat the pair of cost constants as the model's
calibration dials."
  :group 'code-review-budget
  :type 'number)

(defcustom code-review-review-cost-per-100-lines 1.5
  "Estimated minutes per 100 added lines."
  :group 'code-review-budget
  :type 'number)

(defcustom code-review-review-approve-template
  "Approved on gates, no human deep review.

- traceability: no strong findings (no unbound spec change)
- heat: all changed files in the COLD bucket
- delicacy: no hunk at or above the delicacy threshold
- estimated review cost: %c minutes

This is an explicit audit-trail entry: this PR was merged on
machine-checked gate evidence, not on human line review."
  "Body of the approve-on-gates audit-trail comment.
Format-spec chars: %p the PR key (owner/repo#N), %t the title,
%c the estimated cost, %r the signal evidence lines."
  :group 'code-review-budget
  :type 'string)

(defcustom code-review-review-bounce-template
  "Unreviewable, please split.

This PR's estimated review cost is %c minutes (threshold: %b).
A changeset this large cannot receive real human attention, and
size discipline matters more with AI generation, not less.
Please split it into independently reviewable changes."
  "Body of the bounce (\"please split\") comment.
Format-spec chars: %p the PR key, %t the title, %c the estimated
cost, %b the bounce threshold, %r the signal evidence lines."
  :group 'code-review-budget
  :type 'string)

(defcustom code-review-budget-state-file
  (expand-file-name "code-review-budget.el" user-emacs-directory)
  "Local tool state: manually logged minutes and queue decisions.
Untracked personal state, like the criteria home index."
  :group 'code-review-budget
  :type 'file)

;;; Pure: cost estimation from the diff shape

(defun code-review-budget--shape (diff)
  "Size/complexity shape of raw DIFF text.
Plist (:files :hunks :added :deleted) — the deterministic cost
inputs.  Only lines INSIDE hunks count: the file header's own
---/+++ lines are metadata, not changes.  Pure."
  (let ((files 0) (hunks 0) (added 0) (deleted 0) (in-hunk nil))
    (dolist (blk (code-review--diff--split-by-files diff))
      (setq files (1+ files)
            in-hunk nil)
      (dolist (line (split-string (cdr blk) "\n" t))
        (cond
         ((string-prefix-p "diff --git" line) (setq in-hunk nil))
         ((string-prefix-p "@@" line)
          (setq hunks (1+ hunks)
                in-hunk t))
         (in-hunk
          (cond
           ((string-prefix-p "+" line) (setq added (1+ added)))
           ((string-prefix-p "-" line) (setq deleted (1+ deleted))))))))
    (list :files files :hunks hunks :added added :deleted deleted)))

(defun code-review-budget--cost (shape)
  "Estimated deep-review MINUTES for the diff SHAPE.
Deterministic: base context + per-hunk + per-100-added-lines,
rounded to one decimal.  Pure."
  (let ((c (+ code-review-review-cost-base
              (* code-review-review-cost-per-hunk
                 (plist-get shape :hunks))
              (* code-review-review-cost-per-100-lines
                 (/ (plist-get shape :added) 100.0)))))
    (/ (round (* 10.0 c)) 10.0)))

;;; Pure: the risk model (composition of existing signals)

(defun code-review-budget--heat-factor (heat-entries)
  "Heat factor and reason from HEAT-ENTRIES (phase 14 plists).
nil entries = heat unknown: factor 1.0, honestly reported.
HOT 1.5, WARM 1.2, COLD 1.0; the worst bucket wins.  Pure."
  (if (null heat-entries)
      (list 1.0 "heat unknown")
    (let ((worst (cl-find-if
                  (lambda (b)
                    (cl-some (lambda (e)
                               (equal (plist-get e :bucket) b))
                             heat-entries))
                  '("HOT" "WARM" "COLD"))))
      (list (cond ((equal worst "HOT") 1.5)
                  ((equal worst "WARM") 1.2)
                  (t 1.0))
            (format "heat: %s" (or worst "none"))))))

(defun code-review-budget--delicacy-factor (delicate-hunks)
  "Delicacy factor and reason from DELICATE-HUNKS (phase 15).
Delicacy is phase 15's per-hunk RISK score
(`code-review-analysis--cache'): how much blast radius the
touched code has — how many call sites depend on the defs the
hunk edits (history/coupling data), how old and how owned the
blamed lines are (ancient code rewritten by a new author hides
subtle breakage), whether the branch has drifted, whether the
touched def is dead.  It answers a different question than the
cost model does: hunk counts and added lines measure READING
EFFORT, delicacy measures CONSEQUENCE — a two-line change to a
function half the codebase calls is cheap to read and expensive
to get wrong, and it is exactly where sampled-out risk
accumulates.  That is why delicacy multiplies the risk ranking
(and can force ADMIT tier) while leaving the minute estimate
untouched: the minutes price the reading, the tier prices the
stakes.

nil = delicacy unknown (factor 1.0).  The factor is 1 + the
worst hunk score, capped at 3.0 — the WORST hunk wins because
review attention is not additive across hunks: one delicate hunk
already demands the deep pass.  Pure."
  (if (null delicate-hunks)
      (list 1.0 "delicacy unknown")
    (let ((worst 0.0))
      (dolist (h delicate-hunks)
        (setq worst (max worst (or (plist-get h :score) 0.0))))
      (list (min 3.0 (+ 1.0 worst))
            (format "delicacy max %.2f" worst)))))

(defun code-review-budget--incident-factor (incident-hits)
  "Incident factor and reason from INCIDENT-HITS.
nil = unknown (factor 1.0).  Each hit +0.5, capped at +2.0;
hits > 0 make the PR MANDATORY human review (phase 22: incident
paths are never sampleable).  Pure."
  (cond
   ((null incident-hits) (list 1.0 "incidents unknown"))
   ((zerop incident-hits) (list 1.0 "no incidents"))
   (t (list (+ 1.0 (min 2.0 (* 0.5 incident-hits)))
            (format "%d incident path(s)" incident-hits)))))

(defun code-review-budget--size-factor (shape)
  "Size/complexity factor and reason from the diff SHAPE.  Pure."
  (let ((added (plist-get shape :added)))
    (list (+ 1.0 (min 2.0 (/ added 500.0)))
          (format "%d file(s), %d hunks, +%d/-%d"
                  (plist-get shape :files)
                  (plist-get shape :hunks)
                  added (plist-get shape :deleted)))))

(defun code-review-budget--assess (diff signals)
  "Assess one PR whose raw diff is DIFF, given SIGNALS.
SIGNALS is a plist (:trace FINDINGS :heat-entries ENTRIES-or-nil
:delicate-analyzed BOOL :delicate-hunks ENTRIES-or-nil
:incident-hits N-or-nil) — nil members mean \"signal unknown\",
never \"signal negative\".  Returns a plist (:cost :risk :tier
:reasons :mandatory :shape :trace).

Tiers: `bounce' (estimated over the split threshold), `safe'
(low heat, low blast radius, traceability green, fits one ideal
slot), `admit' (everything else).  Incident hits force tier
`admit' with :mandatory t.  Unknown signals never make a PR
safe.  Pure."
  (let* ((shape (code-review-budget--shape diff))
         (cost (code-review-budget--cost shape))
         (trace (plist-get signals :trace))
         (strong (cl-count-if (lambda (f)
                                (eq (plist-get f :kind) 'strong))
                              (or trace nil)))
         (hits (plist-get signals :incident-hits))
         (heat-known (not (null (plist-get signals :heat-entries))))
         (heat-low (and heat-known
                        (not (cl-some
                              (lambda (e)
                                (member (plist-get e :bucket)
                                        '("HOT" "WARM")))
                              (plist-get signals :heat-entries)))))
         (del-known (plist-get signals :delicate-analyzed))
         (del-low (and del-known
                       (not (cl-some
                             (lambda (h)
                               (>= (or (plist-get h :score) 0.0)
                                   code-review-analysis-delicacy-threshold))
                             (or (plist-get signals :delicate-hunks)
                                 ())))))
         (mandatory (and hits (> hits 0)))
         (tier (cond
                ((>= cost code-review-review-bounce-minutes) 'bounce)
                (mandatory 'admit)
                ((and (= strong 0)
                      (and hits (zerop hits))
                      heat-known heat-low
                      del-known del-low
                      (<= cost code-review-review-per-pr-minutes))
                 'safe)
                (t 'admit)))
         (hf (code-review-budget--heat-factor
              (plist-get signals :heat-entries)))
         (df (code-review-budget--delicacy-factor
              (plist-get signals :delicate-hunks)))
         (inf (code-review-budget--incident-factor hits))
         (sf (code-review-budget--size-factor shape))
         (reasons (append (list (nth 1 sf) (nth 1 hf)
                                (nth 1 df) (nth 1 inf))
                          (unless (zerop strong)
                            (list (format "%d strong trace finding(s)"
                                          strong)))))
         (risk (* (car hf) (car df) (car inf) (car sf))))
    (list :cost cost :risk risk :tier tier
          :reasons reasons :mandatory mandatory
          :shape shape :trace trace)))

;;; Pure: ranking and admission

(defun code-review-budget--rank (entries)
  "Sort budget ENTRIES (plists carrying :assess) risk-first.
Total order for determinism: risk desc, then cost desc, then key
asc.  Pure."
  (sort entries
        (lambda (a b)
          (let* ((aa (plist-get a :assess))
                 (ba (plist-get b :assess)))
            (or (> (plist-get aa :risk) (plist-get ba :risk))
                (and (= (plist-get aa :risk) (plist-get ba :risk))
                     (or (> (plist-get aa :cost) (plist-get ba :cost))
                         (and (= (plist-get aa :cost)
                                (plist-get ba :cost))
                              (string< (plist-get a :key)
                                       (plist-get b :key))))))))))

(defun code-review-budget--allocate (ranked remaining audit-p)
  "Allocate the RANKED queue against REMAINING minutes.
AUDIT-P is a one-argument predicate selecting SAFE entries for
the random audit slice (injected for determinism).  Returns a
plist (:admit :audit :safe :deferred :bounce).  Semantics:

- bounced entries never consume budget (they leave the queue
  with the split template);
- MANDATORY entries (incident paths) are admitted FIRST, even
  beyond the balance: that over-run is the honest \"risk
  creation is outpacing verification capacity\" signal;
- then the risk-ranked ADMIT slice, greedy top-down: an entry
  is admitted only if it still fits the remaining balance, and
  entries that do not fit are DEFERRED, not cherry-picked away;
- the audit draw comes last (human minutes), also budget-bound;
  SAFE entries never wait for a human either way.

Spent minutes are sunk: nothing here reclaims them.  Pure."
  (let (admit audit safe deferred bounce still rest)
    (dolist (e ranked)
      (let* ((a (plist-get e :assess))
             (tier (plist-get a :tier)))
        (pcase tier
          (`bounce (push e bounce))
          (`safe (push e safe))
          (`admit
           (if (plist-get a :mandatory)
               (progn
                 (setq remaining (- remaining (plist-get a :cost)))
                 (push e admit))
             (push e deferred))))))
    ;; greedy fill of the remaining balance, risk order preserved
    ;; (separate accumulation lists: never push onto the list the
    ;; dolist is iterating — the aliasing corrupts both buckets)
    (dolist (e (nreverse deferred))
      (let ((cost (plist-get (plist-get e :assess) :cost)))
        (if (>= remaining cost)
            (progn
              (setq remaining (- remaining cost))
              (push e admit))
          (push e still))))
    ;; the random audit slice: SAFE entries drawn by AUDIT-P, also
    ;; bounded by the remaining balance
    (dolist (e (nreverse safe))
      (let ((cost (plist-get (plist-get e :assess) :cost)))
        (if (and (funcall audit-p e) (>= remaining cost))
            (progn
              (setq remaining (- remaining cost))
              (push e audit))
          (push e rest))))
    (list :admit (nreverse admit)
          :audit (nreverse audit)
          :safe (nreverse rest)
          :deferred (nreverse still)
          :bounce (nreverse bounce))))

;;; Local state (untracked personal tool state, like specs/index.md)

(defun code-review-budget--state-load ()
  "Load the budget state plist (:log :approved :bounced).
An empty or unreadable state file reads as fresh state."
  (condition-case err
      (or (when (file-exists-p code-review-budget-state-file)
            (with-temp-buffer
              (insert-file-contents code-review-budget-state-file)
              (unless (zerop (buffer-size))
                (read (current-buffer)))))
          (list :log nil :approved nil :bounced nil))
    (error
     (message "code-review-budget: state unreadable, resetting (%s)"
              (error-message-string err))
     (list :log nil :approved nil :bounced nil))))

(defun code-review-budget--state-save (state)
  "Persist the budget STATE plist."
  (let ((print-length nil))
    (with-temp-file code-review-budget-state-file
      (prin1 state (current-buffer)))))

(defun code-review-budget--state-record (field key)
  "Record KEY under the state plist's FIELD (idempotent)."
  (let* ((state (code-review-budget--state-load))
         (keys (plist-get state field)))
    (unless (member key keys)
      (plist-put state field (cons key keys))
      (code-review-budget--state-save state))))

(defun code-review-budget-log-minutes (minutes)
  "Log manually spent review MINUTES against today's budget.
For the time that never shows up in a finished row."
  (interactive "nMinutes spent reviewing: ")
  (let* ((state (code-review-budget--state-load))
         (today (format-time-string "%Y-%m-%d"))
         (log (plist-get state :log)))
    (plist-put state :log
               (cons (cons today (+ minutes (or (cdr (assoc today log)) 0)))
                     (cl-remove-if (lambda (e) (equal (car e) today))
                                   log)))
    (code-review-budget--state-save state)
    (message "Logged %d review minutes for %s" minutes today)))

(defun code-review-budget--manual-spent-today ()
  "Manually logged minutes for today."
  (cdr (assoc (format-time-string "%Y-%m-%d")
              (plist-get (code-review-budget--state-load) :log))))

(defun code-review-budget--finished-today-cost ()
  "Minutes spent on the reviews finished today (sunk, still counted).
MEASURED minutes replace the estimate: the focus tracker
(`code-review-budget-focus.el', phase 25b follow-up) books the
seconds a review buffer held the selected window, and that
actual spend is what a finished row consumes.  The diff-shape
estimate is the FALLBACK for rows the tracker never saw (daemon
restart, tracking off, a review finished without focus time).
Never signals: a db problem reads as 0."
  (condition-case err
      (let ((db (code-review-db))
            (today-re
             (format "\\`[A-Z][a-z][a-z] %s +%s [0-9:]* %s\\'"
                     (format-time-string "%b")
                     (format-time-string "%e")
                     (format-time-string "%Y")))
            (total 0.0))
        (dolist (row (emacsql db
                              [:select [owner repo number raw-diff
                                        finished-at]
                                       :from pullreq
                                       :where (= finished 't)]))
          (let ((finished-at (nth 4 row)))
            (when (and finished-at
                       (string-match-p today-re finished-at))
              (let* ((key (format "%s/%s#%s"
                                  (nth 0 row) (nth 1 row) (nth 2 row)))
                     (measured
                      (and (fboundp
                            'code-review-budget-focus-minutes-for)
                           (code-review-budget-focus-minutes-for
                            key))))
                (setq total
                      (+ total (or (and measured (> measured 0)
                                        measured)
                                   (code-review-budget--cost
                                    (code-review-budget--shape
                                     (nth 3 row))))))))))
        total)
    (error
     (code-review-utils--log
      "code-review-budget"
      (format "finished-today scan failed: %s" (error-message-string err)))
     0.0)))

(defun code-review-budget--spent-today ()
  "Minutes already spent today: finished rows + manual log."
  (+ (code-review-budget--finished-today-cost)
     (or (code-review-budget--manual-spent-today) 0)))

(defun code-review-budget--remaining ()
  "Deep-review minutes left today, or nil when the budget is off."
  (and code-review-review-budget-minutes
       (max 0 (- code-review-review-budget-minutes
                 (code-review-budget--spent-today)))))

;;; Signal assembly (no new harvest: read existing caches only)

(defun code-review-budget--pr-key (pr)
  "Human key for the db PR row PR: owner/repo#N."
  (format "%s/%s#%s" (oref pr owner) (oref pr repo) (oref pr number)))

(defun code-review-budget--delicate-signal (pr diff)
  "(:analyzed . HUNKS) from the phase 15 analysis cache.
The cache stores nil for \"analyzed, nothing to report\", so the
missing-key sentinel distinguishes never-analyzed (nil car) from
analyzed-and-clean (t car, nil hunks).  Never computes."
  (let* ((id (oref pr id))
         (key (and id diff (concat id "|" (md5 diff))))
         (cached (and (boundp 'code-review-analysis--cache) key
                      (gethash key code-review-analysis--cache
                               'missing))))
    (cond
     ((eq cached 'missing) (cons nil nil))
     (t (cons t (plist-get cached :hunks))))))

(defvar code-review-budget--current-pr nil
  "The current review's db row, captured at queue build time.
The incident signal only fires for rows of THIS repository.")

(defun code-review-budget--incident-hits (pr diff)
  "Changed paths of PR's DIFF touching its repository's incident paths.
Computed only when the row's repository is the CURRENT review's
repository (the phase 22 registry scan is per repo and this
queue never harvests); nil = unknown otherwise.  Never signals."
  (when (and diff
             code-review-repo-worktree
             code-review-budget--current-pr
             (equal (oref pr owner)
                    (ignore-errors (oref code-review-budget--current-pr
                                         owner)))
             (equal (oref pr repo)
                    (ignore-errors (oref code-review-budget--current-pr
                                         repo))))
    (ignore-errors
      (let* ((incidents (code-review-registry--incidents
                         code-review-repo-worktree))
             (paths (and incidents
                         (code-review-registry--incident-paths
                          incidents)))
             (n 0))
        (when paths
          (dolist (blk (code-review--diff--split-by-files diff))
            (let ((ids (gethash (car blk) paths)))
              (when ids
                (setq n (+ n (length ids)))))))
        n))))

(defun code-review-budget--signals (pr diff)
  "Composed risk SIGNALS for the db PR row PR from its DIFF.
Reads ONLY caches the review renders already populate: the phase
14 heat order table, the phase 15 delicacy cache, the phase 22
registry scan, and the phase 24 traceability pass (pure, itself
cached).  A missing signal is nil = unknown."
  (let* ((id (oref pr id))
         (key (and id diff (concat id "|" (md5 diff))))
         (delicate (code-review-budget--delicate-signal pr diff))
         (heat (and (boundp 'code-review-history--order-table) key
                    (gethash key code-review-history--order-table)))
         (hits (code-review-budget--incident-hits pr diff)))
    (list :trace (code-review-criteria--traceability-run id diff)
          :heat-entries heat
          :delicate-analyzed (car delicate)
          :delicate-hunks (cdr delicate)
          :incident-hits hits)))

;;; The queue

(defun code-review-budget--entry (pr)
  "Budget ENTRY plist for the db PR row PR, or nil without a diff.
(:key :pr :title :url :localp :assess)."
  (let ((diff (oref pr raw-diff)))
    (when diff
      (let* (;; exact-class check (cl-typep signals "Unknown type"
             ;; for an unloaded class symbol)
             (localp (eq (eieio-object-class pr)
                         'code-review-local-diff))
             (assess (code-review-budget--assess
                      diff (code-review-budget--signals pr diff))))
        (list :key (code-review-budget--pr-key pr)
              :pr pr
              :title (or (oref pr title) "")
              :url (and (not localp) (ignore-errors (oref pr url)))
              :localp localp
              :assess assess)))))

(defun code-review-budget--collect ()
  "Budget entries for every unfinished saved review in the db.
`code-review-db-all-unfinished' (one row per PR since phase 21),
assessed.  Entries already approved or bounced locally are
EXCLUDED — honest accounting: their decision is recorded."
  (setq code-review-budget--current-pr
        (ignore-errors (code-review-db-get-pullreq)))
  (let* ((state (code-review-budget--state-load))
         (decided (append (plist-get state :approved)
                          (plist-get state :bounced))))
    (cl-loop for pr in (code-review-db-all-unfinished)
             for entry = (code-review-budget--entry pr)
             when (and entry
                       (not (member (plist-get entry :key) decided)))
             collect entry)))

(defun code-review-budget--audit-p (_entry)
  "Random audit draw for a SAFE entry.
`code-review-review-audit-rate' of the safe list; deterministic
tests inject their own predicate into
`code-review-budget--allocate' instead."
  (< (abs (random 1.0)) code-review-review-audit-rate))

(defun code-review-budget--allocations (entries)
  "Allocation plist over the ranked ENTRIES."
  (code-review-budget--allocate
   (code-review-budget--rank (copy-sequence entries))
   (or (code-review-budget--remaining) most-positive-fixnum)
   #'code-review-budget--audit-p))

(defun code-review-budget--tier-of (entry allocations)
  "The displayed tier tag for ENTRY in ALLOCATIONS."
  (cond
   ((member entry (plist-get allocations :admit)) "ADMIT")
   ((member entry (plist-get allocations :audit)) "AUDIT")
   ((member entry (plist-get allocations :safe)) "SAFE")
   ((member entry (plist-get allocations :bounce)) "BOUNCE")
   (t "DEFER")))

(defun code-review-budget--backlog-x (entries)
  "Risk backlog: total ADMIT-tier minutes over one daily budget.
The honest version of \"nobody has time to review\" — team-level,
addressed to whoever plans the verification capacity."
  (if (or (null code-review-review-budget-minutes)
          (zerop code-review-review-budget-minutes))
      "n/a"
    (let ((total 0.0))
      (dolist (e entries)
        (when (eq (plist-get (plist-get e :assess) :tier) 'admit)
          (setq total (+ total (plist-get (plist-get e :assess)
                                          :cost)))))
      (format "%.1fx daily budget"
              (/ total (float code-review-review-budget-minutes))))))

(defvar code-review-budget--table nil
  "Buffer-local hash of the displayed entries, keyed by PR key.")

(defun code-review-budget--row (entry tier)
  "Tabulated (ID COLS) row for the ENTRY at TIER.
A proper two-element list, NOT a bare (ID . VECTOR) cons: the
default column sorter reads (cadr A) and aref's the vector."
  (let* ((a (plist-get entry :assess))
         (mandatory (plist-get a :mandatory)))
    (list (plist-get entry :key)
          (vector (plist-get entry :key)
                  (format "%.2f" (plist-get a :risk))
                  (format "%.1f" (plist-get a :cost))
                  (concat tier (when mandatory "!"))
                  (string-join (plist-get a :reasons) "; ")))))

(defun code-review-budget--refresh ()
  "Rebuild the queue buffer's contents from the db."
  (let* ((entries (code-review-budget--collect))
         (allocs (code-review-budget--allocations entries))
         (all (append (plist-get allocs :admit)
                      (plist-get allocs :audit)
                      (plist-get allocs :safe)
                      (plist-get allocs :deferred)
                      (plist-get allocs :bounce))))
    (setq code-review-budget--table
          (let ((table (make-hash-table :test #'equal)))
            (dolist (e entries table) (puthash (plist-get e :key) e table)))
          tabulated-list-entries
          (mapcar (lambda (e)
                    (code-review-budget--row
                     e (code-review-budget--tier-of e allocs)))
                  all)
          header-line-format
          (format " Review budget: %s min left today | backlog: %s | %d open PR(s)"
                  (or (code-review-budget--remaining)
                      "unlimited (budget off)")
                  (code-review-budget--backlog-x entries)
                  (length all))))
  (tabulated-list-print))

;;;###autoload
(defun code-review-budget-queue ()
  "Open the review budget queue: risk-ranked admission, honest accounting."
  (interactive)
  (pop-to-buffer (get-buffer-create "*Code Review Budget*"))
  (with-current-buffer "*Code Review Budget*"
    (unless (derived-mode-p 'code-review-budget-mode)
      (code-review-budget-mode))
    (code-review-budget--refresh)))

(define-derived-mode code-review-budget-mode tabulated-list-mode
  "Code Review Budget"
  "The review budget queue (phase 25)."
  (setq tabulated-list-format
        [("PR" 22 t)
         ("Risk" 7 t)
         ("Cost" 6 t)
         ("Tier" 8 t)
         ("Signals" 60 nil)]
        tabulated-list-sort-key '("Risk" . t))
  (tabulated-list-init-header)
  ;; g (tabulated-list-revert) re-collects from the db
  (add-hook 'tabulated-list-revert-hook
            #'code-review-budget--refresh nil t))

(defun code-review-budget--entry-at-point ()
  "The queue ENTRY at point, or user-error."
  (let* ((key (or (tabulated-list-get-id)
                  (user-error "No PR at point")))
         (entry (and code-review-budget--table
                     (gethash key code-review-budget--table))))
    (or entry (user-error "No budget entry for %s" key))))

(defun code-review-budget--open-at-point ()
  "Open the review of the PR at point."
  (interactive)
  (let ((entry (code-review-budget--entry-at-point)))
    (cond
     ((plist-get entry :localp)
      (user-error "Local reviews are opened from their own buffer"))
     ((plist-get entry :url)
      (code-review-start (plist-get entry :url)))
     (t (user-error "No URL stored for %s" (plist-get entry :key))))))

(defun code-review-budget--substitute (template entry &optional extra-pairs)
  "Fill TEMPLATE's format-spec chars for ENTRY.
EXTRA-PAIRS extends the spec with (CHAR . STRING) conses."
  (let* ((a (plist-get entry :assess))
         (pairs (append (list (cons ?p (plist-get entry :key))
                              (cons ?t (plist-get entry :title))
                              (cons ?c (format "%.1f" (plist-get a :cost)))
                              (cons ?r (string-join
                                        (plist-get a :reasons) "\n- ")))
                        extra-pairs)))
    (format-spec template (apply #'format-spec-make
                                 (apply #'append
                                        (mapcar (lambda (p)
                                                  (list (car p) (cdr p)))
                                                pairs))))))

(defun code-review-budget--post (entry body subject)
  "Post BODY as a comment on the ENTRY's PR (the forge write path)."
  (code-review-new-issue-comment
   (plist-get entry :pr) body
   (lambda (&rest _)
     (message "code-review-budget: %s (%s)" subject
              (plist-get entry :key)))))

(defun code-review-budget-approve-on-gates ()
  "Approve the SAFE PR at point on gate evidence, one key.
Leaves the explicit audit-trail comment on the PR — honest
accounting: this PR was merged on machine-checked gates, not on
human line review.  The decision is recorded locally and the
queue drops the row."
  (interactive)
  (let* ((entry (code-review-budget--entry-at-point))
         (allocs (code-review-budget--allocations
                  (code-review-budget--collect)))
         (tier (code-review-budget--tier-of entry allocs)))
    (unless (member tier '("SAFE" "AUDIT"))
      (user-error "%s is %s-tier: approve-on-gates is only for gate-covered PRs"
                  (plist-get entry :key) tier))
    (when (y-or-n-p
           (format "Post the approve-on-gates audit comment to %s? "
                   (plist-get entry :key)))
      (code-review-budget--state-record
       :approved (plist-get entry :key))
      (code-review-budget--post
       entry
       (code-review-budget--substitute
        code-review-review-approve-template entry)
       "approved on gates"))))

(defun code-review-budget-bounce ()
  "Bounce the PR at point: \"unreviewable, please split\".
Posts the bounce template comment, records the decision, and the
queue drops the row.  Size discipline matters MORE with AI
generation, not less."
  (interactive)
  (let* ((entry (code-review-budget--entry-at-point))
         (tier (plist-get (plist-get entry :assess) :tier)))
    (unless (eq tier 'bounce)
      (user-error "%s is not over the bounce threshold (%.0f min)"
                  (plist-get entry :key)
                  code-review-review-bounce-minutes))
    (when (y-or-n-p
           (format "Post the please-split bounce comment to %s? "
                   (plist-get entry :key)))
      (code-review-budget--state-record
       :bounced (plist-get entry :key))
      (code-review-budget--post
       entry
       (code-review-budget--substitute
        code-review-review-bounce-template entry
        (list (cons ?b (format "%.0f"
                               code-review-review-bounce-minutes))))
       "bounced"))))

(defun code-review-budget-quit ()
  "Quit the queue window."
  (interactive)
  (quit-window))

(let ((map code-review-budget-mode-map))
  (define-key map (kbd "RET") #'code-review-budget--open-at-point)
  (define-key map (kbd "a") #'code-review-budget-approve-on-gates)
  (define-key map (kbd "b") #'code-review-budget-bounce)
  (define-key map (kbd "s") #'code-review-budget-log-minutes)
  (define-key map (kbd "q") #'code-review-budget-quit))

(provide 'code-review-budget)

;;; code-review-budget.el ends here
