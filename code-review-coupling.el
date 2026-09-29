;;; code-review-coupling.el --- Change-coupling completeness cross-check -*- lexical-binding: t; -*-

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

;; Phase 18 (Improvements.org): change-coupling completeness
;; cross-check.
;;
;; The repository's own history knows which files change TOGETHER;
;; an agent's patch is often narrower than the historical change set
;; — an incomplete refactor.  Change coupling correlates with defects
;; (D'Ambros & Lanza 2009; Wiese et al. 2015), and CodeScene treats
;; source+test co-change as POSITIVE coupling (tests kept in sync),
;; which gives the coupled-test check below.
;;
;; Layers:
;;
;; - MATRIX (harvested with the phase 14 history, shared cache): the
;;   co-occurrence counts of the SAME `git log --name-only' the heat
;;   metrics parse — ONE parse per repository, computed in the harvest
;;   child, cached on disk per repository with the phase 14 TTL.
;;   Changesets wider than `--max-changeset-size' are skipped whole
;;   (code-maat's `coupling' noise rule: wide changesets are
;;   reformatting/branch-merge noise).
;;
;; - CHECK (pure, cached per PR+diff like phase 5/17): for each changed
;;   file, its co-change peers at or above the degree threshold whose
;;   files the PR does NOT touch.  A coupled TEST file left behind is
;;   the star case (positive coupling broken); a coupled config or
;;   sibling is the incomplete-refactor case.  Rendered as hoisted
;;   "completeness:" entries in the Analysis section with a jump
;;   button into the untouched peer.
;;
;; Local diff reviews work (phase 12 constraint): the repository
;; itself is the worktree and its own history is the evidence.

;;; Code:

(require 'cl-lib)
(require 'code-review-utils)
(require 'code-review-testimpact)

(defvar code-review-repo-worktree)        ; code-review-repo.el

(declare-function code-review-history-data "code-review-history")
(declare-function code-review-history--files "code-review-history")
(declare-function code-review-db-get-pullreq "code-review-db")
(declare-function code-review-db--pullreq-raw-diff "code-review-db")

;;; Configuration

(defgroup code-review-coupling nil
  "Change-coupling completeness cross-check (phase 18)."
  :group 'code-review)

(defcustom code-review-coupling-enabled t
  "When non-nil, run the change-coupling completeness check
when rendering."
  :group 'code-review-coupling
  :type 'boolean)

(defcustom code-review-coupling-threshold 0.3
  "Degree at or above which a co-change peer is reported.
Degree is co-changes / the changed file's revisions (the share of
its commits that also touched the peer) — code-maat's coupling
default is 30%."
  :group 'code-review-coupling
  :type 'number)

(defcustom code-review-coupling-max-changeset-size 15
  "Changesets with more files than this are skipped whole.
Wide changesets are reformatting/branch-merge noise, not coupling
(code-maat's `--max-changeset-size').  0 disables the cap."
  :group 'code-review-coupling
  :type 'natnum)

(defcustom code-review-coupling-min-cochanges 3
  "Minimum co-changes before a pair is evidence of coupling.
A 2-revision file coupled to everything at 100% is noise, not a
historical convention."
  :group 'code-review-coupling
  :type 'natnum)

(defcustom code-review-coupling-max-findings 20
  "Completeness findings stored and rendered per (PR, diff)."
  :group 'code-review-coupling
  :type 'natnum)

;;; Pure: the co-change matrix

(defun code-review-coupling--parse-changesets (log &optional bot-regexp)
  "Parse LOG (a `git log --name-only' text) into CHANGESETS.
Each changeset is the file list of one human commit, in log order
(newest first); commits whose author matches BOT-REGEXP are
skipped (bot churn is not coupling), merge commits list no files
and contribute nothing.  The pretty format is code-compass's
`--%h--%ad--%aN' (see `code-compass--parse-git-log-metrics')."
  (let ((sets nil)
        (current nil)
        (skip nil)
        (open nil))
    (dolist (line (split-string log "\n"))
      (cond
       ((string-match "\\`--[0-9a-f]+--\\([0-9][0-9-]*\\)--\\(.*\\)\\'" line)
        ;; bind BEFORE anything else can match (match-data gotcha)
        (let ((author (match-string-no-properties 2 line)))
          (when (and open current)
            (push (nreverse current) sets))
          (setq open t
                current nil
                skip (and bot-regexp
                          (string-match-p bot-regexp author)))))
       ((and open (not skip) (not (string-empty-p line)))
        (push line current))))
    (when (and open current)
      (push (nreverse current) sets))
    (nreverse sets)))

(defun code-review-coupling--pair-key (a b)
  "Hash key for the file pair A/B (order-independent)."
  (if (string< a b)
      (concat a "\x1f" b)
    (concat b "\x1f" a)))

(defun code-review-coupling--matrix (changesets &optional max-size)
  "Co-change matrix over CHANGESETS ((FILES...)...).
Return (:pairs HASH pair-key -> co-change count :revisions HASH
path -> commits touching it).  Changesets with more files than
MAX-SIZE (nil or 0 = uncapped) are skipped whole; single-file
changesets count toward :revisions but contribute no pairs."
  (let ((pairs (make-hash-table :test #'equal))
        (revisions (make-hash-table :test #'equal)))
    (dolist (files changesets)
      (unless (and max-size (> max-size 0) (> (length files) max-size))
        (dolist (f files)
          (puthash f (1+ (gethash f revisions 0)) revisions))
        ;; copy first: `delete-dups' is DESTRUCTIVE (AGENTS.md)
        (let ((files (delete-dups (copy-sequence files))))
          (while (cdr files)
            (let ((a (pop files)))
              (dolist (b files)
                (let ((k (code-review-coupling--pair-key a b)))
                  (puthash k (1+ (gethash k pairs 0)) pairs))))))))
    (list :pairs pairs :revisions revisions)))

(defun code-review-coupling--findings (data pr-files)
  "Completeness findings for the PR's files from the matrix DATA.
DATA is the (:pairs :revisions) plist of `--matrix', PR-FILES the
changed file paths.  Each finding is (:path :peer :co :degree
:test-p): PATH historically changes together with PEER (CO
co-changes, DEGREE = CO / PATH's revisions, 0..1) and this PR
touches PATH but NOT PEER — the incomplete-refactor signal.
Findings strongest first, capped at
`code-review-coupling-max-findings'.  TEST-P marks the star case:
the untouched peer is a TEST file (CodeScene's positive coupling
broken — tests historically kept in sync with PATH)."
  (let* ((pairs (plist-get data :pairs))
         (revs (plist-get data :revisions))
         (want (make-hash-table :test #'equal))
         (found nil))
    (dolist (f pr-files)
      (puthash f t want))
    ;; ONE pass over the pairs, both sides of each: bounded by the
    ;; matrix size, not PR files x matrix
    (maphash
     (lambda (key co)
       (let* ((sep (string-search "\x1f" key))
              (a (substring key 0 sep))
              (b (substring key (1+ sep))))
         (dolist (side (list (cons a b) (cons b a)))
           (let ((path (car side))
                 (peer (cdr side)))
             (when (and (gethash path want)
                        (not (gethash peer want))
                        (>= co code-review-coupling-min-cochanges))
               (let* ((rev (gethash path revs 0))
                      (degree (if (zerop rev) 0 (/ co (float rev)))))
                 (when (>= degree code-review-coupling-threshold)
                   (push (list :path path :peer peer :co co
                               :degree degree
                               :test-p (code-review-testimpact--test-file-p
                                       peer))
                         found))))))))
     pairs)
    (setq found (sort found (lambda (x y)
                              (> (plist-get x :degree)
                                 (plist-get y :degree)))))
    (cl-subseq found 0 (min (length found)
                             code-review-coupling-max-findings))))

;;; Compute and cache

(defvar code-review-coupling--cache (make-hash-table :test #'equal)
  "Completeness findings keyed by (pullreq-id, diff md5).")

(defun code-review-coupling--compute (worktree diff)
  "Completeness findings for DIFF against WORKTREE's repository.
The matrix comes from the phase 14 harvest cache (see
`code-review-history-data'; nil while the async harvest runs —
the sentinel re-renders when it lands).  Pure: no git, the matrix
is a hash lookup."
  (let* ((data (code-review-history-data worktree (current-buffer)))
         (matrix (and data (plist-get data :coupling)))
         (findings (and matrix
                        (code-review-coupling--findings
                         matrix (code-review-history--files diff)))))
    (when findings
      (list :findings findings))))

(defun code-review-coupling-run ()
  "Compute (or reuse) the completeness check of the current review.
Return (:findings ...) or nil — when disabled, there is no
worktree/diff/PR, the history harvest is still running, or there
are no findings.  Cached per (PR, diff) like the phase 5 analysis.
A failing compute is LOGGED and reported as no findings: the
completeness check is a heuristic overlay and must never take the
review buffer down with it (the phase 5 survivability rule)."
  (when code-review-coupling-enabled
    (let* ((worktree code-review-repo-worktree)
           (diff (and worktree (code-review-db--pullreq-raw-diff)))
           (pr (and diff (code-review-db-get-pullreq))))
      (when (and worktree diff pr)
        (let ((key (concat (oref pr id) "|" (md5 diff))))
          (or (gethash key code-review-coupling--cache)
              (let ((res (condition-case err
                             (code-review-coupling--compute worktree diff)
                           (error
                            (code-review-utils--log
                             "code-review-coupling"
                             (format "coupling check failed, skipping (%s): %S"
                                     key err))
                            nil))))
                (puthash key res code-review-coupling--cache)
                res)))))))

(defun code-review-coupling-reset ()
  "Forget all cached completeness findings."
  (interactive)
  (clrhash code-review-coupling--cache))

(provide 'code-review-coupling)

;;; code-review-coupling.el ends here
