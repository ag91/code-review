;;; code-review-registry.el --- Incident registry and agent conventions -*- lexical-binding: t; -*-
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

;;  Phase 22: the incident registry.  A postmortem today produces a
;;  document nobody rereads until the NEXT incident in the same file
;;  — attention spent on an artifact that evaporates.  One incident
;;  must leave TWO durable artifacts behind: a PRIORITY SIGNAL
;;  (permanent review heat on the touched paths) and a REGRESSION
;;  TEST reference.  Pure git (L0): COMMIT TRAILERS are the source
;;  of truth, the markdown registry under incidents/ is GENERATED
;;  from them — never hand-curated, so it cannot drift.
;;
;;  The model: an incident is a plist
;;    (:id \"2026-047\" :title ... :date \"2026-04-11\"
;;     :invariant \"req-14\" :tests (\"test_export_is_atomic\")
;;     :paths (\"src/payments/export.py\") :pr \"762\" :sha \"a1b2...\")
;;
;;  Sources, in order (trailers first, generated files only as
;;  fallback):
;;    - `code-review-registry--log-incidents': ONE bounded
;;      `git log' pass reading the fix-commit trailers
;;      (Incident / Invariant-Ref / Regression-Test / Paths);
;;    - `code-review-registry--read-file-incidents': the
;;      incidents/*.md front matter, byte-capped (a monster file is
;;      data, not an incident entry) and entry-count-capped.
;;  Both are cached per (REPO . HEAD) — a new commit invalidates
;;  naturally — and never signal into a render.
;;
;;  Consumption (existing machinery, just fed): the phase 15
;;  delicacy badge counts incidents on touched files, the phase 5
;;  dead-code analysis NEVER flags incident-touched paths as
;;  possibly dead, the phase 16 dossier lists the incidents of a
;;  hunk's file with jump buttons, and phase 25 (later) will treat
;;  incident paths as never-sampleable.
;;
;;  Virality: `code-review-install-conventions' appends a
;;  CONVENTIONS block (idempotent, sentinel-marked) to the
;;  repository's AGENTS.md — every AI agent working in the repo
;;  reads it before touching anything, so agents author registry
;;  entries, criteria files and test tags THEMSELVES, correctly
;;  formatted, with zero new tooling on their side.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'code-review-utils)
(require 'code-review-diff)

;; Buffer-side glue talks to the db/interfaces layers without
;; requiring them (dependency order: registry stays below them).
(defvar code-review-repo-worktree)         ; code-review-repo.el
(defvar code-review-db--pullreq-id)        ; code-review-db.el
(declare-function code-review-db-get-pullreq "code-review-db")
(declare-function code-review-db-local-pr-p "code-review-db")
(declare-function code-review-db--pullreq-raw-diff "code-review-db")
(declare-function code-review-new-issue-comment "code-review-interfaces")

(defcustom code-review-incident-keywords
  '("incident" "outage" "regression" "hotfix" "sev1" "sev2" "sev3"
    "cve" "rollback")
  "Keywords marking a review as incident-related (phase 22 detection).
Matched case-insensitively as substrings against the PR title and
description; a match offers ONCE per PR to tag the review with a
registry entry."
  :type '(repeat string)
  :group 'code-review)

(defcustom code-review-registry-dir "incidents"
  "Directory (repo-relative) holding the incident registry entries.
GENERATED from commit trailers by `code-review-registry-generate';
hand-curating it only drifts."
  :type 'directory
  :group 'code-review)

(defcustom code-review-registry-max-entry-bytes 65536
  "Byte cap per incidents/ registry entry file.
Oversized files are skipped (data, not incident entries)."
  :type 'natnum
  :group 'code-review)

(defcustom code-review-registry-max-total-bytes 262144
  "Total byte budget for one incidents/ directory scan."
  :type 'natnum
  :group 'code-review)

(defcustom code-review-registry-max-entries 200
  "Maximum incidents read from one incidents/ directory scan."
  :type 'natnum
  :group 'code-review)

(defcustom code-review-registry-max-log-commits 5000
  "History cap for the trailer `git log' scan.
One bounded pass either way; incidents sit in recent history."
  :type 'natnum
  :group 'code-review)

(defconst code-review-registry--trailer-keys
  '("Incident" "Invariant-Ref" "Regression-Test" "Paths")
  "The fix-commit trailer keys of the incident convention.")

(defconst code-review-registry--conventions-sentinel
  "<!-- code-review-conventions v1 -->"
  "Sentinel marking an installed conventions block (idempotency).")

;;; Pure parsing

(defun code-review-registry--parse-trailers (text)
  "Parse the trailer block of commit message TEXT.
Return an alist (KEY . VALUE), or nil when the LAST paragraph is
not all `Key: value' lines — git's own rule: only the final
paragraph counts, the subject line never does.  Continuation
lines (leading whitespace) fold into the previous value."
  (let* ((paras (split-string text "[ \t]*\n[ \t]*\n"))
         (last-para (car (last paras)))
         (lines (and last-para (split-string last-para "\n" t)))
         (trailers nil)
         (ok (and lines lines t)))
    (dolist (line lines)
      (cond
       ((and trailers (string-match-p "\\`[ \t]" line))
        (let ((cell (car trailers)))
          (setcdr cell
                  (string-trim (concat (cdr cell) " " (string-trim line))))))
       ((string-match "\\`\\([-A-Za-z0-9]+\\):[ \t]*\\(.*\\)[ \t]*\\'" line)
        (push (cons (match-string 1 line) (match-string 2 line)) trailers))
       (t (setq ok nil))))
    (and ok trailers (nreverse trailers))))

(defun code-review-registry--split-values (value)
  "Split a trailer or front-matter VALUE into its items.
Comma- or newline-separated, surrounding [...] stripped, items
trimmed, empties dropped.  nil for a nil/blank value."
  (let* ((v (and value (string-trim value)))
         (v (if (and v (> (length v) 1)
                      (string-prefix-p "[" v) (string-suffix-p "]" v))
                (substring v 1 -1)
              v)))
    (and v (not (string-empty-p v))
         (delete "" (mapcar #'string-trim (split-string v "[,\n]+"))))))

(defun code-review-registry--first-value (value)
  "The first item of VALUE (see `--split-values'), or nil."
  (car (code-review-registry--split-values value)))

(defun code-review-registry--incident-p (x)
  "Non-nil when plist X is a well-formed incident (an Incident id)."
  (and (plist-get x :id)
       (not (string-empty-p (plist-get x :id)))
       t))

(defun code-review-registry--incident-from-trailers (trailers
                                                     &optional sha title date)
  "Incident plist from parsed commit TRAILERS.
SHA/TITLE/DATE enrich it from the same commit.  nil when the
trailers carry no Incident id."
  (let ((id (code-review-registry--first-value
             (cdr (assoc "Incident" trailers)))))
    (when id
      (list :id id
            :title (or title "")
            :date (or date "")
            :invariant (code-review-registry--first-value
                        (cdr (assoc "Invariant-Ref" trailers)))
            :tests (code-review-registry--split-values
                    (cdr (assoc "Regression-Test" trailers)))
            :paths (code-review-registry--split-values
                    (cdr (assoc "Paths" trailers)))
            :sha sha))))

(defun code-review-registry--log-records (log-text)
  "Commit records ((SHA TITLE DATE TRAILERS)...) from LOG-TEXT.
Records are \\x1e-separated, fields \\x1f-separated — the format
of the `git log' call in `code-review-registry--log-incidents'.
The four trailer keys are fixed (`--trailer-keys').  Pure."
  (let (records)
    (dolist (rec (split-string log-text "\x1e"))
      (let ((fields (split-string rec "\x1f")))
        (when (>= (length fields) 7)
          (push (list (nth 0 fields) (nth 1 fields) (nth 2 fields)
                      (list (cons (nth 0 code-review-registry--trailer-keys)
                                  (nth 3 fields))
                            (cons (nth 1 code-review-registry--trailer-keys)
                                  (nth 4 fields))
                            (cons (nth 2 code-review-registry--trailer-keys)
                                  (nth 5 fields))
                            (cons (nth 3 code-review-registry--trailer-keys)
                                  (nth 6 fields))))
                records))))
    (nreverse records)))

(defun code-review-registry--incidents-from-log (log-text)
  "Incident plists from a trailer `git log' scan LOG-TEXT.  Pure.
Only commits carrying an Incident trailer become incidents."
  (delq nil
        (mapcar (lambda (rec)
                  (code-review-registry--incident-from-trailers
                   (nth 3 rec) (nth 0 rec) (nth 1 rec) (nth 2 rec)))
                (code-review-registry--log-records log-text))))

(defun code-review-registry--fm-key (key)
  "Front-matter KEY string to plist keyword."
  (pcase key
    ("id" :id) ("title" :title) ("date" :date)
    ("invariant" :invariant) ("req" :invariant)
    ("tests" :tests) ("paths" :paths) ("pr" :pr)
    (_ (intern (concat ":" key)))))

(defun code-review-registry--parse-front-matter (text)
  "Front-matter plist of registry entry TEXT.
YAML-shaped: `---' delimited `key: value' lines, values may be
[comma-separated] lists.  nil when TEXT has no closed front
matter.  Pure."
  (when (and text (string-prefix-p "---\n" text))
    (let ((plist nil)
          (closed nil))
      (dolist (line (cdr (split-string text "\n")))
        (cond
         (closed nil)
         ((string= line "---") (setq closed t))
         ((string-match "\\`\\([a-z-]+\\):[ \t]*\\(.*\\)[ \t]*\\'" line)
          (setq plist (plist-put plist
                                (code-review-registry--fm-key
                                 (match-string 1 line))
                                (match-string 2 line))))))
      (and closed plist plist))))

(defun code-review-registry--incident-from-front-matter (fm)
  "Incident plist from a parsed front-matter FM plist."
  (when (code-review-registry--incident-p fm)
    (list :id (plist-get fm :id)
          :title (or (plist-get fm :title) "")
          :date (or (plist-get fm :date) "")
          :invariant (code-review-registry--first-value
                      (plist-get fm :invariant))
          :tests (code-review-registry--split-values (plist-get fm :tests))
          :paths (code-review-registry--split-values (plist-get fm :paths))
          :pr (plist-get fm :pr))))

;;; The git scan (bounded, cached)

(defun code-review-registry--git (repo &rest args)
  "Run git ARGS in REPO and return its stdout, or nil on failure.
One bounded subprocess; stderr is discarded on purpose (a failed
call reads as no data, never as a signal)."
  (with-temp-buffer
    (let ((code (apply #'call-process "git" nil (list (current-buffer) nil)
                      nil
                      (append (list "-C" (expand-file-name repo)) args))))
      (and (zerop code)
           (string-trim-right
            (buffer-substring-no-properties (point-min) (point-max)))))))

(defun code-review-registry--head (repo)
  "HEAD sha of REPO, or nil when it is not a git repository."
  (let ((head (code-review-registry--git repo "rev-parse" "HEAD")))
    (and head (not (string-empty-p head)) head)))

(defun code-review-registry--log-incidents (repo)
  "Incident plists from the fix-commit TRAILERS of REPO.
One bounded `git log' pass (`--no-merges': fix commits are not
merges), capped by `code-review-registry-max-log-commits'.  The
generated incidents/ files are never consulted here: trailers
are the source of truth."
  (let ((log (code-review-registry--git
              repo "log"
              (format "-n%d" code-review-registry-max-log-commits)
              "--no-merges" "--date=short"
              "--format=%H%x1f%s%x1f%ad%x1f%(trailers:key=Incident,valueonly)%x1f%(trailers:key=Invariant-Ref,valueonly)%x1f%(trailers:key=Regression-Test,valueonly)%x1f%(trailers:key=Paths,valueonly)%x1e")))
    (and log (code-review-registry--incidents-from-log log))))

(defun code-review-registry--read-file-incidents (repo)
  "Incident plists from the incidents/ markdown files of REPO.
The FALLBACK source (trailers first): byte-capped per file and in
total, entry-count capped, front matter must close.  Never
signals."
  (let* ((dir (expand-file-name code-review-registry-dir repo))
         (files (and (file-directory-p dir)
                    (directory-files dir t "\\.md\\'"))))
    (when files
      (let ((budget code-review-registry-max-total-bytes)
            (incidents nil))
        (cl-block scan
          (dolist (file files)
            (when (>= (length incidents)
                      code-review-registry-max-entries)
              (cl-return))
            (let ((size (or (file-attribute-size (file-attributes file))
                            0)))
              (cond
               ((or (> size code-review-registry-max-entry-bytes)
                    (> size budget))
                (code-review-utils--log
                 "code-review-registry"
                 (format "registry entry skipped (byte cap): %s" file)))
               (t
                (setq budget (- budget size))
                (with-temp-buffer
                  (insert-file-contents file)
                  (let ((fm (code-review-registry--parse-front-matter
                             (buffer-substring-no-properties
                              (point-min) (point-max)))))
                    (let ((inc (and fm
                                    (code-review-registry--incident-from-front-matter fm))))
                      (when inc (push inc incidents))))))))))
        (nreverse incidents)))))

(defvar code-review-registry--cache (make-hash-table :test #'equal)
  "Incident plists keyed by (REPO . HEAD-SHA).
A new commit in the repository (new HEAD) invalidates naturally;
the scan is one bounded `git log' pass either way.")

(defun code-review-registry--incidents (repo)
  "The incidents of REPO, trailers first, files only as fallback.
Cached per (REPO . HEAD).  nil when the repository has neither
source.  Never signals; every cost is bounded."
  (ignore-errors
    (when (and repo (file-directory-p repo))
      (let* ((head (code-review-registry--head repo))
             (key (cons (expand-file-name repo) head))
             (cached (gethash key code-review-registry--cache 'missing)))
        (if (eq cached 'missing)
            (let ((res (or (code-review-registry--log-incidents repo)
                           (code-review-registry--read-file-incidents
                            repo))))
              (puthash key res code-review-registry--cache)
              res)
          cached)))))

(defun code-review-registry--incident-paths (incidents)
  "Hash PATH -> incident ids touching it, from INCIDENTS.
The consumers' lookup table (phase 15 badge, phase 5 dead filter,
phase 16 dossier)."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (inc (or incidents ()))
      (dolist (path (plist-get inc :paths))
        (puthash path (cons (plist-get inc :id) (gethash path table))
                 table)))
    table))

(defun code-review-registry--file-tag (path)
  "The file-heading tag for PATH when incidents touch it.
\"[1 incident]\" / \"[2 incidents]\" (phase 22: incident count
on file headings — permanent review heat), nil when the
review's worktree has no incident touching PATH.  Reads the
(REPO . HEAD)-cached scan; a failing lookup is simply no tag,
never a signal."
  (ignore-errors
    (when code-review-repo-worktree
      (let* ((incidents (code-review-registry--incidents
                         code-review-repo-worktree))
             (ids (and incidents
                       (gethash path
                                (code-review-registry--incident-paths
                                 incidents)))))
        (when ids
          (format "[%d incident%s]" (length ids)
                  (if (cdr ids) "s" "")))))))

;;; Entry generation

(defun code-review-registry--slug (title)
  "File-name slug of TITLE: lowercase, non-alphanumerics to dashes."
  (string-trim (downcase (replace-regexp-in-string
                          "[^[:alnum:]]+" "-" (or title "")))
               "-" "-"))

(defun code-review-registry--entry-file-name (incident)
  "The registry file name for INCIDENT: <date>-<slug>.md."
  (format "%s-%s.md"
          (or (plist-get incident :date) (format-time-string "%Y-%m-%d"))
          (code-review-registry--slug (plist-get incident :title))))

(defun code-review-registry--entry-text (incident)
  "The registry markdown entry text for INCIDENT.
YAML front matter (the machine keys — the generator owns them)
plus a short human body template.  Pure."
  (concat
   "---\n"
   (format "id: %s\n" (plist-get incident :id))
   (format "date: %s\n" (or (plist-get incident :date)
                            (format-time-string "%Y-%m-%d")))
   (format "title: %s\n" (or (plist-get incident :title) ""))
   (format "invariant: %s\n" (or (plist-get incident :invariant) ""))
   (format "tests: [%s]\n" (string-join (or (plist-get incident :tests) ())
                                        ", "))
   (format "paths: [%s]\n" (string-join (or (plist-get incident :paths) ())
                                       ", "))
   (format "pr: %s\n" (or (plist-get incident :pr) ""))
   "---\n"
   "\n"
   "## What happened\n"
   "\n"
   "<one paragraph, user impact first>\n"
   "\n"
   "## Invariant\n"
   "\n"
   "<the requirement this incident violated; the req id when it exists>\n"
   "\n"
   "## Guard\n"
   "\n"
   (format "- Regression test(s): %s\n"
           (or (string-join (plist-get incident :tests) ", ")
               "<the test that would have caught it>"))
   (format "- Criteria: %s\n"
           (or (plist-get incident :invariant) "<req id>"))))

(defun code-review-registry--next-id (incidents year)
  "The next incident ID for YEAR: YEAR-NNN.
The sequence is counted from the existing INCIDENTS of the same
year.  Pure."
  (let ((max 0))
    (dolist (inc (or incidents ()))
      (let ((id (plist-get inc :id)))
        (when (and id (string-prefix-p (concat year "-") id))
          (let ((n (and (string-match (concat year "-\\([0-9]+\\)\\'") id)
                        (string-to-number (match-string 1 id)))))
            (when (and n (> n max)) (setq max n))))))
    (format "%s-%03d" year (1+ max))))

(defun code-review-registry--repo-dir ()
  "The repository directory for interactive registry commands.
The review buffer's worktree when in one, else the repository
root of `default-directory' (magit's resolver, feature-detected),
else nil."
  (or (bound-and-true-p code-review-repo-worktree)
      (let ((root (and (fboundp 'magit-toplevel) (magit-toplevel))))
        (and root (file-directory-p root) root))))

(defun code-review-registry--file-front-matter (file)
  "The parsed front matter of FILE (byte-capped), or nil."
  (let ((size (or (file-attribute-size (file-attributes file)) 0)))
    (and (<= size code-review-registry-max-entry-bytes)
         (with-temp-buffer
           (insert-file-contents file)
           (code-review-registry--parse-front-matter
            (buffer-substring-no-properties (point-min) (point-max)))))))

;;;###autoload
(defun code-review-registry-generate (&optional repo)
  "Generate the incidents/ registry entries of REPO from trailers.
REPO defaults to the review buffer's worktree, else the
repository of `default-directory'.  One entry per Incident-
trailer commit; an existing file with the SAME id is left alone
(the human body is never clobbered), a stale one is rewritten.
Return the number of entries written."
  (interactive)
  (let* ((root (or repo (code-review-registry--repo-dir)))
         (incidents (and root (code-review-registry--log-incidents root)))
         (dir (and root (expand-file-name code-review-registry-dir root)))
         (written 0))
    (cond
     ((not root) (user-error "No repository here"))
     ((not incidents)
      (message "code-review-registry: no Incident trailers; nothing generated"))
     (t
      (make-directory dir t)
      (dolist (inc incidents)
        (let* ((file (expand-file-name
                      (code-review-registry--entry-file-name inc) dir))
               (existing (and (file-exists-p file)
                              (code-review-registry--file-front-matter
                               file))))
          (unless (and existing
                       (equal (plist-get existing :id)
                              (plist-get inc :id)))
            (with-temp-file file
              (insert (code-review-registry--entry-text inc)))
            (setq written (1+ written)))))
      (message "code-review-registry: %d entr%s written under %s"
               written (if (= written 1) "y" "ies") dir)))
    written))

;;; Detection and tagging

(defun code-review-registry--keyword-match-p (title &optional description)
  "Non-nil when TITLE or DESCRIPTION matches the incident keywords.
Case-insensitive substring match against
`code-review-incident-keywords'.  Pure."
  (let ((text (downcase (concat (or title "") " " (or description "")))))
    (and (cl-some (lambda (kw) (string-match-p (downcase kw) text))
                  code-review-incident-keywords)
         t)))

(defvar code-review-registry--prompted (make-hash-table :test #'equal)
  "PR ids already offered incident tagging (the prompt is ONCE per PR).")

(defun code-review-registry--detect ()
  "Offer incident tagging for incident-shaped reviews.
Runs from `code-review-post-hook': when the review's title or
description matches `code-review-incident-keywords', prompt ONCE
per PR (never in batch emacs) to tag the review with a registry
entry.  Never signals: a failing detection is logged and the
render stays untouched."
  (ignore-errors
    (when (and (eq major-mode 'code-review-mode)
               (not noninteractive)
               (fboundp 'code-review-db-get-pullreq))
      (let ((pr (code-review-db-get-pullreq)))
        (when pr
          (let ((id (oref pr id)))
            (when (and (not (gethash id code-review-registry--prompted))
                       (code-review-registry--keyword-match-p
                        (oref pr title)
                        (oref pr description)))
              (puthash id t code-review-registry--prompted)
              (when (y-or-n-p
                     "Looks incident-related — tag this review with a registry entry? ")
                (code-review-incident-tag)))))))))

(defun code-review-registry--changed-paths ()
  "The changed file paths of the current review's diff.
The registry's prefill edge: the PR's OWN changed paths, read
straight from the raw diff — a thing generic review tooling
cannot do.  nil when there is no diff."
  (let ((diff (and (fboundp 'code-review-db--pullreq-raw-diff)
                   (code-review-db--pullreq-raw-diff))))
    (and diff (mapcar #'car (code-review--diff--split-by-files diff)))))

(defun code-review-registry--comment-text (incident)
  "The PR-comment text for INCIDENT (strategy 3 of the chain)."
  (format "## Incident registry entry\n\n\
Please land this entry with the fix:\n\n```markdown\n%s```\n\n\
and carry the trailers on the fix commit:\n\n```\nIncident: %s\nInvariant-Ref: %s\nRegression-Test: %s\nPaths: %s\n```\n"
          (code-review-registry--entry-text incident)
          (plist-get incident :id)
          (or (plist-get incident :invariant) "<req id>")
          (or (string-join (plist-get incident :tests) ", ") "<test names>")
          (or (string-join (plist-get incident :paths) ", ") "<paths>")))

(defun code-review-registry--write-entry (repo incident)
  "Write INCIDENT's registry entry file into REPO.
Creates the incidents/ directory when missing.  Return the file
path."
  (let* ((dir (expand-file-name code-review-registry-dir repo))
         (file (expand-file-name
                (code-review-registry--entry-file-name incident) dir)))
    (make-directory dir t)
    (with-temp-file file
      (insert (code-review-registry--entry-text incident)))
    file))

(defun code-review-registry--submit-entry (repo incident)
  "Submit INCIDENT of REPO through the strategy chain.
Strategies, tried in order; phase 22 ships (3) and (4):
  (1) a PR-attached agent — arrives with phase 24;
  (2) pushing a commit to the PR branch — arrives with phase 24;
  (3) forge reviews: the entry is posted as ONE PR comment (the
      file content plus the trailer conventions for the fix
      commit);
  (4) the entry file is written into the repository working tree
      — the phase 12 local-review path, and the committable
      deliverable for forge reviews.
Return non-nil when anything was submitted."
  (let ((submitted nil))
    ;; (4) the durable artifact, always
    (when repo
      (let ((file (code-review-registry--write-entry repo incident)))
        (message "code-review-registry: entry written: %s (commit it with the fix)"
                 file)
        (setq submitted t)))
    ;; (3) the PR comment, for forge reviews (a live PR id only:
    ;; never touch the db from a stray buffer)
    (when (and code-review-db--pullreq-id
               (fboundp 'code-review-db-local-pr-p)
               (fboundp 'code-review-new-issue-comment)
               (fboundp 'code-review-db-get-pullreq)
               (not (code-review-db-local-pr-p)))
      (let ((pr (code-review-db-get-pullreq)))
        (when pr
          (code-review-new-issue-comment
           pr
           (code-review-registry--comment-text incident)
           (lambda (&rest _)
             (message "code-review-registry: entry posted as a PR comment")))
          (setq submitted t))))
    submitted))

;;;###autoload
(defun code-review-incident-tag ()
  "Tag the review at point with an incident registry entry.
Creates this review's entry (id year-numbered from the existing
registry, title from the PR, PATHS PREFILLED from the review's
own changed files) and submits it through the
`code-review-registry--submit-entry' strategy chain."
  (interactive)
  (let ((repo (code-review-registry--repo-dir)))
    (unless repo (user-error "No repository here (worktree or repo root)"))
    (let* ((pr (and (fboundp 'code-review-db-get-pullreq)
                    (code-review-db-get-pullreq)))
           (year (format-time-string "%Y"))
           (incidents (code-review-registry--incidents repo))
           (incident (list :id (code-review-registry--next-id incidents year)
                           :title (or (and pr (oref pr title)) "")
                           :date (format-time-string "%Y-%m-%d")
                           :invariant ""
                           :tests nil
                           :paths (code-review-registry--changed-paths)
                           :pr (or (and pr (oref pr number)) ""))))
      (code-review-registry--submit-entry repo incident))))

;;; The conventions installer (virality)

(defun code-review-registry-conventions-text ()
  "The conventions block appended to AGENTS.md/CONTRIBUTING.md.
Factual for what exists today (the incident registry, trailers,
the review-risk behavior); forward-referencing for the criteria
files (phases 23/24 install them next).  Pure."
  (concat
   code-review-registry--conventions-sentinel "\n"
   "\n"
   "## Code review conventions\n"
   "\n"
   "### Incident registry\n"
   "\n"
   "- Every production incident leaves a REGISTRY ENTRY under\n"
   "  `incidents/` and a regression test.  Entries are GENERATED\n"
   "  from commit trailers (`M-x code-review-registry-generate`) —\n"
   "  never hand-curated; edit only the human body below the\n"
   "  front matter.\n"
   "- Fix commits carry the trailers:\n"
   "  `Incident: <year-numbered id>`, `Invariant-Ref: <req id>`,\n"
   "  `Regression-Test: <test names>`, `Paths: <files the incident\n"
   "  touched>`.\n"
   "- Changes touching incident paths get ELEVATED review\n"
   "  attention (delicacy badge, dossier incidents, review\n"
   "  budget).\n"
   "\n"
   "### Criteria (requirements)\n"
   "\n"
   "- Criteria live under `specs/` as markdown, one requirement\n"
   "  per file, EARS/Gherkin shape (`WHEN ... THE SYSTEM SHALL\n"
   "  ...`), with a stable date-numbered req id in the file name.\n"
   "- Every requirement binds to its guard tests: the req id\n"
   "  appears in the ERT test name (`file/req-<id>-description`)\n"
   "  or the test docstring.\n"
   "\n"
   "### If you are an AI agent working in this repository\n"
   "\n"
   "- Author registry entries, criteria files and test tags\n"
   "  yourself, in the formats above, as part of the change.\n"
   "- PRs touching incident paths get elevated review attention.\n"
   "<!-- end code-review-conventions v1 -->\n"))

(defun code-review-registry--file-has-sentinel-p (file)
  "Non-nil when FILE exists and carries the conventions sentinel.
Byte-capped (a monster file is not read whole)."
  (let ((size (or (file-attribute-size (file-attributes file)) 0)))
    (and (< size code-review-registry-max-total-bytes)
         (file-exists-p file)
         (with-temp-buffer
           (insert-file-contents file)
           (string-match-p
            (regexp-quote code-review-registry--conventions-sentinel)
            (buffer-substring-no-properties
             (point-min) (point-max)))))))

(defun code-review-registry--install-into (file)
  "Append the conventions block to FILE (idempotent).
Return `installed', `present' or `skipped' (byte cap)."
  (cond
   ((not (code-review-registry--file-has-sentinel-p file))
    (with-temp-file file
      (when (file-exists-p file)
        (insert-file-contents file))
      (goto-char (point-max))
      (unless (or (zerop (buffer-size)) (eq (char-before) ?\n))
        (insert "\n"))
      (insert "\n" (code-review-registry-conventions-text)))
    'installed)
   (t 'present)))

;;;###autoload
(defun code-review-install-conventions (&optional arg)
  "Install the code-review conventions block into AGENTS.md.
Idempotent (sentinel-marked — run it twice, nothing happens).
With ARG non-nil, also install into CONTRIBUTING.md.  Uses the
review buffer's worktree when in one, else the repository of
`default-directory'."
  (interactive "P")
  (let ((repo (code-review-registry--repo-dir)))
    (unless repo (user-error "No repository here (worktree or repo root)"))
    (dolist (name (if arg '("AGENTS.md" "CONTRIBUTING.md") '("AGENTS.md")))
      (let ((file (expand-file-name name repo)))
        (message "code-review: conventions %s in %s"
                 (code-review-registry--install-into file) file)))))

(provide 'code-review-registry)
;;; code-review-registry.el ends here
