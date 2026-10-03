;;; code-review-criteria.el --- Requirements criteria engine (phase 23) -*- lexical-binding: t; -*-
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

;;  Phase 23: the solo criteria loop.  The local review (phase 12)
;;  becomes the enforcement point that a change's REQUIREMENTS
;;  exist and are visible NEXT TO THE DIFF, so the solo reviewer
;;  judges CONFORMANCE TO INTENT, not just the shape of the code.
;;
;;  The model (L0, pure git — no forge, no infrastructure):
;;    - A REQUIREMENT is one criteria FILE: one EARS/Gherkin
;;      sentence ("WHEN <precondition> THE SYSTEM SHALL
;;      <postcondition>"), one requirement per file so git
;;      diffs work like code diffs.  The stable, date-numbered
;;      req ID lives in the FILE NAME (`2026-10-01-001-<slug>.md`)
;;      — the phase 24 traceability pass greps it out of ERT
;;      test names (`file/req-<id>-description`).
;;    - The criteria files are RECOGNIZED by path
;;      (`code-review-criteria-file-regexp`: under specs?/ or
;;      requirements?/, markdown or .feature) — written by hand,
;;      by an agent (the phase 22 conventions block), or drafted
;;      from the template (`code-review-criteria-insert-template`)
;;      with an optional AI hook the mode CALLS but never
;;      implements (`code-review-criteria-generate-function`).
;;    - A requirement COVERS a changed file when its front
;;      matter declares the path (exact, or a parent directory),
;;      or — undeclared — its text mentions the path or its
;;      base name.
;;
;;  Enforcement lives in the section render
;;  (`code-review-section-criteria.el`, the checklist next to the
;;  diff) and in the local review entry
;;  (`code-review-criteria--enforce-local`, the REQUIRE-mode
;;  refusal).  Every scan is bounded: one `git ls-files' pass,
;;  per-file byte caps, a file-count cap, and nothing ever
;;  signals into a render.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'code-review-utils)
(require 'code-review-diff)
(require 'code-review-registry)

;; The render-side glue stays below the db layer (dependency
;; order: criteria never requires it).
(declare-function code-review-db--pullreq-raw-diff "code-review-db")

(defcustom code-review-criteria-file-regexp
  "\\`\\(?:specs?\\|requirements?\\)/.+\\.\\(?:md\\|feature\\)\\'"
  "Regexp matching repository files recognized as CRITERIA.
Default: markdown or Gherkin `.feature' files under `specs/' /
`spec/' / `requirements/' / `requirement/'.  Matched against
repo-relative paths from one `git ls-files' pass."
  :type 'regexp
  :group 'code-review)

(defcustom code-review-criteria-dir "specs"
  "Directory (repo-relative) where criteria files are created
by `code-review-criteria-insert-template'."
  :type 'directory
  :group 'code-review)

(defcustom code-review-criteria-required 'warn
  "How strictly LOCAL reviews require criteria coverage (phase 23).
nil: off entirely.  warn: a warning banner in the review when no
criteria cover the files in the change.  require: the local
review REFUSES TO OPEN until at least one criteria file covers a
touched path.  Forge reviews are never refused — the solo loop
is the local-review enforcement point (phase 24 builds the
forge surface)."
  :type '(choice (const :tag "Off" nil)
                 (const :tag "Warn" warn)
                 (const :tag "Require" require))
  :group 'code-review)

(defcustom code-review-criteria-generate-function nil
  "AI-draft hook the mode CALLS but NEVER implements (phase 23).
Wire it to gptel/ellama/llm.el or a shell call to your agent.
Called with (FILE PATHS): FILE is the fresh criteria template,
PATHS the changed files of the change under review; the function
drafts the requirement text INTO FILE.  nil (the default) keeps
the mode fully usable with no agent wired: the template and the
ID discipline still work, you draft the text."
  :type '(choice function (const nil))
  :group 'code-review)

(defcustom code-review-criteria-max-entry-bytes 65536
  "Byte cap per criteria file read.
Oversized files are skipped (data, not requirements)."
  :type 'natnum
  :group 'code-review)

(defcustom code-review-criteria-max-entries 300
  "Maximum criteria files read from one repository scan."
  :type 'natnum
  :group 'code-review)

;;; Pure parsing

(defun code-review-criteria--id-from-file (file)
  "The req ID of criteria FILE, read from its NAME.
The file stem is <id>-<slug>; the ID is the date-numbered head
(`2026-10-01-001').  nil when the name carries no such ID."
  (let* ((stem (file-name-sans-extension (file-name-nondirectory file)))
         (parts (split-string stem "-")))
    (when (>= (length parts) 4)
      (let ((id (string-join (cl-subseq parts 0 4) "-")))
        (and (string-match-p
              "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}-[0-9]\\{3\\}\\'"
              id)
             id)))))

(defun code-review-criteria--split-front-matter (text)
  "Split criteria TEXT into (FRONT-MATTER . BODY).
FRONT-MATTER is the parsed plist (nil when TEXT opens with no
`---' block); BODY is everything after the closing `---' line
(all of TEXT without front matter).  Pure."
  (let ((close (and text (string-search "\n---\n" text))))
    (if (and (string-prefix-p "---\n" (or text ""))
             close)
        (cons (code-review-registry--parse-front-matter
               (substring text 0 (+ close 5)))
              (substring text (+ close 5)))
      (cons nil (or text "")))))

(defun code-review-criteria--sentence (body)
  "The EARS requirement sentence from criteria BODY.
The first paragraph, lines joined with single spaces (one
sentence per file).  nil when the body is empty."
  (let* ((text (string-trim (or body "")))
         (para (car (split-string text "[ \t]*\n[ \t]*\n" t)))
         (joined (and para
                      (string-trim
                       (string-join (split-string para "[ \t\n]+")
                                    " ")))))
    (and joined (not (string-empty-p joined)) joined)))

(defun code-review-criteria--requirement-from-text (file text)
  "Requirement plist from criteria FILE content TEXT.  Pure.
\(:id :file :sentence :paths :text), or nil when the file name
carries no date-numbered ID or the text has no sentence."
  (let* ((id (code-review-criteria--id-from-file file))
         (split (code-review-criteria--split-front-matter text))
         (fm (car split))
         (sentence (code-review-criteria--sentence (cdr split))))
    (and id sentence
         (list :id id
               :file file
               :sentence sentence
               :paths (and fm (code-review-registry--split-values
                               (plist-get fm :paths)))
               :text text))))

;;; The scan (bounded, one git pass)

(defun code-review-criteria--files (repo)
  "The criteria files of REPO (repo-relative paths), or nil.
ONE `git ls-files' pass: tracked PLUS untracked-not-ignored —
criteria are often drafted BEFORE the commit (that is the solo
loop), so a not-yet-committed criteria file must already count.
Filtered by `code-review-criteria-file-regexp', capped by
`code-review-criteria-max-entries'.  nil on any failure (a
failed scan reads as no criteria, never as a signal)."
  (ignore-errors
    (let ((out (code-review-registry--git
                repo "ls-files" "--cached" "--others"
                "--exclude-standard")))
      (when out
        (let ((files (cl-remove-if-not
                      (lambda (f)
                        (string-match-p
                         code-review-criteria-file-regexp f))
                      (split-string out "\n" t))))
          (cl-subseq files 0 (min (length files)
                                   code-review-criteria-max-entries)))))))

(defun code-review-criteria--criteria (repo)
  "The requirements (criteria) of REPO as plists.  Never signals.
Each file read is byte-capped
(`code-review-criteria-max-entry-bytes'), the file list
count-capped; an oversized or unparseable file is logged and
skipped, never fatal.  NOT cached on purpose: the solo loop
drafts criteria and immediately re-renders the local review —
freshness beats the one cheap `git ls-files' pass."
  (ignore-errors
    (let ((res nil))
      (dolist (file (or (code-review-criteria--files repo) ()))
        (let ((full (expand-file-name file repo))
              (size (or (file-attribute-size
                         (file-attributes (expand-file-name file repo)))
                        0)))
          (cond
           ((> size code-review-criteria-max-entry-bytes)
            (code-review-utils--log
             "code-review-criteria"
             (format "criteria file skipped (byte cap): %s" file)))
           (t
            (let ((text (with-temp-buffer
                          (insert-file-contents full nil 0
                                                code-review-criteria-max-entry-bytes)
                          (buffer-substring-no-properties
                           (point-min) (point-max)))))
              (let ((req (code-review-criteria--requirement-from-text
                          file text)))
                (when req (push req res))))))))
      (nreverse res))))

;;; Matching criteria to a change (pure)

(defun code-review-criteria--covers-p (req path)
  "Non-nil when REQ covers PATH.  Pure.
Declared paths (front matter `paths:'): exact match, or a parent
directory (a declared `src/' covers `src/x.py').  Undeclared: the
requirement text must MENTION the path or its base name — a
plain literal search, no globbing."
  (let ((declared (plist-get req :paths))
        (text (or (plist-get req :text) "")))
    (or (and declared
             (cl-some (lambda (p)
                        (or (string= p path)
                            (and (string-suffix-p "/" p)
                                 (string-prefix-p p path))))
                      declared)
             t)
        (and (not declared)
             (or (string-search path text)
                 (string-search (file-name-nondirectory path) text))
             t))))

(defun code-review-criteria--matching (requirements paths)
  "The requirements covering PATHS: ((:req R :paths COVERED)...),
ID order (oldest requirement first).  Pure — the checklist's
input: every requirement that covers at least one changed
path, with exactly the changed paths it covers."
  (let ((res nil))
    (dolist (req (or requirements ()))
      (let ((covered (cl-remove-if-not
                      (lambda (p)
                        (code-review-criteria--covers-p req p))
                      (or paths ()))))
        (when covered
          (push (list :req req :paths covered) res))))
    (cl-sort (nreverse res) #'string<
             :key (lambda (m) (plist-get (plist-get m :req) :id)))))

(defun code-review-criteria--changed-paths (diff)
  "The changed file paths of raw DIFF text.  Pure.
The entry-side helper (the render side reads the db)."
  (and diff (mapcar #'car (code-review--diff--split-by-files diff))))

;;; Enforcement (the REQUIRE-mode refusal)

(defun code-review-criteria--enforce-local (repo diff)
  "The phase 23 REQUIRE-mode refusal for a local review of DIFF.
When `code-review-criteria-required' is `require' and no
criteria file covers any changed path, signal `user-error': the
local review REFUSES TO OPEN until the change's requirements
exist.  No-op in the nil/warn modes (warn renders its banner in
the review instead)."
  (when (eq code-review-criteria-required 'require)
    (let* ((paths (code-review-criteria--changed-paths diff))
           (criteria (and paths (code-review-criteria--criteria repo)))
           (matching (and criteria
                          (code-review-criteria--matching
                           criteria paths))))
      (unless matching
        (user-error
         "No criteria cover the files in this change%s; draft one \
with M-x code-review-criteria-insert-template"
         (if paths (format " (%s)" (string-join paths ", "))
           ""))))))

;;; The template (stable IDs, EARS skeleton, optional AI hook)

(defconst code-review-criteria--template
  "---\npaths: [<paths this requirement governs>]\n---\n\nWHEN <precondition> THE SYSTEM SHALL <postcondition>.\n"
  "The criteria file skeleton (EARS shape, one sentence).")

(defun code-review-criteria--template-text (paths)
  "Template text with PATHS prefilled (nil leaves the
placeholder).  Pure."
  (if paths
      (replace-regexp-in-string
       "<paths this requirement governs>"
       (string-join paths ", ")
       code-review-criteria--template)
    code-review-criteria--template))

(defun code-review-criteria--next-id (repo)
  "The next req ID for REPO: <today>-<NNN>, date-numbered.
Counted from the criteria of the SAME DATE.  Stable across
drafting (a template file already carries its ID)."
  (let* ((today (format-time-string "%Y-%m-%d"))
         (max 0))
    (dolist (req (or (code-review-criteria--criteria repo) ()))
      (let ((id (plist-get req :id)))
        (when (and id (string-prefix-p (concat today "-") id))
          (let ((n (and (string-match
                         (concat today "-\\([0-9]+\\)\\'") id)
                        (string-to-number (match-string 1 id)))))
            (when (and n (> n max)) (setq max n))))))
    (format "%s-%03d" today (1+ max))))

(defun code-review-criteria--review-paths ()
  "The changed paths of the review at point, when in one.
nil anywhere else (the template's prefill edge — a thing
generic tooling cannot do)."
  (ignore-errors
    (and (derived-mode-p 'code-review-mode)
         (code-review-criteria--changed-paths
          (code-review-db--pullreq-raw-diff)))))

(defun code-review-criteria--insert-template (&optional repo)
  "Insert a new criteria file under `code-review-criteria-dir'.
The req ID is date-numbered from the existing criteria
(stable, greppable into ERT test names: `req-<id>-'); the
skeleton is EARS-shaped, with the changed paths of the review
at point prefilled when in one.  Rename the `new-requirement'
slug to the requirement's subject.  Return the file's path
(the caller decides whether to visit it)."
  (let* ((root (or repo (code-review-registry--repo-dir)))
         (paths (unless repo (code-review-criteria--review-paths))))
    (unless root (user-error "No repository here (worktree or repo root)"))
    (let* ((id (code-review-criteria--next-id root))
           (dir (expand-file-name code-review-criteria-dir root))
           (file (expand-file-name (format "%s-new-requirement.md" id)
                                   dir)))
      (make-directory dir t)
      (with-temp-file file
        (insert (code-review-criteria--template-text paths)))
      (message "code-review-criteria: template %s (rename the \
new-requirement slug, fill the WHEN/SHALL)" file)
      file)))

;;;###autoload
(defun code-review-criteria-insert-template (&optional repo)
  "Insert a new criteria file under `code-review-criteria-dir'.
The req ID is date-numbered from the existing criteria
(stable, greppable into ERT test names: `req-<id>-'); the
skeleton is EARS-shaped, with the changed paths of the review
at point prefilled when in one.  Rename the `new-requirement'
slug to the requirement's subject.  Run interactively it also
VISITS the file (other window) so you can draft it right away.
Return the file's path."
  (interactive)
  (let ((file (code-review-criteria--insert-template repo)))
    (when (called-interactively-p 'any)
      (find-file-other-window file))
    file))

;;;###autoload
(defun code-review-criteria-draft (&optional arg)
  "Insert the template, then AI-draft the requirement text.
Calls `code-review-criteria-generate-function' with (FILE
PATHS) when wired — the hook drafts the text INTO the file and
the human reviews it (soft dependency only: the mode never
implements the hook).  Without a hook the template stands and
you draft the text.  With ARG non-nil, visit the file after."
  (interactive "P")
  (let* ((paths (code-review-criteria--review-paths))
         (file (code-review-criteria--insert-template)))
    (cond
     ((functionp code-review-criteria-generate-function)
      (funcall code-review-criteria-generate-function file paths)
      (message "code-review-criteria: AI-drafted %s — review \
the requirement text" file)
      (when arg (find-file-other-window file)))
     (t
      (message "code-review-criteria: no generate function \
wired (code-review-criteria-generate-function); template only")
      (when arg (find-file-other-window file))))
    file))

(provide 'code-review-criteria)
;;; code-review-criteria.el ends here
