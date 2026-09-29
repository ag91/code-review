;;; code-review-testimpact.el --- Test-impact verification and CI-gaming detector -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026 Andrea <andrea-dev@hotmail.com>
;;
;; This file is part of code-review.
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Phase 17 (see Improvements.org): when agents fail CI they have an
;; obvious path to green — remove tests, add skip markers, lower
;; thresholds, gate workflow steps.  All of that is mechanically
;; checkable in the worktree phase 1 gives us.
;;
;;  - TEST MAPPING: every hunk's touched definitions (phase 5 name
;;    extraction) are mapped to the test files referencing them (one
;;    batched worktree grep, the phase 5 one).  A hunk whose touched
;;    def is USED but referenced by NO test file gets a [NO-TEST]
;;    tag on its heading and an entry in the Analysis section.
;;
;;  - `T' on a file/hunk runs EXACTLY the mapped test subset in the
;;    review worktree via `compile' (jumpable failures).  The command:
;;    a `code-review-testimpact-test-command-alist' entry when set,
;;    else any test command projectile/project.el define (only when
;;    already loaded and actually defined), else built-in conventions
;;    (a Makefile `test' target, pytest, jest, go test, cargo test).  With C-u,
;;    the same subset also runs at the diff's BASE rev (a detached
;;    temporary worktree) and reports a FAKE-FIX verdict: a test that
;;    passes at BASE and at HEAD does not cover the change.  Local
;;    diff reviews work unchanged — they have no CI at all, so this
;;    is their only automated verification.
;;
;;  - CI-GAMING DETECTOR (pure regexps, cheap enough for the
;;    render): skip markers added (test.skip, xit(, @pytest.mark.skip
;;    ...), `|| true' in CI commands, workflow steps newly gated or
;;    removed, coverage thresholds lowered.  A hard [CI-GAME] tag on
;;    the offending file plus a HOISTED entry in the Analysis
;;    section.  DOC-classified files (docs, release notes) are
;;    skipped whole, comment-only lines are dropped, and skip
;;    markers apply only to their OWN languages (a python marker in
;;    an elisp string or an org snippet is not gaming): all three
;;    legitimately mention the markers in prose.
;;
;;  The mapping is a heuristic (dynamic dispatch, reflection and
;;  build-convention entry points are missed) and is labeled as such
;;  in the UI, phase 5 style.  Everything is cached per (PR, diff):
;;  the render cost is one batched grep plus pure elisp, and re-
;;  renders are instant.

;;; Code:

(require 'magit-section)
(require 'cl-lib)
(require 'code-review-analysis)
(require 'code-review-diff)
(require 'code-review-repo)
(require 'code-review-utils)

(declare-function compilation-start "compile")
(declare-function code-review-db-get-pullreq "code-review-db")
(declare-function code-review-db--pullreq-raw-diff "code-review-db")

;;; Configuration

(defgroup code-review-testimpact nil
  "Test-impact verification and CI-gaming detection (phase 17)."
  :group 'code-review)

(defcustom code-review-testimpact-enabled t
  "When non-nil, run the test-impact mapping and CI-gaming scan
when rendering (and allow `code-review-testimpact-run-tests')."
  :group 'code-review-testimpact
  :type 'boolean)

(defcustom code-review-testimpact-test-path-regexp
  "\\`test\\|/tests?/\\|/specs?/\\|_test\\.\\|\\.test\\.\\|_spec\\.\\|\\.spec\\."
  "Regexp matching file paths that are test files.
Matched against the DOWNCASED path (mirrors
`code-review-hunkhighlight-test-path-regexp' and
`code-review-dossier-test-path-regexp').  Used to classify
references: a def referenced from here is covered by a test."
  :group 'code-review-testimpact
  :type 'regexp)

(defcustom code-review-testimpact-skip-marker-regexps
  '(("\\.\\(js\\|mjs\\|cjs\\|jsx\\|ts\\|tsx\\)\\'"
     "\\_<\\(?:test\\|it\\|describe\\)\\.skip\\s-*("
     "\\_<x\\(?:it\\|describe\\)\\s-*(")
    ("\\.py\\'"
     "@pytest\\.mark\\.\\(?:skip\\|skipif\\|xfail\\)"
     "@unittest\\.skip"
     "\\_<pytest\\.skip\\s-*("
     "\\_<skipTest\\s-*(")
    ("\\.go\\'"
     "\\_<t\\.Skip\\s-*(")
    ("\\.rs\\'"
     "#\\[ignore\\]")
    ("\\.\\(java\\|kt\\)\\'"
     "\\_<@\\(?:Ignore\\|Disabled\\|Skip\\)"))
  "Regexps matching skip markers ADDED by a diff line.
Each entry is (FILE-REGEXP MARKER-REGEXP...): markers are scanned
only on ADDED lines of files matching FILE-REGEXP — a PYTHON
marker in an elisp string or an org snippet is not gaming (the
detector flagged its own test fixtures before this guard).  A
bare string entry (the old format) scans any file.  Removing a
marker is good news, not gaming, so deleted lines never match.
Every line is capped first (the megabyte-line regexp overflow
gotcha, see AGENTS.md).  Tunable: add your framework's marker
under its FILE-REGEXP here."
  :group 'code-review-testimpact
  :type '(repeat (cons regexp (repeat regexp))))

(defcustom code-review-testimpact-comment-prefix-alist
  '(("\\.\\(el\\|lisp\\)\\'" . ";;")
    ("\\.\\(c\\|cc\\|cpp\\|h\\|hpp\\|go\\|java\\|js\\|mjs\\|cjs\\|ts\\|tsx\\|rs\\|swift\\|kt\\|scala\\|php\\)\\'" . "//")
    ("\\.\\(py\\|rb\\|r\\|pl\\|sh\\|bash\\|zsh\\|ya?ml\\|toml\\|conf\\|ini\\|mk\\|service\\)\\'\\|\\(^\\|/\\)Makefile\\(\\.[^/]*\\)?\\'\\|\\(^\\|/\\)Dockerfile\\(\\.[^/]*\\)?\\'" . "#"))
  "Comment-line prefixes per file, an alist
(FILE-REGEXP . PREFIX-REGEXP).  A diff line whose content (after
the +/- marker and leading whitespace) starts with the prefix is
a COMMENT: every CI-gaming rule skips it, because comments
legitimately mention skip markers (a tutorial, the detector's own
docstring).  No entry for a file means its comment lines are
scanned.  DOC-classified files (see
`code-review-diff-noise-rules') are skipped whole."
  :group 'code-review-testimpact
  :type '(alist :key-type regexp :value-type regexp))

(defcustom code-review-testimpact-ci-file-regexp
  "\\.github/workflows/\\|\\.gitlab-ci\\|Jenkinsfile\\|\\(?:\\`\\|/\\)Makefile\\|\\.mk\\'\\|azure-pipelines\\|\\.circleci/\\|\\.drone\\.yml\\|cloudbuild"
  "Regexp matching CI/workflow/build file paths.
The `|| true', step-gating and step-removal rules only apply to
files matching this."
  :group 'code-review-testimpact
  :type 'regexp)

(defcustom code-review-testimpact-ci-true-regexp
  "||[[:space:]]*true\\b"
  "Regexp matching `|| true' (or friends) ADDED to a CI command:
the classic way to make a failing step stop failing."
  :group 'code-review-testimpact
  :type 'regexp)

(defcustom code-review-testimpact-ci-gating-regexps
  '("continue-on-error: *true"
    "if:.*\\bfalse\\b")
  "Regexps matching workflow steps NEWLY GATED by the diff
(added lines in files matching
`code-review-testimpact-ci-file-regexp')."
  :group 'code-review-testimpact
  :type '(repeat regexp))

(defcustom code-review-testimpact-ci-step-removed-regexps
  '("\\`[[:space:]]*-[[:space:]]*run:"
    "\\`[[:space:]]*-[[:space:]]*uses:")
  "Regexps matching CI steps REMOVED by the diff (deleted lines in
files matching `code-review-testimpact-ci-file-regexp')."
  :group 'code-review-testimpact
  :type '(repeat regexp))

(defcustom code-review-testimpact-coverage-file-regexp
  "\\.coveragerc\\'\\|pyproject\\.toml\\'\\|codecov\\.ya?ml\\'\\|jest\\.config\\|vitest\\.config\\|karma\\.conf\\|cypress\\.config\\|\\.nycrc"
  "Regexp matching coverage configuration file paths.
Only in these files is a lowered threshold looked for."
  :group 'code-review-testimpact
  :type 'regexp)

(defcustom code-review-testimpact-coverage-threshold-regexp
  "\\(?:fail_under\\|threshold\\|minimum\\)\\s-*[=:]?\\s-*\\(?1:[0-9]+\\(?:\\.[0-9]+\\)?\\)"
  "Regexp whose group 1 captures the NUMBER of a coverage
threshold line.  A deleted/added pair setting the SAME threshold
(matched by the text before the number) counts as gaming only when
the number went DOWN."
  :group 'code-review-testimpact
  :type 'regexp)

(defcustom code-review-testimpact-test-command-alist nil
  "Alist mapping projects to the command running a test SUBSET.
Each entry is (WORKTREE-REGEXP . TEMPLATE): the first entry whose
WORKTREE-REGEXP matches the review worktree's absolute path wins.
TEMPLATE is a shell command; %f expands to the mapped test FILES
(shell-quoted, space separated) and %n to the touched definition
names.  Examples:

  ((\"myproject/\" . \"pytest %f\")
   (\"webapp/\" . \"npx jest %f\"))

Usually NO entry is needed: with no match, any test command
projectile or project.el define is reused first (only when the
package is already loaded and a command is actually defined), then
built-in conventions (a Makefile with a `test' target, pytest, a
jest config or test script, go test, cargo test).  Only a
project none of those recognize needs an entry here.
A template without %f runs the FULL suite (you are told via a
message when that happens)."
  :group 'code-review-testimpact
  :type '(alist :key-type regexp :value-type string))

(defcustom code-review-testimpact-timeout 600
  "Seconds an async test run may take before it is killed
(the fake-fix check runs the mapped subset twice)."
  :group 'code-review-testimpact
  :type 'natnum)

(defcustom code-review-testimpact-max-output-chars 20000
  "Test log chars kept per run in the fake-fix report buffer
(the TAIL is kept: test runners summarize at the end).  0 keeps
everything."
  :group 'code-review-testimpact
  :type 'natnum)

(defcustom code-review-testimpact-max-notest 30
  "No-test-coverage hunks stored and rendered per (PR, diff)."
  :group 'code-review-testimpact
  :type 'natnum)

;;; Pure helpers: classification

(defun code-review-testimpact--doc-file-p (path)
  "Non-nil when PATH is noise-classified as a DOC file.
Docs MENTION CI markers in prose: the detector flagged its own
release notes once.  See `code-review-diff-noise-rules'."
  (string= (plist-get (code-review--diff--classify-path path)
                      :tag)
           "DOC"))

(defun code-review-testimpact--comment-prefix (path)
  "The comment-line prefix regexp for PATH, or nil.
Nil (no `code-review-testimpact-comment-prefix-alist' entry
matches) means comment lines are not filtered."
  (cdr (cl-assoc path code-review-testimpact-comment-prefix-alist
                :test (lambda (path re)
                        (string-match-p re path)))))

(defun code-review-testimpact--non-comment (lines prefix)
  "LINES ((LINE . TEXT)...) without comment-only ones.
PREFIX is a comment-start regexp from
`code-review-testimpact--comment-prefix'; nil keeps everything."
  (if prefix
      (cl-remove-if (lambda (x)
                      (string-match-p (concat "\\`[ \t]*" prefix)
                                      (cdr x)))
                    lines)
    lines))

(defun code-review-testimpact--markers-for (path)
  "The skip-marker regexps applying to file PATH.
Entries of `code-review-testimpact-skip-marker-regexps' are
\(FILE-REGEXP MARKER-REGEXP...): a marker applies only to its own
language.  A bare string entry (the old format) applies to any
file."
  (cl-loop for entry in code-review-testimpact-skip-marker-regexps
           if (stringp entry)
             collect entry
           else
             when (string-match-p (car entry) path)
               append (cdr entry)))

(defun code-review-testimpact--test-file-p (path)
  "Non-nil when PATH is a test file."
  (and code-review-testimpact-test-path-regexp
       (string-match-p code-review-testimpact-test-path-regexp
                       (downcase path))))

(defun code-review-testimpact--test-entry-p (name)
  "Non-nil when definition NAME is a test entry point.
Those are invoked by BUILD-TOOL CONVENTION, not by explicit
references: they do not need covering tests of their own (reuses
`code-review-analysis-dead-test-name-regexp')."
  (and code-review-analysis-dead-test-name-regexp
       (string-match-p code-review-analysis-dead-test-name-regexp
                       name)))

(defun code-review-testimpact--test-files (refs)
  "The test files among REFS (a definition's worktree occurrences,
the list from `code-review-analysis--definitions-refs')."
  (and refs
       (delete-dups
        (cl-loop for (path _line _text) in refs
                 when (code-review-testimpact--test-file-p path)
                 collect path))))

;;; Pure: CI-gaming scan

(defun code-review-testimpact--threshold-key (text)
  "When TEXT sets a coverage threshold: (KEY . NUMBER), else nil.
KEY is everything before the number, so a -/+ pair changing the
SAME threshold matches; only lowerings are findings."
  (let ((capped (code-review-analysis--cap-line text)))
    (when (string-match code-review-testimpact-coverage-threshold-regexp
                        capped)
      ;; bind BEFORE anything else can match (match data gotcha)
      (let ((prefix (string-trim (substring capped
                                            (match-beginning 0)
                                            (match-beginning 1))))
            (num (string-to-number
                  (match-string-no-properties 1 capped))))
        (cons prefix num)))))

(defun code-review-testimpact--scan-ci (blocks)
  "Pure CI-gaming scan over diff BLOCKS ((PATH . BLOCK)...).
Findings are (:kind :path :line :text) plists with KIND one of
skip, ci-true, gate, removed, threshold.  Skip markers scan ADDED
lines of files matching the marker entry's FILE-REGEXP (a marker
applies to its own language: a python marker in an elisp string
is not gaming); the CI rules scan added/deleted lines of files
matching `code-review-testimpact-ci-file-regexp'; threshold
lowerings pair deleted/added lines of coverage config files.
DOC-classified files (docs, release notes, changelogs) are
skipped whole and COMMENT-only lines are dropped: both
legitimately MENTION the markers (the detector flagged its own
release notes once).  Every line is capped first (the
megabyte-line regexp overflow gotcha, see AGENTS.md)."
  (let ((findings nil))
    (pcase-dolist (`(,path . ,block) blocks)
      (let* ((lines (code-review-analysis--block-lines block))
             ;; DOC files mention markers in prose: skipped whole
             (doc-p (code-review-testimpact--doc-file-p path))
             (prefix (code-review-testimpact--comment-prefix path))
             (markers (code-review-testimpact--markers-for path))
             (added (and (not doc-p)
                         (code-review-testimpact--non-comment
                          (plist-get lines :added) prefix)))
             (deleted (and (not doc-p)
                           (code-review-testimpact--non-comment
                            (plist-get lines :deleted) prefix)))
             (ci-p (and (not doc-p)
                        code-review-testimpact-ci-file-regexp
                        (string-match-p
                         code-review-testimpact-ci-file-regexp path)))
             (cov-p (and (not doc-p)
                         code-review-testimpact-coverage-file-regexp
                         (string-match-p
                          code-review-testimpact-coverage-file-regexp
                          path))))
        ;; skip markers: ADDED lines of the marker's OWN languages
        (dolist (x added)
          (let ((text (code-review-analysis--cap-line (cdr x))))
            (when (cl-loop for re in markers
                           thereis (string-match-p re text))
              (push (list :kind 'skip :path path :line (car x) :text text)
                    findings))))
        (when ci-p
          (dolist (x added)
            (let ((text (code-review-analysis--cap-line (cdr x))))
              (when (and code-review-testimpact-ci-true-regexp
                         (string-match-p code-review-testimpact-ci-true-regexp
                                         text))
                (push (list :kind 'ci-true :path path :line (car x)
                            :text text)
                      findings))
              (when (cl-loop for re in code-review-testimpact-ci-gating-regexps
                             thereis (string-match-p re text))
                (push (list :kind 'gate :path path :line (car x)
                            :text text)
                      findings))))
          (dolist (x deleted)
            (let ((text (code-review-analysis--cap-line (cdr x))))
              (when (cl-loop for re
                             in code-review-testimpact-ci-step-removed-regexps
                             thereis (string-match-p re text))
                (push (list :kind 'removed :path path :line (car x)
                            :text text)
                      findings)))))
        (when cov-p
          (let ((old (make-hash-table :test #'equal)))
            (dolist (x deleted)
              (when-let* ((kv (code-review-testimpact--threshold-key
                               (cdr x))))
                (puthash (car kv) (cdr kv) old)))
            (dolist (x added)
              (when-let* ((kv (code-review-testimpact--threshold-key
                               (cdr x))))
                (let ((old-val (gethash (car kv) old)))
                  (when (and old-val (< (cdr kv) old-val))
                    (push (list :kind 'threshold :path path :line (car x)
                                :text (format "was %s; now %s"
                                              old-val (cdr kv)))
                          findings)))))))))
    (nreverse findings)))

(defun code-review-testimpact--kind-label (kind)
  "Human label for a CI-gaming finding KIND."
  (or (plist-get '(:skip "skip marker added"
                   :ci-true "|| true added to a CI command"
                   :gate "CI step newly gated"
                   :removed "CI step removed"
                   :threshold "coverage threshold lowered")
                 kind)
      (format "%s" kind)))

;;; Pure: test mapping

(defun code-review-testimpact--map-hunks (worktree blocks)
  "Map every hunk's touched definitions to its covering tests.
BLOCKS are ((PATH . BLOCK)...) from `code-review--diff--split-by-files'.
Return a list of entries (:path :ranges :defs :tests :notest) for
every hunk touching at least one definition: :DEFS all added-side
definition names of the hunk, :TESTS the test files referencing
any of them (deduped), :NOTEST the subset of DEFS that are USED
in the worktree but referenced by NO test file.  Entirely
unreferenced defs (phase 5 reports those as possibly dead) and
test entry points (build-tool convention) stay out of :NOTEST.
ONE batched worktree `git grep' for all names (the phase 5 one).
DOC-classified files are skipped: prose lines are not
definitions even when they mention them."
  (let ((hunks nil)
        (names nil))
    (pcase-dolist (`(,path . ,block) blocks)
      ;; DOC files mention definitions in prose: skipped whole
      (dolist (hu (and (not (code-review-testimpact--doc-file-p path))
                       (code-review-analysis--split-hunks block)))
        (let ((defs (mapcar #'car
                            (code-review-analysis--definitions-in
                             path (plist-get hu :added)))))
          (when defs
            (push (list :path path
                        :ranges (plist-get hu :ranges)
                        :defs defs)
                  hunks)
            (setq names (append names defs))))))
    (let ((refs (when names
                  (code-review-analysis--definitions-refs
                   worktree (delete-dups names)))))
      (mapcar
       (lambda (hu)
         (let ((tests nil)
               (notest nil))
           (dolist (d (plist-get hu :defs))
             (let ((tfiles (and refs
                                (code-review-testimpact--test-files
                                 (gethash d refs)))))
               (setq tests (append tests tfiles))
               (when (and (gethash d refs)
                          (not (code-review-testimpact--test-entry-p d))
                          (null tfiles))
                 (push d notest))))
           (list :path (plist-get hu :path)
                 :ranges (plist-get hu :ranges)
                 :defs (plist-get hu :defs)
                 :tests (delete-dups tests)
                 :notest (nreverse notest))))
       (nreverse hunks)))))

(defun code-review-testimpact--tests-for (tres path ranges)
  "(:tests FILES :defs NAMES) mapped to PATH (and RANGES) in TRES.
A hunk target (RANGES non-nil) maps its own defs only; a file
target maps the union over all its hunks."
  (let ((tests nil)
        (defs nil))
    (dolist (e (plist-get tres :hunks))
      (when (equal (plist-get e :path) path)
        (when (or (null ranges)
                  (equal (plist-get e :ranges) ranges))
          (setq tests (append tests (plist-get e :tests))
                defs (append defs (plist-get e :defs))))))
    ;; copy first: `delete-dups' is DESTRUCTIVE and `append' shares
    ;; the last list's conses with the cached entry — splicing it
    ;; would corrupt the cache for later T presses
    (list :tests (delete-dups (copy-sequence tests))
          :defs (delete-dups (copy-sequence defs)))))

;;; Pure: tags

(defun code-review-testimpact--hunk-tag (tres path ranges)
  "The NO-TEST tag text for hunk PATH+RANGES in map TRES, or nil.
Lists up to three untested definition names."
  (let ((defs (cl-loop for e in (plist-get tres :hunks)
                       when (and (equal (plist-get e :path) path)
                                 (equal (plist-get e :ranges) ranges))
                       append (plist-get e :notest))))
    (when defs
      (format "NO-TEST: %s"
              (string-join
               (append (cl-subseq defs 0 (min 3 (length defs)))
                       (when (> (length defs) 3) '("...")))
               ", ")))))

(defun code-review-testimpact--file-tag (tres path)
  "The CI-GAME tag for file PATH in map TRES (nil when clean)."
  (when (cl-loop for f in (plist-get tres :ci)
                 thereis (equal (plist-get f :path) path))
    "CI-GAME"))

;;; Compute and cache

(defvar code-review-testimpact--cache (make-hash-table :test #'equal)
  "Test-impact results keyed by (pullreq-id, diff md5).")

(defun code-review-testimpact--compute (worktree diff)
  "All test-impact findings for DIFF against WORKTREE.
Return (:ci FINDINGS :hunks ENTRIES :notest NOTEST) or nil when
there is nothing at all.  The CI scan is pure diff text; the test
mapping runs one batched worktree grep (see `--map-hunks')."
  (let* ((blocks (code-review--diff--split-by-files diff))
         (ci (code-review-testimpact--scan-ci blocks))
         (hunks (code-review-testimpact--map-hunks worktree blocks))
         (notest (cl-loop for e in hunks
                          when (plist-get e :notest)
                          collect (list :path (plist-get e :path)
                                        :ranges (plist-get e :ranges)
                                        :defs (plist-get e :notest)))))
    (setq notest (cl-subseq notest 0
                            (min (length notest)
                                 code-review-testimpact-max-notest)))
    (when (or ci hunks)
      (list :ci ci :hunks hunks :notest notest))))

(defun code-review-testimpact-run ()
  "Compute (or reuse) the test-impact map of the current review.
Return the plist (see `code-review-testimpact--compute'), nil when
disabled, there is no worktree/diff/PR, or nothing was
computable.  Cached per (PR, diff) like the phase 5 analysis.  A
failing compute is LOGGED and reported as no findings: the
test-impact map is a heuristic overlay and must never take the
review buffer down with it (the phase 5 survivability rule)."
  (when code-review-testimpact-enabled
    (let* ((worktree code-review-repo-worktree)
           (diff (and worktree (code-review-db--pullreq-raw-diff)))
           (pr (and diff (code-review-db-get-pullreq))))
      (when (and worktree diff pr)
        (let ((key (concat (oref pr id) "|" (md5 diff))))
          (or (gethash key code-review-testimpact--cache)
              (let ((res (condition-case err
                             (code-review-testimpact--compute worktree diff)
                           (error
                            (code-review-utils--log
                             "code-review-testimpact"
                             (format "test-impact failed, skipping (%s): %S"
                                     key err))
                            nil))))
                (puthash key res code-review-testimpact--cache)
                res)))))))

(defun code-review-testimpact--hunk-tag-for (path ranges)
  "The NO-TEST tag for hunk PATH+RANGES of the current render, nil when none.
Called from the hunk wash: `code-review-testimpact-run' is a cache
hit there (the Analysis section runs before the diff wash in
`code-review-sections-hook')."
  (let ((tres (code-review-testimpact-run)))
    (and tres (code-review-testimpact--hunk-tag tres path ranges))))

(defun code-review-testimpact--file-tag-for (path)
  "The CI-GAME tag for file PATH of the current render, nil when clean.
Called from the file wash (cache hit, see `--hunk-tag-for')."
  (let ((tres (code-review-testimpact-run)))
    (and tres (code-review-testimpact--file-tag tres path))))

(defun code-review-testimpact-reset ()
  "Forget all cached test-impact results."
  (interactive)
  (clrhash code-review-testimpact--cache))

;;; The T command: run the mapped subset

(defun code-review-testimpact--strip-path (path)
  "Remove a leading \"a/\" or \"b/\" from diff file PATH."
  (cond ((string-prefix-p "a/" path) (substring path 2))
        ((string-prefix-p "b/" path) (substring path 2))
        (t path)))

(defun code-review-testimpact--section-target ()
  "(:path PATH :ranges RANGES-or-nil) for the hunk/file at point.
The innermost wins: inside a hunk that hunk, else the file
section, else nil (containers and the root do not map to a test
subset)."
  (let ((sec (magit-current-section))
        (res nil))
    (while (and sec (not res))
      (cond
       ((and (eq (eieio-object-class sec) 'magit-hunk-section)
             (slot-boundp sec 'value))
        (let ((v (oref sec value)))
          (setq res (list :path (cdr (assq 'path v))
                          :ranges (cdr (assq 'ranges v))))))
       ((and (magit-file-section-p sec)
             (slot-boundp sec 'value))
        (setq res (list :path (code-review-testimpact--strip-path
                                (substring-no-properties
                                 (oref sec value))))))
       (t
        (setq sec (and (slot-boundp sec 'parent)
                       (oref sec parent))))))
    (and res (plist-get res :path) res)))

(defun code-review-testimpact--expand (template files names)
  "Expand TEMPLATE's %f (FILES) and %n (NAMES), shell-quoted."
  (let ((cmd (replace-regexp-in-string
              "%n" (mapconcat #'shell-quote-argument names " ")
              template nil t)))
    (replace-regexp-in-string
     "%f" (mapconcat #'shell-quote-argument files " ")
     cmd nil t)))

(defun code-review-testimpact--read-head (file)
  "The first 512KB of FILE, or nil (a BOUNDED read).
Project markers are tiny; a giant file is noise anyway."
  (when (file-exists-p file)
    (with-temp-buffer
      (insert-file-contents file nil 0 524288)
      (buffer-string))))

(defun code-review-testimpact--command-project (worktree)
  "A test command from projectile or project.el for WORKTREE, or nil.
Consulted only when the package is ALREADY loaded (never loading
user packages from a review render) and only when a command is
actually defined: projectile's resolver for WORKTREE (the last
command run via projectile there, then .dir-locals, then its
per-project-type default), then project.el's
`project-test-command' (not in Emacs 30 — boundp-guarded, it may
appear later or be user-defined)."
  (or (and (fboundp 'projectile-test-command)
           ;; the cmd-map keys are directory strings; try both the
           ;; slash and no-slash spellings of the worktree
           (cl-loop for dir in (list worktree
                                     (directory-file-name worktree))
                    thereis (stringp
                             (ignore-errors
                               (with-temp-buffer
                                 (setq default-directory worktree)
                                 (projectile-test-command dir))))))
      (and (boundp 'project-test-command)
           (stringp project-test-command)
           (not (string-empty-p project-test-command))
           project-test-command)))

(defun code-review-testimpact--command-conventions (worktree)
  "Built-in test-command conventions for WORKTREE, or nil.
The last fallback: a Makefile with a `test' target, pytest
(pyproject.toml [tool.pytest...] or pytest.ini), go.mod, a
`test' script in package.json, a jest config, Cargo.toml."
  (cond
   ((when-let* ((mk (code-review-testimpact--read-head
                     (expand-file-name "Makefile" worktree))))
      (string-match-p "^test\\s-*:" mk))
    "make test")
   ((when-let* ((pp (code-review-testimpact--read-head
                     (expand-file-name "pyproject.toml" worktree))))
      (string-match-p "^\\[tool\\.pytest" pp))
    "pytest %f")
   ((file-exists-p (expand-file-name "pytest.ini" worktree))
    "pytest %f")
   ((file-exists-p (expand-file-name "go.mod" worktree))
    "go test ./...")
   ((when-let* ((pj (code-review-testimpact--read-head
                     (expand-file-name "package.json" worktree))))
      (string-match-p "\"test\":" pj))
    "npm test")
   ((cl-loop for ext in '("js" "mjs" "cjs" "ts" "json")
             thereis (file-exists-p
                      (expand-file-name
                       (concat "jest.config." ext) worktree)))
    "npx jest %f")
   ((file-exists-p (expand-file-name "Cargo.toml" worktree))
    "cargo test")))

(defun code-review-testimpact--command-for (worktree tests defs)
  "The test command for WORKTREE running the mapped TESTS (defs DEFS).
The first `code-review-testimpact-test-command-alist' entry whose
regexp matches WORKTREE wins.  With no entry: any test command
projectile or project.el define (see
`code-review-testimpact--command-project'), then built-in
conventions (see `code-review-testimpact--command-conventions').
A template without %f runs the FULL suite (the user is told via
a message).  nil only when nothing at all matches."
  (let ((template (or (cl-loop for (re . tpl)
                               in code-review-testimpact-test-command-alist
                               when (and (stringp re) (stringp tpl)
                                         (string-match-p re
                                                         (expand-file-name
                                                          worktree)))
                               return tpl)
                      (code-review-testimpact--command-project worktree)
                      (code-review-testimpact--command-conventions
                       worktree))))
    (when template
      (unless (string-match-p "%f" template)
        (message "code-review: the test command template has no %%f: \
running the FULL suite"))
      (code-review-testimpact--expand template tests defs))))

(defun code-review-testimpact--status (code)
  "Run CODE (exit status or `timeout') as a status symbol."
  (cond
   ((eq code 'timeout) 'timeout)
   ((numberp code) (if (zerop code) 'pass 'fail))
   (t nil)))

(defun code-review-testimpact--verdict (head base)
  "The fake-fix verdict for statuses HEAD and BASE.
Each is \\='pass, \\='fail, \\='timeout or nil (not run).  The headline
case: pass at HEAD and pass at BASE means the subset does not
cover the change."
  (cond
   ((not head)
    "No verdict: the HEAD run did not produce a status.")
   ((eq head 'timeout)
    "The HEAD run timed out; no verdict.")
   ((eq head 'fail)
    (if (eq base 'fail)
        "The subset fails at HEAD and at BASE too: pre-existing failure, \
not caused by this change.  Read the logs."
      "The subset FAILS at HEAD: read the log below."))
   ((eq base 'timeout)
    "HEAD passes; the BASE run timed out, no fake-fix verdict.")
   ((not base)
    "HEAD passes.  No base comparison available (see the note).")
   ((eq base 'pass)
    "WARNING: the subset passes on the PRE-change code too — these tests \
do not cover the change (fake-fix shaped).")
   (t
    "Good: the subset fails on the pre-change code, so it covers the \
change.")))

;;; The async runner (fake-fix check)

(defun code-review-testimpact--run-async (dir command callback)
  "Run shell COMMAND in DIR, calling CALLBACK with (CODE OUTPUT).
CODE is the exit status, or the symbol `timeout' when
`code-review-testimpact-timeout' killed the run.  Runs through the
shell so templates stay plain shell strings; the callback is
wrapped in a condition-case (a failing callback must not leave
process machinery broken) and the process buffer is always
removed."
  (let* ((buf (generate-new-buffer " *code-review-testimpact-run*"))
         (proc
          (with-current-buffer buf
            (setq default-directory (file-name-as-directory dir))
            (start-file-process "code-review-testimpact" buf
                                shell-file-name shell-command-switch
                                command))))
    (process-put proc 'cr-ti-callback callback)
    (process-put proc 'cr-ti-buffer buf)
    (when code-review-testimpact-timeout
      (process-put proc 'cr-ti-timer
                   (run-at-time code-review-testimpact-timeout nil
                                #'code-review-testimpact--timeout proc)))
    (set-process-sentinel proc #'code-review-testimpact--sentinel)))

(defun code-review-testimpact--timeout (proc)
  "Kill PROC (the run exceeded `code-review-testimpact-timeout')."
  (when (process-live-p proc)
    (delete-process proc)))

(defun code-review-testimpact--sentinel (proc _event)
  "Report an async test run's outcome (see `--run-async')."
  (when (memq (process-status proc) '(exit signal))
    (let* ((timer (process-get proc 'cr-ti-timer))
           (callback (process-get proc 'cr-ti-callback))
           (buf (process-get proc 'cr-ti-buffer))
           (timed-out (eq (process-status proc) 'signal))
           (code (if timed-out 'timeout (process-exit-status proc)))
           (out (when (buffer-live-p buf)
                  (with-current-buffer buf
                    (buffer-substring-no-properties (point-min)
                                                    (point-max))))))
      (when timer (cancel-timer timer))
      (when (buffer-live-p buf) (kill-buffer buf))
      (when (functionp callback)
        (condition-case err
            (funcall callback code (or out ""))
          (error
           (code-review-utils--log
            "code-review-testimpact"
            (format "async test callback failed: %S" err))))))))

(defun code-review-testimpact--truncate (out)
  "The TAIL of OUT within `code-review-testimpact-max-output-chars'.
Test runners summarize at the end, so the tail is the useful
part.  0 keeps everything."
  (if (or (<= code-review-testimpact-max-output-chars 0)
          (<= (length out) code-review-testimpact-max-output-chars))
      out
    (concat "[... output truncated ...]\n"
            (substring out (- (length out)
                              code-review-testimpact-max-output-chars)))))

(defun code-review-testimpact--report (tests head base head-out base-out note)
  "Show the fake-fix report buffer for the TESTS subset run.
HEAD/BASE are the statuses (\\='pass \\='fail \\='timeout or nil when not
run), *-OUT the raw logs, NOTE an extra explanation line."
  (with-current-buffer (get-buffer-create "*Code Review Test Impact*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert "Test-impact check: does the mapped subset COVER the change?\n")
      (insert (format "mapped test files (%d): %s\n\n"
                      (length tests) (string-join tests ", ")))
      (insert (format "  HEAD: %s\n" (or head "not run")))
      (when base
        (insert (format "  BASE: %s\n" base)))
      (when note
        (insert (propertize (format "  note: %s\n" note)
                            'font-lock-face 'font-lock-warning-face)))
      (insert ?\n)
      (insert (propertize (code-review-testimpact--verdict head base)
                          'font-lock-face
                          (if (and (eq head 'pass) (eq base 'pass))
                              'font-lock-warning-face
                            'magit-dimmed)))
      (insert "\n\n")
      (when head-out
        (insert "--- HEAD log ---\n"
                (code-review-testimpact--truncate head-out)))
      (when base-out
        (insert "\n--- BASE log ---\n"
                (code-review-testimpact--truncate base-out)))
      (special-mode)
      (goto-char (point-min)))
    (pop-to-buffer (current-buffer))))

(defun code-review-testimpact--rev-parse (worktree rev)
  "The commit sha of REV in WORKTREE, or nil (rev missing/invalid)."
  (and rev
       (code-review-repo--git worktree "rev-parse" "--verify" "--quiet"
                              (format "%s^{commit}" rev))))

(defun code-review-testimpact--base-rev ()
  "A rev that can be CHECKED OUT for the diff's old side, or nil.
Forge PRs: the fetched refs/remotes/code-review/N/base (or the
base branch name).  Local reviews: derived from the stored diff
args via `code-review-analysis--old-rev' (nil for root-commit
reviews, HEAD for staged ones — where a base comparison is
meaningless, see the note in the report)."
  (let ((pr (ignore-errors (code-review-db-get-pullreq))))
    (cond
     ((not pr) nil)
     ((and (slot-boundp pr 'state)
           (equal (oref pr state) "LOCAL"))
      (and (slot-boundp pr 'base-ref-name)
           (code-review-analysis--old-rev (oref pr base-ref-name))))
     (t
      (let* ((num (and (slot-boundp pr 'number)
                       (format "%s" (oref pr number))))
             (ref (and num (format "refs/remotes/code-review/%s/base" num)))
             (fetched (and ref code-review-repo-worktree
                           (code-review-testimpact--rev-parse
                            code-review-repo-worktree ref))))
        (or fetched
            (and (slot-boundp pr 'base-ref-name)
                 (let ((base (oref pr base-ref-name)))
                   (and (stringp base) (not (string-empty-p base))
                        base)))))))))

(defun code-review-testimpact--fake-fix-run (worktree cmd tests)
  "Run CMD for TESTS at HEAD and at the base rev, then report.
HEAD runs in the review WORKTREE as-is (forge reviews: the
worktree is at the PR head; local reviews of older commits: the
working tree is whatever it is — approximated, and said so in
the report note).  The base runs in a DETACHED TEMPORARY worktree
at the diff's old-side rev, removed afterwards.  On promisor
(partial) clones the base checkout lazy-fetches the base tree's
blobs once — bounded, explicit user action, off the render path."
  (message "code-review: running the mapped tests at HEAD...")
  (code-review-testimpact--run-async
   worktree cmd
   (lambda (head-code head-out)
     (let ((base-rev (code-review-testimpact--base-rev)))
       (cond
        ((not base-rev)
         (code-review-testimpact--report
          tests (code-review-testimpact--status head-code) nil
          head-out nil
          "no base rev available for this review"))
        ((equal (code-review-testimpact--rev-parse worktree base-rev)
                (code-review-testimpact--rev-parse worktree "HEAD"))
         (code-review-testimpact--report
          tests (code-review-testimpact--status head-code) nil
          head-out nil
          "base and HEAD are the same commit here; a base comparison \
is meaningless"))
        (t
         (let ((tmp (make-temp-file "cr-testimpact-base-" t)))
           (if (not (code-review-repo--git worktree "worktree" "add"
                                           "--detach" tmp base-rev))
               (progn
                 (ignore-errors (delete-directory tmp :recursive))
                 (code-review-testimpact--report
                  tests (code-review-testimpact--status head-code) nil
                  head-out nil
                  (format "could not check out the base rev %s" base-rev)))
             (message "code-review: running the same tests at the BASE \
rev (%s)..." base-rev)
             (code-review-testimpact--run-async
              tmp cmd
              (lambda (base-code base-out)
                (ignore-errors
                  (code-review-repo--git worktree "worktree" "remove"
                                         "--force" tmp))
                (code-review-testimpact--report
                 tests
                 (code-review-testimpact--status head-code)
                 (code-review-testimpact--status base-code)
                 head-out base-out nil)))))))))))

;;;###autoload
(defun code-review-testimpact-run-tests (&optional arg)
  "Run the tests mapped to the hunk or file at point (phase 17).
The mapped subset is the set of test files referencing the
definitions this hunk (or file) touches — see the Analysis
section.  The command comes from
`code-review-testimpact-test-command-alist' (%f expands to the
mapped test files, %n to the touched definition names) — or,
with no entry, from any test command projectile/project.el
define, then built-in conventions (Makefile `test' target,
pytest, jest, go test, cargo test) — and runs
via `compile' in the review worktree, so failures are jumpable.
This also works in LOCAL diff reviews (their only automated
verification — no CI there).

With \\[universal-argument] ARG, run the same subset at the HEAD
worktree AND at the diff's BASE rev (a detached temporary
worktree, removed afterwards) and report a FAKE-FIX verdict: a
test that passes at BASE and at HEAD does not cover the change."
  (interactive "P")
  (unless code-review-testimpact-enabled
    (user-error "Test-impact disabled (code-review-testimpact-enabled)"))
  (unless code-review-repo-worktree
    (user-error "No worktree for this review"))
  (let ((target (code-review-testimpact--section-target)))
    (unless target
      (user-error "Point is not on a file or hunk section"))
    (let* ((tres (code-review-testimpact-run))
           (res (and tres
                     (code-review-testimpact--tests-for
                      tres (plist-get target :path)
                      (plist-get target :ranges))))
           (tests (and res (plist-get res :tests)))
           (defs (and res (plist-get res :defs))))
      (unless tests
        (user-error
         "No mapped tests for %s%s (defs: %s)"
         (plist-get target :path)
         (if (plist-get target :ranges)
             (format "@%s" (plist-get target :ranges))
           "")
         (if defs (string-join defs ", ") "none touched")))
      (let ((cmd (code-review-testimpact--command-for
                  code-review-repo-worktree tests defs)))
        (unless cmd
          (user-error
           "No test command for this project: none of projectile/project.el \
defines one, no convention matched, and code-review-testimpact-test-command-alist \
has no entry"))
        (if (equal arg '(4))
            (code-review-testimpact--fake-fix-run
             code-review-repo-worktree cmd tests)
          (message "code-review: running %d mapped test file%s (%s)"
                   (length tests) (if (cdr tests) "s" "")
                   (string-join tests ", "))
          (let ((default-directory
                  (file-name-as-directory code-review-repo-worktree)))
            (compilation-start cmd nil
                               (lambda (_mode) "*Code Review Tests*"))))))))

(provide 'code-review-testimpact)
;;; code-review-testimpact.el ends here
