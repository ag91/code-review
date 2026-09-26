;;; code-review-analysis.el --- Heuristic duplicate and dead code detection -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026 Andrea <andrea-dev@hotmail.com>
;;
;; This file is part of code-review.
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;  Phase 5: heuristic analysis of the reviewed diff, shown in a
;;  toggleable "Analysis" section of the review buffer (works for
;;  forge PRs AND local diff reviews, see Improvements.org).
;;
;;  - Similar code: normalize added lines (mask strings, numbers,
;;    comments, whitespace), take N-line shingles, match them
;;    against an index of the repository built at the *base* commit.
;;    Reported as "12/40 added lines also in FILE:A-B".
;;
;;  - Dead code: (a) newly added definitions with zero references in
;;    the head tree -> "possibly dead"; (b) symbols deleted by the
;;    diff but still referenced elsewhere -> "dangling reference".
;;
;;  Phase 15: hunk delicacy.  Per hunk: blast radius (callers of the
;;  defs the hunk touches, reusing the batched phase 5 grep), line
;;  age and ownership (one bounded `git blame --porcelain' per
;;  changed file, -L ranges), complexity delta (branch keywords
;;  added vs removed), dead-on-arrival.  Scored, cached with the
;;  rest; the wash paints a badge on delicate hunk headings and the
;;  Analysis section lists the top-K (jump links) for
;;  `code-review-next-delicate-hunk' (C-c C-d).
;;
;;  Everything here is heuristic and labeled as such in the UI: it
;;  never blocks the review, it only adds a section with jump links.
;;
;;  All git greps run on the WORKTREE, never on the packed base
;;  tree (`git grep <rev>' inflates every blob and takes minutes on
;;  big repositories).  Candidate discovery uses `git grep -l'
;;  (first match per file), references are one batched grep for
;;  all definition names, and everything elisp-side is hard-capped
;;  (probes, candidate files, index bytes).  Results are cached
;;  per (pullreq, diff) so re-renders are instant.

;;; Code:

(require 'magit-git)
(require 'cl-lib)
(require 'code-review-db)
(require 'code-review-diff)
(require 'code-review-repo)
(require 'code-review-utils)

;;; Configuration

(defgroup code-review-analysis nil
  "Heuristic duplicate and dead code analysis."
  :group 'code-review)

(defcustom code-review-analysis-enabled t
  "When non-nil, run the heuristic analysis when rendering."
  :group 'code-review-analysis
  :type 'boolean)

(defcustom code-review-analysis-shingle-size 4
  "Number of consecutive added lines forming one shingle."
  :group 'code-review-analysis
  :type 'integer)

(defcustom code-review-analysis-min-covered 6
  "Minimum number of covered added lines to report a similarity."
  :group 'code-review-analysis
  :type 'integer)

(defcustom code-review-analysis-max-findings-per-file 3
  "Maximum similar-code findings reported per diff file."
  :group 'code-review-analysis
  :type 'integer)

(defcustom code-review-analysis-min-covered-ratio 0.1
  "Minimum fraction of the file's ADDED lines a similarity must
cover to be reported.  Absolute line counts alone invite noise:
a 12-line import-and-boilerplate overlap in a 574-line test file
is 2% and meaningless, while 7 of 9 added lines in a small file
is a real duplication.  0 disables the ratio check."
  :group 'code-review-analysis
  :type 'number)

(defcustom code-review-analysis-boilerplate-line-regexp
  "\\`[ \t]*\\(?:import\\|from[ \t]\\|package\\|using[ \t]\\|require[ \t]\\|#include\\)"
  "Regexp matching STRUCTURAL boilerplate lines (imports, package
declarations) that carry no duplication signal: they look alike in
every file of a codebase, so they are dropped from the similarity
index, the added-line shingles AND the grep probes.  Set to nil
to disable the filter."
  :group 'code-review-analysis
  :type '(choice regexp (const nil)))

(defcustom code-review-analysis-dead-test-name-regexp
  "\\(?:Test\\|Tests\\|Spec\\|Suite\\|Case\\|IT\\)\\'\\|\\`test[_[:upper:]]"
  "Regexp matching definition names that are TEST ENTRY POINTS
(*Test classes, *Suite, pytest test_* functions...).  Those are
invoked by BUILD-TOOL CONVENTION, not by explicit references:
zero references in the worktree is normal for them, so the
dead-code check skips them.  Set to nil to disable."
  :group 'code-review-analysis
  :type '(choice regexp (const nil)))

(defcustom code-review-analysis-probe-min-length 10
  "Minimum length of a probe line to seed candidate discovery.
Short lines (closing parens, `return y') appear everywhere and
would flood the `git grep' candidate search."
  :group 'code-review-analysis
  :type 'integer)

(defcustom code-review-analysis-max-probes 250
  "Maximum number of probe lines given to the candidate `git grep'."
  :group 'code-review-analysis
  :type 'integer)

(defcustom code-review-analysis-max-candidate-files 100
  "Maximum number of candidate repository files indexed."
  :group 'code-review-analysis
  :type 'integer)

(defcustom code-review-analysis-max-index-bytes 3145728
  "Hard ceiling on the total bytes of base-commit file contents
normalized and indexed in elisp.  The analysis runs inside the
render: this cap bounds its worst-case duration to a couple of
seconds even on huge repositories with huge files.  Candidates
over it are dropped, biggest files last."
  :group 'code-review-analysis
  :type 'integer)

(defcustom code-review-analysis-line-max-length 2000
  "Maximum line length the analysis regexps will ever see.
Lines longer than this are truncated before normalization, and
candidate files whose longest line exceeds it are not indexed at
all.  Data files masquerading as text (Jupyter notebook JSON,
minified bundles) pack megabytes into single lines; the
normalization regexps recurse per character and the regexp
matcher dies with \"Stack overflow in regexp matcher\" on them,
which killed whole review renders with a misleading
\"error from your VC provider\".  0 disables the cap."
  :group 'code-review-analysis
  :type 'integer)

(defcustom code-review-analysis-ref-exclude-globs
  '("*-autoloads.el" "*-pkg.el" "*.elc")
  "Glob patterns excluded from the reference search (git pathspec)."
  :group 'code-review-analysis
  :type '(repeat string))

(defcustom code-review-analysis-def-regexps
  '(("\\.el\\'" .
     ("^[ \t]*(\\(?:cl-\\)?def\\(?:un\\|subst\\|macro\\|var\\|const\\|custom\\|class\\|generic\\|method\\|face\\)\\s-+\\(?1:[^ ()\t\n]+\\)"
      "^[ \t]*(define-\\(?:derived-mode\\|minor-mode\\|generic\\|error\\|condition\\)\\s-+\\(?1:[^ ()\t\n]+\\)"))
    ("\\.py\\'" .
     ("^[ \t]*\\(?:async[ \t]+\\)?def[ \t]+\\(?1:[A-Za-z_][A-Za-z0-9_]*\\)"
      "^[ \t]*class[ \t]+\\(?1:[A-Za-z_][A-Za-z0-9_]*\\)"))
    ("\\.\\(?:scala\\|sc\\|sbt\\)\\'" .
     ("^[ \t]*\\(?:override[ \t]+\\|private\\(?:\\[[^]\n]+\\]\\)?[ \t]+\\|protected\\(?:\\[[^]\n]+\\]\\)?[ \t]+\\|final[ \t]+\\|implicit[ \t]+\\|abstract[ \t]+\\|lazy[ \t]+\\|inline[ \t]+\\)*def[ \t]+\\(?1:[A-Za-z_][A-Za-z0-9_]*\\)"
      "^[ \t]*\\(?:abstract[ \t]+\\|sealed[ \t]+\\|final[ \t]+\\|private\\(?:\\[[^]\n]+\\]\\)?[ \t]+\\|protected\\(?:\\[[^]\n]+\\]\\)?[ \t]+\\)*\\(?:case[ \t]+\\)?class[ \t]+\\(?1:[A-Za-z_][A-Za-z0-9_]*\\)"
      "^[ \t]*\\(?:case[ \t]+\\|final[ \t]+\\|private[ \t]+\\|protected[ \t]+\\)*object[ \t]+\\(?1:[A-Za-z_][A-Za-z0-9_]*\\)"
      "^[ \t]*\\(?:sealed[ \t]+\\|private[ \t]+\\|protected[ \t]+\\)*trait[ \t]+\\(?1:[A-Za-z_][A-Za-z0-9_]*\\)"
      "^[ \t]*\\(?:opaque[ \t]+\\)?type[ \t]+\\(?1:[A-Za-z_][A-Za-z0-9_]*\\)"))
    ("\\.\\(?:clj\\|cljs\\|cljc\\)\\'" .
     ("^[ \t]*(\\(?:def\\|declare\\|defn-?\\|defmacro\\|defonce\\|defmulti\\|defprotocol\\|defrecord\\|deftype\\|defstruct\\|definline\\)[ \t]+\\(?:\\^\\(?::[^ \t(){}\n]+\\|{[^}\n]*}\\)[ \t]+\\)*\\(?1:[^ ()\t\n]+\\)"))

    ("\\.\\(?:go\\|js\\|ts\\|rs\\|c\\|h\\|cc\\|cpp\\)\\'" .
     ("[ \t]func[ \t]+\\(?1:[A-Za-z_][A-Za-z0-9_]*\\)"
      "[ \t]fn[ \t]+\\(?1:[A-Za-z_][A-Za-z0-9_]*\\)"
      "[ \t]function[ \t]+\\(?1:[A-Za-z_][A-Za-z0-9_]*\\)"
      "^[ \t]*\\(?:#[a-z \t]*\\)?def[ \t]+\\(?1:[A-Za-z_][A-Za-z0-9_]*\\)")))
  "Definition regexps per file extension.
Each entry maps an extension regexp to a list of regexps whose
match group 1 captures the defined symbol name.  Used by the
dead-code and dangling-reference heuristics."
  :group 'code-review-analysis
  :type '(alist :key-type regexp :value-type (repeat regexp)))

;;; Phase 15: hunk delicacy score

(defcustom code-review-analysis-branch-keyword-regexp
  (concat "\\<\\(?:if\\|else\\|elif\\|elsif\\|for\\|foreach\\|"
          "while\\|switch\\|case\\|catch\\|except\\|finally\\|"
          "when\\|match\\|loop\\|unless\\|guard\\)\\>"
          "\\|&&\\|||")
  "Regexp of branch keywords/operators for the hunk complexity
delta (one match = one branch).  A deliberate language-agnostic
approximation of the phase 10 treesitter branch nodes: diff text
has no treesit buffer, and keyword counting carries the same
signal for the complexity-delta purpose (added vs removed
branches).  Ternary `?` is excluded: elisp character literals
and string contents make it noise.  Set to nil to disable the
complexity ingredient."
  :group 'code-review-analysis
  :type '(choice regexp (const nil)))

(defcustom code-review-analysis-delicacy-threshold 0.5
  "Score at which a hunk counts as DELICATE (badge on the hunk
heading, entry in the Delicate hunks jump list, target of
`code-review-next-delicate-hunk')."
  :group 'code-review-analysis
  :type 'number)

(defcustom code-review-analysis-delicacy-report-min 0.3
  "Score floor for STORING a hunk entry at all.  Kept below
`code-review-analysis-delicacy-threshold' so lowering the
threshold does not invalidate the cache."
  :group 'code-review-analysis
  :type 'number)

(defcustom code-review-analysis-delicacy-top-k 8
  "Hunks shown in the Delicate hunks jump list (and cycled by
`code-review-next-delicate-hunk'), hottest first."
  :group 'code-review-analysis
  :type 'integer)

(defcustom code-review-analysis-delicacy-blast-saturation 40
  "Caller count at which the blast-radius ingredient saturates
(the phase's example signal: 40 callers)."
  :group 'code-review-analysis
  :type 'integer)

(defcustom code-review-analysis-delicacy-age-saturation 1095
  "Old-line age in DAYS at which the age ingredient saturates
(three years: stable code edited is the classic delicacy)."
  :group 'code-review-analysis
  :type 'integer)

(defcustom code-review-analysis-max-blame-files 10
  "Files given a `git blame' pass for hunk delicacy.  One
subprocess per changed file WITH an old side; files over the
budget are skipped (logged), first in diff order."
  :group 'code-review-analysis
  :type 'integer)

(defcustom code-review-analysis-max-blame-lines 400
  "Total old-side lines blamed per file (`-L' ranges): bounds the
blame output bytes and the touched history."
  :group 'code-review-analysis
  :type 'integer)

(defcustom code-review-analysis-max-hunk-entries 200
  "Hunk delicacy entries stored per (PR, diff)."
  :group 'code-review-analysis
  :type 'integer)

(defcustom code-review-analysis-blame-partial-clones nil
  "When non-nil, run the hunk blame even on partial clones.
On a promisor/partial clone (blob:none) `git blame' lazy-fetches
the blob of EVERY historical version of the blamed lines: an
old, frequently-modified file blocked the render for minutes
(litellm PR 43310: blaming 60 lines of proxy_server.py at the
base branch took over 90 seconds of network fetches).  Default
nil: on partial clones the age/ownership ingredients are simply
absent (the rest of the score works)."
  :group 'code-review-analysis
  :type 'boolean)

;;; Normalization (pure)

(defun code-review-analysis--cap-line (line)
  "Return LINE truncated to `code-review-analysis-line-max-length'.
EVERY analysis regexp that consumes a raw line must go through
this: data files masquerading as text (Jupyter notebook JSON,
minified bundles) pack megabytes into single lines, and the
matcher recurses per character on them until the stack overflows.
0 disables the cap."
  (if (and (> code-review-analysis-line-max-length 0)
           (> (length line) code-review-analysis-line-max-length))
      (substring line 0 code-review-analysis-line-max-length)
    line))

(defun code-review-analysis--normalize-line (line)
  "Normalize LINE for similarity comparison.
Masks string literals, numbers and comments; collapses whitespace.
This is heuristic: it trades precision for language-agnosticism.
Input is capped first (see `code-review-analysis--cap-line')."
  (let ((s (substring-no-properties
            (code-review-analysis--cap-line line))))
    ;; mask string literals first: quoted comment markers etc hide
    (setq s (replace-regexp-in-string
             "\"\\(?:[^\"\\]\\|\\\\.\\)*\"" "\"\"" s))
    ;; comments (rough, language-agnostic): the rules are
    ;; deliberately conservative.  `//' is unambiguous for the
    ;; c-family; `#' and `;' only strip when clearly comments (see
    ;; test file) so that c-family statement separators and elisp
    ;; function quotes survive.
    (setq s (replace-regexp-in-string "//.*\\'" "" s))
    (setq s (replace-regexp-in-string "[ \t]#[^'(\n].*\\'" "" s))
    (setq s (replace-regexp-in-string "[ \t];.*\\'" "" s))
    (setq s (replace-regexp-in-string "\\`[ \t]*;+.*\\'" "" s))
    (setq s (replace-regexp-in-string
             "\\_<[0-9][0-9_.a-fA-FxX]*\\_>" "N" s))
    (string-trim (replace-regexp-in-string "[ \t]+" " " s))))

(defun code-review-analysis--boilerplate-p (line)
  "Non-nil when raw LINE is structural boilerplate (imports etc).
Such lines match everywhere in a codebase and carry no
duplication signal; `code-review-analysis-boilerplate-line-regexp'
is the tunable.  Input is capped first (see
`code-review-analysis--cap-line')."
  (and code-review-analysis-boilerplate-line-regexp
       (string-match-p
        code-review-analysis-boilerplate-line-regexp
        (code-review-analysis--cap-line line))))

;;; Diff line extraction (pure)

(defun code-review-analysis--parse-hunk-header (line)
  "Return (OLD-START . NEW-START) for hunk header LINE, else nil."
  (when (string-match
         "^@@ -\\([0-9]+\\)\\(?:,[0-9]+\\)? +\\+\\([0-9]+\\)\\(?:,[0-9]+\\)? @@" line)
    (cons (string-to-number (match-string 1 line))
          (string-to-number (match-string 2 line)))))

(defun code-review-analysis--block-lines (block)
  "Extract added/deleted lines from one diff BLOCK.
Return ((:added . ((NEW-LINE . TEXT) ...))
        (:deleted . ((OLD-LINE . TEXT) ...)))."
  (let ((added nil) (deleted nil)
        (old-ln nil) (new-ln nil))
    (dolist (line (split-string block "\n"))
      (let ((hdr (code-review-analysis--parse-hunk-header line)))
        (cond
         (hdr (setq old-ln (car hdr) new-ln (cdr hdr)))
         ((not old-ln) nil)
         ((string-prefix-p "+" line)
          (push (cons new-ln (substring line 1)) added)
          (cl-incf new-ln))
         ((string-prefix-p "-" line)
          (push (cons old-ln (substring line 1)) deleted)
          (cl-incf old-ln))
         (t (cl-incf old-ln) (cl-incf new-ln)))))
    (list :added (nreverse added) :deleted (nreverse deleted))))

;;; Shingles and matching (pure)

(defun code-review-analysis--shingles (items size)
  "Return ((START . SHINGLE) ...) for consecutive ITEMS.
ITEMS is a list of (LINE-NO . NORMALIZED-TEXT).  Only windows of
SIZE items with consecutive line numbers are shingled; windows
with two or more empty lines are skipped (blank stretches match
everywhere and would drown the report in false positives)."
  (let ((len (length items))
        (res nil)
        (i 0))
    (while (<= (+ i size) len)
      (let* ((window (cl-subseq items i (+ i size)))
             (ok (cl-loop for (a b) on window
                         while b
                         always (= (car b) (1+ (car a))))))
        (when (and ok
                   (<= (cl-loop for (_ . txt) in window
                                count (string-empty-p txt))
                       1))
          (push (cons (car (car window))
                      (mapconcat (lambda (x) (cdr x)) window "\n"))
                res))
        (cl-incf i)))
    (nreverse res)))

(defun code-review-analysis--index-contents (contents size)
  "Build the shingle index from CONTENTS, a list of (PATH . TEXT).
Return a hash table SHINGLE -> ((PATH . START-LINE) ...)."
  (let ((index (make-hash-table :test #'equal :size 4096)))
    (pcase-dolist (`(,path . ,text) contents)
      (let* ((lines (mapcar #'code-review-analysis--normalize-line
                            (split-string text "\n")))
             (items (cl-loop for txt in lines
                            for ln from 1
                            ;; structural lines (imports) match
                            ;; everywhere: no signal, pure noise.
                            ;; (empty lines stay: `--shingles' has
                            ;; its own blank-stretch rule, and a
                            ;; single blank inside a copied block
                            ;; is a legitimate match)
                            unless (code-review-analysis--boilerplate-p txt)
                            collect (cons ln txt))))
        (pcase-dolist (`(,start . ,shingle)
                       (code-review-analysis--shingles items size))
          (puthash shingle (cons (cons path start)
                                 (gethash shingle index))
                   index))))
    index))

(defun code-review-analysis--find-similar (added-items index size min-covered)
  "Match ADDED-ITEMS against INDEX.
Return a list of (REPO-PATH COVERED REPO-LO REPO-HI), sorted by
COVERED descending: COVERED added lines also appear (normalized)
in REPO-PATH around REPO-LO..REPO-HI."
  (let ((cover (make-hash-table :test #'equal)))
    (pcase-dolist (`(,start . ,shingle)
                   (code-review-analysis--shingles added-items size))
      (pcase-dolist (`(,path . ,repo-start) (gethash shingle index))
        (let ((entry (or (gethash path cover)
                         (puthash path (list (cons 'lines (make-hash-table))
                                             (cons 'lo repo-start)
                                             (cons 'hi repo-start))
                                  cover))))
          (cl-loop for ln from start to (+ start size -1)
                   do (puthash ln t (cdr (assq 'lines entry))))
          (when (< repo-start (cdr (assq 'lo entry)))
            (setcdr (assq 'lo entry) repo-start))
          (when (> repo-start (cdr (assq 'hi entry)))
            (setcdr (assq 'hi entry) repo-start)))))
    (let ((res nil))
      (maphash
       (lambda (path entry)
         (let ((covered 0))
           (maphash (lambda (_ _) (cl-incf covered))
                    (cdr (assq 'lines entry)))
           (when (>= covered min-covered)
             (push (list path covered
                        (cdr (assq 'lo entry))
                        (cdr (assq 'hi entry)))
                   res))))
       cover)
      (cl-sort res #'> :key #'cadr))))

;;; Definitions (pure)

(defun code-review-analysis--def-regexps-for (path)
  "Return the definition regexps for file PATH."
  (cl-loop for (ext . regexps) in code-review-analysis-def-regexps
           when (string-match-p ext path)
           return regexps))

(defun code-review-analysis--definitions-in (path items)
  "Extract definitions from ITEMS ((LINE . TEXT)...) of file PATH.
Return ((NAME . LINE) ...)."
  (let ((regexps (code-review-analysis--def-regexps-for path))
        (res nil))
    (pcase-dolist (`(,ln . ,text) items)
      (let ((capped (code-review-analysis--cap-line text)))
        (cl-loop for re in regexps
                 thereis (when (string-match re capped)
                           (let ((name (match-string-no-properties 1 capped)))
                             (when (and name (not (string-empty-p name)))
                               (push (cons name ln) res)))))))
    (nreverse res)))

;;; Reference search (git)

(defun code-review-analysis--git-grep (worktree name)
  "Search NAME in WORKTREE's tracked files.
Return a list of (PATH LINE TEXT), or nil when there is no match."
  (let* ((default-directory worktree)
         (out (magit-git-output
               "grep" "-n" "-I" "-w" "-F" "-e" name "--" "."
               (mapcar (lambda (g) (format ":(exclude)%s" g))
                       code-review-analysis-ref-exclude-globs))))
    (when (and out (not (string-empty-p out)))
      (cl-loop for line in (split-string out "\n" t)
               for capped = (code-review-analysis--cap-line line)
               when (string-match "^\\(.+?\\):\\([0-9]+\\):\\(.*\\)$" capped)
               collect (list (match-string 1 capped)
                             (string-to-number (match-string 2 capped))
                             (match-string 3 capped))))))

(defun code-review-analysis--hit-is-definition-p (path name text)
  "Non-nil when TEXT at PATH is a definition of NAME (not a reference).
TEXT is capped first: git grep hits can be megabyte-long
notebook/bundle lines."
  (let ((capped (code-review-analysis--cap-line text)))
    (cl-loop for re in (code-review-analysis--def-regexps-for path)
             thereis (and (string-match re capped)
                          (equal (match-string-no-properties 1 capped)
                                 name)))))

(defun code-review-analysis--references (worktree name)
  "Return NAME's non-definition occurrences in WORKTREE."
  (cl-remove-if (lambda (hit)
                  (code-review-analysis--hit-is-definition-p
                   (nth 0 hit) name (nth 2 hit)))
                (code-review-analysis--git-grep worktree name)))

;;; Base commit access (git)

(defun code-review-analysis--definitions-refs (worktree names)
  "Return a hash NAME -> its non-definition occurrences in WORKTREE.
ONE `git grep' for all NAMES (word-fixed, generated files
excluded) instead of one subprocess per symbol: on big
repositories a grep per symbol, multiplied by dozens of added
definitions, froze the render for minutes."
  (let ((h (make-hash-table :test #'equal))
        (nf (make-temp-file "code-review-analysis-names")))
    (when names
      (unwind-protect
          (progn
            (with-temp-file nf
              (dolist (n names)
                (insert n)
                (insert ?\n)))
            (let ((outbuf (generate-new-buffer " *code-review-analysis*")))
              (unwind-protect
                  (progn
                    (with-current-buffer outbuf
                      (setq default-directory worktree)
                      (apply #'call-process "git" nil
                             (list (current-buffer) nil) nil
                             "grep" "-nw" "-I" "-F" "-f" nf "--" "."
                             (mapcar (lambda (g) (format ":(exclude)%s" g))
                                     code-review-analysis-ref-exclude-globs)))
                    (with-current-buffer outbuf
                      (dolist (line (split-string
                                     (buffer-substring-no-properties
                                      (point-min) (point-max))
                                     "\n" t))
                        (let ((capped (code-review-analysis--cap-line line)))
                        (when (string-match
                               "^\\(.+?\\):\\([0-9]+\\):\\(.*\\)$" capped)
                          ;; bind BEFORE any call that could run its
                          ;; own string-match: the match data is
                          ;; GLOBAL and gets clobbered
                          (let ((path (match-string 1 capped))
                                (line-no
                                 (string-to-number (match-string 2 capped)))
                                (text (match-string 3 capped)))
                            (dolist (n names)
                              ;; presence check on the FULL raw line
                              ;; (string-search is not a regexp: no
                              ;; stack overflow on megabyte lines).
                              ;; A reference buried deep inside a
                              ;; notebook line is still a reference;
                              ;; capping here caused false "dead"
                              ;; findings.
                              (when (and (string-search n line)
                                         (not (code-review-analysis--hit-is-definition-p
                                               path n text)))
                                (puthash n
                                         (cons (list path line-no text)
                                               (gethash n h))
                                         h)))))))))
                (kill-buffer outbuf))))
        (delete-file nf)))
    h))

(defun code-review-analysis--similar-candidates (worktree added exclude-paths)
  "Discover repository files likely to contain ADDED lines.
One `git grep -F -f PROBEFILE -- . :(exclude)...' over the
WORKTREE finds files containing any sufficiently long added
line verbatim.  EXCLUDE-PATHS (the diff's own files) prevent
self-matches.  The search must run on the worktree, not on the
base tree: `git grep <rev>' inflates every packed blob and costs
minutes on big repositories, while one worktree pass is seconds.
With `-l' git stops scanning each file at the first match, which
is what makes this fast; hit counts are not needed downstream.
Return ((PATH . 1) ...)."
  (let* ((probes (delete-dups
                  (cl-loop for (_ln . text) in added
                           when (>= (length text)
                                    code-review-analysis-probe-min-length)
                           ;; imports/package lines are in EVERY file:
                           ;; as probes they just flood the candidate
                           ;; list with the whole repository
                           unless (code-review-analysis--boilerplate-p text)
                           collect text)))
         (probes (cl-subseq probes 0
                            (min (length probes)
                                 code-review-analysis-max-probes)))
         (default-directory worktree)
         (res nil))
    (when probes
      (let ((pf (make-temp-file "code-review-analysis-probes")))
        (unwind-protect
            (progn
              (with-temp-file pf
                (dolist (p probes)
                  (insert p)
                  (insert ?\n)))
              (let ((grepbuf (generate-new-buffer " *code-review-analysis*"))
                    (out nil))
                (unwind-protect
                    (with-current-buffer grepbuf
                      (setq default-directory worktree)
                      (apply
                       #'call-process "git" nil
                       (list (current-buffer) nil) nil
                       "grep" "-l" "-I" "-F" "-f" pf "--" "."
                       (append
                        (mapcar (lambda (p) (format ":(exclude)%s" p))
                                exclude-paths)
                        (mapcar (lambda (g)
                                  (format ":(exclude)%s" g))
                                code-review-analysis-ref-exclude-globs)))
                      (setq out (buffer-substring-no-properties
                                 (point-min) (point-max))))
                  (kill-buffer grepbuf))
                (when (and out (not (string-empty-p out)))
                  (setq res
                        (cl-loop for line in (split-string out "\n" t)
                                 collect (cons line 1))))))
          (delete-file pf))))
    (cl-subseq res 0
               (min (length res)
                    code-review-analysis-max-candidate-files))))



(defun code-review-analysis--max-line-length (text)
  "Length of the longest line in TEXT, cheap and bounded.
Stops scanning as soon as a line exceeds
`code-review-analysis-line-max-length' (the only threshold
callers use), so a megabyte-long line costs one scan up to it.
With the cap disabled (0) the whole TEXT is scanned."
  (let ((cap (if (> code-review-analysis-line-max-length 0)
                 code-review-analysis-line-max-length
               most-positive-fixnum))
        (pos 0)
        (max 0)
        nl)
    (while (and pos (< max cap))
      (setq nl (string-search "\n" text pos))
      (setq max (max max (- (or nl (length text)) pos)))
      (setq pos (and nl (1+ nl))))
    max))

(defun code-review-analysis--read-files (worktree paths)
  "Return ((PATH . CONTENT) ...) for WORKTREE PATHS.
Applies the hard byte cap while selecting: biggest files last,
dropped over budget.  Plain `insert-file-contents', no git
subprocess, so this is cheap and bounded.  Files with any line
longer than `code-review-analysis-line-max-length' (notebook
JSON, minified bundles: data, not reviewable code) are skipped
entirely: their lines are noise for the shingle matcher and
indexing them would burn the byte budget real code needs."
  (let ((budget code-review-analysis-max-index-bytes)
        (res nil))
    (dolist (path paths)
      (let* ((full (expand-file-name path worktree))
             (size (or (ignore-errors
                         (file-attribute-size
                          (file-attributes full)))
                       0)))
        (when (and (<= size (max 0 (/ budget 2)))
                   (<= size budget))
          (let ((text (with-temp-buffer
                        (insert-file-contents full)
                        (buffer-string))))
            (unless (and (> code-review-analysis-line-max-length 0)
                         (> (code-review-analysis--max-line-length text)
                            code-review-analysis-line-max-length))
              (setq budget (- budget size))
              (push (cons path text) res))))))
    (nreverse res)))

(defvar code-review-analysis--cache (make-hash-table :test #'equal)
  "Analysis results keyed by (pullreq-id, diff md5).")

;;; Phase 15: hunk delicacy (pure parts)

(defun code-review-analysis--split-hunks (block)
  "Split one diff file BLOCK into its hunks.
Return a list of plists (:ranges RANGES :old OL :added AD :deleted DL):
RANGES is the raw ranges text between the @@ markers — the hunk
KEY, byte-identical to what the wash reads, and stable across
renders (phase 13 read-tracking reuses it); OL is ((OLD-LINE .
TEXT)...) of the CONTEXT and DELETED lines (the `git blame'
side); AD/DL are ((LINE . TEXT)...) like
`code-review-analysis--block-lines'.  Pure."
  (let ((hunks nil)
        (ranges nil) (old nil) (added nil) (deleted nil)
        (old-ln nil) (new-ln nil))
    (dolist (line (split-string block "\n"))
      (let ((hdr (code-review-analysis--parse-hunk-header line)))
        (cond
         (hdr
          (when ranges
            (push (list :ranges ranges
                        :old (nreverse old)
                        :added (nreverse added)
                        :deleted (nreverse deleted))
                  hunks))
          (let ((capped (code-review-analysis--cap-line line)))
            (setq old nil added nil deleted nil
                  old-ln (car hdr) new-ln (cdr hdr)
                  ranges (and (string-match "^@@ \\(.+?\\) @@" capped)
                              (match-string-no-properties 1 capped)))))
         ;; before the first hunk header (file headers): not code
         ((not old-ln) nil)
         ((string-prefix-p "\\" line) nil) ; "\ No newline"
         ((string-empty-p line) nil) ; trailing newline artifact
         ((string-prefix-p "+" line)
          (push (cons new-ln (substring line 1)) added)
          (cl-incf new-ln))
         ((string-prefix-p "-" line)
          (push (cons old-ln (substring line 1)) deleted)
          (push (cons old-ln (substring line 1)) old)
          (cl-incf old-ln))
         (t
          (push (cons old-ln (substring line 1)) old)
          (cl-incf old-ln)
          (cl-incf new-ln)))))
    (when ranges
      (push (list :ranges ranges
                  :old (nreverse old)
                  :added (nreverse added)
                  :deleted (nreverse deleted))
            hunks))
    (nreverse hunks)))

(defun code-review-analysis--old-rev (base-ref-name)
  "Rev holding the reviewed diff's OLD side, for `git blame'.
Forge PRs carry the base BRANCH in `base-ref-name' (the diff's
old side is at the merge base — close enough for a heuristic age
signal).  LOCAL reviews carry the git diff args: \"A..B\" /
\"A^..B\" (old side A / A^), \"REV^..REV\" (old side REV^),
\"HEAD\" (old side HEAD).  A ROOT commit review (\"REV^!\") has
no old side: nil.  Staged reviews (\"--cached\") diff against
HEAD.  nil when nothing can be derived.  Pure."
  (cond
   ((not (stringp base-ref-name)) nil)
   ((string= base-ref-name "--cached") "HEAD")
   ((string-match "\\`\\(.+\\)\\.\\.\\(.+\\)\\'" base-ref-name)
    (match-string-no-properties 1 base-ref-name))
   ((string-match "\\`\\(.+\\)\\^!\\'" base-ref-name) nil)
   (t base-ref-name)))

(defun code-review-analysis--slice-blame-ranges (ranges)
  "Keep RANGES ((START . END)...) under the blame line budget.
`code-review-analysis-max-blame-lines' bounds the blame output
and the history a partial clone would lazy-fetch."
  (let ((res nil)
        (budget code-review-analysis-max-blame-lines))
    (dolist (r (cl-sort (copy-sequence ranges) #'< :key #'car))
      (let ((n (1+ (- (cdr r) (car r)))))
        (when (<= n budget)
          (push r res)
          (cl-decf budget n))))
    (nreverse res)))

(defun code-review-analysis--parse-blame (out table)
  "Parse `git blame --porcelain' output OUT into TABLE.
TABLE: OLD-LINE -> (AUTHOR . AUTHOR-TIME).  With -L ranges the
output carries only the blamed lines, so TABLE covers exactly
the requested ranges.  Pure."
  (let ((author nil) (author-time nil) (orig-line nil))
    (dolist (line (split-string out "\n"))
      (let ((capped (code-review-analysis--cap-line line)))
        (cond
         ((string-match
           "\\`[0-9a-f]\\{40\\} \\([0-9]+\\) [0-9]+\\(?: [0-9]+\\)?"
           capped)
          (setq orig-line (string-to-number (match-string 1 capped))
                author nil author-time nil))
         ((string-match "\\`author \\(.+\\)" capped)
          (setq author (match-string-no-properties 1 capped)))
         ((string-match "\\`author-time \\([0-9]+\\)" capped)
          (setq author-time (string-to-number (match-string 1 capped))))
         (t
          ;; the content line ("\t...") closes the entry
          (when (and orig-line author author-time)
            (puthash orig-line (cons author author-time) table)
            (setq orig-line nil))))))
    table))

(defun code-review-analysis--blame (worktree rev path ranges)
  "One bounded `git blame --porcelain' for PATH at REV, RANGES.
RANGES is ((OLD-START . OLD-END)...): all ranges ride ONE
subprocess as -L args.  Return a hash OLD-LINE -> (AUTHOR .
AUTHOR-TIME), nil when blame is impossible (no REV, no ranges,
git error: rev missing in a partial clone, ...).  This runs
inside the cached analysis compute — once per (PR, diff) — never
silently per render."
  (when (and rev ranges)
    (let ((buf (generate-new-buffer " *code-review-analysis-blame*"))
          (h (make-hash-table :test #'eql)))
      (unwind-protect
          (with-current-buffer buf
            (setq default-directory worktree)
            (apply #'call-process "git" nil (list (current-buffer) nil) nil
                   "blame" "--porcelain"
                   (append
                    (apply #'append
                           (mapcar (lambda (r)
                                     (list "-L" (format "%d,%d"
                                                        (car r) (cdr r))))
                                   ranges))
                    (list rev "--" path)))
            (code-review-analysis--parse-blame
             (buffer-substring-no-properties (point-min) (point-max))
             h))
        (kill-buffer buf))
      h)))

(defun code-review-analysis--branch-count (text)
  "Branch keyword/operator occurrences in TEXT (capped first)."
  (let ((n 0) (start 0) m)
    (when code-review-analysis-branch-keyword-regexp
      (setq m (code-review-analysis--cap-line text))
      (while (setq start (string-match code-review-analysis-branch-keyword-regexp
                                       m start))
        (setq n (1+ n)
              start (match-end 0))))
    n))

(defun code-review-analysis--age-string (days)
  "Compact age for DAYS: \"3y\" or \"14m\"."
  (if (>= days 365)
      (format "%.0fy" (/ days 365.0))
    (format "%.0fm" (max 1 (/ days 30.0)))))

(defun code-review-analysis--hunk-entry (path hunk refs-hash blame)
  "Delicacy entry plist for HUNK of PATH.  Pure.
REFS-HASH: definition name -> occurrences (phase 5 batched
grep) for blast radius and dead-on-arrival.  BLAME: hash
OLD-LINE -> (AUTHOR . AUTHOR-TIME), nil when blame was skipped:
age and ownership ingredients are simply absent then.  Entry:
(:path :ranges :score :callers :dead :median-age :authors
:cplx)."
  (let* ((added (plist-get hunk :added))
         (deleted (plist-get hunk :deleted))
         (old-lines (plist-get hunk :old))
         (now (float-time))
         (defs (delete-dups
                (append (mapcar #'car
                                (code-review-analysis--definitions-in
                                 path added))
                        (mapcar #'car
                                (code-review-analysis--definitions-in
                                 path deleted)))))
         (callers (cl-loop for d in defs
                           for n = (length (gethash d refs-hash))
                           maximize n))
         (dead (cl-loop for d in defs
                        unless (or (gethash d refs-hash)
                                   (and code-review-analysis-dead-test-name-regexp
                                        (string-match-p
                                         code-review-analysis-dead-test-name-regexp
                                         d)))
                        collect d))
         ;; blame author-time is EPOCH SECONDS: age is in DAYS here
         (ages (when blame
                 (cl-loop for (ln . _) in old-lines
                          for hit = (gethash ln blame)
                          when hit collect (max 0.0
                                                (/ (- now (cdr hit))
                                                   86400.0)))))
         (median-age (when ages
                       (nth (floor (/ (length ages) 2))
                            (sort (copy-sequence ages) #'<))))
         (authors (when blame
                    (delete-dups
                     (cl-loop for (ln . _) in old-lines
                              for hit = (gethash ln blame)
                              when (car hit) collect (car hit)))))
         (cplx (- (cl-loop for (_ . txt) in added
                           sum (code-review-analysis--branch-count txt))
                  (cl-loop for (_ . txt) in deleted
                           sum (code-review-analysis--branch-count txt))))
         (authors-n (length authors))
         (score (+ (if callers
                       (min 1.0 (/ (float callers)
                                   (max 1 code-review-analysis-delicacy-blast-saturation)))
                     0.0)
                   (if median-age
                       (min 1.0 (/ (float median-age)
                                   (max 1 code-review-analysis-delicacy-age-saturation)))
                     0.0)
                   (if (>= authors-n 3) 0.25 0.0)
                   (min 0.5 (/ (float (max 0 cplx)) 20.0))
                   (if dead (min 1.0 (* 0.5 (length dead))) 0.0))))
    (list :path path
          :ranges (plist-get hunk :ranges)
          :score score
          :callers callers
          :dead dead
          :median-age median-age
          :authors authors-n
          :cplx cplx)))

(defun code-review-analysis--hunk-reasons (entry)
  "Compact risk reasons for ENTRY, nil when nothing stands out."
  (let* ((callers (or (plist-get entry :callers) 0))
         (age (plist-get entry :median-age))
         (authors (plist-get entry :authors))
         (cplx (plist-get entry :cplx))
         (dead (plist-get entry :dead)))
    (delq nil
          (list (when (>= callers 5)
                  (format "%d callers" callers))
                (when (and age (>= age 365))
                  (format "lines %s old"
                          (code-review-analysis--age-string age)))
                (when (>= authors 3)
                  (format "%d authors" authors))
                (when (>= cplx 3)
                  (format "+%d branches" cplx))
                (when dead
                  (format "%d dead def%s" (length dead)
                          (if (cdr dead) "s" "")))))))

(defun code-review-analysis--hunk-badge (entry)
  "Badge string for ENTRY (nil below the delicacy threshold)."
  (when (>= (plist-get entry :score)
            code-review-analysis-delicacy-threshold)
    (let ((reasons (code-review-analysis--hunk-reasons entry)))
      (when reasons
        (concat "  (risk: " (string-join reasons "; ") ")")))))

(defun code-review-analysis--partial-clone-p (worktree)
  "Non-nil when WORKTREE's repository is a partial clone.
A configured promisor remote means blobs are fetched lazily on
demand: any `git blame' on an old file walks history and fetches
one blob per version (network, minutes — see
`code-review-analysis-blame-partial-clones').  One bounded
subprocess; nil on any failure."
  (with-temp-buffer
    (apply #'call-process "git" nil (list (current-buffer) nil) nil
           "-C" (expand-file-name worktree)
           (list "config" "--get-regexp" "^remote\\..*\\.promisor$"))
    (> (buffer-size) 0)))

(defun code-review-analysis--hunks (worktree blocks refs-hash pr)
  "Delicacy entries for all hunks of BLOCKS, hottest first.
One `git blame' per changed file with an old side (capped by
`code-review-analysis-max-blame-files'), the shared phase 5
refs hash for blast radius and dead-on-arrival.  Entries below
`code-review-analysis-delicacy-report-min' are not stored, and
at most `code-review-analysis-max-hunk-entries' are."
  (let* ((old-rev (code-review-analysis--old-rev (oref pr base-ref-name)))
         ;; partial clone: blame lazy-fetches a blob per historical
         ;; version and can block the render for minutes — skip it
         ;; (one bounded `git config' call, only when a blame could
         ;; run at all)
         (blame-rev (and old-rev
                         (or code-review-analysis-blame-partial-clones
                             (not (code-review-analysis--partial-clone-p
                                   worktree)))
                         old-rev))
         (blame-left code-review-analysis-max-blame-files)
         (entries nil))
    (when (and old-rev (not blame-rev))
      (code-review-utils--log
       "code-review-analysis"
       "partial clone: skipping hunk blame age (blob lazy-fetch \
would block the render; see code-review-analysis-blame-partial-clones)"))
    (pcase-dolist (`(,path . ,block) blocks)
      (unless (or (string-match-p "^Binary files" block)
                  (string-match-p "^GIT binary patch" block))
        (let* ((hunks (code-review-analysis--split-hunks block))
               (old-hunks (cl-remove-if-not
                           (lambda (hu) (plist-get hu :old)) hunks))
               (blame
                (cond
                 ((not (and blame-rev old-hunks)) nil)
                 ((<= blame-left 0)
                  (code-review-utils--log
                   "code-review-analysis"
                   (format "blame budget spent: skipping hunk age for %s"
                           path))
                  nil)
                 (t
                  (cl-decf blame-left)
                  (let* ((raw (mapcar
                               (lambda (hu)
                                 (let ((lines (mapcar #'car (plist-get hu :old))))
                                   (cons (apply #'min lines)
                                         (apply #'max lines))))
                               old-hunks))
                         (ranges (code-review-analysis--slice-blame-ranges raw)))
                    (when ranges
                      (code-review-analysis--blame
                       worktree blame-rev path ranges)))))))
          (dolist (hu hunks)
            (let ((entry (code-review-analysis--hunk-entry
                          path hu refs-hash blame)))
              (when (>= (plist-get entry :score)
                        code-review-analysis-delicacy-report-min)
                (push entry entries)))))))
    (cl-subseq (nreverse
                (cl-sort entries #'> :key (lambda (e) (plist-get e :score))))
               0 (min (length entries)
                      code-review-analysis-max-hunk-entries))))

(defun code-review-analysis--hunk-badge-for (path ranges)
  "Badge string for hunk RANGES of PATH in the current render.
nil when there is no entry over the threshold (analysis
disabled, no worktree, or nothing delicate).  Called from the
hunk wash: `code-review-analysis-run' is a cache hit there (the
Analysis section runs before the diff wash in
`code-review-sections-hook')."
  (let ((res (code-review-analysis-run)))
    (cl-loop for e in (and res (plist-get res :hunks))
             thereis (when (and (equal (plist-get e :path) path)
                                (equal (plist-get e :ranges) ranges))
                       (code-review-analysis--hunk-badge e)))))

(defun code-review-analysis--delicate-hunks ()
  "The top-K delicate hunk entries of the current render.
Score order (hottest first), the same list the Analysis section
shows; entries under `code-review-analysis-delicacy-threshold'
are filtered out and the list is capped at
`code-review-analysis-delicacy-top-k'."
  (let* ((res (code-review-analysis-run))
         (entries (when res
                    (cl-remove-if
                     (lambda (e)
                       (< (plist-get e :score)
                          code-review-analysis-delicacy-threshold))
                     (or (plist-get res :hunks) nil)))))
    (cl-subseq entries 0 (min (length entries)
                              code-review-analysis-delicacy-top-k))))

(defun code-review-analysis--analyze-one-file (index refs path block)
  "Analyze one diff file BLOCK at PATH against INDEX.
REFS is the batched references hash (all definitions at once),
see `code-review-analysis--definitions-refs'.
Return (SIMILAR DEAD DANGLING) findings for this file."
  (if (or (string-match-p "^Binary files" block)
          (string-match-p "^GIT binary patch" block))
      ;; base85 garbage: matching it would only produce noise
      (list nil nil nil)
    (let* ((lines (code-review-analysis--block-lines block))
         (added (plist-get lines :added))
         (deleted (plist-get lines :deleted))
         (size code-review-analysis-shingle-size)
         (added-items (cl-loop for x in added
                               ;; boilerplate (imports, package lines)
                               ;; matches everywhere: not a signal
                               unless (code-review-analysis--boilerplate-p
                                        (cdr x))
                               collect (cons (car x)
                                             (code-review-analysis--normalize-line
                                              (cdr x)))))
         (similar
          (when (and index (>= (length added-items) size))
            ;; self-matches (the file's own worktree copy, which of
            ;; course contains its own added lines) are not findings
            (cl-loop for (repo-path covered lo hi)
                     in (cl-remove-if
                         (lambda (hit) (string= (nth 0 hit) path))
                         (code-review-analysis--find-similar
                          added-items index size
                          code-review-analysis-min-covered))
                     for n from 1
                     while (<= n code-review-analysis-max-findings-per-file)
                     ;; an absolute line count alone is noise: a
                     ;; 12-line boilerplate overlap in a 574-line
                     ;; test file is 2% and meaningless.  Require a
                     ;; minimum FRACTION of the added lines too.
                     when (or (zerop code-review-analysis-min-covered-ratio)
                              (>= covered
                                  (* code-review-analysis-min-covered-ratio
                                     (length added))))
                     collect (list path (length added) repo-path covered lo hi))))
         (dead
          (cl-loop for (name . ln) in (code-review-analysis--definitions-in
                                       path added)
                   unless (gethash name refs)
                   ;; *Test classes, test_* functions... are entry
                   ;; points BY CONVENTION (sbt, pytest, scalatest):
                   ;; zero references is normal, not death
                   unless (and code-review-analysis-dead-test-name-regexp
                               (string-match-p
                                code-review-analysis-dead-test-name-regexp
                                name))
                   collect (list name path ln)))
         (dangling
          (cl-loop for (name . _ln) in (code-review-analysis--definitions-in
                                        path deleted)
                   for name-refs = (gethash name refs)
                   when name-refs
                   collect (list name path name-refs))))
      (list similar dead dangling))))

(defun code-review-analysis--compute (worktree diff pr)
  "Compute all findings for DIFF of PR against WORKTREE.
Return (:similar SIMS :dead DEAD :dangling DANGLINGS :hunks
HUNKS — phase 15 delicacy entries, hottest first), or nil."
  (let* ((blocks (code-review--diff--split-by-files diff))
         (added-all
          (cl-loop for (_path . block) in blocks
                   nconc (plist-get (code-review-analysis--block-lines
                                     block)
                                    :added)))
         (candidates
          ;; diff files are NOT excluded: a PR copying code between
          ;; two files both in the diff is the primary similar case.
          ;; Self-matches are filtered per-file in the analysis.
          (when added-all
            (code-review-analysis--similar-candidates
             worktree added-all nil)))
         (contents
          (when candidates
            (code-review-analysis--read-files
             worktree (mapcar #'car candidates))))
         (index (when contents
                  (code-review-analysis--index-contents
                   contents code-review-analysis-shingle-size)))
         (all-defs
          (cl-loop for (dpath . block) in blocks
                   nconc (append
                          (code-review-analysis--definitions-in
                           dpath (plist-get
                                  (code-review-analysis--block-lines block)
                                  :added))
                          (code-review-analysis--definitions-in
                           dpath (plist-get
                                  (code-review-analysis--block-lines block)
                                  :deleted)))
                   into defs
                   finally return (delete-dups
                                   (mapcar #'car defs))))
         (refs-hash (code-review-analysis--definitions-refs
                     worktree all-defs))
         (similar nil) (dead nil) (dangling nil))
    (pcase-dolist (`(,path . ,block) blocks)
      (let ((res (code-review-analysis--analyze-one-file
                  index refs-hash path block)))
        (setq similar (append similar (nth 0 res))
              dead (append dead (nth 1 res))
              dangling (append dangling (nth 2 res)))))
    ;; only interesting dangling references: keep the first few
    (setq dangling (when dangling
                     (cl-subseq dangling 0 (min (length dangling) 10))))
    ;; phase 15: hunk delicacy entries (blast radius, blame age and
    ;; ownership, complexity delta, dead-on-arrival), hottest first
    (let ((hunks (code-review-analysis--hunks
                  worktree blocks refs-hash pr)))
      (when (or similar dead dangling hunks)
        (list :similar similar :dead dead :dangling dangling
              :hunks hunks)))))

(defun code-review-analysis-run ()
  "Run the heuristic analysis for the review in the current buffer.
Return the findings plist (see `code-review-analysis--compute'),
nil when analysis is disabled, there is no local worktree, or
there is nothing to report.  Results are cached per (PR, diff).
A failing compute is LOGGED and reported as no findings: the
analysis is a heuristic overlay and must never take the review
buffer down with it (a regexp stack overflow on a megabyte-long
notebook line once surfaced as \"error from your VC provider\"
and left the user without a PR buffer)."
  (when code-review-analysis-enabled
    (let* ((worktree code-review-repo-worktree)
           (diff (and worktree (code-review-db--pullreq-raw-diff)))
           (pr (and diff (code-review-db-get-pullreq))))
      (when (and worktree diff pr)
        (let ((key (concat (oref pr id) "|" (md5 diff))))
          (or (gethash key code-review-analysis--cache)
              (let ((res (condition-case err
                             (code-review-analysis--compute worktree diff pr)
                           (error
                            (code-review-utils--log
                             "code-review-analysis"
                             (format "analysis failed, skipping section (%s): %S"
                                     key err))
                            nil))))
                (puthash key res code-review-analysis--cache)
                res)))))))

(defun code-review-analysis-reset ()
  "Forget all cached analysis results."
  (interactive)
  (clrhash code-review-analysis--cache))

(provide 'code-review-analysis)
;;; code-review-analysis.el ends here
