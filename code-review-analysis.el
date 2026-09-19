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

;;; Normalization (pure)

(defun code-review-analysis--normalize-line (line)
  "Normalize LINE for similarity comparison.
Masks string literals, numbers and comments; collapses whitespace.
This is heuristic: it trades precision for language-agnosticism."
  (let ((s (substring-no-properties line)))
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
      (cl-loop for re in regexps
               thereis (when (string-match re text)
                         (let ((name (match-string-no-properties 1 text)))
                           (when (and name (not (string-empty-p name)))
                             (push (cons name ln) res))))))
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
               when (string-match "^\\(.+?\\):\\([0-9]+\\):\\(.*\\)$" line)
               collect (list (match-string 1 line)
                             (string-to-number (match-string 2 line))
                             (match-string 3 line))))))

(defun code-review-analysis--hit-is-definition-p (path name text)
  "Non-nil when TEXT at PATH is a definition of NAME (not a reference)."
  (cl-loop for re in (code-review-analysis--def-regexps-for path)
            thereis (and (string-match re text)
                         (equal (match-string-no-properties 1 text) name))))

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
                        (when (string-match
                               "^\\(.+?\\):\\([0-9]+\\):\\(.*\\)$" line)
                          ;; bind BEFORE any call that could run its
                          ;; own string-match: the match data is
                          ;; GLOBAL and gets clobbered
                          (let ((path (match-string 1 line))
                                (line-no
                                 (string-to-number (match-string 2 line)))
                                (text (match-string 3 line)))
                            (dolist (n names)
                              (when (and (string-search n text)
                                         (not (code-review-analysis--hit-is-definition-p
                                               path n text)))
                                (puthash n
                                         (cons (list path line-no text)
                                               (gethash n h))
                                         h))))))))
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



(defun code-review-analysis--read-files (worktree paths)
  "Return ((PATH . CONTENT) ...) for WORKTREE PATHS.
Applies the hard byte cap while selecting: biggest files last,
dropped over budget.  Plain `insert-file-contents', no git
subprocess, so this is cheap and bounded."
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
          (setq budget (- budget size))
          (let ((text (with-temp-buffer
                        (insert-file-contents full)
                        (buffer-string))))
            (push (cons path text) res)))))
    (nreverse res)))

(defvar code-review-analysis--cache (make-hash-table :test #'equal)
  "Analysis results keyed by (pullreq-id, diff md5).")

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
         (added-items (mapcar (lambda (x)
                                (cons (car x)
                                      (code-review-analysis--normalize-line
                                       (cdr x))))
                              added))
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
                     collect (list path (length added) repo-path covered lo hi))))
         (dead
          (cl-loop for (name . ln) in (code-review-analysis--definitions-in
                                       path added)
                   unless (gethash name refs)
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
Return (:similar SIMS :dead DEAD :dangling DANGLINGS), or nil."
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
    (when (or similar dead dangling)
      (list :similar similar :dead dead :dangling dangling))))

(defun code-review-analysis-run ()
  "Run the heuristic analysis for the review in the current buffer.
Return the findings plist (see `code-review-analysis--compute'),
nil when analysis is disabled, there is no local worktree, or
there is nothing to report.  Results are cached per (PR, diff)."
  (when code-review-analysis-enabled
    (let* ((worktree code-review-repo-worktree)
           (diff (and worktree (code-review-db--pullreq-raw-diff)))
           (pr (and diff (code-review-db-get-pullreq))))
      (when (and worktree diff pr)
        (let ((key (concat (oref pr id) "|" (md5 diff))))
          (or (gethash key code-review-analysis--cache)
              (let ((res (code-review-analysis--compute worktree diff pr)))
                (puthash key res code-review-analysis--cache)
                res)))))))

(defun code-review-analysis-reset ()
  "Forget all cached analysis results."
  (interactive)
  (clrhash code-review-analysis--cache))

(provide 'code-review-analysis)
;;; code-review-analysis.el ends here
