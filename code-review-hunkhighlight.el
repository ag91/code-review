;;; code-review-hunkhighlight.el --- Tree-sitter semantic hunk faces -*- lexical-binding: t; -*-
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

;;  Phase 10: semantic highlighting of diff hunks.  After the diff
;;  wash paints the base faces (+/-/context), this library makes
;;  hunks read as CODE.  The face set is deliberately MINIMAL so
;;  the reviewer's eyes focus on the important identifiers:
;;  definition names, parameters, assignment targets, val/var
;;  definition names, for-comprehension bindings, match-case
;;  patterns, UPPER_CASE constants, literal CONSTANT VALUES
;;  (strings/numbers — in test code those are the test case names
;;  and expected values), the `return' keyword — and in test
;;  files, test definition names and assertion lines.  For SQL
;;  the set is performance-shaped: table references, selected/
;;  filter/join columns, the WHERE/JOIN/GROUP/ORDER/LIMIT
;;  keywords, and cartesian joins (CROSS JOIN, and the comma in
;;  FROM a, b — a cross join in ClickHouse) in warning red; YAML
;;  captures the values of `name:' keys (dbt model/column names).
;;  Mid-file yaml fragments lose their indentation context (the
;;  grammar recovers only the first block), so for languages in
;;  `code-review-hunkhighlight-fragment-line-languages' every
;;  non-blank line is ALSO parsed as a one-line document and
;;  those captures merge.
;;  Only ADDED lines get
;;  semantic faces (context is parse input, not signal; deleted
;;  lines are not part of the new side).
;;
;;  Phase 20a adds the SECURITY layer on top: SINK tokens
;;  (eval/exec, pickle.loads, os.system, shell=True,
;;  verify=False, except-pass, string-concatenated SQL,
;;  innerHTML, Math.random, dangerouslySetInnerHTML, ...) in
;;  warning red, and untrusted-input SOURCES (request.*,
;;  sys.argv, os.environ, input(), process.env) in the dimmer
;;  `code-review-source-face' — marking where taint enters and
;;  where it is used.  The vocabulary lives in
;;  `code-review-hunkhighlight-security-queries' (in
;;  code-review-hunkhighlight-queries.el) and stays SPARSE on
;;  purpose: priming stops working when everything is marked.
;;
;;  Phase 20b retargets the literal emphasis at where reviewers
;;  actually look: conditions and loops (eye-tracking of defect
;;  finding: fixation locks onto the conditions — Sharif,
;;  Falcone & Maletic, ETRA 2012).  Beacon entries (in
;;  `code-review-hunkhighlight-beacon-queries') capture the
;;  condition subtrees under the helper @_cond; comparison
;;  operators paint strong only inside conditions; literals
;;  paint strong inside conditions and in test files, and DIM
;;  outside (`code-review-constant-dim-face', or plain — see
;;  `code-review-hunkhighlight-dim-outside-literals').
;;
;;  Phase 20c (code-review-hunkhighlight-intraline.el) marks the
;;  CHANGED TOKENS of modified added lines with an underline
;;  (`code-review-changed-token-face'): the old side is already
;;  in the hunk, so the marking is grammar-free and needs no
;;  external tools — it works for every language, even ones with
;;  no treesit grammar at all.
;;
;;  How it works, per hunk:
;;   1. strip the +/-/space prefixes and reconstruct the NEW side
;;      of the hunk (added + context lines);
;;   2. parse it with treesit and run `treesit-query-capture' with
;;      per-language queries (see the defcustoms);
;;   3. map every captured node's range back onto the diff buffer
;;      positions and lay the semantic face as an OVERLAY over the
;;      line's diff face: overlay faces override text-property
;;      faces, so the semantic foreground displays on top of the
;;      red/green background (a face-LIST property would NOT work:
;;      earlier faces in the list win attribute conflicts, and
;;      `magit-diff-added' sets its own foreground).  Overlays also
;;      survive magit 4.x lazily replacing hunk face properties.
;;
;;  Everything degrades gracefully: no treesit, no grammar, no
;;  queries for the language, or `code-review-semantic-highlight'
;;  nil  =>  exactly the old faces, no error.
;;
;;  The core (`code-review-hunkhighlight-region') works on any
;;  buffer region containing prefixed diff lines, so it also
;;  works in plain magit diff buffers:
;;  `code-review-hunkhighlight-magit-buffer' walks the magit
;;  section tree of the current buffer and highlights every hunk
;;  (phase 10 bonus: reusable outside code-review).

;;; Code:

(require 'treesit nil t)          ; optional: Emacs 29+
(require 'magit-section)
(require 'cl-lib)
(require 'code-review-hunkhighlight-queries)
(require 'code-review-hunkhighlight-intraline)

(defcustom code-review-semantic-highlight t
  "When non-nil, highlight hunks semantically with tree-sitter.
Ignored (does nothing) when treesit is unavailable or the file's
language grammar is not installed: then hunks keep the plain
diff faces and no error is signaled."
  :type 'boolean
  :group 'code-review-hunkhighlight)

(defcustom code-review-hunkhighlight-language-map
  '(("\\.py\\'" . python)
    ("\\.scala\\'" . scala)
    ("\\.sc\\'" . scala)
    ("\\.sbt\\'" . scala)
    ("\\.el\\'" . elisp)
    ("\\.clj[scx]?\\'" . clojure)
    ("\\.ts\\'" . typescript)
    ("\\.tsx\\'" . tsx)
    ("\\.sql\\'" . sql)
    ("\\.ya?ml\\'" . yaml))
  "Map file-name regexp to tree-sitter language symbol.
Extend this (and `code-review-hunkhighlight-queries') when adding
languages: only ship queries you have validated against the
installed grammar, a broken query silently disables that entry."
  :type '(alist :key-type regexp :value-type symbol)
  :group 'code-review-hunkhighlight)

(defcustom code-review-hunkhighlight-test-path-regexp
  "\\`test\\|/tests?/\\|_test\\.\\|\\.test\\."
  "Regexp matching file paths that are test files.
Matched against the DOWNCASED path.  In test files the
`code-review-hunkhighlight-test-queries' run on top of the
general ones."
  :type 'regexp
  :group 'code-review-hunkhighlight)

(defcustom code-review-hunkhighlight-fragment-line-languages '(yaml)
  "Languages that additionally parse hunk lines INDIVIDUALLY.
A hunk reconstructs a mid-file FRAGMENT (added + context lines).
Indentation-relative grammars lose the enclosing context on
such fragments, and error recovery keeps only the fragment's
first block: a real dbt _marts.yml hunk (mixed column depths,
blank lines) yields ONE name pair out of dozens, and dedenting
the text does not help (probed live).  For these languages every
non-blank line is ALSO parsed as a one-line document and the
same queries run per line; captures merge with the
whole-fragment ones.  One tiny parse per line, cached like
everything else.  Whitespace-insensitive grammars (sql, the
programming languages) do not need this."
  :type '(repeat symbol)
  :group 'code-review-hunkhighlight)

(defvar-local code-review-hunkhighlight--cache nil
  "Hash table \"LANG|TESTP|md5(BODY)\" -> ((LINE BEG END FACE)...).
LINE is 1-based in the reconstructed new-side text, BEG/END are
0-based columns within that line: position-independent, so the
ranges survive re-renders.  Buffer-local on purpose: it dies with
the review buffer.")

(defvar code-review-hunkhighlight--query-cache
  (make-hash-table :test #'equal)
  "Compiled-query cache: (LANG . QUERY-STRING) -> compiled query,
or `broken' when compilation failed against the grammar.")

(defvar code-review-hunkhighlight--predicate-contract nil
  "Memoized query-predicate contract of this Emacs: new or old.
new: the standard `(#match? @capture \"REGEXP\")' spelling
  (Emacs 31+, and the spelling the defcustom queries are
  authored in).
old: Emacs 30 only supports `(#match \"REGEXP\" @capture)' at
  capture time — the `#match?' spelling compiles but every
  capture using it fails with \"Invalid predicate\".")

(defun code-review-hunkhighlight--probe-query (parser query)
  "Non-nil when QUERY captures without error on PARSER.
The probe is the CAPTURE itself, never the compile: Emacs 30
happily compiles a `#match?' query whose predicate fails only at
capture time."
  (condition-case nil
      (progn (treesit-query-capture parser query) t)
    (error nil)))

(defun code-review-hunkhighlight--contract (&optional lang)
  "Return the query-predicate contract of this Emacs: new or old.
Probed once against LANG's grammar (python when nil) by actually
CAPTURE-ing one query of each spelling — compile alone cannot
discriminate: Emacs 30 happily compiles `#match?' and only fails
when the predicate is evaluated during capture.  Memoized in
`code-review-hunkhighlight--predicate-contract'; never signals
(falls back to new, which passes queries through unchanged)."
  (or code-review-hunkhighlight--predicate-contract
      (setq code-review-hunkhighlight--predicate-contract
            (condition-case nil
                (with-temp-buffer
                  (insert "x")
                  (let ((parser (treesit-parser-create (or lang 'python))))
                    (cond
                     ((code-review-hunkhighlight--probe-query
                       parser "((_) @p (#match? @p \"x\"))")
                      'new)
                     ((code-review-hunkhighlight--probe-query
                       parser "((_) @p (#match \"x\" @p))")
                      'old)
                     ;; neither spelling works (no grammar?): leave
                     ;; queries alone, entries degrade per-query
                     (t 'new))))
              (error 'new)))))

(defun code-review-hunkhighlight--old-style-query (query)
  "Rewrite QUERY's `#match?' predicates into the Emacs 30 form.
`(#match? @cap \"REG\")' becomes `(#match \"REG\" @cap)' — same
regexp, same captures, only the predicate spelling and argument
order change.  Queries without predicates pass through unchanged.

The scan never touches the global match data beyond
`save-match-data', and every `match-string'/`match-beginning'
call names QUERY explicitly: with an implicit string argument the
positions of the last STRING match get read against the CURRENT
BUFFER (here the hunk parse buffer), splicing hunk text into the
query — a variant of the classic global-match-data trap."
  (save-match-data
    (let ((regexp "(#match\\?[ \t\n]*\\(@[-A-Za-z0-9_]+\\)[ \t\n]*\\(\"\\(?:[^\"\\]\\|\\\\.\\)*\"\\)[ \t\n]*)")
          (start 0)
          (out ""))
      (while (string-match regexp query start)
        (let ((mb (match-beginning 0))
              (cap (match-string 1 query))
              (reg (match-string 2 query)))
          (setq out (concat out (substring query start mb)
                            "(#match " reg " " cap ")")
                ;; bound before any further matching, per the
                ;; match-data gotcha
                start (match-end 0))))
      (concat out (substring query start)))))

(defun code-review-hunkhighlight--compat-query (lang query)
  "Return QUERY spelled the way this Emacs's treesit accepts it.
The defcustom queries are authored in the standard `#match?'
spelling (Emacs 31+).  On Emacs 30 the predicates are rewritten
with `code-review-hunkhighlight--old-style-query' so the same
defcustom works on both versions without user configuration."
  (if (eq (code-review-hunkhighlight--contract lang) 'new)
      query
    (code-review-hunkhighlight--old-style-query query)))

(defun code-review-hunkhighlight--language-for (path)
  "Return the tree-sitter language symbol for file PATH, or nil."
  (cl-loop for (ext . lang) in code-review-hunkhighlight-language-map
           when (string-match-p ext path)
           return lang))

(defun code-review-hunkhighlight--compiled (lang query)
  "Return compiled QUERY for LANG, nil when it cannot compile.
QUERY is first normalized for this Emacs with
`code-review-hunkhighlight--compat-query' (Emacs 30/31 predicate
spelling), and the cache is keyed by the normalized query, so
reloading the library with a changed contract picks up fresh
compiles instead of replaying stale broken entries."
  (let* ((query (code-review-hunkhighlight--compat-query lang query))
         (key (cons lang query)))
    (or (gethash key code-review-hunkhighlight--query-cache)
        (let ((compiled
               (condition-case nil
                   (treesit-query-compile lang query)
                 (error 'broken))))
          (puthash key compiled code-review-hunkhighlight--query-cache)
          (and (not (eq compiled 'broken)) compiled)))))

(defun code-review-hunkhighlight--reconstruct (beg end)
  "Reconstruct the NEW side of hunk body BEG..END.
Return (TEXT LINES): TEXT is the new-side text, LINES is a list
of (CONTENT-BEG . CONTENT-END) review-buffer positions per
reconstructed line, prefix excluded, in order.  Deleted (-) and
`\\ No newline' lines are not part of the new side."
  (let (rows lines)
    (save-excursion
      (goto-char beg)
      (while (< (point) end)
        (let ((ch (char-after)))
          (when (or (eq ch ?+) (eq ch ?\s))
            (push (buffer-substring-no-properties
                   (1+ (point)) (line-end-position))
                  rows)
            (push (cons (1+ (point)) (line-end-position)) lines)))
        (forward-line)))
    (list (string-join (nreverse rows) "\n") (nreverse lines))))

(defun code-review-hunkhighlight--lay-line-face (beg end face)
  "Lay FACE idempotently on the single line segment BEG..END.
A fresh overlay marked with its own `cr-hh-face' value replaces
any previous one for that face, so re-running merges instead of
duplicating."
  (remove-overlays beg end 'cr-hh-face face)
  (let ((ov (make-overlay beg end)))
    (overlay-put ov 'cr-hh-face face)
    (overlay-put ov 'face face)
    (overlay-put ov 'evaporate t)))

(defun code-review-hunkhighlight--put-face (beg end face)
  "Lay FACE on BEG..END as an overlay, over the line's diff face.
OVERLAYS, not text properties, for two reasons found the hard way:

 1. a face-LIST property merges with EARLIER faces winning
    attribute conflicts; since `magit-diff-added' sets its own
    foreground (#22aa22), appending the semantic face left the
    diff foreground winning and nothing was visible.  Overlay
    faces on the other hand OVERRIDE text-property faces, so the
    semantic foreground shows on top of the diff background.

 2. magit 4.x repaints hunk bodies by replacing their face text
    properties, which would erase semantic faces stored there.

Different semantic faces stack: each face carries its own
`cr-hh-face' marker so re-running is idempotent per face."
  (save-excursion
    (goto-char beg)
    (while (< (point) end)
      (let ((lend (min end (line-end-position))))
        (when (< (point) lend)
          (code-review-hunkhighlight--lay-line-face (point) lend face)))
      (forward-line 1))))

(defun code-review-hunkhighlight--node-to-lines (beg end line-starts line-lens)
  "Convert parse-buffer range BEG..END into ((LINE COL-B COL-E)...).
LINE is 1-based, columns 0-based; LINE-STARTS/LINE-LENS describe
the line layout of the parse buffer."
  (let ((res nil)
        (k 0)
        (n (length line-starts)))
    (while (< k n)
      (let* ((bol (nth k line-starts))
             (len (nth k line-lens))
             (eol (+ bol len)))
        (when (and (< beg eol) (< bol end))
          (let ((cb (max beg bol))
                (ce (min end eol)))
            (when (< cb ce)
              (push (list (1+ k) (- cb bol) (- ce bol)) res))))
        (when (<= end eol)
          (setq k n))
        (setq k (1+ k))))
    (nreverse res)))

(defun code-review-hunkhighlight--entry-conds (entry captures)
  "Condition ranges ((BEG . END)...) from an entry's @_cond captures.
Beacon entries (phase 20b) capture the CONDITION subtrees of
if/while/case clauses under the helper capture name @_cond
(leading underscore: no face mapping).  The classification test
is plain range containment, so tokens at any nesting depth inside
the condition classify as inside, without ancestor walking.
Nil for entries that do not use @_cond."
  (when (string-match-p "@_cond" (car entry))
    (let (conds)
      (pcase-dolist (`(,name . ,node) captures)
        (when (eq name '_cond)
          (push (cons (treesit-node-start node)
                      (treesit-node-end node))
                conds)))
      conds)))

(defun code-review-hunkhighlight--inside-conds-p (node conds)
  "Non-nil when NODE's span lies inside one of the COND ranges.
Plain range containment, no ancestor walking: tokens at any
nesting depth inside the condition classify as inside."
  (and conds
       (cl-some (lambda (c)
                  (and (<= (car c) (treesit-node-start node))
                       (>= (cdr c) (treesit-node-end node))))
                conds)))

(defun code-review-hunkhighlight--entry-face (entry name node conds test-p)
  "Face for NODE's capture NAME in ENTRY, or nil when not painted.
A SYMBOL mapping value paints always (the phase 10 mapping).  A
CONS mapping value is the phase 20b BEACON mapping (STRONG . DIM):
STRONG when the node lies inside one of CONDS (the entry's @_cond
condition ranges) or in a TEST file (expected values are the
payload there); DIM outside — and plain when
`code-review-hunkhighlight-dim-outside-literals' is nil, or when
DIM itself is nil (comparison operators paint only inside
conditions)."
  (let ((val (cdr (assq name (cdr entry)))))
    (cond
     ((null val) nil)
     ((symbolp val) val)
     ((or test-p (code-review-hunkhighlight--inside-conds-p node conds))
      (car val))
     ((and (cdr val) code-review-hunkhighlight-dim-outside-literals)
      (cdr val))
     (t nil))))

(defun code-review-hunkhighlight--entries-for (lang test-p)
  "The query ENTRIES for LANG (a test file when TEST-P).
General, security and beacon entries merge in every file; the
test-file entries ride on top in test files only."
  (append (cdr (assq lang code-review-hunkhighlight-queries))
          (cdr (assq lang code-review-hunkhighlight-security-queries))
          (cdr (assq lang code-review-hunkhighlight-beacon-queries))
          (when test-p
            (cdr (assq lang code-review-hunkhighlight-test-queries)))))

(defun code-review-hunkhighlight--line-layout (lines)
  "The (LINE-STARTS LINE-LENS) layout of the parse buffer for LINES."
  (let ((starts (list 1))
        (lens nil))
    (dolist (len (mapcar #'length lines))
      (setq lens (cons len lens)
            starts (cons (+ (car starts) len 1) starts)))
    (list (nreverse starts) (nreverse lens))))

(defun code-review-hunkhighlight--line-mapper (line-no)
  "A node-to-positions MAPPER for a one-line document at LINE-NO.
Buffer columns map directly (bol is 1), so node columns are the
mapper's columns; the node must be non-empty."
  (lambda (node)
    (let ((b (treesit-node-start node))
          (e (treesit-node-end node)))
      (when (< b e)
        (list (list line-no (1- b) (1- e)))))))

(defun code-review-hunkhighlight--node-ranges (entry lang parser test-p mapper quiet-p)
  "Range rows ((LINE BEG END FACE)...) from one ENTRY on PARSER.
MAPPER converts a captured node to (LINE BEG END) position lists
(see `--node-to-lines' and `--line-mapper'); the capture is
isolated so a broken query disables only its entry (QUIET-P: the
per-line pass stays quiet — the whole-fragment pass already
messaged a truly broken query)."
  (let ((compiled (code-review-hunkhighlight--compiled lang (car entry)))
        (rows nil))
    (when compiled
      (let* ((captures
              (if quiet-p
                  (ignore-errors
                    (treesit-query-capture parser compiled))
                (condition-case err
                    (treesit-query-capture parser compiled)
                  (error
                   (message "code-review-hunkhighlight: \
query %S disabled: %S" (car entry) err)
                   nil))))
             (conds (code-review-hunkhighlight--entry-conds
                     entry captures)))
        (pcase-dolist (`(,name . ,node) captures)
          (let ((face (code-review-hunkhighlight--entry-face
                       entry name node conds test-p)))
            (when face
              (dolist (pos (funcall mapper node))
                (push (append pos (list face)) rows)))))))
    (nreverse rows)))

(defun code-review-hunkhighlight--whole-fragment-rows (entries lang test-p lines)
  "Range rows from parsing the whole new-side fragment.
LINES is the fragment's reconstructed lines; node positions map
through `--node-to-lines'."
  (let* ((layout (code-review-hunkhighlight--line-layout lines))
         (line-starts (nth 0 layout))
         (line-lens (nth 1 layout))
         (rows nil))
    (with-temp-buffer
      (insert (string-join lines "\n"))
      (let ((parser (treesit-parser-create lang))
            (mapper (lambda (node)
                      (code-review-hunkhighlight--node-to-lines
                       (treesit-node-start node)
                       (treesit-node-end node)
                       line-starts line-lens))))
        (dolist (entry entries)
          (setq rows (nconc rows
                           (code-review-hunkhighlight--node-ranges
                            entry lang parser test-p mapper nil))))))
    rows))

(defun code-review-hunkhighlight--per-line-rows (entries lang test-p lines)
  "Range rows from parsing each non-blank line as a ONE-LINE document.
The fragment strategy for
`code-review-hunkhighlight-fragment-line-languages' (see the
defcustom): a one-line document is always well-formed, so this
recovers the pairs the fragment's error recovery dropped."
  (when (memq lang code-review-hunkhighlight-fragment-line-languages)
    (let ((rows nil)
          (line-no 0))
      (dolist (line lines)
        (setq line-no (1+ line-no))
        (unless (string-blank-p line)
          (with-temp-buffer
            (insert line)
            (let ((parser (treesit-parser-create lang))
                  (mapper (code-review-hunkhighlight--line-mapper
                           line-no)))
              (dolist (entry entries)
                (setq rows (nconc rows
                                 (code-review-hunkhighlight--node-ranges
                                  entry lang parser test-p mapper t))))))))
      rows)))

(defun code-review-hunkhighlight--ranges (text lang test-p)
  "Return ((LINE BEG END FACE)...) for new-side TEXT of LANG.
LINE is 1-based, BEG/END are 0-based columns within that line.
TEST-P adds the test-file queries; the security queries (phase
20a) and the beacon queries (phase 20b) run in every file, like
the general ones.  Nil when treesit is unusable or nothing was
captured.  For languages in
`code-review-hunkhighlight-fragment-line-languages' every
non-blank line is ALSO parsed as a one-line document and those
captures merge (mid-file fragments lose the indentation context
an indentation-relative grammar needs).

Two passes, one shared per-entry runner
(`code-review-hunkhighlight--node-ranges'): the whole-fragment
pass (`--whole-fragment-rows') and the per-line fragment pass
(`--per-line-rows')."
  (when (fboundp 'treesit-parser-create)
    (let ((entries (code-review-hunkhighlight--entries-for lang test-p)))
      (when entries
        (let ((lines (split-string text "\n")))
          (append
           (code-review-hunkhighlight--whole-fragment-rows
            entries lang test-p lines)
           (code-review-hunkhighlight--per-line-rows
            entries lang test-p lines)))))))

(defun code-review-hunkhighlight--apply (ranges lines)
  "Apply cached RANGES onto the review buffer.
LINES is the per-line (CONTENT-BEG . CONTENT-END) mapping from
`code-review-hunkhighlight--reconstruct'.  Only ADDED lines are
painted: the diff prefix char just before the content start must
be a `+'.  Context lines are parse input (they help treesit see
the structure) but they carry no changes, so they stay
diff-colored only; deleted lines never reach the new side at all."
  (dolist (r ranges)
    (let ((pair (nth (1- (nth 0 r)) lines)))
      (when (and pair (eq (char-after (1- (car pair))) ?+))
        (let* ((line-len (- (cdr pair) (car pair)))
               (beg (+ (car pair) (min (nth 1 r) line-len)))
               (end (+ (car pair) (min (nth 2 r) line-len)))
               (face (nth 3 r)))
          (when (< beg end)
            (code-review-hunkhighlight--put-face beg end face)))))))

(defun code-review-hunkhighlight--behavior-key ()
  "The defcustom values that change WHICH ranges exist.
Phase 20c intra-line marks and caps, the phase 20b outside-literal
treatment, the fragment strategy."
  (list code-review-hunkhighlight-intra-line
        code-review-hunkhighlight-intra-line-max-length
        code-review-hunkhighlight-intra-line-max-tokens
        code-review-hunkhighlight-dim-outside-literals
        code-review-hunkhighlight-fragment-line-languages))

(defun code-review-hunkhighlight--vocabulary-key (lang test-p)
  "The query inputs that change which ranges exist for LANG.
The predicate contract (Emacs 30/31) and the four query alists;
the test-file list rides only when TEST-P."
  (list (code-review-hunkhighlight--contract lang)
        (assq lang code-review-hunkhighlight-queries)
        (assq lang code-review-hunkhighlight-security-queries)
        (assq lang code-review-hunkhighlight-beacon-queries)
        (and test-p
             (assq lang code-review-hunkhighlight-test-queries))))

(defun code-review-hunkhighlight--cache-key (body lang test-p)
  "Cache key for the ranges of hunk BODY (file LANG, TEST-P file?).
The key covers the body plus every defcustom and query list that
changes WHICH ranges exist: cached ranges carry resolved faces, so
a changed defcustom must invalidate the cache (learned live:
repainting after a face swap silently reapplied the old face).
FLAT `list's on purpose (behavior / vocabulary): the nested-cons
version of this key was a paren-count bug farm."
  (md5 (concat body "\e"
               (prin1-to-string
                (list (code-review-hunkhighlight--behavior-key)
                      (code-review-hunkhighlight--vocabulary-key
                       lang test-p))))))

(defun code-review-hunkhighlight--treesit-available-p (lang)
  "Non-nil when LANG's grammar is usable in this Emacs."
  (and lang
       (fboundp 'treesit-parser-create)
       (fboundp 'treesit-language-available-p)
       (or (require 'treesit nil t) t)
       (treesit-language-available-p lang)))

(defun code-review-hunkhighlight--ranges-cache ()
  "The buffer-local ranges cache table, created on first use."
  (or code-review-hunkhighlight--cache
      (setq code-review-hunkhighlight--cache
            (make-hash-table :test #'equal))))

(defun code-review-hunkhighlight--compute-ranges (body text lang test-p treesit-p)
  "The UNCACHED range rows of one hunk: intra-line + treesit."
  (append
   ;; phase 20c: grammar-free changed-token marks, for every
   ;; language (including unknown ones: no grammar needed)
   (and code-review-hunkhighlight-intra-line
        (code-review-hunkhighlight--intra-line-ranges body))
   ;; treesit semantic faces, only when the language and grammar
   ;; are there
   (and treesit-p
        (code-review-hunkhighlight--ranges text lang test-p))))

(defun code-review-hunkhighlight--cached-ranges (body text lang test-p treesit-p)
  "The range rows for a hunk body, cache-served under `--cache-key'."
  (let ((key (code-review-hunkhighlight--cache-key body lang test-p))
        (cache (code-review-hunkhighlight--ranges-cache)))
    (or (gethash key cache)
        (puthash key
                 (code-review-hunkhighlight--compute-ranges
                  body text lang test-p treesit-p)
                 cache))))

(defun code-review-hunkhighlight-region (beg end path)
  "Apply semantic faces to hunk body BEG..END of file PATH.
BEG is the first body line (after the @@ heading), END the end of
the hunk.  Two layers, independently available: the treesit
semantic faces (does nothing when tree-sitter, the grammar or the
language's queries are unavailable, or when the file's language is
unknown) and the phase 20c intra-line changed-token marks
(`code-review-hunkhighlight-intra-line', grammar-free — they run
for EVERY file, including unknown languages, so local diff
reviews of any code inherit them).  Both off when
`code-review-semantic-highlight' is nil.  Never signals; returns
non-nil when faces were applied."
  (condition-case err
      (let ((applied nil))
        (when code-review-semantic-highlight
          (let* ((lang (code-review-hunkhighlight--language-for path))
                 (test-p (string-match-p
                          code-review-hunkhighlight-test-path-regexp
                          (downcase path)))
                 (body (buffer-substring-no-properties beg end))
                 (treesit-p (code-review-hunkhighlight--treesit-available-p
                             lang)))
            (when (or treesit-p code-review-hunkhighlight-intra-line)
              (let* ((recon (code-review-hunkhighlight--reconstruct beg end))
                     (lines (nth 1 recon))
                     (ranges (code-review-hunkhighlight--cached-ranges
                              body (nth 0 recon) lang test-p treesit-p)))
                (code-review-hunkhighlight--apply ranges lines)
                (setq applied (not (null ranges)))))))
        applied)
    (error
     (message "code-review-hunkhighlight: %S" err)
     nil)))

(defun code-review-hunkhighlight-hunk (section path)
  "Highlight hunk SECTION belonging to file PATH.
Thin wrapper over `code-review-hunkhighlight-region'."
  (when (and section (oref section start) (oref section end))
    (code-review-hunkhighlight-region
     (save-excursion
       (goto-char (oref section start))
       (forward-line)
       (point))
     (oref section end)
     path)))

(defun code-review-hunkhighlight--heading-text (parent)
  "The trimmed heading text of PARENT's section, or nil.
Magit diff FILE section headings contain the file name; the
markers must live in a live buffer (`markerp')."
  (when-let* ((beg (oref parent start))
              (end (oref parent content))
              (buf (and (markerp beg) (marker-buffer beg))))
    (with-current-buffer buf
      (string-trim (buffer-substring-no-properties beg end)))))

(defun code-review-hunkhighlight--section-path (section)
  "Best-effort file path for hunk SECTION (code-review or magit).
Never signals: unknown value shapes (plain magit hunk values are
not alists, and an improper cons would break `assq') just make it
fall back to the parent file section, and finally to the parent's
heading text.  Uses the `parent' slot directly because Magit 4.x
removed `magit-section-parent' and `magit-section-heading'."
  (let ((value (oref section value)))
    (or (and (listp value)
             (ignore-errors (cdr (assq 'path value))))
        (when-let* ((parent (oref section parent)))
          (let ((pv (oref parent value)))
            (or (and (stringp pv) pv)
                (and (listp pv) (stringp (car pv)) (car pv))
                ;; last resort: the parent's heading text
                (code-review-hunkhighlight--heading-text parent)))))))

(defun code-review-hunkhighlight--paint-hunk-sections (section)
  "Paint every hunk under SECTION recursively; return the count.
Hunk children paint via `code-review-hunkhighlight-hunk', every
other child recurses into."
  (let ((n 0))
    (dolist (child (oref section children))
      (setq n
            (+ n
               (if (magit-section-match 'hunk child)
                   (if (code-review-hunkhighlight-hunk
                        child
                        (code-review-hunkhighlight--section-path child))
                       1 0)
                 (code-review-hunkhighlight--paint-hunk-sections child)))))
    n))

;;;###autoload
(defun code-review-hunkhighlight-magit-buffer ()
  "Highlight every hunk in the current magit-style diff buffer.
Works in `magit-diff' buffers and in code-review buffers alike:
walks the magit section tree and applies semantic faces on top of
the diff faces.  Re-runnable: face application merges instead of
duplicating."
  (interactive)
  (when (and code-review-semantic-highlight
             (fboundp 'treesit-parser-create)
             ;; start at the ROOT: walking from the section at point
             ;; would miss every sibling
             (bound-and-true-p magit-root-section))
    (message "code-review-hunkhighlight: %d hunk(s) highlighted"
             (code-review-hunkhighlight--paint-hunk-sections
              magit-root-section))))

(provide 'code-review-hunkhighlight)
;;; code-review-hunkhighlight.el ends here
