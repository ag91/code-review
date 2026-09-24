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

(defgroup code-review-hunkhighlight nil
  "Tree-sitter semantic highlighting of diff hunks."
  :group 'code-review)

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

(defface code-review-test-face
  '((t :inherit font-lock-function-name-face :weight bold :slant italic))
  "Face for test definition names and assertion lines.
Used by the tree-sitter hunk highlighting in test files."
  :group 'code-review-hunkhighlight)

(defface code-review-constant-face
  '((((class color) (background light))
     :foreground "tomato" :weight bold)
    (((class color) (background dark))
     :foreground "MediumPurple1" :weight bold)
    (t :weight bold))
  "Face for literal constant values (strings, numbers) in hunks.
Theme `font-lock-constant-face's are often low-contrast over the
green/red diff backgrounds (the one in use renders as a murky
dark cyan there); this face is tuned to stay clearly readable on
top of `magit-diff-added' while staying distinct from the keyword
purple and the function-name blue."
  :group 'code-review-hunkhighlight)

(defcustom code-review-hunkhighlight-queries
  '((python
     ;; Deliberately MINIMAL (user request): definition names,
     ;; parameters, assignment targets, UPPER_CASE constants and
     ;; the `return' keyword — the reviewer's eyes should go to
     ;; the important identifiers, not to every keyword.  Also
     ;; note `--apply' only paints ADDED (+) lines.
     ("\"return\" @kw"
      . ((kw . font-lock-keyword-face)))
     ("(function_definition name: (identifier) @fn)"
      . ((fn . font-lock-function-name-face)))
     ("(class_definition name: (identifier) @cls)"
      . ((cls . font-lock-type-face)))
     ("(parameters (identifier) @param)"
      . ((param . font-lock-variable-name-face)))
     ("((assignment left: (identifier) @const)
       (#match? @const \"\\\\`[A-Z_][A-Z0-9_]*\\\\'\"))"
      . ((const . code-review-constant-face)))
     ("((assignment left: (identifier) @var)
       (#match? @var \"\\\\`[a-z_]\"))"
      . ((var . font-lock-variable-name-face)))
     ;; literal CONSTANT VALUES (strings, numbers) stand out in
     ;; every language: in test code they are the test case names
     ;; and expected values — the stuff the reviewer wants to see
     ("[(string) (integer) (float)] @cval"
      . ((cval . code-review-constant-face))))
    (scala
     ("\"return\" @kw"
      . ((kw . font-lock-keyword-face)))
     ("(function_definition name: (identifier) @fn)"
      . ((fn . font-lock-function-name-face)))
     ("(class_definition name: (identifier) @cls)"
      . ((cls . font-lock-type-face)))
     ("(object_definition name: (identifier) @obj)"
      . ((obj . font-lock-type-face)))
     ("(trait_definition name: (identifier) @trait)"
      . ((trait . font-lock-type-face)))
     ("(parameters (parameter name: (identifier) @param))"
      . ((param . font-lock-variable-name-face)))
     ("((val_definition pattern: (identifier) @const)
       (#match? @const \"\\\\`[A-Z_][A-Z0-9_]*\\\\'\"))"
      . ((const . code-review-constant-face)))
     ("((val_definition pattern: (identifier) @var)
       (#match? @var \"\\\\`[a-z_]\"))"
      . ((var . font-lock-variable-name-face)))
     ("((var_definition pattern: (identifier) @const)
       (#match? @const \"\\\\`[A-Z_][A-Z0-9_]*\\\\'\"))"
      . ((const . code-review-constant-face)))
     ("((var_definition pattern: (identifier) @var)
       (#match? @var \"\\\\`[a-z_]\"))"
      . ((var . font-lock-variable-name-face)))
     ;; for-comprehension: both `<-' generators and `=' bindings;
     ;; the leading `.' anchors to the first child (the bound
     ;; pattern), not the iterable/value expression
     ("(enumerators (enumerator . (identifier) @for))"
      . ((for . font-lock-variable-name-face)))
     ;; match arms: the `case' keyword plus the whole pattern
     ("(case_clause pattern: (_) @case)
       (case_clause \"case\" @kw)"
      . ((case . font-lock-type-face)
         (kw . font-lock-keyword-face)))
     ("(string) @cval"
      . ((cval . code-review-constant-face)))
     ("[(integer_literal) (floating_point_literal)] @cval"
      . ((cval . code-review-constant-face))))
    (elisp
     ;; grammar nodes (probed): defun forms are
     ;; `function_definition' with a named "defun" token child;
     ;; defvar/defconst/let are `special_form'.  defmacro/defsubst
     ;; are NOT special nodes here (defmacro: structure error,
     ;; defsubst: params leak into the name capture) — left out.
     ("(function_definition \"defun\" (symbol) @fn (list (symbol) @param))"
      . ((fn . font-lock-function-name-face)
         (param . font-lock-variable-name-face)))
     ("(special_form \"defvar\" (symbol) @var)"
      . ((var . font-lock-variable-name-face)))
     ("(special_form \"defconst\" (symbol) @const)"
      . ((const . code-review-constant-face)))
     ("(special_form \"let\" (list (list (symbol) @v)))"
      . ((v . font-lock-variable-name-face)))
     ("[(string) (integer)] @cval"
      . ((cval . code-review-constant-face))))
    (clojure
     ;; grammar nodes (probed): everything is list_lit of sym_lits,
     ;; so definitions are matched by the head symbol with #match?
     ;; (#eq is NOT supported at capture time in Emacs 30.2).
     ;; defn-family name-only pattern first (defprotocol etc lack a
     ;; name-adjacent vector), then name+params for defn shapes.
     ("((list_lit . (sym_lit) @_h . (sym_lit) @name)
       (#match? @_h \"\\\\`\\\\(defn-\\\\|defn\\\\|defmacro\\\\|defonce\\\\|defmulti\\\\|defprotocol\\\\|defrecord\\\\|deftype\\\\)\\\\'\"))"
      . ((name . font-lock-function-name-face)))
     ("((list_lit . (sym_lit) @_h . (sym_lit) @name . (vec_lit (sym_lit) @param))
       (#match? @_h \"\\\\`\\\\(defn-\\\\|defn\\\\|defmacro\\\\|defonce\\\\)\\\\'\"))"
      . ((name . font-lock-function-name-face)
         (param . font-lock-variable-name-face)))
     ("((list_lit . (sym_lit) @_h . (sym_lit) @name)
       (#match? @_h \"\\\\`def\\\\'\"))"
      . ((name . font-lock-variable-name-face)))
     ("((list_lit . (sym_lit) @_h . (vec_lit (sym_lit) @param))
       (#match? @_h \"\\\\`\\\\(fn\\\\|let\\\\|loop\\\\)\\\\'\"))"
      . ((param . font-lock-variable-name-face)))
     ("[(str_lit) (num_lit)] @cval"
      . ((cval . code-review-constant-face))))
    (typescript
     ;; grammar nodes (probed against
     ;; libtree-sitter-typescript): enum names are plain
     ;; `identifier' (NOT type_identifier); `for (const x of ys)'
     ;; is FLATTENED — the binding is the `left:' identifier,
     ;; not a variable_declaration child.
     ("\"return\" @kw"
      . ((kw . font-lock-keyword-face)))
     ("(function_declaration name: (identifier) @fn)"
      . ((fn . font-lock-function-name-face)))
     ("(method_definition name: (property_identifier) @m)"
      . ((m . font-lock-function-name-face)))
     ("(class_declaration name: (type_identifier) @cls)"
      . ((cls . font-lock-type-face)))
     ("(interface_declaration name: (type_identifier) @iface)"
      . ((iface . font-lock-type-face)))
     ("(type_alias_declaration name: (type_identifier) @alias)"
      . ((alias . font-lock-type-face)))
     ("(enum_declaration name: (identifier) @en)"
      . ((en . font-lock-type-face)))
     ("(formal_parameters (required_parameter pattern: (identifier) @param))"
      . ((param . font-lock-variable-name-face)))
     ("(formal_parameters (optional_parameter pattern: (identifier) @param))"
      . ((param . font-lock-variable-name-face)))
     ("((lexical_declaration (variable_declarator name: (identifier) @const))
       (#match? @const \"\\\\`[A-Z_][A-Z0-9_]*\\\\'\"))"
      . ((const . code-review-constant-face)))
     ("((lexical_declaration (variable_declarator name: (identifier) @var))
       (#match? @var \"\\\\`[a-z_]\"))"
      . ((var . font-lock-variable-name-face)))
     ("(variable_declaration (variable_declarator name: (identifier) @var))"
      . ((var . font-lock-variable-name-face)))
     ;; for-of/for-in bindings (const/let/bare all take the
     ;; `left:' identifier slot)
     ("(for_in_statement left: (identifier) @for)"
      . ((for . font-lock-variable-name-face)))
     ;; switch arms: the `case' keyword plus the matched value
     ("(switch_case value: (_) @case)
       (switch_case \"case\" @kw)"
      . ((case . font-lock-type-face)
         (kw . font-lock-keyword-face)))
     ("[(string) (template_string) (number)] @cval"
      . ((cval . code-review-constant-face))))
    (tsx
     ;; the tsx grammar shares the typescript node names for
     ;; every query above (validated: identical captures), so
     ;; this block repeats them verbatim
     ("\"return\" @kw"
      . ((kw . font-lock-keyword-face)))
     ("(function_declaration name: (identifier) @fn)"
      . ((fn . font-lock-function-name-face)))
     ("(method_definition name: (property_identifier) @m)"
      . ((m . font-lock-function-name-face)))
     ("(class_declaration name: (type_identifier) @cls)"
      . ((cls . font-lock-type-face)))
     ("(interface_declaration name: (type_identifier) @iface)"
      . ((iface . font-lock-type-face)))
     ("(type_alias_declaration name: (type_identifier) @alias)"
      . ((alias . font-lock-type-face)))
     ("(enum_declaration name: (identifier) @en)"
      . ((en . font-lock-type-face)))
     ("(formal_parameters (required_parameter pattern: (identifier) @param))"
      . ((param . font-lock-variable-name-face)))
     ("(formal_parameters (optional_parameter pattern: (identifier) @param))"
      . ((param . font-lock-variable-name-face)))
     ("((lexical_declaration (variable_declarator name: (identifier) @const))
       (#match? @const \"\\\\`[A-Z_][A-Z0-9_]*\\\\'\"))"
      . ((const . code-review-constant-face)))
     ("((lexical_declaration (variable_declarator name: (identifier) @var))
       (#match? @var \"\\\\`[a-z_]\"))"
      . ((var . font-lock-variable-name-face)))
     ("(variable_declaration (variable_declarator name: (identifier) @var))"
      . ((var . font-lock-variable-name-face)))
     ("(for_in_statement left: (identifier) @for)"
      . ((for . font-lock-variable-name-face)))
     ("(switch_case value: (_) @case)
       (switch_case \"case\" @kw)"
      . ((case . font-lock-type-face)
         (kw . font-lock-keyword-face)))
     ("[(string) (template_string) (number)] @cval"
      . ((cval . code-review-constant-face))))
    (sql
     ;; grammar: DerekStride/tree-sitter-sql (installed in
     ;; ~/.emacs.d/tree-sitter).  Probed against real ClickHouse
     ;; and dbt shapes: node names are create_table,
     ;; column_definitions/column_definition, object_reference,
     ;; select_expression/term/field, relation, invocation,
     ;; binary_expression, where/join/cross_join/group_by/
     ;; order_by.  KNOWN LIMIT: jinja {{ ... }} breaks the parse
     ;; into ERROR nodes, but everything OUTSIDE the soup still
     ;; captures — and ref()/source() arguments survive as
     ;; invocation literals, so dbt model names still highlight.
     ("(create_table (object_reference) @tbl)"
      . ((tbl . font-lock-type-face)))
     ("(create_table (column_definitions
       (column_definition (identifier) @col)))"
      . ((col . font-lock-variable-name-face)))
     ("(select_expression (term (field) @sel))"
      . ((sel . font-lock-variable-name-face)))
     ("(from (relation (object_reference) @from))"
      . ((from . font-lock-type-face)))
     ("(join (relation (object_reference) @jt))"
      . ((jt . font-lock-type-face)))
     ("(from (relation (invocation (term (literal) @ref))))"
      . ((ref . font-lock-type-face)))
     ("(join (relation (invocation (term (literal) @jref))))"
      . ((jref . font-lock-type-face)))
     ;; WHERE / ON columns: two patterns each, because AND/OR
     ;; compound predicates nest binary_expression one level
     ;; deeper than simple ones
     ("(where (binary_expression (field) @wcol))
       (where (binary_expression
               (binary_expression (field) @wcol)))"
      . ((wcol . font-lock-variable-name-face)))
     ("(join (binary_expression (field) @oncol))
       (join (binary_expression
              (binary_expression (field) @oncol)))"
      . ((oncol . font-lock-variable-name-face)))
     ("(group_by (field) @gcol)"
      . ((gcol . font-lock-variable-name-face)))
     ("(order_by (order_target (field) @ocol))"
      . ((ocol . font-lock-variable-name-face)))
     ;; performance markers: the clause keywords
     ("[(keyword_where) (keyword_group) (keyword_by)
       (keyword_order) (keyword_limit) (keyword_on)
       (keyword_from) (keyword_join)] @kw"
      . ((kw . font-lock-keyword-face)))
     ;; cartesian products go terrible red: explicit CROSS JOIN,
     ;; and implicit comma joins (FROM a, b IS a cross join in
     ;; ClickHouse)
     ("(cross_join [(keyword_cross) (keyword_join)] @danger)"
      . ((danger . font-lock-warning-face)))
     ("(from \",\" @danger)"
      . ((danger . font-lock-warning-face))))
    (yaml
     ;; tree-sitter-yaml: scalars live in flow_node; capture the
     ;; VALUES of `name:' keys — dbt model/column/test names, the
     ;; identifiers a reviewer scans for.  #match? not #eq (#eq is
     ;; not supported at capture time on Emacs 30); @_k is a
     ;; helper capture with no face mapping.
     ("((block_mapping_pair
        key: (flow_node (plain_scalar) @_k)
        value: (flow_node) @name)
       (#match? @_k \"\\\\`name\\\\'\"))"
      . ((name . font-lock-function-name-face)))))
  "Per-language treesit queries.
Each entry: (LANGUAGE (QUERY-STRING . ((CAPTURE . FACE) ...)) ...).
CAPTURE names must match the @captures in QUERY-STRING; a capture
can map to any face.  Author predicates in the standard
`#match? @capture \"REGEXP\"' spelling (Emacs 31+): on Emacs 30,
which only supports `#match \"REGEXP\" @capture' at capture time,
they are rewritten automatically at compile time (see
`code-review-hunkhighlight--old-style-query') — the same queries
work on both versions.  A query that fails to compile against the
installed grammar disables that entry silently (and capture-time
predicate errors disable only that query)."
  :type '(repeat (cons symbol (repeat (cons string (repeat (cons symbol face))))))
  :group 'code-review-hunkhighlight)

(defcustom code-review-hunkhighlight-test-queries
  '((python
     ("((function_definition name: (identifier) @tn) (#match? @tn \"\\\\`test\"))"
      . ((tn . code-review-test-face)))
     ("(assert_statement \"assert\" @as)
       ((call function: (identifier) @af) (#match? @af \"\\\\`assert\"))
       ((call function: (attribute attribute: (identifier) @am))
        (#match? @am \"\\\\`assert\"))"
      . ((as . code-review-test-face)
         (af . code-review-test-face)
         (am . code-review-test-face))))
    (typescript
     ;; jest/vitest shapes: describe/it/test define the cases,
     ;; expect/assert are the assertions (probed: plain call
     ;; expressions, no dedicated grammar nodes)
     ("((call_expression function: (identifier) @tf)
       (#match? @tf \"\\\\`\\\\(describe\\\\|it\\\\|test\\\\)\\\\'\"))"
      . ((tf . code-review-test-face)))
     ("((call_expression function: (identifier) @af)
       (#match? @af \"\\\\`\\\\(expect\\\\|assert\\\\)\\\\'\"))"
      . ((af . code-review-test-face))))
    (tsx
     ("((call_expression function: (identifier) @tf)
       (#match? @tf \"\\\\`\\\\(describe\\\\|it\\\\|test\\\\)\\\\'\"))"
      . ((tf . code-review-test-face)))
     ("((call_expression function: (identifier) @af)
       (#match? @af \"\\\\`\\\\(expect\\\\|assert\\\\)\\\\'\"))"
      . ((af . code-review-test-face)))))
  "Like `code-review-hunkhighlight-queries', but test files only."
  :type '(repeat (cons symbol (repeat (cons string (repeat (cons symbol face))))))
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
                     ((condition-case nil
                          (progn (treesit-query-capture
                                  parser
                                  "((_) @p (#match? @p \"x\"))")
                                 t)
                          (error nil))
                      'new)
                     ((condition-case nil
                          (progn (treesit-query-capture
                                  parser
                                  "((_) @p (#match \"x\" @p))")
                                 t)
                          (error nil))
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
          (remove-overlays (point) lend 'cr-hh-face face)
          (let ((ov (make-overlay (point) lend)))
            (overlay-put ov 'cr-hh-face face)
            (overlay-put ov 'face face)
            (overlay-put ov 'evaporate t))))
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

(defun code-review-hunkhighlight--ranges (text lang test-p)
  "Return ((LINE BEG END FACE)...) for new-side TEXT of LANG.
LINE is 1-based, BEG/END are 0-based columns within that line.
TEST-P adds the test-file queries.  Nil when treesit is unusable
or nothing was captured.  For languages in
`code-review-hunkhighlight-fragment-line-languages' every
non-blank line is ALSO parsed as a one-line document and those
captures merge (mid-file fragments lose the indentation context
an indentation-relative grammar needs)."
  (when (fboundp 'treesit-parser-create)
    (let ((entries
           (append (cdr (assq lang code-review-hunkhighlight-queries))
                   (when test-p
                     (cdr (assq lang
                                 code-review-hunkhighlight-test-queries))))))
      (when entries
        (let ((line-starts (list 1))
              (line-lens nil)
              (res nil))
          (dolist (row (split-string text "\n"))
            (setq line-lens (cons (length row) line-lens)
                  line-starts
                  (cons (+ (car line-starts) (length row) 1)
                        line-starts)))
          (setq line-lens (nreverse line-lens)
                line-starts (nreverse line-starts))
          (with-temp-buffer
            (insert text)
            (let ((parser (treesit-parser-create lang)))
              (dolist (entry entries)
                (let ((compiled
                       (code-review-hunkhighlight--compiled
                        lang (car entry))))
                  (when compiled
                    ;; NB: capture-time predicate errors (e.g. a
                    ;; predicate COMPILES fine but is unsupported at
                    ;; runtime) disable only THIS entry, not the
                    ;; whole language.
                    (pcase-dolist
                        (`(,name . ,node)
                         (condition-case err
                             (treesit-query-capture parser compiled)
                           (error
                            (message "code-review-hunkhighlight: \
query %S disabled: %S" (car entry) err)
                            nil)))
                      (let ((face (cdr (assq name (cdr entry)))))
                        (when face
                          (dolist
                              (pos
                               (code-review-hunkhighlight--node-to-lines
                                (treesit-node-start node)
                                (treesit-node-end node)
                                line-starts line-lens))
                            (push (append pos (list face)) res))))))))))
          ;; FRAGMENT STRATEGY (see
          ;; `code-review-hunkhighlight-fragment-line-languages'):
          ;; parse every non-blank line as a ONE-LINE document and
          ;; run the same queries; captures merge with the
          ;; whole-fragment ones.  A one-line document is always
          ;; well-formed for these grammars, so this recovers the
          ;; pairs the fragment's error recovery dropped.
          (when (memq lang code-review-hunkhighlight-fragment-line-languages)
            (let ((line-no 0))
              (dolist (line (split-string text "\n"))
                (setq line-no (1+ line-no))
                (unless (string-blank-p line)
                  (with-temp-buffer
                    (insert line)
                    (let ((parser (treesit-parser-create lang)))
                      (dolist (entry entries)
                        (let ((compiled
                               (code-review-hunkhighlight--compiled
                                lang (car entry))))
                          (when compiled
                            (pcase-dolist
                                (`(,name . ,node)
                                 ;; per-line failures stay quiet: the
                                 ;; whole-fragment pass already
                                 ;; messaged a truly broken query
                                 (ignore-errors
                                   (treesit-query-capture parser compiled)))
                              (let ((face (cdr (assq name (cdr entry)))))
                                (when face
                                  ;; one-line doc: buffer columns map
                                  ;; directly (bol is 1)
                                  (let ((b (treesit-node-start node))
                                        (e (treesit-node-end node)))
                                    (when (< b e)
                                      (push (list line-no (1- b) (1- e)
                                                  face)
                                            res)))))))))))))))
          (nreverse res))))))

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

(defun code-review-hunkhighlight-region (beg end path)
  "Apply semantic faces to hunk body BEG..END of file PATH.
BEG is the first body line (after the @@ heading), END the end of
the hunk.  Does nothing when tree-sitter, the grammar or the
language's queries are unavailable, when the file's language is
unknown, or when `code-review-semantic-highlight' is nil.
Never signals; returns non-nil when faces were applied."
  (condition-case err
      (let ((applied nil))
        (when (and code-review-semantic-highlight
                   (fboundp 'treesit-parser-create)
                   (fboundp 'treesit-language-available-p)
                   (or (require 'treesit nil t) t)
                   (let ((lang (code-review-hunkhighlight--language-for
                                path)))
                     (and lang
                          (treesit-language-available-p lang))))
          (let* ((lang (code-review-hunkhighlight--language-for path))
                 (test-p (string-match-p
                          code-review-hunkhighlight-test-path-regexp
                          (downcase path)))
                 (body (buffer-substring-no-properties beg end))
                 ;; the cache key includes the query+face MAPPINGS:
                 ;; cached ranges carry resolved faces, so a
                 ;; changed defcustom must invalidate the cache
                 ;; (learned live: repainting after a face swap
                 ;; silently reapplied the old face)
                 (key (concat (symbol-name lang) "|"
                              (if test-p "t" "nil") "|"
                              (md5 (concat
                                    body "\e"
                                    (prin1-to-string
                                     (cons
                                      ;; the fragment strategy changes
                                      ;; which ranges exist, so it must
                                      ;; invalidate the cache too
                                      code-review-hunkhighlight-fragment-line-languages
                                      (cons
                                       ;; the predicate contract matters
                                       ;; for the ranges: it changes which
                                       ;; queries capture (Emacs 30/31),
                                       ;; so it must invalidate the cache
                                       (code-review-hunkhighlight--contract
                                        lang)
                                       (cons
                                        (assq lang
                                              code-review-hunkhighlight-queries)
                                        (when test-p
                                          (assq lang
                                                code-review-hunkhighlight-test-queries))))))))))
                 (cache (or code-review-hunkhighlight--cache
                            (setq code-review-hunkhighlight--cache
                                  (make-hash-table :test #'equal))))
                 (recon (code-review-hunkhighlight--reconstruct beg end))
                 (text (nth 0 recon))
                 (lines (nth 1 recon)))
            (let ((ranges (or (gethash key cache)
                              (puthash key
                                       (code-review-hunkhighlight--ranges
                                        text lang test-p)
                                       cache))))
              (code-review-hunkhighlight--apply ranges lines)
              (setq applied (not (null ranges))))))
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
                ;; last resort: the parent's heading text (magit
                ;; diff file headings contain the file name)
                (when-let* ((beg (oref parent start))
                            (end (oref parent content))
                            (buf (and (markerp beg)
                                      (marker-buffer beg))))
                  (with-current-buffer buf
                    (string-trim
                     (buffer-substring-no-properties beg end))))))))))

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
    (let ((n 0))
      (cl-labels ((walk (section)
                      (dolist (child (oref section children))
                        (if (magit-section-match 'hunk child)
                            (when (code-review-hunkhighlight-hunk
                                   child
                                   (code-review-hunkhighlight--section-path
                                    child))
                              (setq n (1+ n)))
                          (walk child)))))
        (walk magit-root-section))
      (message "code-review-hunkhighlight: %d hunk(s) highlighted" n))))

(provide 'code-review-hunkhighlight)
;;; code-review-hunkhighlight.el ends here
