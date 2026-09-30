;;; code-review-hunkhighlight-queries.el --- Per-language treesit query vocabularies -*- lexical-binding: t; -*-
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

;;  The per-language treesit QUERY lists and FACES that drive
;;  `code-review-hunkhighlight' (phase 10), plus the phase 20a
;;  security layer and the phase 20b beacon retargeting.  Four
;;  query lists:
;;
;;   - `code-review-hunkhighlight-queries': the general face set
;;     (definition names, parameters, assignment targets,
;;     UPPER_CASE constants - and, for languages WITHOUT a beacon
;;     entry, literal constant values);
;;   - `code-review-hunkhighlight-test-queries': test-file
;;     extras (test definition names, assertion lines);
;;   - `code-review-hunkhighlight-security-queries' (20a):
;;     security SINKS in warning red (eval/exec, pickle.loads,
;;     os.system, shell=True, verify=False, except-pass,
;;     string-concatenated SQL, innerHTML, Math.random, ...) and
;;     untrusted-input SOURCES in a dimmer dedicated face
;;     (request.*, sys.argv, os.environ, input(), process.env).
;;     Vocabulary from measured rule corpora (bandit, Semgrep
;;     registry, Brakeman, Gosec); the SPARSENESS is the design -
;;     priming stops working when everything is marked.
;;   - `code-review-hunkhighlight-beacon-queries' (20b): the
;;     defect-oriented retargeting - condition subtrees captured
;;     under @_cond, comparison operators strong only inside
;;     conditions, literals strong inside conditions and in test
;;     files / dim outside (see the defcustom docstring).
;;
;;  Every shipped query was PROBED against the INSTALLED grammar
;;  (compile success means nothing): python has `binary_operator'
;;  (not binary_expression), the attribute-call sink queries
;;  anchor on the receiver `object:' so json.loads stays quiet,
;;  and tsx's `jsx_attribute' has NO name field - its first
;;  child is the property_identifier.  The engine merges these
;;  lists and isolates per-query capture failures; this file
;;  holds vocabulary only, no code.

;;; Code:

(defgroup code-review-hunkhighlight nil
  "Tree-sitter semantic highlighting of diff hunks."
  :group 'code-review)

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

(defface code-review-source-face
  '((((class color) (background light))
     :foreground "DarkOrange3" :slant italic)
    (((class color) (background dark))
     :foreground "LightSalmon2" :slant italic)
    (t :slant italic))
  "Face for untrusted-input SOURCE tokens (phase 20a).
Dimmer than the warning-red sinks on purpose: sources mark where
taint ENTERS (request.*, sys.argv, os.environ, input(), ...); the
reviewer's eye should brush over them and stop at the sink where
the taint is USED."
  :group 'code-review-hunkhighlight)

(defface code-review-constant-dim-face
  '((t :inherit code-review-constant-face :weight normal))
  "Dimmer variant of `code-review-constant-face' (phase 20b).
Literals OUTSIDE conditions (log strings, ordinary arguments,
config values) paint with it: eye-tracking of defect finding
shows reviewers lock onto conditions and loops (Sharif, Falcone
& Maletic, ETRA 2012), so the strong face stays reserved for
boundary values and comparison operators INSIDE conditions — and
for everything in test files, where the literal values ARE the
payload."
  :group 'code-review-hunkhighlight)

(defface code-review-changed-token-face
  '((t :underline t))
  "Face for the CHANGED TOKENS of modified added lines (phase 20c).
Whole-line green leaves the reviewer scanning the line for the
delta by hand; most edits are small token edits inside the line
(ChangeDistiller, TSE 2007; GumTree, ASE 2014), so exactly the
changed tokens get an underline.  An underline composes with the
semantic foreground overlays instead of fighting them and leaves
the diff base backgrounds alone (a background tint would replace
the green/red the line needs to keep)."
  :group 'code-review-hunkhighlight)

(defcustom code-review-hunkhighlight-dim-outside-literals t
  "Pick the OUTSIDE-conditions literal treatment (phase 20b).
Non-nil paints them with the dimmer
`code-review-constant-dim-face'; nil leaves them plain (diff
faces only).  Inside conditions (and in TEST files, where the
expected values are the payload) literals and comparison
operators always keep the strong `code-review-constant-face'.
Languages without beacon entries (see
`code-review-hunkhighlight-beacon-queries') are not affected."
  :type 'boolean
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
     ;; literal CONSTANT VALUES are managed by the BEACON list
     ;; below for python (phase 20b: strong inside conditions,
     ;; dim outside, strong everywhere in test files)
     )
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
     ;; literal CONSTANT VALUES are managed by the BEACON list
     ;; below for scala (phase 20b)
     )
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
     ;; literal CONSTANT VALUES are managed by the BEACON list
     ;; below for elisp (phase 20b)
     )
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
     ;; literal CONSTANT VALUES are managed by the BEACON list
     ;; below for clojure (phase 20b)
     )
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
     ;; literal CONSTANT VALUES are managed by the BEACON list
     ;; below for typescript (phase 20b)
     )
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
     ;; literal CONSTANT VALUES are managed by the BEACON list
     ;; below for tsx (phase 20b)
     )
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

(defcustom code-review-hunkhighlight-security-queries
  '((python
     ;; SINKS - dangerous-in-themselves tokens (bandit/Semgrep
     ;; vocabulary; deliberately tiny: priming only works while
     ;; it stays rare - Braz et al. ICSE 2022, Sarkar PPIG 2015).
     ("((call function: (identifier) @sink)
       (#match? @sink \"\\\\`\\\\(eval\\\\|exec\\\\)\\\\'\"))"
      . ((sink . font-lock-warning-face)))
     ;; receiver-anchored attribute calls: the @_o anchor keeps
     ;; the safe twins quiet (json.loads, hashlib.sha256,
     ;; yaml.safe_load)
     ("((call function: (attribute object: (identifier) @_o
                                     attribute: (identifier) @sink))
       (#match? @_o \"\\\\`\\\\(pickle\\\\|os\\\\|yaml\\\\|hashlib\\\\)\\\\'\")
       (#match? @sink \"\\\\`\\\\(loads?\\\\|system\\\\|popen\\\\|unsafe_load\\\\|md5\\\\|sha1\\\\)\\\\'\"))"
      . ((sink . font-lock-warning-face)))
     ;; the FLAG is the defect: shell=True (B602), verify=False
     ;; (requests TLS).  shell=False / verify=True stay quiet.
     ("((call arguments: (argument_list
                          (keyword_argument name: (identifier) @sink
                                            value: (true))))
       (#match? @sink \"\\\\`shell\\\\'\"))"
      . ((sink . font-lock-warning-face)))
     ("((call arguments: (argument_list
                          (keyword_argument name: (identifier) @sink
                                            value: (false))))
       (#match? @sink \"\\\\`verify\\\\'\"))"
      . ((sink . font-lock-warning-face)))
     ;; swallowing handlers (bandit B110): an except whose body
     ;; is ONLY pass - bare or filtered, both swallow.  The
     ;; anchors pin pass_statement as the block's sole named
     ;; child
     ("(except_clause \"except\" @sink (block . (pass_statement) .))"
      . ((sink . font-lock-warning-face)))
     ;; string-concatenated SQL (B608): python's ARITHMETIC
     ;; operator node - comparisons are `comparison_operator' and
     ;; stay quiet by grammar; the predicate requires a SQL
     ;; keyword so plain concatenation stays quiet too
     ("((binary_operator left: (string) @sink)
       (#match? @sink \"\\\\(SELECT\\\\|INSERT\\\\|UPDATE\\\\|DELETE\\\\|REPLACE\\\\)[[:space:]]\"))"
      . ((sink . font-lock-warning-face)))
     ("((binary_operator right: (string) @sink)
       (#match? @sink \"\\\\(SELECT\\\\|INSERT\\\\|UPDATE\\\\|DELETE\\\\|REPLACE\\\\)[[:space:]]\"))"
      . ((sink . font-lock-warning-face)))
     ;; SOURCES - untrusted-input entry points, dimmer face
     ("((attribute object: (identifier) @_o attribute: (identifier) @src)
       (#match? @_o \"\\\\`request\\\\'\"))"
      . ((src . code-review-source-face)))
     ("((attribute object: (identifier) @_o attribute: (identifier) @src)
       (#match? @_o \"\\\\`\\\\(sys\\\\|os\\\\)\\\\'\")
       (#match? @src \"\\\\`\\\\(argv\\\\|environ\\\\)\\\\'\"))"
      . ((src . code-review-source-face)))
     ("((call function: (identifier) @src)
       (#match? @src \"\\\\`input\\\\'\"))"
      . ((src . code-review-source-face))))
    (typescript
     ("((call_expression function: (identifier) @sink)
       (#match? @sink \"\\\\`eval\\\\'\"))"
      . ((sink . font-lock-warning-face)))
     ;; DOM sinks: assignment (and +=) to innerHTML/outerHTML;
     ;; textContent is a safe setter and stays quiet
     ("((assignment_expression left: (member_expression property: (property_identifier) @sink))
       (#match? @sink \"\\\\`\\\\(innerHTML\\\\|outerHTML\\\\)\\\\'\"))"
      . ((sink . font-lock-warning-face)))
     ("((augmented_assignment_expression left: (member_expression property: (property_identifier) @sink))
       (#match? @sink \"\\\\`\\\\(innerHTML\\\\|outerHTML\\\\)\\\\'\"))"
      . ((sink . font-lock-warning-face)))
     ;; Math.random: weak randomness - marked unconditionally
     ;; (auth context is not structurally detectable)
     ("((call_expression function: (member_expression object: (identifier) @_m
                                                    property: (property_identifier) @sink))
       (#match? @_m \"\\\\`Math\\\\'\")
       (#match? @sink \"\\\\`random\\\\'\"))"
      . ((sink . font-lock-warning-face)))
     ("((pair key: (property_identifier) @sink)
       (#match? @sink \"\\\\`dangerouslySetInnerHTML\\\\'\"))"
      . ((sink . font-lock-warning-face)))
     ("((member_expression object: (identifier) @_p property: (property_identifier) @src)
       (#match? @_p \"\\\\`process\\\\'\")
       (#match? @src \"\\\\`\\\\(env\\\\|argv\\\\)\\\\'\"))"
      . ((src . code-review-source-face))))
    (tsx
     ;; the tsx grammar accepts every typescript query above
     ;; (probed: identical captures) plus the JSX sink: a
     ;; jsx_attribute has NO name field, its first child IS the
     ;; property_identifier
     ("((jsx_attribute (property_identifier) @sink)
       (#match? @sink \"\\\\`dangerouslySetInnerHTML\\\\'\"))"
      . ((sink . font-lock-warning-face)))
     ("((call_expression function: (identifier) @sink)
       (#match? @sink \"\\\\`eval\\\\'\"))"
      . ((sink . font-lock-warning-face)))
     ("((assignment_expression left: (member_expression property: (property_identifier) @sink))
       (#match? @sink \"\\\\`\\\\(innerHTML\\\\|outerHTML\\\\)\\\\'\"))"
      . ((sink . font-lock-warning-face)))
     ("((augmented_assignment_expression left: (member_expression property: (property_identifier) @sink))
       (#match? @sink \"\\\\`\\\\(innerHTML\\\\|outerHTML\\\\)\\\\'\"))"
      . ((sink . font-lock-warning-face)))
     ("((call_expression function: (member_expression object: (identifier) @_m
                                                    property: (property_identifier) @sink))
       (#match? @_m \"\\\\`Math\\\\'\")
       (#match? @sink \"\\\\`random\\\\'\"))"
      . ((sink . font-lock-warning-face)))
     ("((pair key: (property_identifier) @sink)
       (#match? @sink \"\\\\`dangerouslySetInnerHTML\\\\'\"))"
      . ((sink . font-lock-warning-face)))
     ("((member_expression object: (identifier) @_p property: (property_identifier) @src)
       (#match? @_p \"\\\\`process\\\\'\")
       (#match? @src \"\\\\`\\\\(env\\\\|argv\\\\)\\\\'\"))"
      . ((src . code-review-source-face)))))
  "Like `code-review-hunkhighlight-queries', but security-shaped (phase 20a).
Merged in EVERY file (not just test files), on top of the general
queries.  SINK captures (dangerous-in-themselves tokens) map to
`font-lock-warning-face' - the SQL @danger precedent; SOURCE
captures (untrusted-input entry points) map to the dimmer
`code-review-source-face', marking where taint ENTERS.

The sparseness rule is the design (Braz et al., ICSE 2022:
security PRIMING alone made vulnerability detection 8x more
likely; Sarkar, PPIG 2015: highlighting effects decay with
expertise): only tokens dangerous in themselves or unambiguous
input boundaries.  The vocabulary comes from measured rule
corpora (bandit, Semgrep registry, Brakeman, Gosec), not
invention, and receiver anchoring keeps the safe twins quiet:
json.loads, hashlib.sha256, yaml.safe_load, shell=False,
verify=True, textContent.  Expect one tuning round: when the
first vocabulary over- or under-shoots, trim HERE - the engine
needs no changes."
  :type '(repeat (cons symbol (repeat (cons string (repeat (cons symbol face))))))
  :group 'code-review-hunkhighlight)

(defcustom code-review-hunkhighlight-beacon-queries
  '((python
     ;; Beacon shapes (all PROBED against the installed grammars,
     ;; 2026-09-30): python if/elif/while carry `condition:' fields;
     ;; comparison operators are anonymous tokens of
     ;; `comparison_operator' (NOT in field position - a quoted
     ;; anonymous token in a field slot fails to compile).
     ("(if_statement condition: (_) @_cond)
       (elif_clause condition: (_) @_cond)
       (while_statement condition: (_) @_cond)
       (comparison_operator [\"==\" \"!=\" \"<\" \">\" \"<=\" \">=\"] @bop)
       [(integer) (float)] @lit
       (string) @lit"
      . ((bop . (code-review-constant-face . nil))
         (lit . (code-review-constant-face . code-review-constant-dim-face)))))
    (typescript
     ;; Beacon shapes (probed): if/while carry `condition:' fields,
     ;; switch arms are `switch_case value:', comparisons are
     ;; anonymous tokens of `binary_expression'.  Ternaries are
     ;; deliberately out: not `conditional_expression' there, and
     ;; the value branches would over-mark (sparseness).
     ("(if_statement condition: (_) @_cond)
       (while_statement condition: (_) @_cond)
       (switch_case value: (_) @_cond)
       (binary_expression [\"==\" \"!=\" \"===\" \"!==\" \"<\" \">\" \"<=\" \">=\"] @bop)
       [(string) (template_string) (number)] @lit"
      . ((bop . (code-review-constant-face . nil))
         (lit . (code-review-constant-face . code-review-constant-dim-face)))))
    (tsx
     ;; the tsx grammar accepts every typescript beacon query above
     ;; (probed: identical captures)
     ("(if_statement condition: (_) @_cond)
       (while_statement condition: (_) @_cond)
       (switch_case value: (_) @_cond)
       (binary_expression [\"==\" \"!=\" \"===\" \"!==\" \"<\" \">\" \"<=\" \">=\"] @bop)
       [(string) (template_string) (number)] @lit"
      . ((bop . (code-review-constant-face . nil))
         (lit . (code-review-constant-face . code-review-constant-dim-face)))))
    (elisp
     ;; Beacon shapes (probed): if/while/cond are `special_form'
     ;; with the head as an anonymous token child - the `.' anchor
     ;; pins the condition to come right after it; cond clause
     ;; tests are the first child of each clause list; when/unless
     ;; are MACROS (plain `list's), matched positionally;
     ;; comparison operators are plain symbols, painted only when
     ;; the classification finds them inside a condition range.
     ("(special_form \"if\" . (_) @_cond)
       (special_form \"while\" . (_) @_cond)
       (special_form \"cond\" (list . (_) @_cond))
       ((list . (symbol) @_h . (_) @_cond)
        (#match? @_h \"\\\\`\\\\(when\\\\|unless\\\\)\\\\'\"))
       ((symbol) @bop
        (#match? @bop \"\\\\`\\\\(=\\\\|<\\\\|<=\\\\|>\\\\|>=\\\\|eq\\\\|equal\\\\|string=\\\\)\\\\'\"))
       [(string) (integer)] @lit"
      . ((bop . (code-review-constant-face . nil))
         (lit . (code-review-constant-face . code-review-constant-dim-face)))))
    (scala
     ;; Beacon shapes (probed): if/else-if/while carry `condition:'
     ;; fields (the parenthesized expressions); infix operators are
     ;; `operator_identifier' nodes (NOT anonymous tokens: "=="/">="
     ;; are not quotable there, only "<" happens to exist), so the
     ;; predicate keeps the comparison spelling only.
     ("(if_expression condition: (_) @_cond)
       (while_expression condition: (_) @_cond)
       ((infix_expression (operator_identifier) @bop)
        (#match? @bop \"\\\\`\\\\(==\\\\|!=\\\\|<=\\\\|>=\\\\|<\\\\|>\\\\|eq\\\\|ne\\\\|lt\\\\|gt\\\\|le\\\\|ge\\\\)\\\\'\"))
       [(integer_literal) (floating_point_literal)] @lit
       (string) @lit"
      . ((bop . (code-review-constant-face . nil))
         (lit . (code-review-constant-face . code-review-constant-dim-face)))))
    (clojure
     ;; Beacon shapes (probed): everything is list_lit of sym_lits,
     ;; so if/when/when-not/while/if-not conditions are matched
     ;; positionally after the head symbol; comparison operators are
     ;; plain sym_lits, painted only inside condition ranges.
     ("((list_lit . (sym_lit) @_h . (_) @_cond)
        (#match? @_h \"\\\\`\\\\(if\\\\|when\\\\|when-not\\\\|while\\\\|if-not\\\\)\\\\'\"))
       ((sym_lit) @bop
        (#match? @bop \"\\\\`\\\\(=\\\\|<\\\\|<=\\\\|>\\\\|>=\\\\|==\\\\|not=\\\\)\\\\'\"))
       [(str_lit) (num_lit)] @lit"
      . ((bop . (code-review-constant-face . nil))
         (lit . (code-review-constant-face . code-review-constant-dim-face)))))
    (sql
     ;; Beacon shapes (probed): comparisons are anonymous tokens of
     ;; `binary_expression'; the condition ranges are the whole
     ;; WHERE clauses (already the phase 10 fixation targets); SQL
     ;; literals are a single `literal' node (numbers AND strings -
     ;; no separate `number'/'string' nodes exist).
     ("(where) @_cond
       (binary_expression [\"=\" \"<>\" \"!=\" \"<\" \">\" \"<=\" \">=\"] @bop)
       (literal) @lit"
      . ((bop . (code-review-constant-face . nil))
         (lit . (code-review-constant-face . code-review-constant-dim-face))))))
  "Like `code-review-hunkhighlight-queries', but beacon-shaped (phase 20b).
Eye-tracking of defect finding shows reviewers scan broadly, then
LOCK fixations onto the conditions and loops where the defects
live (Sharif, Falcone & Maletic, ETRA 2012, replicating Uwano et
al. 2006) - while the phase 10 face set made EVERY literal loud
(691 of 959 overlays on scalafmt#5264 were constant-face
literals: the loudest face pointed at the least defect-relevant
tokens).  Beacon entries RETARGET that emphasis; they add no new
keyword volume.

Entry structure (a mapping value is a CONS (STRONG . DIM) instead
of a plain face):
  - the CONDITION subtrees of if/while/case clauses are captured
    under the helper name @_cond (leading underscore: no face
    mapping; the engine collects the ranges and classifies by
    range containment, so beacons at any nesting depth inside the
    condition work without ancestor walking);
  - comparison OPERATORS map to (STRONG . nil): the strong
    `code-review-constant-face' inside a condition, NOTHING
    outside (the < vs <= distinction is the classic off-by-one
    habitat);
  - LITERALS map to (STRONG . DIM): strong inside conditions and
    in TEST files (expected values are the payload there), the
    dimmer `code-review-constant-dim-face' outside - or plain,
    see `code-review-hunkhighlight-dim-outside-literals'.

The general queries of these languages carry no literal entries:
the beacon list owns them.  Languages without entries (yaml)
keep their phase 10 shape; add per language when a real PR needs
one, node names PROBED against the installed grammar first (see
`code-review-hunkhighlight-queries')."
  :type '(repeat (cons symbol
                       (repeat (cons string
                                     (repeat (cons symbol
                                                   (choice face
                                                           (cons face face))))))))
  :group 'code-review-hunkhighlight)

(provide 'code-review-hunkhighlight-queries)
;;; code-review-hunkhighlight-queries.el ends here
