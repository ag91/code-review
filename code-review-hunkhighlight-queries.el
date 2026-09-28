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
;;  security layer.  Three query lists:
;;
;;   - `code-review-hunkhighlight-queries': the general face set
;;     (definition names, parameters, assignment targets,
;;     UPPER_CASE constants, literal constant values);
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

(provide 'code-review-hunkhighlight-queries)
;;; code-review-hunkhighlight-queries.el ends here
