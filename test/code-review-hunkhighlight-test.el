;;; code-review-hunkhighlight-test.el --- ERT tests -*- lexical-binding: t; -*-
;;
;; Phase 10 tests: hunk semantic highlighting.  The pure mapping
;; functions run everywhere; the treesit tests skip themselves
;; when the python grammar is not installed.

(require 'ert)
(require 'cl-lib)
(require 'code-review-hunkhighlight)

(defmacro code-review-hunkhighlight-test--with-hunk (lines &rest body)
  "Insert LINES (prefixed diff lines) into a temp buffer.
The first char of each line gets the magit-diff-added base face,
like the diff wash paints it.  BODY runs with `beg'/`end'
bound to the hunk body region and `crhh-buffer' to the buffer."
  (declare (indent 1))
  `(with-temp-buffer
     (dolist (l ,lines)
       (insert l)
       (put-text-property
        (line-beginning-position) (line-end-position)
        'font-lock-face
        (pcase (substring l 0 1)
          ("+" 'magit-diff-added)
          ("-" 'magit-diff-removed)
          (_ 'magit-diff-context)))
       (insert "\n"))
     (let* ((beg (point-min))
            (end (point-max))
            (crhh-buffer (current-buffer)))
       ,@body)))

(defun code-review-hunkhighlight-test--face-at (str)
  "Return the faces at the last char of the first occurrence of
STR: the diff base face (text property) plus every semantic
overlay face at that position."
  (save-excursion
    (goto-char (point-min))
    (search-forward str)
    ;; NB: search-forward does NOT set match data; use point.
    (let ((p (1- (point))))
      (append (if (listp (get-text-property p 'font-lock-face))
                  (get-text-property p 'font-lock-face)
                (list (get-text-property p 'font-lock-face)))
              (mapcar (lambda (ov) (overlay-get ov 'face))
                      (overlays-at p))))))

;;; Pure mapping

(ert-deftest code-review-hunkhighlight/reconstruct-new-side ()
  (code-review-hunkhighlight-test--with-hunk
      '("+import os"
        "-import sys"
        " def kept():"
        "+    return 1"
        "\\ No newline at end of file")
    (let ((recon (code-review-hunkhighlight--reconstruct beg end)))
      (should (equal (nth 0 recon)
                     "import os\ndef kept():\n    return 1"))
      ;; mapping skips the - and \\ lines, prefix excluded
      (should (equal (mapcar (lambda (p) (buffer-substring-no-properties
                                           (car p) (cdr p)))
                             (nth 1 recon))
                     '("import os" "def kept():" "    return 1"))))))

(ert-deftest code-review-hunkhighlight/node-to-lines ()
  ;; one line "abcdef" (bol 1), two lines "ab\ncd" (bols 1 and 4)
  (should (equal (code-review-hunkhighlight--node-to-lines
                  2 4 '(1 4) '(2 2))
                 '((1 1 2))))
  (should (equal (code-review-hunkhighlight--node-to-lines
                  1 6 '(1 4) '(2 2))
                 '((1 0 2) (2 0 2))))
  ;; node fully before / after lines
  (should (null (code-review-hunkhighlight--node-to-lines
                 8 9 '(1 4) '(2 2)))))

(ert-deftest code-review-hunkhighlight/old-style-query-rewrite ()
  ;; Emacs 30 contract: `#match? @cap "REG"' -> `#match "REG" @cap'.
  ;; The REGEXP literal keeps its escaped characters intact.
  (should (equal
           (code-review-hunkhighlight--old-style-query
            "((assignment left: (identifier) @const)
       (#match? @const \"\\\\`[A-Z_][A-Z0-9_]*\\\\'\"))")
           "((assignment left: (identifier) @const)
       (#match \"\\\\`[A-Z_][A-Z0-9_]*\\\\'\" @const))"))
  ;; multiple predicates, underscore/hyphen captures, regexps with
  ;; alternation escapes
  (should (equal
           (code-review-hunkhighlight--old-style-query
            "((call function: (identifier) @_h)
  (#match? @_h \"\\\\`\\\\(defn\\\\|defonce\\\\)\\\\'\")
  (#match? @_h \"x\"))")
           "((call function: (identifier) @_h)
  (#match \"\\\\`\\\\(defn\\\\|defonce\\\\)\\\\'\" @_h)
  (#match \"x\" @_h))"))
  ;; queries without predicates pass through unchanged
  (should (equal
           (code-review-hunkhighlight--old-style-query
            "(function_definition name: (identifier) @fn)")
           "(function_definition name: (identifier) @fn)")))

;;; treesit integration (skipped without the python grammar)

(ert-deftest code-review-hunkhighlight/python-hunk-faces ()
  (skip-unless (and (fboundp 'treesit-language-available-p)
                    (treesit-language-available-p 'python)))
  (code-review-hunkhighlight-test--with-hunk
      '("+import os"
        "+MAX_RETRIES = 3"
        "+def foo(a):"
        "    kept = 1"
        "+    total = a"
        "+    msg = \"hi\""
        "+    return a")
    (should (code-review-hunkhighlight-region beg end "src/foo.py"))
    ;; minimal set: def names, parameters, assignment targets,
    ;; constants, and `return' only — NOT every keyword
    (should (memq 'font-lock-function-name-face
                  (code-review-hunkhighlight-test--face-at "foo")))
    ;; helper reads the last char of the match: "(a" ends on the
    ;; parameter identifier itself
    (should (memq 'font-lock-variable-name-face
                  (code-review-hunkhighlight-test--face-at "(a")))
    (should (memq 'font-lock-variable-name-face
                  (code-review-hunkhighlight-test--face-at "total")))
    (should (memq 'code-review-constant-face
                  (code-review-hunkhighlight-test--face-at "MAX_RETRIES")))
    ;; UPPER_CASE is a constant, not a plain variable
    (should-not (memq 'font-lock-variable-name-face
                      (code-review-hunkhighlight-test--face-at "MAX_RETRIES")))
    ;; literal constant VALUES outside conditions are DIM now (phase
    ;; 20b): the strong face is reserved for conditions (see the
    ;; beacon tests below)
    (should (memq 'code-review-constant-dim-face
                  (code-review-hunkhighlight-test--face-at "ES = 3")))
    (should (memq 'code-review-constant-dim-face
                  (code-review-hunkhighlight-test--face-at "msg = \"hi")))
    (should-not (memq 'code-review-constant-face
                      (code-review-hunkhighlight-test--face-at "msg = \"hi")))
    ;; return is the one keyword we keep
    (should (memq 'font-lock-keyword-face
                  (code-review-hunkhighlight-test--face-at "return")))
    ;; ...but import is not: less is better
    (should-not (memq 'font-lock-keyword-face
                      (code-review-hunkhighlight-test--face-at "import")))
    ;; ADDED lines only: the context line gets no semantic face
    (should-not (memq 'font-lock-variable-name-face
                      (code-review-hunkhighlight-test--face-at "kept")))
    ;; the diff base face stays as the text property underneath
    (should (memq 'magit-diff-added
                  (code-review-hunkhighlight-test--face-at "foo")))))

(ert-deftest code-review-hunkhighlight/scala-hunk-faces ()
  (skip-unless (and (fboundp 'treesit-language-available-p)
                    (treesit-language-available-p 'scala)))
  (code-review-hunkhighlight-test--with-hunk
      '("+import org.scalafmt._"
        "+class Foo(a: Int) {"
        "+  def bar(x: Int): Int ="
        "+    val total = x"
        "+    return total"
        "+}"
        "+test(\"some test name\") {")
    (should (code-review-hunkhighlight-region beg end "src/Foo.scala"))
    (should (memq 'font-lock-type-face
                  (code-review-hunkhighlight-test--face-at "Foo")))
    (should (memq 'font-lock-function-name-face
                  (code-review-hunkhighlight-test--face-at "bar")))
    (should (memq 'font-lock-variable-name-face
                  (code-review-hunkhighlight-test--face-at "bar(x")))
    (should (memq 'font-lock-variable-name-face
                  (code-review-hunkhighlight-test--face-at "total")))
    (should (memq 'font-lock-keyword-face
                  (code-review-hunkhighlight-test--face-at "return")))
    ;; the test case NAME string: a literal outside any condition —
    ;; DIM now (phase 20b)
    (should (memq 'code-review-constant-dim-face
                  (code-review-hunkhighlight-test--face-at "some test name")))
    (should-not (memq 'code-review-constant-face
                      (code-review-hunkhighlight-test--face-at
                       "some test name")))))

(ert-deftest code-review-hunkhighlight/elisp-hunk-faces ()
  (skip-unless (and (fboundp 'treesit-language-available-p)
                    (treesit-language-available-p 'elisp)))
  (code-review-hunkhighlight-test--with-hunk
      '("+(defun my-fun (a b)"
        "+  \"docstring\""
        "+  (let ((x 1)) (list a b x)))"
        "+(defconst MY-CONST 10)"
        "+(defvar my-var nil)")
    (should (code-review-hunkhighlight-region beg end "code-review-fun.el"))
    (should (memq 'font-lock-function-name-face
                  (code-review-hunkhighlight-test--face-at "my-fun")))
    (should (memq 'font-lock-variable-name-face
                  (code-review-hunkhighlight-test--face-at "(a")))
    ;; let-bound symbols are variables
    (should (memq 'font-lock-variable-name-face
                  (code-review-hunkhighlight-test--face-at "((x")))
    ;; literals: docstring and let-bound VALUES are outside any
    ;; condition — DIM now (phase 20b); MY-CONST (a defconst NAME)
    ;; keeps the strong constant face
    (should (memq 'code-review-constant-face
                  (code-review-hunkhighlight-test--face-at "MY-CONST")))
    (should (memq 'code-review-constant-dim-face
                  (code-review-hunkhighlight-test--face-at "\"docstring")))
    ;; plain calls stay diff-colored
    (should-not (memq 'font-lock-function-name-face
                      (code-review-hunkhighlight-test--face-at "list a")))))

(ert-deftest code-review-hunkhighlight/clojure-hunk-faces ()
  (skip-unless (and (fboundp 'treesit-language-available-p)
                    (treesit-language-available-p 'clojure)))
  (code-review-hunkhighlight-test--with-hunk
      '("+(def x 1)"
        "+(defn foo [a b]"
        "+  (+ a b 2))"
        "+(defonce c \"val\")"
        "+(let [q 3] q)")
    (should (code-review-hunkhighlight-region beg end "src/my/ns.clj"))
    ;; defn name and its params
    (should (memq 'font-lock-function-name-face
                  (code-review-hunkhighlight-test--face-at "foo")))
    (should (memq 'font-lock-variable-name-face
                  (code-review-hunkhighlight-test--face-at "[a")))
    ;; def/defonce: variable vs function
    (should (memq 'font-lock-variable-name-face
                  (code-review-hunkhighlight-test--face-at "def x")))
    (should (memq 'font-lock-function-name-face
                  (code-review-hunkhighlight-test--face-at "defonce c")))
    ;; let bindings are variables
    (should (memq 'font-lock-variable-name-face
                  (code-review-hunkhighlight-test--face-at "[q")))
    ;; literals: outside any condition — DIM now (phase 20b);
    ;; test-file literals keep the strong face (separate test below)
    (should (memq 'code-review-constant-dim-face
                  (code-review-hunkhighlight-test--face-at "\"val\"")))
    ;; plain calls stay diff-colored
    (should-not (memq 'font-lock-function-name-face
                      (code-review-hunkhighlight-test--face-at "+ a")))))

(ert-deftest code-review-hunkhighlight/typescript-hunk-faces ()
  (skip-unless (and (fboundp 'treesit-language-available-p)
                    (treesit-language-available-p 'typescript)))
  (code-review-hunkhighlight-test--with-hunk
      '("+import { expect } from 'vitest';"
        "+const MAX_ROWS = 5000;"
        "+let comparedColumns = 0;"
        "+interface BillingEvent { id: number; }"
        "+class Oracle {"
        "+  amend(input: string): string {"
        "+    for (const row of rows) {"
        "+      return row;"
        "+    }"
        "+    switch (input) {"
        "+      case 'add':"
        "+        return 'added';"
        "+    }"
        "+  }"
        "+}"
        "+function computeTotal(base: number) {"
        "+  return base;"
        "+}")
    (should (code-review-hunkhighlight-region beg end "src/oracle.ts"))
    ;; type-shaped definitions: interface, class
    (should (memq 'font-lock-type-face
                  (code-review-hunkhighlight-test--face-at "BillingEvent")))
    (should (memq 'font-lock-type-face
                  (code-review-hunkhighlight-test--face-at "Oracle")))
    ;; function and method names
    (should (memq 'font-lock-function-name-face
                  (code-review-hunkhighlight-test--face-at "computeTotal")))
    (should (memq 'font-lock-function-name-face
                  (code-review-hunkhighlight-test--face-at "amend")))
    ;; parameters (method and function)
    (should (memq 'font-lock-variable-name-face
                  (code-review-hunkhighlight-test--face-at "amend(input")))
    (should (memq 'font-lock-variable-name-face
                  (code-review-hunkhighlight-test--face-at "computeTotal(base")))
    ;; UPPER_CASE declaration = constant, not a variable
    (should (memq 'code-review-constant-face
                  (code-review-hunkhighlight-test--face-at "MAX_ROWS")))
    (should-not (memq 'font-lock-variable-name-face
                      (code-review-hunkhighlight-test--face-at "MAX_ROWS")))
    ;; lowercase declarations and for-of bindings are variables
    (should (memq 'font-lock-variable-name-face
                  (code-review-hunkhighlight-test--face-at "comparedColumns")))
    (should (memq 'font-lock-variable-name-face
                  (code-review-hunkhighlight-test--face-at "const row")))
    ;; switch arms: matched value (type face) + `case' keyword
    (should (memq 'font-lock-type-face
                  (code-review-hunkhighlight-test--face-at "case '")))
    (should (memq 'font-lock-keyword-face
                  (code-review-hunkhighlight-test--face-at "case")))
    ;; return is the one keyword we keep; import is not
    (should (memq 'font-lock-keyword-face
                  (code-review-hunkhighlight-test--face-at "return")))
    (should-not (memq 'font-lock-keyword-face
                      (code-review-hunkhighlight-test--face-at "import")))
    ;; literal constant values outside conditions: DIM now (phase
    ;; 20b) — `'vitest'` sits in an import, 5000 in a plain const
    (should (memq 'code-review-constant-dim-face
                  (code-review-hunkhighlight-test--face-at "'vitest'")))
    (should (memq 'code-review-constant-dim-face
                  (code-review-hunkhighlight-test--face-at "MAX_ROWS = 5000")))
    ;; `'add'` is a switch-case VALUE — a condition by the phase 20b
    ;; beacon — so it keeps the STRONG face
    (should (memq 'code-review-constant-face
                  (code-review-hunkhighlight-test--face-at "case 'add'")))
    ;; `'added'` (a plain return value) is dim
    (should (memq 'code-review-constant-dim-face
                  (code-review-hunkhighlight-test--face-at "return 'added'")))
    ;; the diff base face stays underneath
    (should (memq 'magit-diff-added
                  (code-review-hunkhighlight-test--face-at "computeTotal")))))

(ert-deftest code-review-hunkhighlight/typescript-test-file-faces ()
  (skip-unless (and (fboundp 'treesit-language-available-p)
                    (treesit-language-available-p 'typescript)))
  (code-review-hunkhighlight-test--with-hunk
      '("+describe('billing exclusions', () => {"
        "+  it('applies v1 amendments', () => {"
        "+    expect(total).toBe(42);"
        "+  });"
        "+});")
    (should (code-review-hunkhighlight-region beg end "e2e/oracle.test.ts"))
    ;; jest/vitest test definitions and assertions
    (should (memq 'code-review-test-face
                  (code-review-hunkhighlight-test--face-at "describe")))
    (should (memq 'code-review-test-face
                  (code-review-hunkhighlight-test--face-at "it")))
    (should (memq 'code-review-test-face
                  (code-review-hunkhighlight-test--face-at "expect")))
    ;; the test-case NAME string is a constant value on top
    (should (memq 'code-review-constant-face
                  (code-review-hunkhighlight-test--face-at "applies v1 amendments")))))

(ert-deftest code-review-hunkhighlight/tsx-hunk-faces ()
  (skip-unless (and (fboundp 'treesit-language-available-p)
                    (treesit-language-available-p 'tsx)))
  (code-review-hunkhighlight-test--with-hunk
      '("+const cmp = (a: number) => {"
        "+  return a;"
        "+};")
    (should (code-review-hunkhighlight-region beg end "src/widget.tsx"))
    (should (memq 'font-lock-variable-name-face
                  (code-review-hunkhighlight-test--face-at "cmp")))
    (should (memq 'font-lock-variable-name-face
                  (code-review-hunkhighlight-test--face-at "(a")))
    (should (memq 'font-lock-keyword-face
                  (code-review-hunkhighlight-test--face-at "return")))))

(ert-deftest code-review-hunkhighlight/sql-hunk-faces ()
  (skip-unless (and (fboundp 'treesit-language-available-p)
                    (treesit-language-available-p 'sql)))
  (code-review-hunkhighlight-test--with-hunk
      '("+CREATE TABLE IF NOT EXISTS analytics.estimate_ledger_prices ("
        "+  organization_id UInt64, is_deleted UInt8 DEFAULT 0);"
        "+SELECT a.organization_id, b.event_type"
        "+FROM analytics.t1 AS a INNER JOIN t2 AS b ON a.id = b.id CROSS JOIN t3"
        "+WHERE a.x > 10 AND b.y = 'z'"
        "+GROUP BY a.organization_id")
    (should (code-review-hunkhighlight-region beg end "models/ledger.sql"))
    ;; CREATE TABLE: table reference as a type, columns as variables
    (should (memq 'font-lock-type-face
                  (code-review-hunkhighlight-test--face-at "estimate_ledger_prices")))
    (should (memq 'font-lock-variable-name-face
                  (code-review-hunkhighlight-test--face-at "organization_id")))
    ;; selected fields
    (should (memq 'font-lock-variable-name-face
                  (code-review-hunkhighlight-test--face-at "a.organization_id")))
    ;; FROM/JOIN table references
    (should (memq 'font-lock-type-face
                  (code-review-hunkhighlight-test--face-at "analytics.t1")))
    (should (memq 'font-lock-type-face
                  (code-review-hunkhighlight-test--face-at "JOIN t2")))
    ;; CROSS JOIN is terrible red
    (should (memq 'font-lock-warning-face
                  (code-review-hunkhighlight-test--face-at "CROSS")))
    ;; WHERE: keyword plus the filter columns
    (should (memq 'font-lock-keyword-face
                  (code-review-hunkhighlight-test--face-at "WHERE")))
    (should (memq 'font-lock-variable-name-face
                  (code-review-hunkhighlight-test--face-at "a.x")))
    ;; GROUP BY columns too
    (should (memq 'font-lock-variable-name-face
                  (code-review-hunkhighlight-test--face-at "GROUP BY a.organization_id")))))

(ert-deftest code-review-hunkhighlight/sql-dbt-jinja-faces ()
  (skip-unless (and (fboundp 'treesit-language-available-p)
                    (treesit-language-available-p 'sql)))
  (code-review-hunkhighlight-test--with-hunk
      '("+CREATE TABLE IF NOT EXISTS {{ env_var('DB') }}.estimate_ledger_prices ("
        "+  organization_id UInt64, event_type String);"
        "+SELECT a.id, b.type"
        "+FROM {{ ref('mrt_one') }} AS a"
        "+CROSS JOIN {{ source('db', 'tbl') }}"
        "+WHERE a.x > 0;"
        "+SELECT id FROM t1 AS a, t2 AS b WHERE a.id = b.id")
    (should (code-review-hunkhighlight-region beg end "models/ledger.sql"))
    ;; jinja DDL degrades gracefully: the parse is ERROR soup
    ;; around {{ }} but the clean columns still highlight
    (should (memq 'font-lock-variable-name-face
                  (code-review-hunkhighlight-test--face-at "organization_id")))
    ;; dbt ref()/source() model names survive the jinja errors and
    ;; highlight as table sources
    (should (memq 'font-lock-type-face
                  (code-review-hunkhighlight-test--face-at "mrt_one")))
    ;; jinja CROSS JOIN is still terrible red
    (should (memq 'font-lock-warning-face
                  (code-review-hunkhighlight-test--face-at "CROSS")))
    ;; implicit comma join: the comma itself is red (a cartesian
    ;; product in ClickHouse)
    (should (memq 'font-lock-warning-face
                  (code-review-hunkhighlight-test--face-at "AS a,")))))

(ert-deftest code-review-hunkhighlight/yaml-hunk-faces ()
  (skip-unless (and (fboundp 'treesit-language-available-p)
                    (treesit-language-available-p 'yaml)))
  (code-review-hunkhighlight-test--with-hunk
      '("+version: 2"
        "+models:"
        "+  - name: mrt_estimate_ledger"
        "+    description: estimates"
        "+    columns:"
        "+      - name: organization_id"
        "+        data_type: UInt64")
    (should (code-review-hunkhighlight-region beg end "models/schema.yml"))
    ;; `name:' values stand out: dbt model and column names
    (should (memq 'font-lock-function-name-face
                  (code-review-hunkhighlight-test--face-at "mrt_estimate_ledger")))
    (should (memq 'font-lock-function-name-face
                  (code-review-hunkhighlight-test--face-at "organization_id")))
    ;; other values and the keys themselves stay quiet
    (should-not (memq 'font-lock-function-name-face
                      (code-review-hunkhighlight-test--face-at "estimates")))
    (should-not (memq 'font-lock-function-name-face
                      (code-review-hunkhighlight-test--face-at "UInt64")))))

(ert-deftest code-review-hunkhighlight/yaml-mid-block-fragment-faces ()
  ;; the shape that breaks whole-fragment parsing: a hunk starting
  ;; mid-column-block (deep indent), then a blank line, then a
  ;; model at shallower depth — the yaml grammar's error recovery
  ;; keeps only the first pair (probed live on a real _marts.yml
  ;; hunk).  The per-line strategy
  ;; (`code-review-hunkhighlight-fragment-line-languages') must
  ;; recover the name values the reviewer actually scans for.
  (skip-unless (and (fboundp 'treesit-language-available-p)
                    (treesit-language-available-p 'yaml)))
  (code-review-hunkhighlight-test--with-hunk
      '("       - name: output_tokens"
        "+        description: \"sum(output_tokens)\""
        "+"
        "+  - name: mrt_estimate_ledger"
        "+    description: >"
        "+      Billable usage."
        "+    columns:"
        "+      - name: organization_id")
    (should (code-review-hunkhighlight-region beg end "models/marts/_marts.yml"))
    ;; the ADDED mid-block model name paints despite the fragment
    (should (memq 'font-lock-function-name-face
                  (code-review-hunkhighlight-test--face-at "mrt_estimate_ledger")))
    ;; nested column names paint too
    (should (memq 'font-lock-function-name-face
                  (code-review-hunkhighlight-test--face-at "organization_id")))
    ;; the CONTEXT first pair never paints (only added lines do)
    (should-not (memq 'font-lock-function-name-face
                      (code-review-hunkhighlight-test--face-at "output_tokens")))
    ;; keys and other values stay quiet
    (should-not (memq 'font-lock-function-name-face
                      (code-review-hunkhighlight-test--face-at "Billable")))))

(ert-deftest code-review-hunkhighlight/test-file-faces ()
  (skip-unless (and (fboundp 'treesit-language-available-p)
                    (treesit-language-available-p 'python)))
  (code-review-hunkhighlight-test--with-hunk
      '("+def test_bar():"
        "+    assert bar(1)"
        "+    self.assertEqual(1, 1)")
    (should (code-review-hunkhighlight-region beg end "tests/test_bar.py"))
    (should (memq 'code-review-test-face
                  (code-review-hunkhighlight-test--face-at "test_bar")))
    ;; match must end inside the captured `assert' keyword
    (should (memq 'code-review-test-face
                  (code-review-hunkhighlight-test--face-at "assert")))
    (should (memq 'code-review-test-face
                  (code-review-hunkhighlight-test--face-at "assertEqual")))
    ;; the plain function face also applies (general queries ran)
    (should (memq 'font-lock-function-name-face
                  (code-review-hunkhighlight-test--face-at "test_bar")))))

(ert-deftest code-review-hunkhighlight/non-test-file-no-test-face ()
  (skip-unless (and (fboundp 'treesit-language-available-p)
                    (treesit-language-available-p 'python)))
  (code-review-hunkhighlight-test--with-hunk
      '("+def helper():"
        "+    assert helper()")
    (should (code-review-hunkhighlight-region beg end "src/helper.py"))
    ;; assertions stand out only in TEST files: in regular source
    ;; `assert' is not in the minimal set, so no face at all here
    (should-not (memq 'font-lock-keyword-face
                      (code-review-hunkhighlight-test--face-at "assert")))
    (should-not (memq 'code-review-test-face
                      (code-review-hunkhighlight-test--face-at "assert")))))

(ert-deftest code-review-hunkhighlight/graceful-degradation ()
  (code-review-hunkhighlight-test--with-hunk
      '("+def foo(a):"
        "+    return a")
    ;; unknown language: no faces, no error
    (should-not (code-review-hunkhighlight-region beg end "README.md"))
    ;; disabled: no-op
    (let ((code-review-semantic-highlight nil))
      (should-not (code-review-hunkhighlight-region beg end "foo.py")))
    (should (memq 'magit-diff-added
                  (code-review-hunkhighlight-test--face-at "def")))))

(ert-deftest code-review-hunkhighlight/cache-replay ()
  (skip-unless (and (fboundp 'treesit-language-available-p)
                    (treesit-language-available-p 'python)))
  (code-review-hunkhighlight-test--with-hunk
      '("+def foo(a):"
        "+    return a")
    ;; first run: parses and caches
    (should (code-review-hunkhighlight-region beg end "foo.py"))
    (let ((cache code-review-hunkhighlight--cache))
      (should cache)
      (should (cl-some (lambda (v) (and (listp v) v))
                       (hash-table-values cache)))
      ;; second run on the SAME text: served from the cache, faces applied
      (should (code-review-hunkhighlight-region beg end "foo.py"))
      (should (memq 'font-lock-function-name-face
                    (code-review-hunkhighlight-test--face-at "foo"))))))

;;; Phase 20a: security source/sink layer

(ert-deftest code-review-hunkhighlight/security-sinks-python ()
  (skip-unless (and (fboundp 'treesit-language-available-p)
                    (treesit-language-available-p 'python)))
  (code-review-hunkhighlight-test--with-hunk
      '("+import os, pickle, hashlib, subprocess, requests, yaml"
        "+def handle(request, uid):"
        "+    eval(user_input)"
        "+    exec(code_str)"
        "+    os.system(cmd)"
        "+    pickle.loads(blob)"
        "+    hashlib.md5(data)"
        "+    hashlib.sha256(data)"
        "+    subprocess.run(cmd, shell=True)"
        "+    subprocess.run(cmd, shell=False)"
        "+    requests.get(url, verify=False)"
        "+    yaml.load(raw)"
        "+    yaml.safe_load(raw)"
        "+    try:"
        "+        risky()"
        "+    except:"
        "+        pass"
        "+    q = \"SELECT * FROM t\" + uid")
    (should (code-review-hunkhighlight-region beg end "src/handler.py"))
    ;; sinks paint warning red on the dangerous token itself
    (should (memq 'font-lock-warning-face
                  (code-review-hunkhighlight-test--face-at "eval")))
    (should (memq 'font-lock-warning-face
                  (code-review-hunkhighlight-test--face-at "exec")))
    (should (memq 'font-lock-warning-face
                  (code-review-hunkhighlight-test--face-at "system")))
    (should (memq 'font-lock-warning-face
                  (code-review-hunkhighlight-test--face-at "loads")))
    (should (memq 'font-lock-warning-face
                  (code-review-hunkhighlight-test--face-at "md5")))
    (should (memq 'font-lock-warning-face
                  (code-review-hunkhighlight-test--face-at "shell")))
    (should (memq 'font-lock-warning-face
                  (code-review-hunkhighlight-test--face-at "verify")))
    ;; swallowing handler: the `except' keyword itself
    (should (memq 'font-lock-warning-face
                  (code-review-hunkhighlight-test--face-at "except")))
    ;; string-concatenated SQL: the fragment goes red
    (should (memq 'font-lock-warning-face
                  (code-review-hunkhighlight-test--face-at "SELECT")))
    ;; SPARSENESS: the safe twins stay quiet (receiver anchoring
    ;; and value pinning do the discriminating)
    (should-not (memq 'font-lock-warning-face
                      (code-review-hunkhighlight-test--face-at "sha256")))
    (should-not (memq 'font-lock-warning-face
                      (code-review-hunkhighlight-test--face-at "shell=False")))
    (should-not (memq 'font-lock-warning-face
                      (code-review-hunkhighlight-test--face-at "safe_load")))))

(ert-deftest code-review-hunkhighlight/security-sources-python ()
  (skip-unless (and (fboundp 'treesit-language-available-p)
                    (treesit-language-available-p 'python)))
  (code-review-hunkhighlight-test--with-hunk
      '("+import sys, os"
        "+def handle(request):"
        "+    q = request.args.get(\"q\")"
        "+    x = sys.argv[1]"
        "+    y = os.environ[\"HOME\"]"
        "+    n = input(\"give: \")"
        "+    plain = local.get(\"x\")")
    (should (code-review-hunkhighlight-region beg end "src/handler.py"))
    ;; untrusted-input entry points get the dimmer source face
    (should (memq 'code-review-source-face
                  (code-review-hunkhighlight-test--face-at "args")))
    (should (memq 'code-review-source-face
                  (code-review-hunkhighlight-test--face-at "argv")))
    (should (memq 'code-review-source-face
                  (code-review-hunkhighlight-test--face-at "environ")))
    (should (memq 'code-review-source-face
                  (code-review-hunkhighlight-test--face-at "input")))
    ;; not every attribute access is an input boundary
    (should-not (memq 'code-review-source-face
                      (code-review-hunkhighlight-test--face-at "local")))))

(ert-deftest code-review-hunkhighlight/security-sinks-typescript ()
  (skip-unless (and (fboundp 'treesit-language-available-p)
                    (treesit-language-available-p 'typescript)))
  (code-review-hunkhighlight-test--with-hunk
      '("+const token = Math.random();"
        "+el.innerHTML = userInput;"
        "+el.outerHTML += more;"
        "+el.textContent = safe;"
        "+eval(code);"
        "+const cfg = { dangerouslySetInnerHTML: { __html: html } };"
        "+const key = process.env.API_KEY;")
    (should (code-review-hunkhighlight-region beg end "src/handler.ts"))
    ;; weak randomness, DOM sinks, eval, react escape hatch
    (should (memq 'font-lock-warning-face
                  (code-review-hunkhighlight-test--face-at "random")))
    (should (memq 'font-lock-warning-face
                  (code-review-hunkhighlight-test--face-at "innerHTML")))
    (should (memq 'font-lock-warning-face
                  (code-review-hunkhighlight-test--face-at "outerHTML")))
    (should (memq 'font-lock-warning-face
                  (code-review-hunkhighlight-test--face-at "eval")))
    (should (memq 'font-lock-warning-face
                  (code-review-hunkhighlight-test--face-at "dangerouslySetInnerHTML")))
    ;; textContent is the safe setter and stays quiet
    (should-not (memq 'font-lock-warning-face
                      (code-review-hunkhighlight-test--face-at "textContent")))
    ;; the untrusted-input source face
    (should (memq 'code-review-source-face
                  (code-review-hunkhighlight-test--face-at "env")))))

(ert-deftest code-review-hunkhighlight/security-sinks-tsx ()
  (skip-unless (and (fboundp 'treesit-language-available-p)
                    (treesit-language-available-p 'tsx)))
  (code-review-hunkhighlight-test--with-hunk
      '("+const App = () => ("
        "+  <div dangerouslySetInnerHTML={{ __html: userHtml }} className=\"ok\">"
        "+    <span>{safe}</span>"
        "+  </div>"
        "+);")
    (should (code-review-hunkhighlight-region beg end "src/widget.tsx"))
    ;; the JSX sink (a jsx_attribute has NO name field: the first
    ;; child IS the property_identifier)
    (should (memq 'font-lock-warning-face
                  (code-review-hunkhighlight-test--face-at "dangerouslySetInnerHTML")))
    (should-not (memq 'font-lock-warning-face
                      (code-review-hunkhighlight-test--face-at "className")))))

(ert-deftest code-review-hunkhighlight/security-cache-invalidation ()
  (skip-unless (and (fboundp 'treesit-language-available-p)
                    (treesit-language-available-p 'python)))
  (code-review-hunkhighlight-test--with-hunk
      '("+import os"
        "+def f(x):"
        "+    eval(x)")
    ;; first render with the default vocabulary: the sink paints,
    ;; and `import' paints nothing
    (should (code-review-hunkhighlight-region beg end "src/foo.py"))
    (should (memq 'font-lock-warning-face
                  (code-review-hunkhighlight-test--face-at "eval")))
    (should-not (memq 'code-review-source-face
                      (code-review-hunkhighlight-test--face-at "import")))
    ;; a changed security vocabulary MUST invalidate the cached
    ;; ranges (the cache key hashes the defcustom): the repaint
    ;; applies the substitute query's face — a stale cache would
    ;; serve the old ranges and paint nothing on `import'
    (let ((code-review-hunkhighlight-security-queries
           '((python ("\"import\" @sink"
                      . ((sink . code-review-source-face)))))))
      (should (code-review-hunkhighlight-region beg end "src/foo.py"))
      (should (memq 'code-review-source-face
                    (code-review-hunkhighlight-test--face-at "import"))))))

;;; Phase 20b: beacon retargeting (strong inside conditions, dim
;;; outside, strong everywhere in test files)

(ert-deftest code-review-hunkhighlight/beacon-python-conditions ()
  (skip-unless (and (fboundp 'treesit-language-available-p)
                    (treesit-language-available-p 'python)))
  (code-review-hunkhighlight-test--with-hunk
      '("+MAX_RETRIES = 3"
        "+def f(x):"
        "+    if x >= 5:"
        "+        return 1"
        "+    elif x < 2:"
        "+        x = x + 1"
        "+    while x != 4:"
        "+        x = x + 1"
        "+    msg = \"hi\"")
    (should (code-review-hunkhighlight-region beg end "src/foo.py"))
    ;; literals INSIDE conditions keep the STRONG constant face:
    ;; if / elif / while conditions
    (should (memq 'code-review-constant-face
                  (code-review-hunkhighlight-test--face-at "x >= 5")))
    (should (memq 'code-review-constant-face
                  (code-review-hunkhighlight-test--face-at "x < 2")))
    (should (memq 'code-review-constant-face
                  (code-review-hunkhighlight-test--face-at "x != 4")))
    ;; comparison OPERATORS strong inside conditions (the helper
    ;; reads the last char of the match: each search ends on the
    ;; operator)
    (should (memq 'code-review-constant-face
                  (code-review-hunkhighlight-test--face-at "x >=")))
    (should (memq 'code-review-constant-face
                  (code-review-hunkhighlight-test--face-at "x !=")))
    ;; the `+ 1' increments are OUTSIDE any condition: dim
    (should (memq 'code-review-constant-dim-face
                  (code-review-hunkhighlight-test--face-at "x + 1")))
    ;; config/literals outside conditions: dim, not strong
    (should (memq 'code-review-constant-dim-face
                  (code-review-hunkhighlight-test--face-at "MAX_RETRIES = 3")))
    (should (memq 'code-review-constant-dim-face
                  (code-review-hunkhighlight-test--face-at "msg = \"hi")))))

(ert-deftest code-review-hunkhighlight/beacon-python-test-file-strong ()
  (skip-unless (and (fboundp 'treesit-language-available-p)
                    (treesit-language-available-p 'python)))
  (code-review-hunkhighlight-test--with-hunk
      '("+MAX_RETRIES = 3"
        "+def test_f(x):"
        "+    msg = \"hi\""
        "+    assert x < 5")
    (should (code-review-hunkhighlight-region beg end "tests/test_f.py"))
    ;; TEST files: expected values ARE the payload — literals keep
    ;; the strong face everywhere, conditions or not
    (should (memq 'code-review-constant-face
                  (code-review-hunkhighlight-test--face-at "MAX_RETRIES = 3")))
    (should (memq 'code-review-constant-face
                  (code-review-hunkhighlight-test--face-at "msg = \"hi")))
    (should-not (memq 'code-review-constant-dim-face
                      (code-review-hunkhighlight-test--face-at "msg = \"hi")))))

(ert-deftest code-review-hunkhighlight/beacon-python-dim-toggle ()
  (skip-unless (and (fboundp 'treesit-language-available-p)
                    (treesit-language-available-p 'python)))
  ;; plain treatment (dim-outside-literals nil): outside literals
  ;; paint NOTHING - rendered first so the cache-invalidation
  ;; repaint below is ADDITIVE (old overlays are never removed)
  (code-review-hunkhighlight-test--with-hunk
      '("+def f(x):"
        "+    msg = \"hi\"")
    (let ((code-review-hunkhighlight-dim-outside-literals nil))
      (should (code-review-hunkhighlight-region beg end "src/foo.py"))
      (should-not (memq 'code-review-constant-dim-face
                        (code-review-hunkhighlight-test--face-at "msg = \"hi")))
      (should-not (memq 'code-review-constant-face
                        (code-review-hunkhighlight-test--face-at "msg = \"hi"))))
    ;; back to the default (t): toggling the defcustom MUST
    ;; invalidate the cached ranges (the cache key hashes it): the
    ;; repaint applies the DIM face a stale cache would have
    ;; skipped (the second render is OUTSIDE the let on purpose)
    (should (code-review-hunkhighlight-region beg end "src/foo.py"))
    (should (memq 'code-review-constant-dim-face
                  (code-review-hunkhighlight-test--face-at "msg = \"hi")))))

(ert-deftest code-review-hunkhighlight/beacon-list-cache-invalidation ()
  (skip-unless (and (fboundp 'treesit-language-available-p)
                    (treesit-language-available-p 'python)))
  (code-review-hunkhighlight-test--with-hunk
      '("+def f(x):"
        "+    msg = \"hi\"")
    ;; default beacon vocabulary: the outside literal paints dim
    (should (code-review-hunkhighlight-region beg end "src/foo.py"))
    (should (memq 'code-review-constant-dim-face
                  (code-review-hunkhighlight-test--face-at "msg = \"hi")))
    ;; a changed BEACON vocabulary MUST invalidate the cached
    ;; ranges (the cache key hashes the alist): the repaint
    ;; applies the substitute mapping - a stale cache would serve
    ;; the old dim ranges
    (let ((code-review-hunkhighlight-beacon-queries
           '((python ("(string) @lit"
                      . ((lit . font-lock-warning-face)))))))
      (should (code-review-hunkhighlight-region beg end "src/foo.py"))
      (should (memq 'font-lock-warning-face
                    (code-review-hunkhighlight-test--face-at "msg = \"hi"))))))

(ert-deftest code-review-hunkhighlight/beacon-elisp-conditions ()
  (skip-unless (and (fboundp 'treesit-language-available-p)
                    (treesit-language-available-p 'elisp)))
  (code-review-hunkhighlight-test--with-hunk
      '("+(defun cr-fn (x)"
        "+  (if (= x 1)"
        "+      (list x)"
        "+    (when (>= x 2)"
        "+      (setq m \"msg\"))"
        "+    (cond"
        "+     ((string= s \"a\") 1)"
        "+     (t 2))))"
        "+(setq other \"note\")")
    (should (code-review-hunkhighlight-region beg end "src/foo.el"))
    ;; if/when/cond conditions: numbers and strings inside STRONG
    (should (memq 'code-review-constant-face
                  (code-review-hunkhighlight-test--face-at "= x 1")))
    (should (memq 'code-review-constant-face
                  (code-review-hunkhighlight-test--face-at ">= x 2")))
    ;; the cond clause TEST is the condition - its string is strong
    (should (memq 'code-review-constant-face
                  (code-review-hunkhighlight-test--face-at "s \"a\"")))
    ;; the clause RESULTS (1, 2, the when body) are OUTSIDE: dim
    (should (memq 'code-review-constant-dim-face
                  (code-review-hunkhighlight-test--face-at "m \"msg\"")))
    (should (memq 'code-review-constant-dim-face
                  (code-review-hunkhighlight-test--face-at "other \"note\"")))
    ;; the `=' comparison symbol inside the if condition: strong
    ;; (the search ends ON the operator char)
    (should (memq 'code-review-constant-face
                  (code-review-hunkhighlight-test--face-at " (=")))
    ;; ...and `string=' is also a comparison symbol, but it sits in
    ;; HEAD position of the clause test - it is INSIDE the cond
    ;; condition range, so strong as well
    (should (memq 'code-review-constant-face
                  (code-review-hunkhighlight-test--face-at "((string=")))))

(ert-deftest code-review-hunkhighlight/beacon-scala-conditions ()
  (skip-unless (and (fboundp 'treesit-language-available-p)
                    (treesit-language-available-p 'scala)))
  (code-review-hunkhighlight-test--with-hunk
      '("+class Foo {"
        "+  val LIMIT = 100"
        "+  def check(x: Int): Boolean = {"
        "+    if (x >= 2) {"
        "+      true"
        "+    } else if (x < 0) {"
        "+      false"
        "+    }"
        "+    while (x != 0) {"
        "+      x = x - 1"
        "+    }"
        "+  }"
        "+}")
    (should (code-review-hunkhighlight-region beg end "src/Foo.scala"))
    ;; if/else-if/while conditions: numbers STRONG (scala operators
    ;; are `operator_identifier' nodes - probed, see the queries)
    (should (memq 'code-review-constant-face
                  (code-review-hunkhighlight-test--face-at "x >= 2")))
    (should (memq 'code-review-constant-face
                  (code-review-hunkhighlight-test--face-at "x < 0")))
    (should (memq 'code-review-constant-face
                  (code-review-hunkhighlight-test--face-at "x != 0")))
    (should (memq 'code-review-constant-face
                  (code-review-hunkhighlight-test--face-at "(x >=")))
    ;; the decrement and the config literal: outside conditions, dim
    (should (memq 'code-review-constant-dim-face
                  (code-review-hunkhighlight-test--face-at "x - 1")))
    (should (memq 'code-review-constant-dim-face
                  (code-review-hunkhighlight-test--face-at "LIMIT = 100")))))

(ert-deftest code-review-hunkhighlight/beacon-clojure-conditions ()
  (skip-unless (and (fboundp 'treesit-language-available-p)
                    (treesit-language-available-p 'clojure)))
  (code-review-hunkhighlight-test--with-hunk
      '("+(def LIMIT 100)"
        "+(defn check [x]"
        "+  (if (= x 1)"
        "+    :one"
        "+    (when (>= x 2)"
        "+      (str \"big\"))))")
    (should (code-review-hunkhighlight-region beg end "src/my/ns.clj"))
    ;; positional list_lit conditions: numbers STRONG
    (should (memq 'code-review-constant-face
                  (code-review-hunkhighlight-test--face-at "= x 1")))
    (should (memq 'code-review-constant-face
                  (code-review-hunkhighlight-test--face-at ">= x 2")))
    ;; the `=' sym_lit inside the if condition: strong (the search
    ;; ends on the operator char)
    (should (memq 'code-review-constant-face
                  (code-review-hunkhighlight-test--face-at " (=")))
    ;; the LIMIT config literal and the when-BODY string: dim
    (should (memq 'code-review-constant-dim-face
                  (code-review-hunkhighlight-test--face-at "LIMIT 100")))
    (should (memq 'code-review-constant-dim-face
                  (code-review-hunkhighlight-test--face-at "str \"big\"")))))

(ert-deftest code-review-hunkhighlight/beacon-sql-where-conditions ()
  (skip-unless (and (fboundp 'treesit-language-available-p)
                    (treesit-language-available-p 'sql)))
  (code-review-hunkhighlight-test--with-hunk
      '("+CREATE TABLE IF NOT EXISTS analytics.t ("
        "+  organization_id UInt64, is_deleted UInt8 DEFAULT 0);"
        "+SELECT a.x"
        "+FROM analytics.t AS a"
        "+WHERE a.x > 10 AND b.y = 'z'"
        "+GROUP BY a.x")
    (should (code-review-hunkhighlight-region beg end "models/ledger.sql"))
    ;; the WHERE clause IS the condition: literals inside STRONG
    (should (memq 'code-review-constant-face
                  (code-review-hunkhighlight-test--face-at "a.x > 10")))
    (should (memq 'code-review-constant-face
                  (code-review-hunkhighlight-test--face-at "b.y = 'z'")))
    ;; comparison operators inside the WHERE: strong
    (should (memq 'code-review-constant-face
                  (code-review-hunkhighlight-test--face-at "a.x >")))
    ;; the CREATE TABLE column DEFAULT is OUTSIDE any condition: dim
    (should (memq 'code-review-constant-dim-face
                  (code-review-hunkhighlight-test--face-at "DEFAULT 0")))))

;;; Phase 20c: intra-line changed-token marks

(ert-deftest code-review-hunkhighlight/intraline-tokenize ()
  (should (equal (code-review-hunkhighlight--tokenize "a + b \"x y\"")
                 '(("a" 0 1) ("+" 2 3) ("b" 4 5) ("\"x y\"" 6 11))))
  ;; digit runs and identifier runs with digits stay whole
  (should (equal (mapcar #'car
                         (code-review-hunkhighlight--tokenize "x2 = 10.5;"))
                 '("x2" "=" "10" "." "5" ";")))
  ;; escapes keep the string one token; whitespace dropped
  (should (equal (mapcar #'car
                         (code-review-hunkhighlight--tokenize
                          "f(\"a\\\"b\")"))
                 '("f" "(" "\"a\\\"b\"" ")"))))

(ert-deftest code-review-hunkhighlight/intraline-changed-token-cols ()
  ;; single token change: exactly the new token's columns
  (should (equal (code-review-hunkhighlight--changed-token-cols
                  "total = compute(a, b)" "total = compute(a, c)")
                 '((19 20))))
  ;; identical / whitespace-only: nothing to mark
  (should (null (code-review-hunkhighlight--changed-token-cols
                 "pad(x)" "pad(x)")))
  (should (null (code-review-hunkhighlight--changed-token-cols
                 "pad(x)" "    pad(x)")))
  ;; pure insertion into the middle: all inserted tokens marked
  (should (equal (code-review-hunkhighlight--changed-token-cols
                  "config" "config = 1")
                 '((7 8) (9 10))))
  ;; pure deletion: nothing on the new side
  (should (null (code-review-hunkhighlight--changed-token-cols
                 "x = 1" "x")))
  ;; reordered middles: exactly the unmatched new tokens (one of
  ;; b/c aligns - which one is backtrack detail, the count is not)
  (should (equal 2 (length (code-review-hunkhighlight--changed-token-cols
                            "a + b + c" "a + c + b"))))
  ;; the length cap marks nothing (monster lines are data)
  (let ((code-review-hunkhighlight-intra-line-max-length 10))
    (should (null (code-review-hunkhighlight--changed-token-cols
                   "aaaaaaaaaaaaaaaaaaaa" "aaaaaaaaaaaaaaaaaaaa")))
    (should (null (code-review-hunkhighlight--changed-token-cols
                   "short = 1" "aaaaaaaaaaaaaaaaaaaa"))))
  ;; the token cap: middles bigger than the budget mark nothing
  ;; (3-vs-3 middles under cap 2; `x = 1' stays at 1-vs-1)
  (let ((code-review-hunkhighlight-intra-line-max-tokens 2))
    (should (null (code-review-hunkhighlight--changed-token-cols
                   "a b c" "z b y")))
    (should (equal (code-review-hunkhighlight--changed-token-cols
                    "x = 1" "x = 2")
                   '((4 5))))))

(ert-deftest code-review-hunkhighlight/intraline-ranges ()
  ;; raw prefixed body: block pairing, pure-add unmarked, blank
  ;; context resetting, `\ No newline' skipped
  (should (equal
           (code-review-hunkhighlight--intra-line-ranges
            (mapconcat #'identity
                       '(" same(x)"
                         "-total = compute(a, b)"
                         "+total = compute(a, c)"
                         "+fresh = 9"
                         ""
                         "-def f(x):"
                         "-    pass"
                         "+def f(y):"
                         "\\ No newline at end of file")
                       "\n"))
           '((2 19 20 code-review-changed-token-face)
             (5 6 7 code-review-changed-token-face))))
  ;; pure-add body (no `-' lines): no marks at all
  (should (null (code-review-hunkhighlight--intra-line-ranges
                 (mapconcat #'identity '("+a = 1" "+b = 2") "\n"))))
  ;; whitespace-only difference: nothing marked
  (should (null (code-review-hunkhighlight--intra-line-ranges
                 (mapconcat #'identity '("-    pad(x)" "+  pad(x)") "\n")))))

(ert-deftest code-review-hunkhighlight/intraline-unknown-language ()
  ;; grammar-free layer: a README.md hunk (no language, no treesit)
  ;; still gets its changed tokens underlined
  (code-review-hunkhighlight-test--with-hunk
      '("-total = compute(a, b)"
        "+total = compute(a, c)"
        "+fresh = 9")
    (should (code-review-hunkhighlight-region beg end "README.md"))
    ;; the helper reads the LAST char of the match: "a, c" ends on
    ;; the changed `c' token itself
    (should (memq 'code-review-changed-token-face
                  (code-review-hunkhighlight-test--face-at "a, c")))
    ;; pure-add line: the whole line IS the delta - no underline
    (should-not (memq 'code-review-changed-token-face
                      (code-review-hunkhighlight-test--face-at "fresh = 9")))
    ;; the diff base face stays underneath
    (should (memq 'magit-diff-added
                  (code-review-hunkhighlight-test--face-at "fresh = 9")))))

(ert-deftest code-review-hunkhighlight/intraline-stacks-with-semantic ()
  (skip-unless (and (fboundp 'treesit-language-available-p)
                    (treesit-language-available-p 'python)))
  ;; the underline composes with the semantic overlays: the
  ;; changed `1' token is ALSO a beacon literal (dim outside
  ;; conditions) - both faces at the same char
  (code-review-hunkhighlight-test--with-hunk
      '("+def f(a):"
        "-    total = a + 2"
        "+    total = a + 1")
    (should (code-review-hunkhighlight-region beg end "src/foo.py"))
    (let ((faces (code-review-hunkhighlight-test--face-at "a + 1")))
      (should (memq 'code-review-changed-token-face faces))
      (should (memq 'code-review-constant-dim-face faces)))
    ;; whitespace-only: no underline
    (should-not (memq 'code-review-changed-token-face
                      (code-review-hunkhighlight-test--face-at "total = a"))))
    ;; monster lines: capped, no error (the minus line content
    ;; differs so the searches land on the monster + line)
    (code-review-hunkhighlight-test--with-hunk
        (list "-    y = 1"
              (concat "+    x = " (make-string 2500 ?1)))
      (should (code-review-hunkhighlight-region beg end "src/foo.py"))
      (should-not
       (memq 'code-review-changed-token-face
             (code-review-hunkhighlight-test--face-at "x = ")))
      (should (memq 'magit-diff-added
                    (code-review-hunkhighlight-test--face-at "x = ")))))

(ert-deftest code-review-hunkhighlight/intraline-cache-invalidation ()
  (skip-unless (and (fboundp 'treesit-language-available-p)
                    (treesit-language-available-p 'python)))
  ;; rendered first with the layer OFF so the invalidation repaint
  ;; below is ADDITIVE (old overlays are never removed)
  (code-review-hunkhighlight-test--with-hunk
      '("-x = 1"
        "+x = 2")
    (let ((code-review-hunkhighlight-intra-line nil))
      (should (code-review-hunkhighlight-region beg end "src/foo.py"))
      (should-not (memq 'code-review-changed-token-face
                        (code-review-hunkhighlight-test--face-at "x = 2"))))
    ;; toggling the defcustom MUST invalidate the cached ranges
    ;; (the cache key hashes it)
    (should (code-review-hunkhighlight-region beg end "src/foo.py"))
    (should (memq 'code-review-changed-token-face
                  (code-review-hunkhighlight-test--face-at "x = 2")))))
