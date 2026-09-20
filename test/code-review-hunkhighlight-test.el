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
    (should (memq 'font-lock-constant-face
                  (code-review-hunkhighlight-test--face-at "MAX_RETRIES")))
    ;; UPPER_CASE is a constant, not a plain variable
    (should-not (memq 'font-lock-variable-name-face
                      (code-review-hunkhighlight-test--face-at "MAX_RETRIES")))
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
