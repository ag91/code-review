;;; code-review-analysis-test.el --- ERT tests for the analysis engine -*- lexical-binding: t; -*-
;;
;; Phase 5 tests.  These are pure-function tests plus one git
;; integration test: they must run in a fresh batch emacs so a stale
;; daemon cannot hide missing definitions (see AGENTS.md).

(require 'ert)
(require 'cl-lib)
(require 'code-review-analysis)
(require 'code-review-db)
(require 'code-review-github)
(require 'code-review-test-helpers)

;; `code-review-db-pullreq' is abstract; a concrete subclass is
;; enough for slot access in the base-rev test (no live db needed).
(defclass code-review-analysis-test-pr (code-review-db-pullreq) ())

;;; Normalization

(ert-deftest code-review-analysis/normalize-masks-and-preserves ()
  ;; string contents are masked
  (should (equal (code-review-analysis--normalize-line "foo(\"hello\") = 12;")
                 "foo(\"\") = N;"))
  ;; numbers masked, statement separators kept (c-family safe)
  (should (equal (code-review-analysis--normalize-line "x = 1; y = 2;")
                 "x = N; y = N;"))
  ;; elisp trailing comment stripped
  (should (equal (code-review-analysis--normalize-line "(defun foo (x))   ; doc")
                 "(defun foo (x))"))
  ;; full-line elisp comment becomes empty
  (should (string-empty-p
           (code-review-analysis--normalize-line ";; TODO fix")))
  ;; python trailing comment stripped
  (should (equal (code-review-analysis--normalize-line "x = a # note")
                 "x = a"))
  ;; elisp function quote survives (would otherwise truncate the line)
  (should (equal (code-review-analysis--normalize-line "(mapcar #'car lst)")
                 "(mapcar #'car lst)"))
  ;; c++ comment stripped, statements before it kept
  (should (equal (code-review-analysis--normalize-line "foo(a); // hi")
                 "foo(a);")))

;;; Diff line extraction

(ert-deftest code-review-analysis/block-lines-numbers ()
  (let* ((block "diff --git a/a.py b/a.py
--- a/a.py
+++ b/a.py
@@ -1,3 +1,5 @@
 old1
+new1
+new2
 old2
-old3
@@ -10,2 +20,3 @@
 context
+newX
")
         (lines (code-review-analysis--block-lines block)))
    (should (equal (plist-get lines :added)
                   '((2 . "new1") (3 . "new2") (21 . "newX"))))
    (should (equal (plist-get lines :deleted)
                   '((3 . "old3"))))))

;;; Shingles, index and similarity

(ert-deftest code-review-analysis/shingles-consecutive-only ()
  ;; windows only across consecutive line numbers
  (should (equal (code-review-analysis--shingles
                  '((1 . "a") (2 . "b") (3 . "c") (5 . "d")) 4)
                 nil))
  (should (equal (code-review-analysis--shingles
                  '((1 . "a") (2 . "b") (3 . "c") (4 . "d")) 4)
                 '((1 . "a\nb\nc\nd")))))

(ert-deftest code-review-analysis/shingles-skip-blank-stretches ()
  ;; 4 blank lines would match everywhere: they must not shingle
  (should (equal (code-review-analysis--shingles
                  '((1 . "") (2 . "") (3 . "") (4 . "") (5 . "")) 4)
                 nil)))

(ert-deftest code-review-analysis/find-similar-coverage ()
  (let* ((contents
          '(("lib.py" . "def helper(x):\n    y = x + 1\n    return y\nt = 4\nu = 5\nv = 6\n")))
         (index (code-review-analysis--index-contents contents 4))
         (added-items
          (mapcar (lambda (pair)
                    (cons (car pair)
                          (code-review-analysis--normalize-line (cdr pair))))
                  '((10 . "def helper(x):")
                    (11 . "    y = x + 1")
                    (12 . "    return y")
                    (13 . "t = 4")
                    (14 . "u = 5")
                    (15 . "v = 6")))))
    ;; all 6 added lines are covered, the repo range covers lib.py:1-3
    (should (equal (code-review-analysis--find-similar added-items index 4 3)
                   '(("lib.py" 6 1 3))))
    ;; below min-covered: nothing reported
    (should (null (code-review-analysis--find-similar added-items index 4 7)))))

;;; Definitions

(ert-deftest code-review-analysis/definitions-extraction ()
  (should (equal (code-review-analysis--definitions-in
                  "a.el"
                  (list (cons 1 "(defun foo (x)")
                        (cons 2 "(defvar bar 2)")))
                 (list (cons "foo" 1) (cons "bar" 2))))
  (should (equal (code-review-analysis--definitions-in
                  "b.py"
                  (list (cons 3 "  async def bar(self):")
                        (cons 4 "    def baz():")))
                 (list (cons "bar" 3) (cons "baz" 4))))
  (should (null (code-review-analysis--definitions-in
                 "x.txt"
                 (list (cons 1 "def whatever("))))))
  (should (equal (code-review-analysis--definitions-in
                  "src/Foo.scala"
                  (list (cons 1 "object Foo {")
                        (cons 2 "  private def bar(x: Int): Int = x")
                        (cons 3 "  case class Baz(y: String)")
                        (cons 4 "  val quux = 1")
                        (cons 5 "  x.defaultValue = 2")))
                 (list (cons "Foo" 1) (cons "bar" 2) (cons "Baz" 3))))
  (should (equal (code-review-analysis--definitions-in
                  "src/core.clj"
                  (list (cons 1 "(defn foo [x]")
                        (cons 2 "(defn- helper [x]")
                        (cons 3 "(defonce ^:private bar 2)")
                        (cons 4 "(defmethod bar :default [x] x)")
                        (cons 5 "(deftest my-test ...)")))
                 (list (cons "foo" 1) (cons "helper" 2) (cons "bar" 3))))

;;; Git integration: one temp repository

(defun code-review-analysis-test--make-repo (files-alist)
  "Create a temp git repository with FILES-ALIST committed at HEAD.
Return its directory (with trailing slash)."
  (let* ((dir (file-name-as-directory (make-temp-file "cr-analysis-" t)))
         (default-directory dir))
    (call-process "git" nil nil nil "init" ".")
    (call-process "git" nil nil nil "config" "user.email" "test@test.test")
    (call-process "git" nil nil nil "config" "user.name" "test")
    (pcase-dolist (`(,path . ,text) files-alist)
      (let ((full (expand-file-name path dir)))
        (make-directory (file-name-directory full) t)
        (with-temp-file full (insert text))))
    (call-process "git" nil nil nil "add" "-A")
    (call-process "git" nil nil nil "commit" "-m" "init")
    dir))

(ert-deftest code-review-analysis/git-base-and-references ()
  (let* ((repo (code-review-analysis-test--make-repo
                '(("lib.py" . "def helper(x):\n    y = x + 1\n    return y\n\ndef other(x):\n    return helper(x) - 1\n")
                  ("main.py" . "from lib import helper\nprint(helper(2))\nprint(other(3))\n"))))
         (files (code-review-analysis--similar-candidates
                 repo
                 (list (cons 10 "def helper(x):")
                       (cons 11 "    y = x + 1"))
                 '("dup.py"))))
    ;; only lib.py contains the probe lines in the worktree
    ;; (git grep -l: each matching file listed once)
    (should (equal files '(("lib.py" . 1))))
    ;; contents read straight from the worktree files
    (let ((contents (code-review-analysis--read-files
                     repo (mapcar #'car files))))
      (should (equal (cdr (assoc "lib.py" contents))
                     "def helper(x):\n    y = x + 1\n    return y\n\ndef other(x):\n    return helper(x) - 1\n"))
      ;; index built from real repo contents finds the duplication
      (let* ((index (code-review-analysis--index-contents contents 4))
             (added-items
              (mapcar (lambda (pair)
                        (cons (car pair)
                              (code-review-analysis--normalize-line (cdr pair))))
                      (list (cons 10 "def helper(x):")
                            (cons 11 "    y = x + 1")
                            (cons 12 "    return y")
                            (cons 13 "")
                            (cons 14 "def other(x):")
                            (cons 15 "    return helper(x) - 1")))))
        (should (equal (code-review-analysis--find-similar
                        added-items index 4 3)
                       '(("lib.py" 6 1 3))))))

    ;; references: the definition line itself is not a reference
    (let ((helper-refs (code-review-analysis--references repo "helper"))
          (other-refs (code-review-analysis--references repo "other")))
      (should (equal (cl-sort (mapcar (lambda (r) (list (nth 0 r) (nth 1 r)))
                                     helper-refs) #'string< :key #'car)
                     '(("lib.py" 6) ("main.py" 1) ("main.py" 2))))
      (should (equal (mapcar (lambda (r) (list (nth 0 r) (nth 1 r)))
                             other-refs)
                     '(("main.py" 3)))))
    ;; no match at all
    (should-not (code-review-analysis--references repo "no-such-symbol"))))

(ert-deftest code-review-analysis/analyze-excludes-self-matches ()
  ;; The index always contains the analyzed file's own worktree copy
  ;; (it holds its own added lines); matches against itself are not
  ;; findings, matches against OTHER files are.
  (let* ((contents
          (list (cons "lib.py"
                      "def helper(x):\n    y = x + 1\n    return y\n\ndef other(x):\n    return helper(x) - 1\n")))
         (index (code-review-analysis--index-contents contents 4))
         (refs (let ((h (make-hash-table :test #'equal)))
                 (puthash "helper" '(("main.py" 1 "helper(2)")) h)
                 (puthash "other" '(("main.py" 3 "other(3)")) h)
                 h))
         (block "diff --git a/dup.py b/dup.py\n--- a/dup.py\n+++ b/dup.py\n@@ -0,0 +1,6 @@\n+def helper(x):\n+    y = x + 1\n+    return y\n+\n+def other(x):\n+    return helper(x) - 1\n")
         (res-dup (code-review-analysis--analyze-one-file
                   index refs "dup.py" block))
         (res-lib (code-review-analysis--analyze-one-file
                   index refs "lib.py" block)))
    ;; dup.py's added lines match lib.py in the index: a finding
    (should (equal (nth 0 res-dup) '(("dup.py" 6 "lib.py" 6 1 3))))
    ;; the same added lines matched against lib.py itself: no finding
    (should (null (nth 0 res-lib)))
    ;; definitions are referenced, so no dead/dangling findings
    (should (null (nth 1 res-dup)))
    (should (null (nth 2 res-dup)))))

(ert-deftest code-review-analysis/git-grep-excludes-generated ()
  (let* ((repo (code-review-analysis-test--make-repo
                '(("a.el" . "(defun my-fun ()\n  42)\n")
                  ("a-autoloads.el" . "(add-to-list 'load-path \"/x\")\n") ))))
    ;; a reference only in an excluded generated file does not count
    (should-not (code-review-analysis--references repo "my-fun"))))

(ert-deftest code-review-analysis/imports-are-not-similarity-signal ()
  ;; scalafmt-shaped noise: PRs adding test files "match" existing
  ;; ones through import boilerplate alone.  Import/package lines
  ;; are structural, not duplication signal.
  (let* ((contents
          (list (cons "cli/CliOptionsTest.scala"
                      "import org.scalafmt._\nimport org.scalafmt.config._\nimport org.scalafmt.util._\nimport munit.FunSuite\nimport java.io.File\nimport scala.collection.mutable\n")))
         (index (code-review-analysis--index-contents contents 4))
         (refs (make-hash-table :test #'equal))
         (block "diff --git a/FileHeaderTest.scala b/FileHeaderTest.scala\n--- a/FileHeaderTest.scala\n+++ b/FileHeaderTest.scala\n@@ -0,0 +1,6 @@\n+import org.scalafmt._\n+import org.scalafmt.config._\n+import org.scalafmt.util._\n+import munit.FunSuite\n+import java.io.File\n+import scala.collection.mutable\n")
         (res (code-review-analysis--analyze-one-file
               index refs "FileHeaderTest.scala" block)))
    ;; without the boilerplate filter these 6 import lines would
    ;; be a 6/6 "similarity" finding
    (should (null (nth 0 res)))))

(ert-deftest code-review-analysis/similar-requires-coverage-ratio ()
  ;; an absolute covered-line count alone is noise: 12 covered
  ;; lines in a 574-line test file addition is 2%.  Findings must
  ;; also cover a minimum FRACTION of the file's added lines.
  (let* ((contents
          (list (cons "lib.py"
                      "def helper(x):\n    y = x + 1\n    return y\n\ndef other(x):\n    return helper(x) - 1\n")))
         (index (code-review-analysis--index-contents contents 4))
         (refs (make-hash-table :test #'equal))
         (block "diff --git a/dup.py b/dup.py\n--- a/dup.py\n+++ b/dup.py\n@@ -0,0 +1,6 @@\n+def helper(x):\n+    y = x + 1\n+    return y\n+\n+def other(x):\n+    return helper(x) - 1\n")
         (analyze
          (lambda ()
            (nth 0 (code-review-analysis--analyze-one-file
                    index refs "dup.py" block)))))
    ;; 6/6 covered: passes the default 0.1 ratio floor
    (should (funcall analyze))
    ;; the same finding under an impossible ratio floor: suppressed
    (let ((code-review-analysis-min-covered-ratio 2.0))
      (should (null (funcall analyze))))))

(ert-deftest code-review-analysis/dead-skips-test-entry-points ()
  ;; *Test classes and test_* functions are invoked BY CONVENTION
  ;; (sbt, pytest, scalatest): zero references is normal for them.
  (let* ((refs (make-hash-table :test #'equal))
         (analyze
          (lambda (path block)
            (nth 1 (code-review-analysis--analyze-one-file
                    nil refs path block)))))
    ;; scalafmt shape: new *Test class with no references
    (should (null (funcall analyze "FileHeaderTest.scala"
                           "diff --git a/FileHeaderTest.scala b/FileHeaderTest.scala\n--- a/FileHeaderTest.scala\n+++ b/FileHeaderTest.scala\n@@ -0,0 +1,1 @@\n+class FileHeaderTest {\n")))
    ;; pytest shape: test_* function with no references
    (should (null (funcall analyze "test_foo.py"
                           "diff --git a/test_foo.py b/test_foo.py\n--- a/test_foo.py\n+++ b/test_foo.py\n@@ -0,0 +1,1 @@\n+def test_foo():\n")))
    ;; but an unreferenced NON-test definition is still reported
    (should (equal (funcall analyze "Foo.scala"
                            "diff --git a/Foo.scala b/Foo.scala\n--- a/Foo.scala\n+++ b/Foo.scala\n@@ -0,0 +1,1 @@\n+class Foo {\n")
                   '(("Foo" "Foo.scala" 1))))))

;;; Megabyte single lines must not kill the analysis (litellm bug)

;; Real case: BerriAI/litellm PR 43042.  The cookbook notebooks pack
;; half a megabyte into ONE line; `--normalize-line' string-mask
;; regexp recurses per character and the regexp matcher died with
;; "Stack overflow in regexp matcher", which surfaced as "Got an
;; error from your VC provider" and left no review buffer.

(ert-deftest code-review-analysis/normalize-megabyte-line ()
  "Notebook/minified single lines must not overflow the matcher stack."
  (let ((line (concat "{\"x\": \"" (make-string 300000 ?x)
                      "\", \"y\": \"a\\\"b\"}")))
    (should (stringp (code-review-analysis--normalize-line line)))
    (should (<= (length (code-review-analysis--normalize-line line))
                code-review-analysis-line-max-length))
    (should (null (code-review-analysis--boilerplate-p line)))))

(ert-deftest code-review-analysis/max-line-length-bounded ()
  "`--max-line-length' exits early at the cap and scans fully when disabled."
  (let ((text (concat "short\n" (make-string 500000 ?x) "\ntail")))
    (should (= 5 (code-review-analysis--max-line-length "abc\ndefgh\nx")))
    ;; early exit: any monster line reports over the cap without
    ;; scanning the rest of the text
    (should (> (code-review-analysis--max-line-length text)
               code-review-analysis-line-max-length))
    ;; cap disabled: full scan
    (let ((code-review-analysis-line-max-length 0))
      (should (= 500000 (code-review-analysis--max-line-length text))))))

(ert-deftest code-review-analysis/read-files-skips-monster-line-files ()
  "Files with megabyte lines (notebook JSON, bundles) are data,
not reviewable code: skipped, and their bytes stay available to
real source files."
  (let ((dir (make-temp-file "cr-analysis-read-" t)))
    (with-temp-file (expand-file-name "normal.py" dir)
      (insert "def helper(x):\n    return x\n"))
    (with-temp-file (expand-file-name "data.ipynb" dir)
      (insert "{\"x\": \"" (make-string 300000 ?x) "\"}\n"))
    (let ((res (code-review-analysis--read-files
                dir '("normal.py" "data.ipynb"))))
      (should (equal (mapcar #'car res) '("normal.py")))
      (should (equal (cdr (assoc "normal.py" res))
                     "def helper(x):\n    return x\n")))))

(ert-deftest code-review-analysis/compute-survives-notebook-repo ()
  "End-to-end: a repo with a notebook whose single line is 300KB
(and contains a definition name, so git grep hits it) must
analyze without the regexp stack overflow."
  (let* ((notebook (concat "{\"cell_type\": \"code\", \"outputs\": \""
                           (make-string 150000 ?x)
                           " helper "
                           (make-string 150000 ?x)
                           "\", \"s\": \"a\\\"b\"}"))
         (repo (code-review-analysis-test--make-repo
                `(("notebook.ipynb" . ,notebook)
                  ("lib.py" . "def helper(x):\n    y = x + 1\n    return y\n    z = x - 1\n"))))
         (diff (concat "diff --git a/new.py b/new.py\n"
                       "--- a/new.py\n"
                       "+++ b/new.py\n"
                       "@@ -0,0 +1,4 @@\n"
                       "+def helper(x):\n"
                       "+    y = x + 1\n"
                       "+    return y\n"
                       "+    z = x - 1\n"))
         (code-review-analysis-min-covered 2)
         (res (code-review-analysis--compute
               repo diff (code-review-analysis-test-pr))))
    ;; completed: a plist, not a stack overflow
    (should (listp res))
    ;; the lib.py duplication is still found
    (should (cl-some (lambda (s) (string= (nth 2 s) "lib.py"))
                     (plist-get res :similar)))
    ;; helper has non-definition references (the notebook grep hit),
    ;; so it is not dead
    (should (null (plist-get res :dead)))))

(ert-deftest code-review-analysis/run-contains-compute-failure ()
  "A failing compute is logged and reported as no findings.
It must NEVER bubble into the render chain: the litellm incident
surfaced an analysis bug as \"error from your VC provider\" with
no review buffer at all."
  (code-review-test--with-db
    (code-review-db--pullreq-create
     (code-review-github-repo :owner "o" :repo "r" :number "1"))
    (code-review-db--pullreq-raw-diff-update "diff --git a/x.py b/x.py\n")
    (let ((code-review-repo-worktree "/tmp")
          (code-review-log-file (make-temp-file "cr-analysis-log"))
          (orig (symbol-function 'code-review-analysis--compute)))
      (unwind-protect
          (progn
            (fset 'code-review-analysis--compute
                  (lambda (&rest _) (error "injected boom")))
            (should (null (code-review-analysis-run)))
            (should (with-temp-buffer
                      (insert-file-contents code-review-log-file)
                      (goto-char (point-min))
                      (search-forward "injected boom" nil t))))
        (fset 'code-review-analysis--compute orig)
        (ignore-errors (delete-file code-review-log-file))))))

;;; Phase 15: hunk delicacy

(ert-deftest code-review-analysis/split-hunks-key-and-sides ()
  "Per-hunk :ranges is the raw @@ ranges text — byte-identical to
what the wash reads via `match-string 1', which is the whole point
(delicate-entry lookup from the washer and the phase 13 key)."
  (let* ((block "diff --git a/a.py b/a.py
--- a/a.py
+++ b/a.py
@@ -1,3 +1,4 @@
 context
-removed
+added1
+added2
 context
@@ -10,2 +20,3 @@
 ctx
+addedX
\\ No newline at end of file
")
         (hunks (code-review-analysis--split-hunks block)))
    (should (equal (mapcar (lambda (h) (plist-get h :ranges)) hunks)
                   '("-1,3 +1,4" "-10,2 +20,3")))
    ;; old side: context + deleted lines (blame side), with OLD line
    ;; numbers
    (should (equal (plist-get (car hunks) :old)
                   '((1 . "context") (2 . "removed") (3 . "context"))))
    ;; new-side numbers: context at 1, then added1 at 2, added2 at 3
    (should (equal (plist-get (car hunks) :added)
                   '((2 . "added1") (3 . "added2"))))
    (should (equal (plist-get (car hunks) :deleted)
                   '((2 . "removed"))))
    ;; the "\ No newline" line is not a code line
    (should (equal (plist-get (cadr hunks) :old)
                   '((10 . "ctx"))))
    ;; a block with no @@ header (binary) yields no hunks
    (should (null (code-review-analysis--split-hunks
                   "diff --git a/a.bin b/a.bin\nBinary files differ\n")))))

(ert-deftest code-review-analysis/old-rev-from-diff-args ()
  "The blame rev comes from the PR's `base-ref-name': forge branch
names as-is, local git diff args special-cased."
  (should (equal (code-review-analysis--old-rev "--cached") "HEAD"))
  (should (equal (code-review-analysis--old-rev "HEAD^..HEAD") "HEAD^"))
  (should (equal (code-review-analysis--old-rev "master..feature") "master"))
  (should (null (code-review-analysis--old-rev "abc123^!")))
  (should (equal (code-review-analysis--old-rev "HEAD") "HEAD"))
  (should (equal (code-review-analysis--old-rev "main") "main"))
  (should (null (code-review-analysis--old-rev nil))))

(ert-deftest code-review-analysis/parse-blame-porcelain ()
  "Porcelain blame: header line sets the original line, author
metadata lines fill the entry, the tab-content line closes it."
  (let ((table (make-hash-table :test #'eql))
        (out "1111111111111111111111111111111111111111 1 1 1
author Alice
author-time 1700000000
author-mail <a@x>
\tfirst line
2222222222222222222222222222222222222222 2 2 1
author Bob
author-time 1600000000
\tsecond line
summary x
filename a.py
"))
    (code-review-analysis--parse-blame out table)
    (should (equal (gethash 1 table) '("Alice" . 1700000000)))
    (should (equal (gethash 2 table) '("Bob" . 1600000000)))
    (should (null (gethash 3 table)))))

(ert-deftest code-review-analysis/slice-blame-ranges-budget ()
  "Ranges over the line budget are dropped whole; the budget only
counts BLAMED lines, never the whole hunk."
  ;; two ranges of 3 lines each fit in the default budget
  (should (equal (code-review-analysis--slice-blame-ranges
                  '((4 . 6) (1 . 3)))
                 '((1 . 3) (4 . 6))))
  ;; budget 5: the second range (3 lines) does not fit anymore
  (let ((code-review-analysis-max-blame-lines 5))
    (should (equal (code-review-analysis--slice-blame-ranges
                    '((4 . 6) (1 . 3)))
                   '((1 . 3))))))

(ert-deftest code-review-analysis/hunk-entry-score-ingredients ()
  "The score sums its ingredients, capped: blast radius saturates
at 40 callers, age at 3y, +0.25 from 3 authors, +0.5 per 10
branches added, +0.5 per dead def (max 1.0)."
  (let* ((refs (make-hash-table :test #'equal))
         (blame (make-hash-table :test #'eql))
         (hunk (list :ranges "-1,4 +1,6"
                     :old '((1 . "context") (2 . "def one(x):")
                            (3 . "    return one(x) - 1") (4 . "context"))
                     :added '((2 . "def one(x):")
                              (5 . "    if x and y:"))
                     :deleted '((2 . "def one(x):")
                                (3 . "    return one(x) - 1"))))
         entry)
    ;; 40 references elsewhere -> blast radius 1.0; age is in DAYS
    (puthash "one" (make-list 40 '("caller.py" 1 "one()")) refs)
    ;; blame: line 2 is ancient, line 3 fresh, line 1/4 medium
    (puthash 2 (cons "Alice" (- (float-time) (* 4.0 365 86400))) blame)
    (puthash 3 (cons "Bob" (- (float-time) 0)) blame)
    (puthash 1 (cons "Carol" (- (float-time) (* 2.0 365 86400))) blame)
    (puthash 4 (cons "Dan" (- (float-time) (* 2.0 365 86400))) blame)
    (setq entry (code-review-analysis--hunk-entry "a.py" hunk refs blame))
    ;; def "one" (added+deleted) has 40 callers: blast 1.0;
    ;; median age of (0 2y 2y 4y) = 2y = 730 days -> 730/1095;
    ;; 4 distinct authors -> +0.25; +1 branch -> +0.05; no dead defs
    (should (equal (plist-get entry :callers) 40))
    (should (equal (plist-get entry :authors) 4))
    (should (equal (plist-get entry :cplx) 1))
    (should (null (plist-get entry :dead)))
    (should (< 1.9 (plist-get entry :score) 2.0))
    ;; reasons report only what stands out (callers, age, authors);
    ;; a single branch does not
    (should (equal (code-review-analysis--hunk-reasons entry)
                   '("40 callers" "lines 2y old" "4 authors")))
    ;; badge over the default threshold
    (should (equal (code-review-analysis--hunk-badge entry)
                   "  (risk: 40 callers; lines 2y old; 4 authors)"))
    ;; a definition with no references at all: dead-on-arrival
    (remhash "one" refs)
    (let ((entry2 (code-review-analysis--hunk-entry "a.py" hunk refs blame)))
      (should (equal (plist-get entry2 :dead) '("one")))
      (should (cl-some (lambda (s) (string= s "1 dead def"))
                       (code-review-analysis--hunk-reasons entry2))))))

(ert-deftest code-review-analysis/hunk-entry-without-blame ()
  "No blame (working-tree review, budget spent, rev missing in a
partial clone): age and ownership ingredients are absent, the
rest of the score still works."
  (let* ((refs (make-hash-table :test #'equal))
         (hunk (list :ranges "-1,4 +1,6"
                     :old '((1 . "context") (2 . "def one(x):"))
                     :added '((2 . "def one(x):"))
                     :deleted '((2 . "def one(x):")))))
    (puthash "one" (make-list 40 '("caller.py" 1 "one()")) refs)
    (let ((entry (code-review-analysis--hunk-entry "a.py" hunk refs nil)))
      (should (null (plist-get entry :median-age)))
      ;; authors/ownership ingredient absent without blame (0, not
      ;; a count of authors)
      (should (equal (plist-get entry :authors) 0))
      (should (equal (plist-get entry :callers) 40))
      ;; blast radius alone: exactly 1.0
      (should (equal (plist-get entry :score) 1.0)))))

(ert-deftest code-review-analysis/hunks-entries-and-order ()
  "One blame pass per file, entries sorted hottest first, the
report floor drops the uninteresting ones.  Real git history: the
diff rewrites a definition with 40 (faked) references in a repo
whose lines are days old."
  (let* ((repo (code-review-analysis-test--make-repo
                '(("lib.py" . "def one(x):\n    return x\n"))))
         (default-directory repo))
    (with-temp-file (expand-file-name "lib.py" repo)
      (insert "def one(x, y):\n    if x and y:\n        return x\n    return x - 1\n"))
    (call-process "git" nil nil nil "add" "-A")
    (call-process "git" nil nil nil "commit" "-m" "second")
    (let* ((diff (with-temp-buffer
                   (call-process "git" nil t nil
                                 "diff" "HEAD^..HEAD" "--no-color")
                   (buffer-string)))
           (blocks (code-review--diff--split-by-files diff))
           (refs (make-hash-table :test #'equal))
           (pr (code-review-analysis-test-pr))
           entries)
      (puthash "one" (make-list 40 '("caller.py" 1 "one()")) refs)
      (oset pr base-ref-name "HEAD^..HEAD")
      (setq entries (code-review-analysis--hunks repo blocks refs pr))
      ;; one entry for the one hunk, blast-radius driven
      (should (= 1 (length entries)))
      (let ((e (car entries)))
        (should (equal (plist-get e :path) "lib.py"))
        (should (equal (plist-get e :ranges) "-1,2 +1,4"))
        (should (equal (plist-get e :callers) 40))
        (should (equal (plist-get e :authors) 1))
        ;; fresh repo: age ~0 days, one branch: nothing but callers
        (should (equal (code-review-analysis--hunk-reasons e)
                       '("40 callers")))))))

(ert-deftest code-review-analysis/delicate-hunks-topk ()
  "The top-K accessor filters under the threshold and caps the
list at `code-review-analysis-delicacy-top-k'."
  (let ((orig (symbol-function 'code-review-analysis-run)))
    (unwind-protect
        (progn
          (fset 'code-review-analysis-run
                (lambda ()
                  (list :hunks
                        (list (list :path "a.py" :ranges "-1,3 +1,4"
                                    :score 0.9 :callers 40)
                              (list :path "b.py" :ranges "-1,3 +1,4"
                                    :score 0.6 :callers 30)
                              (list :path "c.py" :ranges "-1,3 +1,4"
                                    :score 0.2 :callers 5)))))
          ;; default threshold 0.5 drops the 0.2 entry
          (should (equal (mapcar (lambda (e) (plist-get e :path))
                                 (code-review-analysis--delicate-hunks))
                         '("a.py" "b.py")))
          (let ((code-review-analysis-delicacy-top-k 1))
            (should (equal (mapcar (lambda (e) (plist-get e :path))
                                   (code-review-analysis--delicate-hunks))
                           '("a.py")))))
      (fset 'code-review-analysis-run orig))))
