;;; code-review-analysis-test.el --- ERT tests for the analysis engine -*- lexical-binding: t; -*-
;;
;; Phase 5 tests.  These are pure-function tests plus one git
;; integration test: they must run in a fresh batch emacs so a stale
;; daemon cannot hide missing definitions (see AGENTS.md).

(require 'ert)
(require 'cl-lib)
(require 'code-review-analysis)

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
