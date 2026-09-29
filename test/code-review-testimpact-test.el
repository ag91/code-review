;;; code-review-testimpact-test.el --- ERT tests for phase 17 -*- lexical-binding: t; -*-
;;
;; Phase 17 tests: the CI-gaming scan and the test mapping are pure
;; functions over diff text (plus one batched git grep); the async
;; runner is exercised with short-lived shell processes and a poll
;; deadline (the deferred-render precedent: batch processes consume
;; process output during `sleep-for').  They must run in a fresh
;; batch emacs so a stale daemon cannot hide missing definitions.

(require 'ert)
(require 'cl-lib)
(require 'code-review-testimpact)
(require 'code-review-analysis)
(require 'code-review-diff)
(require 'code-review-test-helpers)

;;; Fixtures

(defun code-review-testimpact-test--make-repo (files-alist)
  "Create a temp git repository with FILES-ALIST committed at HEAD.
Return its directory (with trailing slash)."
  (let* ((dir (file-name-as-directory (make-temp-file "cr-testimpact-" t)))
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

(defun code-review-testimpact-test--wait (pred)
  "Poll PRED until true (10s deadline); return the last result.
Batch emacs consumes process output during `sleep-for', so
sentinels fire while we wait."
  (let ((deadline (time-add (current-time) 10))
        (res nil))
    (while (and (not (setq res (funcall pred)))
                (time-less-p (current-time) deadline))
      (sleep-for 0.05))
    res))

;;; CI-gaming scan (pure)

(ert-deftest code-review-testimpact/scan-ci-skip-marker-added ()
  (let* ((block "diff --git a/tests/test_a.py b/tests/test_a.py
--- a/tests/test_a.py
+++ b/tests/test_a.py
@@ -3,2 +3,3 @@
 def test_ok():
+    @pytest.mark.skip(reason=\"flaky\")
     assert 1 == 1
")
         (findings (code-review-testimpact--scan-ci
                    (list (cons "tests/test_a.py" block)))))
    (should (= 1 (length findings)))
    (should (eq (plist-get (car findings) :kind) 'skip))
    (should (equal (plist-get (car findings) :path) "tests/test_a.py"))
    (should (equal (plist-get (car findings) :line) 4))))

(ert-deftest code-review-testimpact/scan-ci-removing-skip-is-clean ()
  ;; removing a skip marker is good news, not gaming
  (let* ((block "diff --git a/tests/test_a.py b/tests/test_a.py
--- a/tests/test_a.py
+++ b/tests/test_a.py
@@ -3,3 +3,2 @@
-    @pytest.mark.skip(reason=\"flaky\")
 def test_ok():
     assert 1 == 1
")
         (findings (code-review-testimpact--scan-ci
                    (list (cons "tests/test_a.py" block)))))
    (should (null findings))))

(ert-deftest code-review-testimpact/scan-ci-clean-change ()
  (let* ((block "diff --git a/lib.py b/lib.py
--- a/lib.py
+++ b/lib.py
@@ -1,2 +1,3 @@
 def keep():
+    return 2
     pass
")
         (findings (code-review-testimpact--scan-ci
                    (list (cons "lib.py" block)))))
    (should (null findings))))

(ert-deftest code-review-testimpact/scan-ci-doc-files-are-skipped ()
  ;; docs legitimately MENTION the markers in prose (the detector
  ;; flagged its own release notes once): .org/.md noise-classify
  ;; as DOC and the scan skips them whole
  (let* ((block "diff --git a/Improvements.org b/Improvements.org
--- a/Improvements.org
+++ b/Improvements.org
@@ -2421,2 +2421,4 @@
 old prose
+  =@pytest.mark.skip= and =xit(= are gaming markers
+  =t.Skip= too
 more prose
")
         (findings (code-review-testimpact--scan-ci
                    (list (cons "Improvements.org" block)))))
    (should (null findings))))

(ert-deftest code-review-testimpact/scan-ci-comment-lines-are-skipped ()
  ;; comments legitimately mention markers too: a # comment line is
  ;; dropped, code lines still flag
  (let* ((py-block "diff --git a/tests/test_a.py b/tests/test_a.py
--- a/tests/test_a.py
+++ b/tests/test_a.py
@@ -1,1 +1,3 @@
 context
+    # like pytest.skip( here
+    pytest.skip(\"flaky\")
")
         (yml-block "diff --git a/.github/workflows/ci.yml b/.github/workflows/ci.yml
--- a/.github/workflows/ci.yml
+++ b/.github/workflows/ci.yml
@@ -1,1 +1,3 @@
 context
+    # an || true here is just a comment
+    - run: make test || true
")
         (findings (code-review-testimpact--scan-ci
                    (list (cons "tests/test_a.py" py-block)
                          (cons ".github/workflows/ci.yml" yml-block)))))
    (should (= 2 (length findings)))
    (should (equal (mapcar (lambda (f) (plist-get f :kind)) findings)
                   '(skip ci-true)))))

(ert-deftest code-review-testimpact/non-comment-filter ()
  ;; the comment-only filter itself, per prefix, including the
  ;; elisp ;; branch (elisp has no skip markers, so the scan tests
  ;; cannot exercise it with findings)
  (should (equal (code-review-testimpact--non-comment
                  '((1 . ";; note") (2 . "(code)")
                    (3 . "  ;; indented note"))
                  ";;")
                 '((2 . "(code)"))))
  (should (equal (code-review-testimpact--non-comment
                  '((1 . "# note") (2 . "code"))
                  "#")
                 '((2 . "code"))))
  ;; nil prefix (unknown file type): nothing is filtered
  (should (equal (code-review-testimpact--non-comment
                  '((1 . ";; anything"))
                  nil)
                 '((1 . ";; anything")))))

(ert-deftest code-review-testimpact/scan-ci-markers-are-language-scoped ()
  ;; a PYTHON marker in an elisp string is not gaming: markers
  ;; apply only to their own languages (the detector flagged its
  ;; own test fixtures before this guard)
  (let* ((el-block "diff --git a/lib.el b/lib.el
--- a/lib.el
+++ b/lib.el
@@ -1,1 +1,3 @@
 context
+    @pytest.mark.skip(reason=\"flaky\")
+    (xit (\"nope\"))
")
         (ts-block "diff --git a/a.test.ts b/a.test.ts
--- a/a.test.ts
+++ b/a.test.ts
@@ -1,1 +1,3 @@
 context
+    it.skip(\"flaky\")
+    describe.skip(\"flaky too\")
")
         (findings (code-review-testimpact--scan-ci
                    (list (cons "lib.el" el-block)
                          (cons "a.test.ts" ts-block)))))
    ;; only the JS markers in the TS file flag
    (should (= 2 (length findings)))
    (should (equal (plist-get (car findings) :path) "a.test.ts"))
    (should (equal (plist-get (cadr findings) :path) "a.test.ts"))))

(ert-deftest code-review-testimpact/scan-ci-legacy-string-markers-any-file ()
  ;; a bare string entry (the old defcustom format) scans any file
  (let ((code-review-testimpact-skip-marker-regexps
         '("\\_<skipAlways\\s-*(")))
    (let* ((block "diff --git a/lib.el b/lib.el
--- a/lib.el
+++ b/lib.el
@@ -1,1 +1,2 @@
 context
+    skipAlways(1)
")
           (findings (code-review-testimpact--scan-ci
                      (list (cons "lib.el" block)))))
      (should (= 1 (length findings)))
      (should (eq (plist-get (car findings) :kind) 'skip)))))

(ert-deftest code-review-testimpact/scan-ci-workflow-gaming ()
  (let* ((block "diff --git a/.github/workflows/ci.yml b/.github/workflows/ci.yml
--- a/.github/workflows/ci.yml
+++ b/.github/workflows/ci.yml
@@ -10,4 +10,4 @@
 jobs:
-  - run: make test
+  - run: make test || true
+      continue-on-error: true
")
         (findings (code-review-testimpact--scan-ci
                    (list (cons ".github/workflows/ci.yml" block))))
         (kinds (sort (mapcar (lambda (f) (symbol-name (plist-get f :kind)))
                              findings)
                      #'string<)))
    (should (equal kinds '("ci-true" "gate" "removed")))
    ;; removed: the deleted `- run:' step, old-side line 11
    (should (equal (plist-get (cl-find 'removed findings
                                       :key (lambda (f) (plist-get f :kind)))
                              :line)
                   11))))

(ert-deftest code-review-testimpact/scan-ci-workflow-steps-untouched ()
  ;; a CI file whose run steps merely MOVE (deleted + re-added
  ;; identically) still reports the deletion: the scan is
  ;; deliberately loud on CI files, the reviewer decides.
  (let* ((block "diff --git a/.github/workflows/ci.yml b/.github/workflows/ci.yml
--- a/.github/workflows/ci.yml
+++ b/.github/workflows/ci.yml
@@ -10,4 +10,5 @@
 jobs:
-  - run: make test
+  - run: make test
")
         (findings (code-review-testimpact--scan-ci
                    (list (cons ".github/workflows/ci.yml" block)))))
    (should (= 1 (length findings)))
    (should (eq (plist-get (car findings) :kind) 'removed))))

(ert-deftest code-review-testimpact/scan-ci-threshold-lowered ()
  (let* ((block "diff --git a/pyproject.toml b/pyproject.toml
--- a/pyproject.toml
+++ b/pyproject.toml
@@ -1,3 +1,3 @@
 [tool.coverage.report]
-fail_under = 85
+fail_under = 60
")
         (findings (code-review-testimpact--scan-ci
                    (list (cons "pyproject.toml" block)))))
    (should (= 1 (length findings)))
    (should (eq (plist-get (car findings) :kind) 'threshold))
    (should (string-match-p "was 85; now 60" (plist-get (car findings) :text)))))

(ert-deftest code-review-testimpact/scan-ci-threshold-raised-is-clean ()
  (let* ((block "diff --git a/pyproject.toml b/pyproject.toml
--- a/pyproject.toml
+++ b/pyproject.toml
@@ -1,3 +1,3 @@
 [tool.coverage.report]
-fail_under = 60
+fail_under = 85
")
         (findings (code-review-testimpact--scan-ci
                    (list (cons "pyproject.toml" block)))))
    (should (null findings))))

(ert-deftest code-review-testimpact/scan-ci-caps-megabyte-line ()
  ;; the phase 5 lesson: raw diff lines can be megabytes long and
  ;; must never reach a regexp uncapped.  No error, no finding.
  (let* ((big (make-string 500000 ?x))
         (block (concat "diff --git a/data.json b/data.json\n"
                        "--- a/data.json\n"
                        "+++ b/data.json\n"
                        "@@ -1,1 +1,2 @@\n"
                        "+" big "\n")))
    (should (null (code-review-testimpact--scan-ci
                   (list (cons "data.json" block)))))))

;;; Classification (pure)

(ert-deftest code-review-testimpact/test-file-p ()
  (should (code-review-testimpact--test-file-p "Tests/test_foo.py"))
  (should (code-review-testimpact--test-file-p "src/app.test.ts"))
  (should (code-review-testimpact--test-file-p "test/test_a.py"))
  (should-not (code-review-testimpact--test-file-p "src/app.py"))
  (should-not (code-review-testimpact--test-file-p "docs/notes.md")))

(ert-deftest code-review-testimpact/test-entry-p ()
  (should (code-review-testimpact--test-entry-p "test_foo"))
  (should (code-review-testimpact--test-entry-p "FooTest"))
  (should-not (code-review-testimpact--test-entry-p "helper")))

;;; Test mapping (one batched grep against a temp repository)

(ert-deftest code-review-testimpact/map-hunks ()
  (let* ((dir (code-review-testimpact-test--make-repo
               '(("lib.py" . "def used(x):
    return x

def untested(x):
    return x

def dead_thing(x):
    return x

def test_entry():
    pass
")
                 ("app.py" . "from lib import used, untested
code = used(1) + untested(2)
")
                 ("tests/test_lib.py" . "from lib import used

def test_used():
    assert used(1) == 1
"))))
         (diff "diff --git a/lib.py b/lib.py
--- a/lib.py
+++ b/lib.py
@@ -1,1 +1,5 @@
+def used(x):
+    return x
+
+def untested(x):
+    return x
@@ -10,1 +10,5 @@
+def dead_thing(x):
+    return x
+
+def test_entry():
+    pass
")
         (entries (code-review-testimpact--map-hunks
                   dir (code-review--diff--split-by-files diff))))
    (unwind-protect
        (progn
          (should (= 2 (length entries)))
          (let ((e1 (car entries))
                (e2 (cadr entries)))
            ;; used: referenced by tests/test_lib.py -> covered
            ;; untested: referenced from app.py only -> NO-TEST
            (should (equal (plist-get e1 :ranges) "-1,1 +1,5"))
            (should (equal (plist-get e1 :defs) '("used" "untested")))
            (should (equal (plist-get e1 :tests) '("tests/test_lib.py")))
            (should (equal (plist-get e1 :notest) '("untested")))
            ;; dead_thing: no references at all (phase 5 reports it
            ;; as possibly dead, not a coverage gap);
            ;; test_entry: build-convention entry point
            (should (equal (plist-get e2 :defs) '("dead_thing" "test_entry")))
            (should (null (plist-get e2 :tests)))
            (should (null (plist-get e2 :notest)))))
      (delete-directory dir :recursive))))

(ert-deftest code-review-testimpact/map-hunks-doc-files-are-skipped ()
  ;; a user noise rule can tag a source file DOC; its prose-style
  ;; lines are then not definitions either
  (let ((code-review-diff-noise-rules
         '((:match "special\\.py\\'" :tag "DOC"))))
    (let* ((dir (code-review-testimpact-test--make-repo
                 '(("special.py" . "old
"))))
           (block "diff --git a/special.py b/special.py
--- a/special.py
+++ b/special.py
@@ -1,1 +1,2 @@
 old
+def prose_not_code():
")
           (entries (code-review-testimpact--map-hunks
                     dir (list (cons "special.py" block)))))
      (unwind-protect
          (should (null entries))
        (delete-directory dir :recursive)))))

;;; Tags and subsets (pure, over the computed plist)

(ert-deftest code-review-testimpact/tags ()
  (let* ((tres (list
                :ci (list (list :kind 'skip :path "ci.yml" :line 3
                                :text "xit("))
                :hunks (list (list :path "lib.py" :ranges "-1,2 +1,3"
                                   :defs '("used") :tests '("t.py")
                                   :notest '("used")))
                :notest (list (list :path "lib.py" :ranges "-1,2 +1,3"
                                    :defs '("used"))))))
    (should (equal (code-review-testimpact--hunk-tag
                    tres "lib.py" "-1,2 +1,3")
                   "NO-TEST: used"))
    (should (null (code-review-testimpact--hunk-tag
                   tres "lib.py" "-9,9 +9,9")))
    (should (equal (code-review-testimpact--file-tag tres "ci.yml")
                   "CI-GAME"))
    (should (null (code-review-testimpact--file-tag tres "lib.py")))))

(ert-deftest code-review-testimpact/hunk-tag-caps-def-names ()
  (let* ((tres (list :hunks (list (list :path "a.py" :ranges "-1,2 +1,3"
                                        :defs '("a" "b" "c" "d" "e")
                                        :tests nil
                                        :notest '("a" "b" "c" "d" "e"))))))
    (should (equal (code-review-testimpact--hunk-tag
                    tres "a.py" "-1,2 +1,3")
                   "NO-TEST: a, b, c, ..."))))

(ert-deftest code-review-testimpact/tests-for ()
  (let ((tres (list :hunks
                    (list (list :path "a.py" :ranges "-1,2 +1,3"
                                :defs '("f") :tests '("tests/a.py"))
                          (list :path "a.py" :ranges "-9,2 +9,3"
                                :defs '("g")
                                :tests '("tests/b.py" "tests/a.py"))))))
    ;; file target: union over the file's hunks
    (let ((res (code-review-testimpact--tests-for tres "a.py" nil)))
      (should (equal (plist-get res :tests) '("tests/a.py" "tests/b.py")))
      (should (equal (plist-get res :defs) '("f" "g"))))
    ;; hunk target: its own defs only
    (let ((res (code-review-testimpact--tests-for
                tres "a.py" "-9,2 +9,3")))
      (should (equal (plist-get res :tests) '("tests/b.py" "tests/a.py")))
      (should (equal (plist-get res :defs) '("g"))))))

;;; Command building and verdict (pure)

(ert-deftest code-review-testimpact/expand ()
  (should (equal (code-review-testimpact--expand
                  "pytest %f" '("tests/a.py" "tests/b.py") '("used"))
                 "pytest tests/a.py tests/b.py"))
  (should (equal (code-review-testimpact--expand
                  "pytest -k %n" '("tests/a.py") '("used"))
                 "pytest -k used"))
  ;; odd file names get shell-PROTECTED (quoted or backslash-escaped,
  ;; shell-file-name dependent)
  (should-not (equal (code-review-testimpact--expand
                      "npx jest %f" '("tests/we ird.ts") nil)
                     "npx jest tests/we ird.ts")))

(ert-deftest code-review-testimpact/command-conventions ()
  ;; the built-in fallbacks: Makefile test target, pytest, go,
  ;; package.json test script, jest config; nothing recognized -> nil
  (let ((mk (make-temp-file "cr-ti-cmd-" t))
        (py (make-temp-file "cr-ti-cmd-" t))
        (go (make-temp-file "cr-ti-cmd-" t))
        (pj (make-temp-file "cr-ti-cmd-" t))
        (jest (make-temp-file "cr-ti-cmd-" t))
        (none (make-temp-file "cr-ti-cmd-" t)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "Makefile" mk)
            (insert "all:\n\techo hi\n\ntest:\n\temacs -Q --batch\n"))
          (with-temp-file (expand-file-name "pyproject.toml" py)
            (insert "[tool.pytest.ini_options]\naddopts = \"-q\"\n"))
          (with-temp-file (expand-file-name "go.mod" go)
            (insert "module example.com/x\n"))
          (with-temp-file (expand-file-name "package.json" pj)
            (insert "{\"scripts\": {\"test\": \"jest\"}}\n"))
          (with-temp-file (expand-file-name "jest.config.js" jest)
            (insert "export default {}\n"))
          (should (equal (code-review-testimpact--command-conventions mk)
                         "make test"))
          (should (equal (code-review-testimpact--command-conventions py)
                         "pytest %f"))
          (should (equal (code-review-testimpact--command-conventions go)
                         "go test ./..."))
          (should (equal (code-review-testimpact--command-conventions pj)
                         "npm test"))
          (should (equal (code-review-testimpact--command-conventions jest)
                         "npx jest %f"))
          (should (null (code-review-testimpact--command-conventions none))))
      (dolist (d (list mk py go pj jest none))
        (delete-directory d :recursive)))))

(ert-deftest code-review-testimpact/command-for-fallback-order ()
  ;; the user alist WINS over the conventions; with no entry (and
  ;; projectile/project.el not loaded in batch emacs), the Makefile
  ;; convention runs the full suite (its template has no %f)
  (let ((dir (make-temp-file "cr-ti-cmd-" t)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "Makefile" dir)
            (insert "test:\n\techo hi\n"))
          (let ((code-review-testimpact-test-command-alist
                 `((,(regexp-quote dir) . "pytest %f"))))
            (should (equal (code-review-testimpact--command-for
                            (file-name-as-directory dir)
                            '("tests/a.py") '("used"))
                           "pytest tests/a.py")))
          (should (equal (code-review-testimpact--command-for
                          (file-name-as-directory dir)
                          '("tests/a.py") '("used"))
                         "make test")))
      (delete-directory dir :recursive))))

(ert-deftest code-review-testimpact/status ()
  (should (eq (code-review-testimpact--status 0) 'pass))
  (should (eq (code-review-testimpact--status 3) 'fail))
  (should (eq (code-review-testimpact--status 'timeout) 'timeout))
  (should (null (code-review-testimpact--status "weird"))))

(ert-deftest code-review-testimpact/verdict ()
  ;; pass at HEAD and at BASE: the fake-fix signal
  (should (string-match-p
           "PRE-change"
           (code-review-testimpact--verdict 'pass 'pass)))
  ;; fails on pre-change code: the subset covers the change
  (should (string-match-p
           "covers the change"
           (code-review-testimpact--verdict 'pass 'fail)))
  (should (string-match-p
           "FAILS at HEAD"
           (code-review-testimpact--verdict 'fail 'fail)))
  (should (string-match-p
           "pre-existing failure"
           (code-review-testimpact--verdict 'fail 'fail)))
  (should (string-match-p
           "timed out"
           (code-review-testimpact--verdict 'timeout 'pass)))
  (should (string-match-p
           "no base comparison"
           (code-review-testimpact--verdict 'pass nil))))

;;; The async runner

(ert-deftest code-review-testimpact/run-async-exit-codes ()
  (let* ((dir (make-temp-file "cr-ti-async-" t))
         (result nil))
    (unwind-protect
        (progn
          (code-review-testimpact--run-async
           dir "exit 0"
           (lambda (code _out) (setq result code)))
          (should (code-review-testimpact-test--wait
                   (lambda () (equal result 0))))
          (code-review-testimpact--run-async
           dir "exit 3"
           (lambda (code _out) (setq result code)))
          (should (code-review-testimpact-test--wait
                   (lambda () (equal result 3))))
          (should (equal result 3)))
      (delete-directory dir :recursive))))

(ert-deftest code-review-testimpact/run-async-captures-output ()
  (let* ((dir (make-temp-file "cr-ti-async-" t))
         (result nil))
    (unwind-protect
        (progn
          (code-review-testimpact--run-async
           dir "echo code-review-marker"
           (lambda (_code out) (setq result out)))
          (should (code-review-testimpact-test--wait
                   (lambda () (and result
                                   (string-search "code-review-marker"
                                                  result)))))
          (should result))
      (delete-directory dir :recursive))))

(ert-deftest code-review-testimpact/run-async-timeout ()
  (let* ((dir (make-temp-file "cr-ti-async-" t))
         (code-review-testimpact-timeout 0.2)
         (result nil))
    (unwind-protect
        (progn
          (code-review-testimpact--run-async
           dir "sleep 5"
           (lambda (code _out) (setq result code)))
          (should (code-review-testimpact-test--wait
                   (lambda () (eq result 'timeout))))
          (should (eq result 'timeout)))
      (delete-directory dir :recursive))))

;;; code-review-testimpact-test.el ends here
