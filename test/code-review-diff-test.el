;;; code-review-diff-test.el --- ERT tests for the diff classification engine -*- lexical-binding: t; -*-
;;
;; This file is part of code-review.

(require 'code-review-diff)

(defun code-review-test--diff-sample ()
  "A two-file synthetic diff.
The docs file comes first and the last block deliberately lacks
its trailing newline, like a trimmed stored diff."
  (concat
   "diff --git README.md README.md\n"
   "index e69de29..8b13789 100644\n"
   "--- README.md\n"
   "+++ README.md\n"
   "@@ -0,0 +1 @@\n"
   "+hello docs\n"
   "diff --git src/main.py src/main.py\n"
   "index 1111111..2222222 100644\n"
   "--- src/main.py\n"
   "+++ src/main.py\n"
   "@@ -1 +1 @@\n"
   "-old\n"
   "+new"))

(ert-deftest code-review-diff/split-by-files ()
  (let ((blocks (code-review--diff--split-by-files
                (code-review-test--diff-sample))))
    (should (equal (mapcar #'car blocks)
                   '("README.md" "src/main.py")))
    ;; the last block keeps its exact text, no trailing newline
    (should (string-suffix-p "+new" (cdr (nth 1 blocks))))))

(ert-deftest code-review-diff/classify-tags ()
  (let ((table (code-review--diff--classify-diff
                (code-review-test--diff-sample))))
    ;; the built-in noise rules tag .md files as DOC
    (should (equal "DOC" (plist-get (gethash "README.md" table) :tag)))
    ;; source files carry no tag
    (should (not (plist-get (gethash "src/main.py" table) :tag)))))


(ert-deftest code-review-diff/classify-org-as-doc ()
  ;; org files (plans, release notes, journals) are docs, not code:
  ;; the phase 17 CI-gaming scan relies on this to skip them whole
  (should (equal "DOC" (plist-get
                       (code-review--diff--classify-path "Improvements.org")
                       :tag)))
  (should (equal "DOC" (plist-get
                       (code-review--diff--classify-path "docs/guide.org")
                       :tag))))


(ert-deftest code-review-diff/reorder-never-glues-blocks ()
  "Reordering must never glue two file blocks on one line.
Regression: a diff trimmed of its trailing newline used to end
up with the next block's header glued to the last hunk line."
  (let* ((sample (code-review-test--diff-sample))
         (reordered (code-review--maybe-reorder-diff
                     sample
                     (code-review--diff--classify-diff sample))))
    ;; docs sink to the bottom: main.py now comes first
    (should (string-prefix-p "diff --git src/main.py" reordered))
    (should (> (string-match "diff --git README.md" reordered) 0))
    ;; every header starts at the beginning of a line: no glue
    (should (not (string-match-p "[^\n]diff --git " reordered)))
    ;; the result ends newline-terminated so the wash sees a full line
    (should (string-suffix-p "\n" reordered))))

;;; Phase 13, slice 1: new-since-last-review (the db snapshot IS
;;; "what the reviewer last saw")

(defun code-review-test--diff-sample-v2 ()
  "The sample diff after new commits: README.md untouched,
src/main.py with a new hunk, plus a brand new file."
  (concat
   "diff --git README.md README.md\n"
   "index e69de29..8b13789 100644\n"
   "--- README.md\n"
   "+++ README.md\n"
   "@@ -0,0 +1 @@\n"
   "+hello docs\n"
   "diff --git src/main.py src/main.py\n"
   "index 1111111..3333333 100644\n"
   "--- src/main.py\n"
   "+++ src/main.py\n"
   "@@ -1,2 +1,3 @@\n"
   " new\n"
   "+newer\n"
   "diff --git src/extra.py src/extra.py\n"
   "index 0000000..4444444 100644\n"
   "--- /dev/null\n"
   "+++ src/extra.py\n"
   "@@ -0,0 +1 @@\n"
   "+added"))

(ert-deftest code-review-diff-test/new-since-last-review-no-snapshot ()
  "Without a previous snapshot there is nothing to compare: nil."
  (should (null (code-review-diff--new-since-last-review
                 nil (code-review-test--diff-sample)
                 nil '("a" "b")))))

(ert-deftest code-review-diff-test/new-since-last-review-added-changed-untouched ()
  "A changed block and a new path are reported, the untouched
file is not; commits missing from the snapshot are listed."
  (let ((news (code-review-diff--new-since-last-review
               (code-review-test--diff-sample)
               (code-review-test--diff-sample-v2)
               '("aaa" "bbb")
               '("bbb" "ccc" "ddd"))))
    (should (equal (a-get news 'files)
                   '("src/main.py" "src/extra.py")))
    (should (equal (a-get news 'commits)
                   '("ccc" "ddd")))))

(ert-deftest code-review-diff-test/new-since-last-review-nothing-changed ()
  "Identical snapshots and commit lists yield the EMPTY alist (the
\"no new changes\" signal), not nil — nil is reserved for \"no
previous snapshot\"."
  (let ((news (code-review-diff--new-since-last-review
               (code-review-test--diff-sample)
               (code-review-test--diff-sample)
               '("aaa")
               '("aaa"))))
    (should news)
    (should (null (a-get news 'files)))
    (should (null (a-get news 'commits)))))

(ert-deftest code-review-diff-test/mark-new-files-wins-over-noise ()
  "The NEW tag overrides a DOC/GEN classification and is never
hidden by focus mode."
  (let* ((classifications (code-review--diff--classify-diff
                           (code-review-test--diff-sample))))
    (code-review-diff--mark-new-files
     classifications '("src/main.py"))
    (let ((info (gethash "src/main.py" classifications)))
      (should (equal (plist-get info :tag) "NEW"))
      (should (plist-get info :new))
      (should-not (plist-get info :hide)))
    ;; the untouched file keeps its own classification
    (should-not (plist-get (gethash "README.md" classifications) :new))))

(ert-deftest code-review-diff-test/reorder-puts-new-files-first ()
  "The new-since-last-review files sort ahead of everything else,
even files matched by user order rules."
  (let* ((diff-text (code-review-test--diff-sample))
         (classifications (code-review--diff--classify-diff diff-text)))
    (code-review-diff--mark-new-files classifications '("README.md"))
    (let* ((reordered (code-review--maybe-reorder-diff
                       diff-text classifications))
           (first-block (code-review--diff--split-by-files reordered)))
      (should (equal (car (car first-block)) "README.md")))))
