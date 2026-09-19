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
