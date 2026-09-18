;;; code-review-parse-hunk-test.el --- ERT tests for diff parsing -*- lexical-binding: t; -*-

(require 'ert)
(require 'code-review-parse-hunk)

(defvar code-review-parse-hunk-test--hunk-sample
  "@@ -2,14 +2,7 @@

 var hello = require('./hello.js');

-var names = [
-  'harry',
-  'barry',
-  'garry',
-  'harry',
-  'barry',
-  'marry',
-];
+var names = ['harry', 'barry', 'garry', 'harry', 'barry', 'marry'];

 var names2 = [
   'harry',
@@ -23,9 +16,7 @@ var names2 = [
 // after this line new chunk will be created
 var names3 = [
   'harry',
-  'barry',
-  'garry',
   'harry',
   'barry',
-  'marry',
+  'marry', 'garry',
 ];")

(defvar code-review-parse-hunk-test--expected-hunk-table
  `(((type . "normal") (normal . t) (ln1 . 2) (ln2 . 2) (relative . 1))
    ((type . "normal") (normal . t) (ln1 . 3) (ln2 . 3) (relative . 2))
    ((type . "normal") (normal . t) (ln1 . 4) (ln2 . 4) (relative . 3))
    ((type . "del") (del . t) (ln . 5) (relative . 4))
    ((type . "del") (del . t) (ln . 6) (relative . 5))
    ((type . "del") (del . t) (ln . 7) (relative . 6))
    ((type . "del") (del . t) (ln . 8) (relative . 7))
    ((type . "del") (del . t) (ln . 9) (relative . 8))
    ((type . "del") (del . t) (ln . 10) (relative . 9))
    ((type . "del") (del . t) (ln . 11) (relative . 10))
    ((type . "del") (del . t) (ln . 12) (relative . 11))
    ((type . "add") (add . t) (ln . 5) (relative . 12))
    ((type . "normal") (normal . t) (ln1 . 13) (ln2 . 6) (relative . 13))
    ((type . "normal") (normal . t) (ln1 . 14) (ln2 . 7) (relative . 14))
    ((type . "normal") (normal . t) (ln1 . 15) (ln2 . 8) (relative . 15))
    ((type . "normal") (normal . t) (ln1 . 23) (ln2 . 16) (relative . 17))
    ((type . "normal") (normal . t) (ln1 . 24) (ln2 . 17) (relative . 18))
    ((type . "normal") (normal . t) (ln1 . 25) (ln2 . 18) (relative . 19))
    ((type . "del") (del . t) (ln . 26) (relative . 20))
    ((type . "del") (del . t) (ln . 27) (relative . 21))
    ((type . "normal") (normal . t) (ln1 . 28) (ln2 . 19) (relative . 22))
    ((type . "normal") (normal . t) (ln1 . 29) (ln2 . 20) (relative . 23))
    ((type . "del") (del . t) (ln . 30) (relative . 24))
    ((type . "add") (add . t) (ln . 21) (relative . 25))
    ((type . "normal") (normal . t) (ln1 . 31) (ln2 . 22) (relative . 26))))

(ert-deftest code-review-parse-hunk-test/table ()
  "Given a hunk, we return a table describing each hunk line."
  (should (equal (code-review-parse-hunk-table
                  code-review-parse-hunk-test--hunk-sample)
                 code-review-parse-hunk-test--expected-hunk-table)))

(ert-deftest code-review-parse-hunk-test/relative-pos-old-line ()
  (let ((table (code-review-parse-hunk-table
                code-review-parse-hunk-test--hunk-sample)))
    (should (equal (code-review-parse-hunk-relative-pos
                    table `((old . t) (line-pos . 23)))
                   17))))

(ert-deftest code-review-parse-hunk-test/relative-pos-deleted-line ()
  (let ((table (code-review-parse-hunk-table
                code-review-parse-hunk-test--hunk-sample)))
    (should (equal (code-review-parse-hunk-relative-pos
                    table `((old . t) (line-pos . 6)))
                   5))))

(ert-deftest code-review-parse-hunk-test/relative-pos-another-deleted-line ()
  (let ((table (code-review-parse-hunk-table
                code-review-parse-hunk-test--hunk-sample)))
    (should (equal (code-review-parse-hunk-relative-pos
                    table `((old . t) (line-pos . 30)))
                   24))))

(ert-deftest code-review-parse-hunk-test/relative-pos-new-line ()
  (let ((table (code-review-parse-hunk-table
                code-review-parse-hunk-test--hunk-sample)))
    (should (equal (code-review-parse-hunk-relative-pos
                    table `((new . t) (line-pos . 5)))
                   12))))

(ert-deftest code-review-parse-hunk-test/relative-pos-another-new-line ()
  (let ((table (code-review-parse-hunk-table
                code-review-parse-hunk-test--hunk-sample)))
    (should (equal (code-review-parse-hunk-relative-pos
                    table `((new . t) (line-pos . 21)))
                   25))))

(ert-deftest code-review-parse-hunk-test/line-pos-added ()
  (let ((table (code-review-parse-hunk-table
                code-review-parse-hunk-test--hunk-sample)))
    (should (equal (code-review-parse-hunk-line-pos
                    table `((added . t) (line-pos . 25)))
                   21))))

(ert-deftest code-review-parse-hunk-test/line-pos-deleted ()
  (let ((table (code-review-parse-hunk-table
                code-review-parse-hunk-test--hunk-sample)))
    (should (equal (code-review-parse-hunk-line-pos
                    table `((deleted . t) (line-pos . 21)))
                   27))))

(ert-deftest code-review-parse-hunk-test/line-pos-normal ()
  (let ((table (code-review-parse-hunk-table
                code-review-parse-hunk-test--hunk-sample)))
    (should (equal (code-review-parse-hunk-line-pos
                    table `((normal . t) (line-pos . 23)))
                   `((old-line . 29)
                     (new-line . 20))))))

;;; code-review-parse-hunk-test.el ends here
