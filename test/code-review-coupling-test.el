;;; code-review-coupling-test.el --- ERT tests for phase 18 (change coupling) -*- lexical-binding: t; -*-
;;
;; This file is part of code-review.

(require 'code-review-coupling)
(require 'code-review-history)

;;; Pure: parse, matrix, findings

(ert-deftest code-review-coupling/parse-changesets ()
  ;; the code-compass log format: one --%h--%ad--%aN marker per
  ;; commit, then its files; merge commits list none; bot commits
  ;; are skipped (bot churn is not coupling); file lines before
  ;; any marker are ignored
  (let ((sets (code-review-coupling--parse-changesets
               "--1a2b3c--2026-01-02--andi
src/a.el
src/b.el
--2b3c4d--2026-01-03--bob
src/a.el
--3c4d5e--2026-01-04--andi

--4d5e6f--2026-01-05--renovate[bot]
src/c.el
--5e6f70--2026-01-06--andi
src/c.el
src/a.el
src/c.el")))
    ;; no bot filter: the renovate commit's files are kept too
    (should (equal sets
                   '(("src/a.el" "src/b.el")
                     ("src/a.el")
                     ("src/c.el")
                     ("src/c.el" "src/a.el" "src/c.el"))))
    ;; bot regexp skips the renovate commit entirely
    (should (null (code-review-coupling--parse-changesets
                   "--1a2b3c--2026-01-05--renovate[bot]\nsrc/c.el\n"
                   "renovate")))
    ;; the bot-filtered parse of the SAME log: the renovate
    ;; commit's singleton is gone
    (should (equal (code-review-coupling--parse-changesets
                    "--4d5e6f--2026-01-05--renovate[bot]\nsrc/c.el\n
--5e6f70--2026-01-06--andi\nsrc/c.el\nsrc/a.el\nsrc/c.el\n"
                    "renovate")
                   '(("src/c.el" "src/a.el" "src/c.el"))))
    (should (equal (code-review-coupling--parse-changesets
                    "src/a.el\n--1a2b3c--2026-01-02--andi\nsrc/b.el\n")
                   '(("src/b.el"))))))

(ert-deftest code-review-coupling/matrix ()
  ;; pair co-change counts and per-file revision counts from the
  ;; changesets; duplicates inside ONE changeset count once
  (let* ((data (code-review-coupling--matrix
                '(("a.py" "tests/test_a.py")
                  ("a.py" "tests/test_a.py")
                  ("a.py" "tests/test_a.py")
                  ("a.py" "b.py" "tests/test_a.py")
                  ("a.py")
                  ("zz.py"))))
         (pairs (plist-get data :pairs))
         (revs (plist-get data :revisions)))
    (should (= (gethash "a.py" revs) 5))
    (should (= (gethash "tests/test_a.py" revs) 4))
    (should (= (gethash "b.py" revs) 1))
    (should (= (gethash (code-review-coupling--pair-key
                         "a.py" "tests/test_a.py")
                        pairs)
               4))
    (should (= (gethash (code-review-coupling--pair-key "a.py" "b.py")
                        pairs)
               1))
    ;; single-file changesets count revisions but pair nothing
    (should (null (gethash (code-review-coupling--pair-key
                            "zz.py" "a.py")
                           pairs)))))

(ert-deftest code-review-coupling/matrix-changeset-cap ()
  ;; changesets wider than the cap are reformatting/merge noise and
  ;; are skipped whole: no revisions, no pairs (code-maat's
  ;; --max-changeset-size); cap 0 = uncapped
  (let* ((wide (cl-loop for i to 15 collect (format "f%02d.el" i)))
         (data (code-review-coupling--matrix
                (list wide '("a.el" "b.el")) 15))
         (revs (plist-get data :revisions))
         (pairs (plist-get data :pairs)))
    (should (null (gethash "f00.el" revs)))
    (should (= (gethash "a.el" revs) 1))
    (should (= (gethash (code-review-coupling--pair-key "a.el" "b.el")
                        pairs)
               1)))
  (let* ((wide (cl-loop for i to 15 collect (format "f%02d.el" i)))
         (data (code-review-coupling--matrix (list wide) 0))
         (revs (plist-get data :revisions)))
    (should (= (gethash "f00.el" revs) 1))))

(defun code-review-coupling-test--data ()
  "Fixture matrix: a.py co-changes with its test at 40/55 (73%),
with b.py at 5/55 (9%, under the threshold); b.py co-changes with
its test at 4/5 (80%)."
  (code-review-coupling--matrix
   (append
    (make-list 40 '("a.py" "tests/test_a.py"))
    (make-list 10 '("a.py"))
    (make-list 5 '("a.py" "b.py"))
    (make-list 4 '("b.py" "tests/test_b.py"))
    '(("b.py")))))

(ert-deftest code-review-coupling/findings ()
  (let ((data (code-review-coupling-test--data)))
    ;; only a.py changed: its strong peer (the coupled TEST file)
    ;; is THE finding; the weak b.py pair is under the threshold
    (let ((fs (code-review-coupling--findings data '("a.py"))))
      (should (= 1 (length fs)))
      (should (equal (plist-get (car fs) :peer) "tests/test_a.py"))
      (should (equal (plist-get (car fs) :co) 40))
      (should (>= (plist-get (car fs) :degree) 0.7))
      (should (plist-get (car fs) :test-p)))
    ;; a peer IN the PR is not a finding; with both files changed,
    ;; each flags its own untouched peer, strongest first
    (let ((fs (code-review-coupling--findings data '("a.py" "b.py"))))
      (should (= 2 (length fs)))
      (should (equal (plist-get (car fs) :peer) "tests/test_a.py"))
      (should (equal (plist-get (cadr fs) :peer) "tests/test_b.py")))
    ;; the PR touches everything coupled: no findings
    (should (null (code-review-coupling--findings
                   data '("a.py" "b.py" "tests/test_a.py"
                          "tests/test_b.py"))))
    ;; findings are capped
    (let ((code-review-coupling-max-findings 1))
      (should (= 1 (length (code-review-coupling--findings
                            data '("a.py" "b.py"))))))))

;;; The shared harvest round trip

(ert-deftest code-review-coupling/harvest-round-trip ()
  "The phase 14 harvest stores the coupling matrix beside the
metrics (ONE git log, both parses), and the completeness check
finds the left-behind coupled test file."
  (skip-unless (code-review-history--compass-p))
  (let* ((repo (make-temp-file "cr-coupling-" t))
         (git (lambda (&rest args)
                (apply #'call-process "git" nil nil nil
                       "-C" repo args)))
         key)
    (unwind-protect
        (progn
          (funcall git "init" "--initial-branch=main")
          (funcall git "config" "user.email" "test@example.com")
          (funcall git "config" "user.name" "Test")
          ;; 5 coupled source+test commits, then one source-only:
          ;; the test file is a 5/6 (83%) peer of lib.py
          (cl-loop for i to 4
                   do (progn
                        (with-temp-file (expand-file-name "lib.py" repo)
                          (insert (format "# rev %d\n" i)))
                        (make-directory (expand-file-name "tests" repo) t)
                        (with-temp-file (expand-file-name
                                         "tests/test_lib.py" repo)
                          (insert (format "# test rev %d\n" i)))
                        (funcall git "add" ".")
                        (funcall git "commit" "-m" (format "coupled %d" i))))
          (with-temp-file (expand-file-name "lib.py" repo)
            (insert "# alone\n"))
          (funcall git "add" ".")
          (funcall git "commit" "-m" "source only")
          ;; the shared harvest: metrics AND coupling, one log
          (should (code-review-history--harvest-sync repo))
          (setq key (code-review-history--repo-key repo))
          (let* ((ddata (code-review-history-data repo))
                 (matrix (plist-get ddata :coupling))
                 (revs (plist-get matrix :revisions)))
            (should matrix)
            (should (= (gethash "lib.py" revs) 6))
            (should (= (gethash "tests/test_lib.py" revs) 5)))
          ;; a PR touching only lib.py: the coupled test is THE
          ;; completeness finding (positive coupling broken)
          (let ((fs (code-review-coupling--findings
                     (plist-get (code-review-history-data repo) :coupling)
                     '("lib.py"))))
            (should (= 1 (length fs)))
            (should (equal (plist-get (car fs) :peer)
                           "tests/test_lib.py"))
            (should (plist-get (car fs) :test-p))
            (should (>= (plist-get (car fs) :degree) 0.8)))
          ;; cold reload: the versioned cache file round-trips
          (clrhash code-review-history--cache)
          (should (plist-get (code-review-history-data repo) :coupling)))
      (ignore-errors (remhash key code-review-history--cache))
      (ignore-errors
        (delete-directory (expand-file-name "code-review" key) t))
      (ignore-errors (delete-directory repo t)))))

;;; code-review-coupling-test.el ends here
