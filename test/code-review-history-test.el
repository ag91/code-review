;;; code-review-history-test.el --- ERT tests for phase 14 (review heat) -*- lexical-binding: t; -*-
;;
;; This file is part of code-review.

(require 'code-review-history)
(require 'code-review-diff)

(defun code-review-history-test--metrics ()
  "Fixture repository metrics: five files, one hotspot."
  (let ((m (make-hash-table :test #'equal)))
    (puthash "hot.py"
             (list :revisions 40
                   :authors '(("mrossi" . 30) ("alice" . 10))
                   :main-dev "mrossi" :last-touch "2026-09-01")
             m)
    (puthash "mid.py"
             (list :revisions 10 :authors '(("mrossi" . 10))
                   :main-dev "mrossi" :last-touch "2026-08-01")
             m)
    (puthash "cold.py"
             (list :revisions 1 :authors '(("alice" . 1))
                   :main-dev "alice" :last-touch "2026-01-01")
             m)
    (puthash "other1.py"
             (list :revisions 5 :authors '(("bob" . 5))
                   :main-dev "bob" :last-touch "2026-02-01")
             m)
    (puthash "other2.py"
             (list :revisions 5 :authors '(("bob" . 5))
                   :main-dev "bob" :last-touch "2026-02-01")
             m)
    m))

(defun code-review-history-test--worktree (dir)
  "Fixture PR-head WORKTREE inside DIR: one deeply nested file."
  (let ((hot (expand-file-name "hot.py" dir))
        (flat (expand-file-name "mid.py" dir)))
    (with-temp-file hot
      (insert "def f():\n"
              "    a = 1\n"
              "        b = 2\n"
              "                                        c = 3\n"))
    (with-temp-file flat
      (insert "x = 1\n"))
    dir))

(ert-deftest code-review-history/percentile-normalizes-per-repo ()
  (should (= (code-review-history--percentile 40 '(40 10 5 5 1)) 1.0))
  (should (= (code-review-history--percentile 10 '(40 10 5 5 1)) 0.8))
  (should (= (code-review-history--percentile 1 '(40 10 5 5 1)) 0.2))
  (should (= (code-review-history--percentile 1 nil) 0.0)))

(ert-deftest code-review-history/heat-ranks-and-explains ()
  (skip-unless (code-review-history--compass-p))
  (let* ((temp (make-temp-file "cr-history-" t))
         (worktree (code-review-history-test--worktree temp))
         (entries (code-review-history-heat
                   (code-review-history-test--metrics)
                   '("hot.py" "mid.py" "cold.py" "other1.py" "other2.py")
                   worktree
                   "mrossi")))
    (unwind-protect
        (progn
          ;; hottest first: churn x complexity (hot.py nests 10 deep)
          (should (equal (plist-get (nth 0 entries) :path) "hot.py"))
          (should (equal (plist-get (nth 0 entries) :bucket) "HOT"))
          (should (> (plist-get (nth 0 entries) :score)
                     (plist-get (nth 1 entries) :score)))
          ;; the PR author (mrossi) IS the main dev of hot.py: no
          ;; never-touched claim there
          (should-not (string-match-p "never touched it"
                                      (plist-get (nth 0 entries) :reason)))
          (should (string-match-p
                   "top 1% churn (40 revisions)"
                   (plist-get (nth 0 entries) :reason))
                   )
          (should (string-match-p "deep nesting (10)"
                                  (plist-get (nth 0 entries) :reason))
                  )
          (should (string-match-p "main dev: mrossi"
                                  (plist-get (nth 0 entries) :reason))
                  )
          ;; mid.py: WARM, author present (10 commits) so no claim
          (should (equal (plist-get (nth 1 entries) :bucket) "WARM"))
          (should (equal (plist-get (nth 1 entries) :author-revs) 10))
          (should-not (string-match-p "never touched it"
                                      (plist-get (nth 1 entries) :reason)))
          ;; other1.py: COLD, mrossi is a repo author but never
          ;; touched this file: the claim fires (and bumps the score)
          (let ((other (cl-find "other1.py" entries
                                :key (lambda (e) (plist-get e :path))
                                :test #'equal)))
            (should (equal (plist-get other :bucket) "COLD"))
            (should (equal (plist-get other :author-revs) 0))
            (should (string-match-p "mrossi never touched it"
                                    (plist-get other :reason))))
          ;; cold.py sinks last
          (should (equal (plist-get (nth 4 entries) :path) "cold.py"))
          (should (equal (plist-get (nth 4 entries) :bucket) "COLD")))
      (delete-directory temp t))))

(ert-deftest code-review-history/heat-never-touched-is-gated-by-author-pool ()
  "No `never touched it' claim for a PR author absent from the repo:
the login may simply commit under a different name."
  (skip-unless (code-review-history--compass-p))
  (let* ((temp (make-temp-file "cr-history-" t))
         (worktree (code-review-history-test--worktree temp))
         (entries (code-review-history-heat
                   (code-review-history-test--metrics)
                   '("other1.py")
                   worktree
                   "zebra")))
    (unwind-protect
        (progn
          (should (equal (plist-get (nth 0 entries) :author-revs) 0))
          (should-not (string-match-p "never touched it"
                                      (plist-get (nth 0 entries) :reason))))
      (delete-directory temp t))))

(ert-deftest code-review-history/heat-skips-bot-authors ()
  "A bot PR author gets no knowledge claims at all."
  (skip-unless (code-review-history--compass-p))
  (let* ((temp (make-temp-file "cr-history-" t))
         (worktree (code-review-history-test--worktree temp))
         (bot-regexp code-review-bot-author-regexp)
         (entries
          (unwind-protect
              (let ((code-review-bot-author-regexp "mrossi\\|coderabbit"))
                (code-review-history-heat
                 (code-review-history-test--metrics)
                 '("hot.py") worktree "mrossi"))
            (delete-directory temp t))))
    (should-not (string-match-p "never touched it"
                                (plist-get (nth 0 entries) :reason)))
    (should (equal bot-regexp code-review-bot-author-regexp))))

(ert-deftest code-review-history/tag-and-score-for ()
  (let ((code-review-history--order
         (list (list :path "a.py" :score 2.0 :bucket "HOT")
               (list :path "b.py" :score 0.5 :bucket "COLD"))))
    (should (equal (code-review-history--tag-for "a.py") "HOT"))
    (should (equal (code-review-history--tag-for "b.py") "COLD"))
    (should-not (code-review-history--tag-for "zzz.py"))
    (should (= (code-review-history--score-for "a.py") 2.0))
    ;; no heat data: everything scores 0 and no tags fire
    (let ((code-review-history--order nil))
      (should-not (code-review-history--tag-for "a.py"))
      (should (= (code-review-history--score-for "a.py") 0.0)))))

(defun code-review-history-test--diff-sample ()
  "A three-file synthetic diff: hotspot, docs noise, cold file."
  (concat
   "diff --git docs.md docs.md\n"
   "index e69de29..8b13789 100644\n"
   "--- docs.md\n"
   "+++ docs.md\n"
   "@@ -0,0 +1 @@\n"
   "+hello docs\n"
   "diff --git hot.py hot.py\n"
   "index 1111111..2222222 100644\n"
   "--- hot.py\n"
   "+++ hot.py\n"
   "@@ -1 +1 @@\n"
   "-old\n"
   "+new\n"
   "diff --git cold.py cold.py\n"
   "index 1111111..2222222 100644\n"
   "--- cold.py\n"
   "+++ cold.py\n"
   "@@ -1 +1 @@\n"
   "-old\n"
   "+new"))

(ert-deftest code-review-history/classify-tags-and-focus-hides-cold ()
  (let* ((code-review-history--order
          (list (list :path "hot.py" :score 2.0 :bucket "HOT")
                (list :path "cold.py" :score 0.1 :bucket "COLD")))
         (table (code-review--diff--classify-diff
                 (code-review-history-test--diff-sample)))
         (cold (code-review-history-test--diff-sample)))
    ;; heat tags fire for untagged files
    (should (equal (plist-get (gethash "hot.py" table) :tag) "HOT"))
    (should (equal (plist-get (gethash "cold.py" table) :tag) "COLD"))
    ;; noise classification wins over heat
    (should (equal (plist-get (gethash "docs.md" table) :tag) "DOC"))
    (should (plist-get (gethash "docs.md" table) :hide))
    ;; by default COLD files stay visible in focus mode
    (should-not (plist-get (gethash "cold.py" table) :hide))
    ;; ... and hide when `code-review-history-focus-hide-cold' says so
    (let ((code-review-history-focus-hide-cold t))
      (let ((table (code-review--diff--classify-diff cold)))
        (should (plist-get (gethash "cold.py" table) :hide))
        (should-not (plist-get (gethash "hot.py" table) :hide))))))

(ert-deftest code-review-history/reorder-unmatched-by-heat-noise-still-sinks ()
  (let* ((code-review-history--order
          (list (list :path "hot.py" :score 2.0 :bucket "HOT")
                (list :path "cold.py" :score 0.1 :bucket "COLD")))
         (sample (code-review-history-test--diff-sample))
         (reordered (code-review--maybe-reorder-diff
                     sample
                     (code-review--diff--classify-diff sample))))
    ;; hottest first among unmatched files
    (should (string-prefix-p "diff --git hot.py" reordered))
    ;; noise still sinks below COLD files
    (should (> (string-match "diff --git docs.md" reordered)
               (string-match "diff --git cold.py" reordered)))))

(ert-deftest code-review-history/harvest-and-cache-round-trip ()
  "The harvest reads real git history into the per-repo cache file."
  (skip-unless (code-review-history--compass-p))
  (let* ((repo (make-temp-file "cr-history-repo-" t))
         (git (lambda (&rest args)
                (apply #'call-process "git" nil nil nil
                       "-C" repo args)))
         key file)
    (unwind-protect
        (progn
          (funcall git "init" "--initial-branch=main")
          (funcall git "config" "user.email" "test@example.com")
          (with-temp-file (expand-file-name "a.py" repo)
            (insert "x = 1\n"))
          (funcall git "add" ".")
          (funcall git "-c" "user.name=Alice" "commit" "-m" "one")
          (with-temp-file (expand-file-name "a.py" repo)
            (insert "x = 2\n"))
          (funcall git "add" ".")
          (funcall git "-c" "user.name=Alice" "commit" "-m" "two")
          (with-temp-file (expand-file-name "b.py" repo)
            (insert "y = 1\n"))
          (funcall git "add" ".")
          (funcall git "-c" "user.name=Bob" "commit" "-m" "three")
          (with-temp-file (expand-file-name "c.py" repo)
            (insert "z = 1\n"))
          (funcall git "add" ".")
          (funcall git "-c" "user.name=renovate[bot]" "commit" "-m"
                   "chore: bump")
          ;; harvest with the default bot filter: the renovate
          ;; commit is not repository churn
          (let ((metrics (code-review-history--harvest-sync repo)))
            (should (equal (hash-table-count metrics) 2))
            (should (equal (plist-get (gethash "a.py" metrics)
                                     :revisions)
                           2))
            (should (equal (plist-get (gethash "a.py" metrics)
                                     :main-dev)
                           "Alice"))
            (should (equal (plist-get (gethash "b.py" metrics)
                                     :revisions)
                           1))
            (should-not (gethash "c.py" metrics)))
          ;; the per-repo cache file exists and reloads cold
          (setq key (code-review-history--repo-key repo)
                file (code-review-history--cache-file key))
          (should (file-exists-p file))
          (clrhash code-review-history--cache)
          (should (equal (plist-get (gethash "a.py"
                                             (plist-get
                                              (code-review-history--load key)
                                              :metrics))
                                    :revisions)
                         2))
          ;; regression: the public accessor serves the metrics
          ;; HASH, not the (:metrics ...) cache plist around it
          (clrhash code-review-history--cache)
          (let ((m (code-review-history-metrics repo)))
            (should (hash-table-p m))
            (should (equal (plist-get (gethash "a.py" m) :revisions) 2))))
      ;; cleanup: cache entry, cache file, fixture repo
      (ignore-errors (remhash key code-review-history--cache))
      (ignore-errors (delete-directory (expand-file-name "code-review" key) t))
      (ignore-errors (delete-directory repo t)))))

;;; code-review-history-test.el ends here
