;;; code-review-dossier-test.el --- ERT tests for the hunk dossier -*- lexical-binding: t; -*-

;; Tests for `code-review-dossier.el' (phase 16): the on-demand
;; "Context (dossier)" section a keypress (C-c C-h) inserts after a
;; diff hunk — line history, blame authors, call sites of the touched
;; definitions, referencing tests, file heat.  Written BEFORE the
;; implementation (the spec), then run red against stubs, then green.

(require 'ert)
(require 'code-review-db)
(require 'code-review-github)
(require 'code-review-section)
(require 'code-review-dossier)
(require 'code-review-test-helpers)

;;; Fixtures

(defun code-review-dossier-test--sample-pr ()
  "A fresh sample pullreq (closql writes through `oset')."
  (code-review-github-repo
   :owner "owner"
   :repo "repo"
   :number "num"))

(defmacro code-review-dossier-test--with-env (&rest body)
  "Run BODY with a fresh test db and one pullreq row."
  (declare (indent 0))
  `(code-review-test--with-db
    (code-review-db--pullreq-create
     (code-review-dossier-test--sample-pr))
    ,@body))

;; The reviewed diff: the PR head (commit 3, "agent rewrite")
;; rewrote helper's comment line and replaced `other' with
;; `new_fn'.  Old-side lines 1-6 carry the lore.
(defconst code-review-dossier-test--diff
  (concat
   "diff --git a/lib.py b/lib.py\n"
   "index 111..222 100644\n"
   "--- a/lib.py\n"
   "+++ b/lib.py\n"
   "@@ -1,6 +1,6 @@\n"
   " def helper(x):\n"
   "-    # fixed race in session teardown\n"
   "+    # rewritten by the agent\n"
   "     return x + 1\n"
   " \n"
   "-def other(y):\n"
   "-    return helper(y) - 1\n"
   "+def new_fn(z):\n"
   "+    return helper(z)\n"))

(defun code-review-dossier-test--make-repo ()
  "Temp repository whose HEAD is the reviewed PR and whose older
commits carry the lore.  Return the directory (trailing slash).

- commit 1 (mrossi):  the original library
- commit 2 (alice):   'fix race in session teardown' — rewrites
                     helper's comment line (the lore)
- commit 3 (agent):  the PR head — the reviewed rewrite"
  (let* ((dir (file-name-as-directory (make-temp-file "cr-dossier-" t)))
         (default-directory dir)
         (write (lambda (path text)
                  (let ((full (expand-file-name path dir)))
                    (make-directory (file-name-directory full) t)
                    (with-temp-file full (insert text)))))
         (git (lambda (&rest args)
                (should (zerop (apply #'call-process "git" nil nil nil
                                      (append (list "-C" dir)
                                              args))))))
         (commit (lambda (msg date)
                   ;; deterministic per-commit timestamps: without
                   ;; these the fixture's commits share one second
                   ;; and the last-touch (max commit time) tie is
                   ;; decided by line order, not authorship
                   (let ((process-environment
                          (append (list (concat "GIT_AUTHOR_DATE=" date)
                                        (concat "GIT_COMMITTER_DATE=" date))
                                  process-environment)))
                     (funcall git "commit" "-m" msg)))))
    (funcall git "init" ".")
    (funcall git "config" "user.email" "test@test.test")
    (funcall git "config" "user.name" "mrossi")
    (funcall write "lib.py"
             "def helper(x):\n    # original\n    return x + 1\n\ndef other(y):\n    return helper(y) - 1\n")
    (funcall write "main.py"
              "from lib import helper, other\nprint(helper(2))\nprint(other(3))\n")
    (funcall write "tests/test_lib.py"
             "from lib import helper, other\n\ndef test_helper():\n    assert helper(1) == 2\n\ndef test_other():\n    assert other(1) is not None\n")
    (funcall git "add" "-A")
    (funcall commit "add helper library" "2024-01-01T10:00:00")
    (funcall git "config" "user.name" "alice")
    (funcall write "lib.py"
             "def helper(x):\n    # fixed race in session teardown\n    return x + 1\n\ndef other(y):\n    return helper(y) - 1\n")
    (funcall git "add" "-A")
    (funcall commit "fix race in session teardown" "2024-01-02T10:00:00")
    (funcall git "config" "user.name" "agent")
    (funcall write "lib.py"
             "def helper(x):\n    # rewritten by the agent\n    return x + 1\n\ndef new_fn(z):\n    return helper(z)\n")
    (funcall git "add" "-A")
    (funcall commit "agent rewrite" "2024-01-03T10:00:00")
    dir))

;;; Pure helpers

(ert-deftest code-review-dossier/parse-log-L ()
  "One log line per commit, \\x1f-separated: sha, author, date,
subject.  Malformed lines are skipped; empty input is nil.
NB: the separator is built with `concat' — in a string literal
\"\\x1f\" immediately followed by a letter the reader swallows up
to FOUR hex digits (\"\\x1falice\" reads as U+01FA + \"lice\")."
  (let* ((sep "\x1f")
         (sample (concat "deadbeef" sep "alice" sep "2024-11-08"
                         sep "fix race in session teardown\n"
                         "abc1234" sep "mrossi" sep "2024-03-01"
                         sep "add helper library\n")))
    (should (equal (code-review-dossier--parse-log-L sample)
                   '(("deadbeef" "alice" "2024-11-08"
                      "fix race in session teardown")
                     ("abc1234" "mrossi" "2024-03-01"
                      "add helper library"))))
    (should-not (code-review-dossier--parse-log-L ""))
    ;; a line without the separators (git chatter) is not an entry
    (should-not (code-review-dossier--parse-log-L "garbage line\n"))))

(ert-deftest code-review-dossier/old-range ()
  "The old-side (context+deleted) line span of a hunk plist from
`code-review-analysis--split-hunks', nil when the hunk adds brand
new lines only."
  (let ((hunks (code-review-analysis--split-hunks
                (concat "--- a/lib.py\n+++ b/lib.py\n@@ -4,3 +4,4 @@\n"
                        " context\n-deleted\n+added\n more context\n"))))
    (should (equal (code-review-dossier--old-range (car hunks)) '(4 . 6))))
  (let ((hunks (code-review-analysis--split-hunks
                (concat "--- a/new.py\n+++ b/new.py\n@@ -0,0 +1,3 @@\n"
                        "+brand\n+new\n+lines\n"))))
    (should-not (code-review-dossier--old-range (car hunks)))))

(ert-deftest code-review-dossier/hunk-for ()
  "Find the split-hunks plist for PATH+RANGES in a raw diff; nil
on a miss.  RANGES is the raw @@ ranges text (the hunk key)."
  (let ((diff (concat "diff --git a/lib.py b/lib.py\n--- a/lib.py\n+++ b/lib.py\n@@ -1,3 +1,4 @@\n ctx\n-rem\n+add\n"
                      "diff --git a/other.py b/other.py\n--- a/other.py\n+++ b/other.py\n@@ -9 +9 @@\n-1\n+2\n")))
    (should (equal (plist-get (code-review-dossier--hunk-for
                               diff "lib.py" "-1,3 +1,4")
                              :ranges)
                   "-1,3 +1,4"))
    (should-not (code-review-dossier--hunk-for diff "lib.py" "-9 +9"))
    (should-not (code-review-dossier--hunk-for diff "missing.py" "-1,3 +1,4"))))

(ert-deftest code-review-dossier/defs-capped ()
  "Touched definitions: added-side defs first, then deleted-side,
deduped, capped at `code-review-dossier-max-defs'."
  (let* ((hunk (car (code-review-analysis--split-hunks
                     (concat "@@ -1,7 +1,8 @@\n"
                             " ctx\n"
                             "-def gone(y):\n"
                             "+def new_fn(z):\n"
                             "+def new2(z):\n"
                             "+def new3(z):\n"
                             "+def new4(z):\n"
                             "+class New5:\n"
                             "-    return helper(y) - 1\n"
                             "+    return helper(z)\n")))))
    ;; all 6 defs found when the cap allows
    (let ((code-review-dossier-max-defs 10))
      (should (equal (mapcar #'car (code-review-dossier--defs "lib.py" hunk))
                     '("new_fn" "new2" "new3" "new4" "New5" "gone"))))
    ;; the cap truncates
    (let ((code-review-dossier-max-defs 2))
      (should (equal (length (code-review-dossier--defs "lib.py" hunk)) 2)))
    ;; the default cap (5) keeps the first five (the added defs)
    (should (equal (mapcar #'car (code-review-dossier--defs "lib.py" hunk))
                   '("new_fn" "new2" "new3" "new4" "New5")))))

(ert-deftest code-review-dossier/classify-callsites ()
  "Call sites split into tests and code by
`code-review-dossier-test-path-regexp' (downcased path match)."
  (let ((res (code-review-dossier--classify-callsites
              '(("lib.py" 3 "helper(1)")
                ("tests/test_lib.py" 7 "helper(2)")
                ("spec/lib_spec.rb" 2 "helper(3)")
                ("T/BIG.Integration.Spec.cs" 1 "helper(4)")))))
    (should (equal (plist-get res :code) '(("lib.py" 3 "helper(1)"))))
    (should (equal (plist-get res :tests)
                   '(("tests/test_lib.py" 7 "helper(2)")
                     ("spec/lib_spec.rb" 2 "helper(3)")
                     ("T/BIG.Integration.Spec.cs" 1 "helper(4)"))))))

(ert-deftest code-review-dossier/cache-key ()
  "Cache key: PR id, diff md5, path, ranges — same inputs key
alike, one changed input rekeys."
  (let ((diff "some diff text\n"))
    (should (equal (code-review-dossier--cache-key "PR1" diff "lib.py" "-1,6 +1,6")
                   (code-review-dossier--cache-key "PR1" diff "lib.py" "-1,6 +1,6")))
    (should-not (equal (code-review-dossier--cache-key "PR1" diff "lib.py" "-1,6 +1,6")
                       (code-review-dossier--cache-key "PR1" diff "lib.py" "-1,3 +1,4")))
    (should-not (equal (code-review-dossier--cache-key "PR1" diff "lib.py" "-1,6 +1,6")
                       (code-review-dossier--cache-key "PR2" diff "lib.py" "-1,6 +1,6")))))

;;; Git engine

(ert-deftest code-review-dossier/log-L-history ()
  "`git log --no-patch -L' at a base rev: pre-PR commits that
touched the lines, newest first, capped at
`code-review-dossier-history-limit'.  The rev limit EXCLUDES the
PR head's own rewrite."
  (let* ((repo (code-review-dossier-test--make-repo))
         (full (code-review-dossier--log-L repo "HEAD~1" "lib.py" 1 6))
         (capped (let ((code-review-dossier-history-limit 1))
                   (code-review-dossier--log-L repo "HEAD~1" "lib.py" 1 6))))
    (should (equal (mapcar (lambda (e) (nth 3 e)) full)
                   '("fix race in session teardown" "add helper library")))
    (should (equal (mapcar (lambda (e) (nth 1 e)) full) '("alice" "mrossi")))
    ;; capped at 1: only the most recent pre-PR touch
    (should (equal (mapcar (lambda (e) (nth 3 e)) capped)
                   '("fix race in session teardown")))
    ;; date is the short form
    (should (string-match-p "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\'"
                            (nth 2 (car full))))
    ;; bad rev or missing file: nil, no error
    (should-not (code-review-dossier--log-L repo "no-such-rev" "lib.py" 1 6))))

(ert-deftest code-review-dossier/compute ()
  "The engine dossier for the fixture hunk: pre-PR history (no
agent rewrite), blame authors of the old lines, most recent
touch, touched defs, their call sites."
  (let* ((repo (code-review-dossier-test--make-repo))
         (d (code-review-dossier--compute
             repo code-review-dossier-test--diff "HEAD~1"
             "lib.py" "-1,6 +1,6")))
    (should (equal (plist-get d :path) "lib.py"))
    (should (equal (plist-get d :ranges) "-1,6 +1,6"))
    ;; history: alice's race fix then mrossi's original; the agent's
    ;; rewrite (HEAD) is excluded by the base-rev limit
    (should (equal (mapcar (lambda (e) (nth 3 e)) (plist-get d :history))
                   '("fix race in session teardown" "add helper library")))
    ;; blame of old lines 1-6 at HEAD~1: mrossi wrote 5, alice 1
    (should (equal (plist-get d :authors) '(("mrossi" . 5) ("alice" . 1))))
    ;; the most recent touch of these lines is alice's race fix
    (should (equal (car (plist-get d :last-touch)) "alice"))
    ;; touched defs: new_fn (added) and other (deleted)
    (should (equal (mapcar #'car (plist-get d :defs)) '("new_fn" "other")))
    ;; call sites (worktree greps at the PR head): `other' is gone
    ;; from lib.py but still called from main.py and the tests
    (let* ((calls (plist-get d :calls))
           (other-hit (cdr (assoc "other" calls)))
           (other-paths (sort (delete-dups
                               (mapcar (lambda (h) (nth 0 h)) other-hit))
                              #'string<)))
      (should (equal other-paths '("main.py" "tests/test_lib.py")))
      ;; hits carry line numbers for the jump buttons
      (should (cl-every (lambda (h) (natnump (nth 1 h))) other-hit)))
    ;; `new_fn' is defined but never called: an empty call list
    (should (equal (cdr (assoc "new_fn" (plist-get d :calls))) nil))))

(ert-deftest code-review-dossier/compute-new-lines-no-history ()
  "A hunk adding brand new lines (no old side): no history, no
blame, but the touched defs and their call sites are still
computed."
  (let* ((repo (code-review-dossier-test--make-repo))
         (diff (concat "diff --git a/fresh.py b/fresh.py\n--- a/fresh.py\n"
                       "+++ b/fresh.py\n@@ -0,0 +1,2 @@\n"
                       "+def other(z):\n"
                       "+    return helper(z)\n"))
         (d (code-review-dossier--compute
             repo diff "HEAD~1" "fresh.py" "-0,0 +1,2")))
    (should-not (plist-get d :history))
    (should (equal (plist-get d :history-skipped) "new lines"))
    (should-not (plist-get d :last-touch))
    ;; `other' (deleted from lib.py by the PR head, re-added here in
    ;; a fresh file) is a touched def and still called in the worktree
    (should (equal (mapcar #'car (plist-get d :defs)) '("other")))
    (should (equal (sort (delete-dups
                          (mapcar (lambda (h) (nth 0 h))
                                  (cdr (assoc "other" (plist-get d :calls)))))
                         #'string<)
                   '("main.py" "tests/test_lib.py")))))

(ert-deftest code-review-dossier/compute-partial-clone ()
  "On a partial (promisor) clone the blob-lazy-fetching git calls
are skipped per their defcustoms and the skip is REPORTED, not
silent."
  (let* ((repo (code-review-dossier-test--make-repo))
         (orig (symbol-function 'code-review-analysis--partial-clone-p)))
    (fset 'code-review-analysis--partial-clone-p (lambda (&rest _) t))
    (unwind-protect
        (let* ((d (code-review-dossier--compute
                   repo code-review-dossier-test--diff "HEAD~1"
                   "lib.py" "-1,6 +1,6"))
               (skipped (plist-get d :history-skipped)))
          (should-not (plist-get d :history))
          (should-not (plist-get d :authors))
          (should-not (plist-get d :last-touch))
          ;; log -L skipped by the dossier defcustom (default nil)
          (should (equal skipped "partial clone"))
          ;; enabling it runs the history even on a partial clone
          ;; (bounded to ONE file, the message-once policy)
          (let ((code-review-dossier-partial-clone-history t))
            (let ((d2 (code-review-dossier--compute
                       repo code-review-dossier-test--diff "HEAD~1"
                       "lib.py" "-1,6 +1,6")))
              (should (equal (mapcar (lambda (e) (nth 3 e))
                                     (plist-get d2 :history))
                             '("fix race in session teardown"
                               "add helper library")))
              (should-not (plist-get d2 :history-skipped)))))
      (fset 'code-review-analysis--partial-clone-p orig))))

;;; db/cache layer

(ert-deftest code-review-dossier/get-cached ()
  "`--get' derives its inputs from the db (diff, PR, worktree,
base rev), caches per (PR id, diff md5, path, ranges) and returns
the enriched dossier (:heat from the current render's history
order, :risk from the analysis cache)."
  (code-review-dossier-test--with-env
    (let* ((repo (code-review-dossier-test--make-repo))
           (pr (code-review-db-get-pullreq))
           (runs 0)
           (orig-compute (symbol-function 'code-review-dossier--compute))
           (orig-order code-review-history--order))
      (oset pr base-ref-name "HEAD~1")
      (code-review-db--pullreq-raw-diff-update
       code-review-dossier-test--diff)
      (fset 'code-review-dossier--compute
            (lambda (&rest _)
              (cl-incf runs)
              (list :path "lib.py" :ranges "-1,6 +1,6"
                    :history nil :authors nil :last-touch nil
                    :defs nil :calls nil)))
      (setq code-review-history--order
            (list (list :path "lib.py" :bucket "HOT"
                        :reason "top 5% churn (132 revisions)")))
      (unwind-protect
          (let* ((code-review-repo-worktree repo)
                 (d1 (code-review-dossier--get "lib.py" "-1,6 +1,6"))
                 (d2 (code-review-dossier--get "lib.py" "-1,6 +1,6")))
            (should (= runs 1))          ; second get: cache hit
            ;; :heat enrichment from the render's history order
            (should (equal (plist-get (plist-get d1 :heat) :bucket) "HOT"))
            (should (equal (plist-get (plist-get d1 :heat) :reason)
                           "top 5% churn (132 revisions)"))
            (should (equal d1 d2))
            ;; another hunk: another key, another compute
            (code-review-dossier--get "lib.py" "-1,3 +1,4")
            (should (= runs 2))
            ;; reset clears the cache
            (code-review-dossier-reset)
            (code-review-dossier--get "lib.py" "-1,6 +1,6")
            (should (= runs 3)))
        (fset 'code-review-dossier--compute orig-compute)
        (setq code-review-history--order orig-order)))))

;;; Rendering and the command

(defun code-review-dossier-test--washed-buffer ()
  "Wash the fixture diff into a temp review buffer and return
\(BUFFER HUNK-SEC).  Requires the with-env db harness."
  (let ((buf (generate-new-buffer " *cr-dossier-test*")))
    (with-current-buffer buf
      (magit-section-mode)
      (let ((inhibit-read-only t))
        (insert code-review-dossier-test--diff)
        (goto-char (point-min))
        (magit-insert-section (code-review--root-section)
          (magit-insert-section (code-review-files-chnged)
            (save-restriction
              (narrow-to-region (point) (point-max))
              (magit-wash-sequence #'code-review-wash-diff)))))
      (let ((hunk-sec (code-review-section--find-hunk-section
                       "lib.py" "-1,6 +1,6")))
        (should hunk-sec)
        (list buf hunk-sec)))))

(defun code-review-dossier-test--fake-dossier ()
  "A dossier plist shaped like `code-review-dossier--compute'
output plus the `--get' enrichments."
  (list :path "lib.py" :ranges "-1,6 +1,6"
        :history '(("deadbeef" "alice" "2024-11-08"
                    "fix race in session teardown"))
        :last-touch '("alice" . 30)
        :authors '(("mrossi" . 5) ("alice" . 1))
        :defs '(("new_fn" . 6) ("other" . 5))
        :calls '(("other" . (("main.py" 3 "print(other(3))")
                             ("tests/test_lib.py" 7 "other(1)")))
                 ("new_fn" . nil))
        :heat (list :bucket "HOT" :reason "top 5% churn (132 revisions)")
        :risk "40 callers; lines 4y old"))

(ert-deftest code-review-dossier/insert-after-hunk ()
  "C-c C-h's insertion: a collapsible Context section AFTER the
hunk — a child of the file section, positioned after the hunk in
the children list too — with the dossier facts and jump buttons."
  (code-review-dossier-test--with-env
    (let ((code-review-repo-worktree "/tmp/wt")
          (env (code-review-dossier-test--washed-buffer)))
      (unwind-protect
          (with-current-buffer (car env)
            (let* ((hunk-sec (cadr env))
                   (sec (code-review-dossier--insert
                         hunk-sec (code-review-dossier-test--fake-dossier))))
              (should sec)
              (let ((text (buffer-string)))
                ;; the heading and the facts
                (should (string-match-p "Context (dossier)" text))
                (should (string-match-p
                         "last touched[[:space:]\n]*1m[[:space:]\n]*ago[[:space:]\n]*by[[:space:]\n]*alice" text))
                (should (string-match-p "mrossi (5), alice (1)" text))
                (should (string-match-p "fix race in session teardown" text))
                (should (string-match-p "deadbeef" text))
                (should (string-match-p "2024-11-08" text))
                (should (string-match-p "risk: 40 callers; lines 4y old" text))
                (should (string-match-p "heat: HOT" text))
                (should (string-match-p "top 5% churn (132 revisions)" text))
                (should (string-match-p "touched defs: new_fn, other" text))
                (should (string-match-p "other called from: main.py:3" text))
                (should (string-match-p
                         "tests referencing other: tests/test_lib.py:7" text)))
              ;; a magit section, child of the file section, ordered
              ;; right after the hunk in the children list
              (should (eq (eieio-object-class sec) 'code-review-dossier-section))
              (let* ((file-sec (oref hunk-sec parent))
                     (kids (oref file-sec children)))
                (should (memq sec kids))
                (should (< (cl-position hunk-sec kids)
                           (cl-position sec kids))))
              ;; TAB (magit-section-hide/show) works: the section has
              ;; a heading/content split and the hidden slot toggles
              (should (markerp (oref sec content)))
              (magit-section-hide sec)
              (should (oref sec hidden))
              (magit-section-show sec)
              (should-not (oref sec hidden))))
          (kill-buffer (car env))))))

(ert-deftest code-review-dossier/insert-idempotent ()
  "Re-inserting a dossier for the same hunk REPLACES it: one
dossier section, one heading."
  (code-review-dossier-test--with-env
    (let ((code-review-repo-worktree "/tmp/wt")
          (env (code-review-dossier-test--washed-buffer)))
      (unwind-protect
          (with-current-buffer (car env)
            (let ((hunk-sec (cadr env)))
              (code-review-dossier--insert
               hunk-sec (code-review-dossier-test--fake-dossier))
              (code-review-dossier--insert
               hunk-sec (code-review-dossier-test--fake-dossier))
              (let ((types nil)
                    (walk nil))
                (setq walk (lambda (sec)
                             (dolist (c (oref sec children))
                               (push (oref c type) types)
                               (funcall walk c))))
                (funcall walk magit-root-section)
                (should (= 1 (cl-count 'code-review-dossier-section types)))
                (should (= 1 (count-matches "Context (dossier)"
                                             (point-min) (point-max)))))))
          (kill-buffer (car env))))))

(ert-deftest code-review-dossier/machine-summary-off-by-default ()
  "The LLM garnish is OFF by default: no summarizer configured
means no summary.  With one configured, the summary is a plain
string the renderer marks as machine-generated."
  (should-not (code-review-dossier--machine-summary "dossier text"))
  (let ((code-review-dossier-llm-summarizer
         (lambda (text) (format "Story: %s" text))))
    (should (equal (code-review-dossier--machine-summary "F")
                   "Story: F"))))

(ert-deftest code-review-dossier/command-inserts-and-guards ()
  "The interactive command: point in a hunk inserts the section
(C-u with a summarizer adds the marked machine summary); point
outside a hunk or without a worktree is a user error."
  (code-review-dossier-test--with-env
    (let ((orig-get (symbol-function 'code-review-dossier--get))
          (code-review-repo-worktree "/tmp/wt"))
      (fset 'code-review-dossier--get
            (lambda (&rest _)
              (code-review-dossier-test--fake-dossier)))
      (unwind-protect
          (let ((env (code-review-dossier-test--washed-buffer)))
            (unwind-protect
                (with-current-buffer (car env)
                  (let ((hunk-sec (cadr env)))
                    ;; point inside the hunk: the section is found and
                    ;; the dossier inserted after it
                    (goto-char (oref hunk-sec start))
                    (should (eq (code-review-dossier--hunk-section-at-point)
                                 hunk-sec))
                    (code-review-dossier-hunk)
                    (should (string-match-p
                             "Context (dossier)"
                             (buffer-substring-no-properties
                              (oref hunk-sec start) (point-max))))
                    ;; C-u with no summarizer: a message, no summary line
                    (code-review-dossier-hunk '(4))
                    (should-not
                     (string-match-p "machine summary"
                                     (buffer-string)))
                    ;; C-u with a summarizer: the marked summary
                    (let ((code-review-dossier-llm-summarizer
                           (lambda (_text) "story of these lines")))
                      (code-review-dossier-hunk '(4))
                      (should (string-match-p
                               "(machine summary)[[:space:]\n]*story of these lines"
                               (buffer-string))))
                    ;; point outside any hunk: user error
                    (goto-char (point-min))
                    (should-error (code-review-dossier-hunk)
                                  :type 'user-error)))
              (kill-buffer (car env))))
        (fset 'code-review-dossier--get orig-get)))))

(ert-deftest code-review-dossier/command-requires-worktree ()
  "No worktree for the review (no local repo): a friendly user
error, not a git failure."
  (code-review-dossier-test--with-env
    (let ((env (code-review-dossier-test--washed-buffer))
          (code-review-repo-worktree nil))
      (unwind-protect
          (with-current-buffer (car env)
            (goto-char (oref (cadr env) start))
            (should-error (code-review-dossier-hunk) :type 'user-error))
        (kill-buffer (car env))))))

;;; code-review-dossier-test.el ends here
