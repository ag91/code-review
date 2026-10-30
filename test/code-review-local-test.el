;;; code-review-local-test.el --- ERT tests for magit-buffer local diff review -*- lexical-binding: t; -*-
;;
;; Phase 12 extension: reviewing the diff of the magit buffer at
;; point (a commit in `magit-revision-mode', a range in
;; `magit-diff-mode') through the local pseudo-PR pipeline.

(require 'ert)
(require 'cl-lib)
(require 'magit-diff)
(require 'magit-log)
(require 'magit-section)
(require 'code-review-db)
(require 'code-review-local)
(require 'code-review-section)
(require 'code-review-test-helpers)

(defun code-review-local-test--make-repo ()
  "Create a temp repo with three commits; return its directory.
The commits add one `def' each to lib.py: root (one), second
(two), third (three)."
  (let* ((dir (file-name-as-directory (make-temp-file "cr-local-" t)))
         (default-directory dir))
    (dolist (args '(("init" ".")
                    ("config" "user.email" "test@test.test")
                    ("config" "user.name" "test")))
      (apply #'call-process "git" nil nil nil
             (append (list "-C" dir) args)))
    (with-temp-file (expand-file-name "lib.py" dir)
      (insert "def one(x):\n    return x\n"))
    (call-process "git" nil nil nil "-C" dir "add" "-A")
    (call-process "git" nil nil nil "-C" dir "commit" "-m" "root commit")
    (with-temp-file (expand-file-name "lib.py" dir)
      (insert "def one(x):\n    return x\n\ndef two(x):\n    return one(x) - 1\n"))
    (call-process "git" nil nil nil "-C" dir "add" "-A")
    (call-process "git" nil nil nil "-C" dir "commit" "-m" "second commit")
    (with-temp-file (expand-file-name "lib.py" dir)
      (insert "def one(x):\n    return x\n\ndef two(x):\n    return one(x) - 1\n\ndef three(x):\n    return two(x) - 1\n"))
    (call-process "git" nil nil nil "-C" dir "add" "-A")
    (call-process "git" nil nil nil "-C" dir "commit" "-m" "third commit")
    dir))

(defmacro code-review-local-test--with-magit-buffer (mode vars &rest body)
  "Run BODY in a temp magit buffer running MODE in the current repo.
VARS is an alist of buffer-local variables to set first."
  (declare (indent 2))
  `(with-temp-buffer
     (delay-mode-hooks (funcall ,mode))
     (pcase-dolist (`(,var . ,val) ,vars)
       (set (make-local-variable var) val))
     ,@body))

(ert-deftest code-review-local/magit-revision-buffer-args ()
  "In a magit-revision buffer the shown commit is reviewed.
DIFF-ARGS is REV^..REV (diff against the first parent); a ROOT
commit has no parent, so its args are REV^! (REV^..REV fails on
it: verified with real git, exit 128)."
  (let ((repo (code-review-local-test--make-repo)))
    (let ((default-directory repo))
      (code-review-local-test--with-magit-buffer
          'magit-revision-mode '((magit-buffer-revision . "HEAD"))
        (let ((res (code-review-local--magit-diff-args)))
          (should (equal (car res) "HEAD^..HEAD"))
          (should (string-match-p "\\`Commit [0-9a-f]\\{7\\} (third commit)\\'"
                                  (cadr res))))
        ;; the root commit has no parent: REV^!
        (let ((root (string-trim
                     (with-output-to-string
                       (call-process "git" nil standard-output nil
                                      "-C" repo
                                      "rev-list" "--max-parents=0" "HEAD")))))
          (set (make-local-variable 'magit-buffer-revision) root)
          (let ((res (code-review-local--magit-diff-args)))
            (should (equal (car res) (concat root "^!")))
            (should (string-match-p
                     "\\`Commit [0-9a-f]\\{7\\} (root commit)\\'"
                     (cadr res)))))))))

(ert-deftest code-review-local/magit-diff-range-buffer-args ()
  "In a magit-diff buffer showing a range, the range is reviewed."
  (let ((repo (code-review-local-test--make-repo)))
    (let ((default-directory repo))
      (code-review-local-test--with-magit-buffer
          'magit-diff-mode '((magit-buffer-diff-range . "HEAD~1..HEAD"))
        (should (equal (code-review-local--magit-diff-args)
                       '("HEAD~1..HEAD" "Diff HEAD~1..HEAD")))))))

(ert-deftest code-review-local/not-a-magit-buffer-returns-nil ()
  "Outside a magit revision/diff-range buffer there is no override:
the caller falls back to the working tree."
  (with-temp-buffer
    (should (null (code-review-local--magit-diff-args)))))

(ert-deftest code-review-local/magit-log-commit-at-point-args ()
  "In a magit-log buffer, the commit at point is reviewed against
its first parent (same args as a revision-buffer review)."
  (let ((repo (code-review-local-test--make-repo)))
    (let ((default-directory repo))
      (let ((logbuf (magit-log-head (list "-n3"))))
        (unwind-protect
            (with-current-buffer logbuf
              (goto-char (point-min))
              (re-search-forward "third commit")
              (let ((res (code-review-local--magit-diff-args)))
                (should (equal (car res)
                               (format "%s^..%s"
                                       (oref (magit-current-section) value)
                                       (oref (magit-current-section) value))))
                (should (string-match-p
                         "\\`Commit [0-9a-f]\\{7\\} (third commit)\\'"
                         (cadr res)))))
          (kill-buffer logbuf))))))

(ert-deftest code-review-local/magit-log-region-args ()
  "In a magit-log buffer with a region marked over commit
headings, the COMBINED diff of ALL the commits in the region is
reviewed (from the OLDEST selected commit's parent to the
NEWEST): both commits' additions appear in the diff."
  (let ((repo (code-review-local-test--make-repo)))
    (let ((default-directory repo))
      (let ((logbuf (magit-log-head (list "-n3"))))
        (unwind-protect
            (with-current-buffer logbuf
              ;; region over the two newest commit headings
              (goto-char (point-min))
              (re-search-forward "third commit")
              (beginning-of-line)
              (push-mark (point) t t)
              (re-search-forward "second commit")
              (end-of-line)
              (activate-mark)
              (let* ((res (code-review-local--magit-diff-args))
                     (args (car res))
                     (obj (code-review-local-diff
                           :owner "local"
                           :repo "cr-local-test" :number 0 :url nil)))
                (oset obj host repo)
                (oset obj base-ref-name args)
                ;; log sections carry SHORT shas as values
                (should (string-match-p
                         "\\`[0-9a-f]\\{7,40\\}\\^\\.\\.[0-9a-f]\\{7,40\\}\\'"
                         args))
                (should (string-match-p
                         "\\`Commits [0-9a-f]\\{7\\}\\.\\.[0-9a-f]\\{7\\}\\'"
                         (cadr res)))
                ;; the diff carries BOTH commits' additions
                (let ((diff (code-review-local--diff-text obj)))
                  (should (string-match-p "^\\+def two" diff))
                  (should (string-match-p "^\\+def three" diff))
                  ;; the root commit is NOT in the region
                  (should (not (string-match-p "^\\+def one" diff))))))
          (kill-buffer logbuf))))))

(ert-deftest code-review-local/magit-log-region-with-root-args ()
  "A log region including the repository's ROOT commit reviews
from the git EMPTY TREE (the root has no parent): the root
commit's changes are in the diff too."
  (let ((repo (code-review-local-test--make-repo)))
    (let ((default-directory repo))
      (let ((logbuf (magit-log-head (list "-n3"))))
        (unwind-protect
            (with-current-buffer logbuf
              ;; region over all three commit headings
              (goto-char (point-min))
              (re-search-forward "third commit")
              (beginning-of-line)
              (push-mark (point) t t)
              (re-search-forward "root commit")
              (end-of-line)
              (activate-mark)
              (let* ((res (code-review-local--magit-diff-args))
                     (args (car res))
                     (obj (code-review-local-diff
                           :owner "local"
                           :repo "cr-local-test" :number 0 :url nil)))
                (oset obj host repo)
                (oset obj base-ref-name args)
                (should (string-match-p
                         (concat "\\`" code-review-local--empty-tree
                                 "\\.\\.[0-9a-f]\\{7,40\\}\\'")
                         args))
                ;; EVERY commit's addition is in the diff, the root's
                ;; included (that is the empty-tree range's point)
                (let ((diff (code-review-local--diff-text obj)))
                  (should (string-match-p "^\\+def one" diff))
                  (should (string-match-p "^\\+def two" diff))
                  (should (string-match-p "^\\+def three" diff)))))
          (kill-buffer logbuf))))))

(ert-deftest code-review-local/diff-text-honors-diff-args ()
  "`--diff-text' runs git diff with the OBJ's base-ref-name: the
G (reload) path re-fetches the same commit diff."
  (let* ((repo (code-review-local-test--make-repo))
         (obj (code-review-local-diff
               :owner "local"
               :repo (file-name-nondirectory (directory-file-name repo))
               :number 0
               :url nil)))
    (oset obj host repo)
    (oset obj base-ref-name "HEAD^..HEAD")
    (let ((diff (code-review-local--diff-text obj)))
      (should (string-prefix-p "diff --git" diff))
      ;; only the third commit's addition: the def three block
      (should (string-match-p "^\\+def three" diff))
      (should (not (string-match-p "^\\+def two" diff))))))

(ert-deftest code-review-local/buffer-name-hint ()
  "LOCAL review buffer names carry the reviewed diff as a hint;
classic args keep the plain name (backward compatible)."
  (let ((pr (code-review-local-diff
             :owner "local" :repo "myrepo" :number 0 :url nil)))
    (oset pr state "LOCAL")
    ;; working tree / staged: unchanged names
    (dolist (args '(nil "HEAD" "--cached"))
      (oset pr base-ref-name args)
      (should (equal (code-review-pr-buffer-name pr)
                     "*Code Review: local: myrepo*")))
    ;; a commit review (full sha both sides): short form in the name
    (let ((full "5dc7268fba2115e4da4247297522b024b0f77381"))
      (oset pr base-ref-name (format "%s^..%s" full full))
      (should (equal (code-review-pr-buffer-name pr)
                     "*Code Review: local: myrepo @ 5dc7268*")))
    ;; a root-commit review
    (oset pr base-ref-name "abc1234^!")
    (should (equal (code-review-pr-buffer-name pr)
                   "*Code Review: local: myrepo @ abc1234*"))
    ;; a range review
    (oset pr base-ref-name "master..feature")
    (should (equal (code-review-pr-buffer-name pr)
                   "*Code Review: local: myrepo @ master..feature*"))
    ;; a log-region review (OLD^..NEW): each full-sha side shortened,
    ;; the first-parent marker dropped
    (oset pr base-ref-name
          (format "%s^..%s"
                  "5dc7268fba2115e4da4247297522b024b0f77381"
                  "a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5d6e7f8a9b0"))
    (should (equal (code-review-pr-buffer-name pr)
                   "*Code Review: local: myrepo @ 5dc7268..a1b2c3d*"))
    ;; a log-region review from the ROOT commit: the empty-tree side
    ;; shortens like any other sha
    (oset pr base-ref-name
          (format "%s..%s"
                  "4b825dc642cb6eb9a060e54bf8d69288fbee4904"
                  "a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5d6e7f8a9b0"))
    (should (equal (code-review-pr-buffer-name pr)
                   "*Code Review: local: myrepo @ 4b825dc..a1b2c3d*"))))

(ert-deftest code-review-local/review-commit-end-to-end ()
  "The entry command run inside a magit-revision buffer records a
LOCAL row for THAT commit: title, diff args, and the diff itself.
Called with ARG 1: `interactive \"p\"' passes 1 (NOT nil) for no
prefix, which used to skip the magit-buffer detection entirely
and fall back to the working tree.  The deferred chain is pumped
until its db write lands and the queue drains, so nothing async
survives the test's db reset."
  (let ((repo (code-review-local-test--make-repo))
        (code-review-repo-enable nil)
        ;; hermetic: no async history-harvest child whose sentinel
        ;; would re-render THIS buffer mid-chain (the re-render
        ;; deletes the LOCAL row and stalls the deferred chain)
        (code-review-history-enabled nil))
    (code-review-test--with-db
      (let ((default-directory repo))
        (code-review-local-test--with-magit-buffer
            'magit-revision-mode '((magit-buffer-revision . "HEAD"))
          (code-review-review-local-diff 1)))
      ;; pump the deferred chain: row fields first (synchronous),
      ;; raw-diff via internal-build (async tick)
      (let ((deadline (+ (float-time) 10))
            done)
        (while (and (not done) (< (float-time) deadline))
          (let ((pr (ignore-errors (code-review-db-get-pullreq))))
            (when (and pr (slot-boundp pr 'raw-diff) (oref pr raw-diff)
                       (null deferred:queue))
              (setq done t))
            (unless done (sit-for 0.05))))
        (unless done (ert-fail "deferred chain did not finish: raw-diff missing"))
        (let ((pr (code-review-db-get-pullreq)))
          (should pr)
          (should (equal (oref pr state) "LOCAL"))
          (should (equal (oref pr base-ref-name) "HEAD^..HEAD"))
          (should (string-match-p
                   "\\`Commit [0-9a-f]\\{7\\} (third commit)\\'"
                   (oref pr title)))
          ;; the stored diff is the COMMIT's, not the working tree's
          (should (string-match-p "^\\+def three" (oref pr raw-diff)))
          (should (not (string-match-p "^\\+def two" (oref pr raw-diff))))
          (should (string-match-p
                   "\\`\\*Code Review: local: cr-local-[A-Za-z0-9]+ @ HEAD\\*\\'"
                   (code-review-pr-buffer-name pr))))))))

;;; Local review outcomes (phase 25b): comments -> agent, finish ->
;;; budget booking

(defun code-review-local-test--make-local-pr (diff)
  "A LOCAL review row with the given RAW-DIFF, persisted."
  (let ((obj (code-review-local-diff
              :owner "local" :repo "myrepo" :number 0 :url nil)))
    (code-review-db--pullreq-create obj)
    (oset obj state "LOCAL")
    (oset obj title "Local changes (HEAD)")
    (oset obj host "/tmp/somerepo/")
    (oset obj base-ref-name "HEAD")
    (oset obj saved t)
    (oset obj raw-diff diff)
    (code-review-db-update obj)
    obj))

(defconst code-review-local-test--local-comment-alist
  `((author (login . "me"))
    (state . "LOCAL COMMENT")
    (comments (nodes ((internal-id . "id1")
                      (bodyText . "this duplicate logic must go")
                      (path . "lib.py")
                      (position . 5)
                      (line . 3)
                      (side . "RIGHT")
                      (diffHunk . "@@ -1,3 +1,4 @@\n context\n+dup")
                      (local?))))))

;;; req-2026-10-30-001: local-comments-dispatch-as-agent-work-item
;; covers: code-review-local.el
;; covers-fn: code-review-local--pending-comments,
;; code-review-local-agent--prompt, code-review-local-comments-send
;; enforcement: test
(ert-deftest code-review-local/req-2026-10-30-001-comments-dispatch-to-agent ()
  "WHEN the reviewer sends the pending comments of a local review
THE SYSTEM SHALL hand the attached AI agent ONE batched work item
anchored at each finding's path and line, under a draft-not-push
contract — and WHEN no agent is wired THE SYSTEM SHALL still
deliver the work item to a buffer and the kill ring, so the
finding never evaporates."
  (let ((code-review-history-enabled nil))
    (code-review-test--with-db
      (let ((pr (code-review-local-test--make-local-pr
                 "diff --git a/lib.py b/lib.py\n--- a/lib.py\n+++ b/lib.py\n@@ -1,1 +1,2 @@\n x\n+y\n")))
        (code-review-db--pullreq-raw-comments-update
         code-review-local-test--local-comment-alist)
        ;; extraction: the one local comment, anchors intact
        (let ((cs (code-review-local--pending-comments)))
          (should (= (length cs) 1))
          (should (equal (plist-get (car cs) :path) "lib.py"))
          (should (equal (plist-get (car cs) :line) 3))
          (should (equal (plist-get (car cs) :body)
                         "this duplicate logic must go")))
        ;; the pure prompt: anchors, contract, repo
        (let ((prompt (code-review-local-agent--prompt
                       (code-review-local--pending-comments)
                       "/tmp/somerepo/")))
          (should (string-match-p "lib.py:3" prompt))
          (should (string-match-p "this duplicate logic must go" prompt))
          (should (string-match-p "Do NOT commit" prompt))
          (should (string-match-p "/tmp/somerepo/" prompt)))
        ;; wired agent: the hook receives (PROMPT REPO-DIR)
        ;; let* NOT let: a lambda in a plain let's binding list does
        ;; not capture a sibling binding on this Emacs build
        (let* ((calls nil)
               (code-review-local-agent-dispatch-function
                (lambda (prompt repo)
                  (setq calls (list prompt repo)))))
          (code-review-local-comments-send)
          (should (= (length calls) 2))
          (should (equal (nth 1 calls) "/tmp/somerepo/"))
          (should (string-match-p "lib.py:3" (car calls))))
        ;; no agent: the work item lands in a buffer + kill ring
        (let ((code-review-local-agent-dispatch-function nil))
          (unwind-protect
              (progn
                (code-review-local-comments-send)
                (with-current-buffer "*Local Review Findings*"
                  (should (string-match-p
                           "this duplicate logic must go"
                           (buffer-string)))))
            (and (get-buffer "*Local Review Findings*")
                 (kill-buffer "*Local Review Findings*"))))))))

(ert-deftest code-review-local/comments-send-requires-comments ()
  "Sending with no pending comments is a friendly error, and the
send refuses to run outside a LOCAL review."
  (let ((code-review-history-enabled nil))
    (code-review-test--with-db
      (code-review-local-test--make-local-pr "diff --git a/lib.py b/lib.py\n--- a/lib.py\n+++ b/lib.py\n@@ -1,1 +1,2 @@\n x\n+y\n")
      (should-error (code-review-local-comments-send)
                    :type 'user-error))))

;;; req-2026-10-30-002: local-finish-books-the-budget
;; covers: code-review-local.el
;; covers-fn: code-review-local--finish
;; enforcement: test
(ert-deftest code-review-local/req-2026-10-30-002-finish-books-budget ()
  "WHEN the reviewer completes a local review THE SYSTEM SHALL
stamp it finished so its estimated cost counts as spent today —
the row leaves the unfinished queue and the budget's balance
shrinks accordingly."
  (let ((code-review-history-enabled nil)
        (code-review-review-budget-minutes 90)
        (code-review-budget-focus-enabled nil)
        (code-review-budget-state-file (make-temp-file "cr-fin-")))
    (code-review-test--with-db
      (unwind-protect
          (progn
            (code-review-local-test--make-local-pr
             "diff --git a/lib.py b/lib.py\n--- a/lib.py\n+++ b/lib.py\n@@ -1,1 +1,2 @@\n x\n+y\n")
            ;; before: unfinished, nothing spent
            (should (= (length (code-review-db-all-unfinished)) 1))
            (should (= (code-review-budget--finished-today-cost) 0.0))
            ;; finish: booked
            (code-review-local--finish)
            (let ((pr (code-review-db-get-pullreq)))
              (should (oref pr finished))
              (should (oref pr finished-at)))
            (should (null (code-review-db-all-unfinished)))
            (should (> (code-review-budget--finished-today-cost) 0.0))
            (should (< (code-review-budget--remaining) 90))
            ;; idempotent: a second finish does not double-book
            (code-review-local--finish)
            (should (< (code-review-budget--finished-today-cost)
                       100.0)))
        (ignore-errors (delete-file code-review-budget-state-file))))))

(ert-deftest code-review-local/transient-dispatch-keybinding ()
  "`r' routes by review kind: the dispatcher is bound in the mode
map, and local reviews get the local transient."
  (should (equal (lookup-key code-review-mode-map (kbd "r"))
                 'code-review-transient-dispatch))
  ;; the local transient declares the two outcome commands
  (should (fboundp 'code-review-transient-local))
  (should (commandp 'code-review-local-finish))
  (should (commandp 'code-review-local-comments-send)))

(provide 'code-review-local-test)
;;; code-review-local-test.el ends here
