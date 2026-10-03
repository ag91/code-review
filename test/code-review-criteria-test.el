;;; code-review-criteria-test.el --- ERT tests for the criteria engine (phase 23) -*- lexical-binding: t; -*-
;;
;; Phase 23 tests.  Test names carry the req-ID segment
;; (`req-<id>-`) binding each requirement to the ERT test(s) that
;; guard it — the dogfood of the phase 24 traceability convention,
;; and the criteria files live under this repository's own specs/.

(require 'ert)
(require 'cl-lib)
(require 'code-review-criteria)
(require 'code-review-section-criteria)
(require 'code-review-local)
(require 'code-review-section)
(require 'code-review-test-helpers)

;;; Git integration: one temp repository

(defun code-review-criteria-test--make-repo (files-alist)
  "Create a temp git repository with FILES-ALIST committed at HEAD.
Return its directory (with trailing slash)."
  (let* ((dir (file-name-as-directory (make-temp-file "cr-criteria-" t)))
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

;;; req-2026-10-01-009: requirement extraction

(ert-deftest code-review-criteria-test/req-2026-10-01-009-requirement-extraction ()
  "Requirement extraction from criteria file text: the id from
the file NAME (the date-numbered stem head), the EARS sentence
from the first body paragraph (lines joined), declared paths
from the front matter."
  (let* ((text "---\npaths: [src/x.py, src/]\n---\n\nWHEN a payment export runs\nTHE SYSTEM SHALL NOT write a partial file.\n")
         (req (code-review-criteria--requirement-from-text
               "specs/2026-10-01-001-export-atomic.md" text)))
    (should (equal (plist-get req :id) "2026-10-01-001"))
    (should (equal (plist-get req :sentence)
                   "WHEN a payment export runs THE SYSTEM SHALL NOT write a partial file."))
    (should (equal (plist-get req :paths) '("src/x.py" "src/"))))
  ;; no front matter: body only, no declared paths
  (let ((req (code-review-criteria--requirement-from-text
              "specs/2026-10-01-002-no-fm.md"
              "WHEN x THE SYSTEM SHALL y.\n")))
    (should (equal (plist-get req :id) "2026-10-01-002"))
    (should (null (plist-get req :paths)))
    (should (equal (plist-get req :sentence) "WHEN x THE SYSTEM SHALL y.")))
  ;; a file name with no date-numbered id: no requirement
  (should-not (code-review-criteria--requirement-from-text
               "specs/criteria.md" "WHEN x THE SYSTEM SHALL y.\n"))
  ;; a body with no sentence: no requirement
  (should-not (code-review-criteria--requirement-from-text
               "specs/2026-10-01-003-empty.md" "---\npaths: [a.py]\n---\n\n"))
  ;; the id is the stem HEAD; the slug after it is free
  (should (equal (plist-get (code-review-criteria--requirement-from-text
                             "specs/2026-10-01-004-anything-else-goes.md"
                             "WHEN a THE SYSTEM SHALL b.\n")
                            :id)
                 "2026-10-01-004")))

(ert-deftest code-review-criteria-test/req-2026-10-01-009-scan-finds-drafted-files ()
  "The scan reads tracked AND untracked-not-ignored criteria
(the solo loop drafts criteria BEFORE the commit) and skips
non-criteria paths."
  (let* ((repo (code-review-criteria-test--make-repo
                '(("lib.py" . "def one():\n    return 1\n")
                  ("specs/2026-10-01-001-committed.md"
                   . "---\npaths: [lib.py]\n---\n\nWHEN a THE SYSTEM SHALL b.\n")
                  ("docs/2026-10-01-002-not-criteria.md"
                   . "WHEN a THE SYSTEM SHALL b.\n"))))
         (drafted (expand-file-name
                   "specs/2026-10-01-003-drafted.md" repo))
         (ignored (expand-file-name "specs/ignored.md" repo)))
    (make-directory (file-name-directory drafted) t)
    (with-temp-file drafted
      (insert "WHEN a draft THE SYSTEM SHALL count before the commit.\n"))
    (with-temp-file ignored (insert "nothing\n"))
    (with-temp-file (expand-file-name ".gitignore" repo)
      (insert "specs/ignored.md\n"))
    (let ((reqs (code-review-criteria--criteria repo)))
      ;; committed, drafted-not-committed; neither the docs/ file
      ;; nor the gitignored one.  ls-files combined output is NOT
      ;; sorted (untracked group first): compare sorted — the
      ;; render side (`--matching') orders by id anyway.
      (should (equal (sort (mapcar (lambda (r) (plist-get r :id)) reqs)
                           #'string<)
                     '("2026-10-01-001" "2026-10-01-003"))))))

;;; req-2026-10-01-010: criteria cover changed files

(ert-deftest code-review-criteria-test/req-2026-10-01-010-covers-and-matching ()
  "Coverage rules: a declared path exact, a declared parent
directory, an undeclared text mention of the path or its base
name; a DECLARED-but-uncovering requirement never falls back
to the heuristic.  The matching list keeps only covering
requirements, oldest id first, with exactly the covered paths."
  (let* ((r1 (list :id "2026-10-01-001" :paths '("src/x.py")
                   :text "" :sentence "S1" :file "f1"))
         (r2 (list :id "2026-10-01-002" :paths '("src/")
                   :text "" :sentence "S2" :file "f2"))
         (r3 (list :id "2026-10-01-003" :paths nil
                   :text "mentions src/y.py directly"
                   :sentence "S3" :file "f3"))
         (r4 (list :id "2026-10-01-004" :paths nil
                   :text "talks about z.py and other things"
                   :sentence "S4" :file "f4"))
         (r5 (list :id "2026-10-01-005" :paths '("other.py")
                   :text "" :sentence "S5" :file "f5"))
         (paths '("src/x.py" "src/y.py" "src/sub/z.py"))
         (matching (code-review-criteria--matching
                    (list r5 r4 r3 r2 r1) paths)))
    (should (equal (mapcar (lambda (m) (plist-get (plist-get m :req) :id))
                           matching)
                   '("2026-10-01-001" "2026-10-01-002"
                     "2026-10-01-003" "2026-10-01-004")))
    ;; exactly the changed paths each requirement covers
    (should (equal (plist-get (nth 0 matching) :paths) '("src/x.py")))
    (should (equal (plist-get (nth 1 matching) :paths) paths))
    (should (equal (plist-get (nth 2 matching) :paths) '("src/y.py")))
    (should (equal (plist-get (nth 3 matching) :paths) '("src/sub/z.py")))
    ;; declared-but-uncovering: no heuristic fallback
    (should-not (code-review-criteria--covers-p r5 "src/x.py"))
    ;; an undeclared base-name mention covers
    (should (code-review-criteria--covers-p r4 "src/sub/z.py"))
    ;; changed paths from raw diff text
    (should (equal (code-review-criteria--changed-paths
                    "diff --git a/a.py b/a.py\n--- a/a.py\n+++ b/a.py\n@@ -1 +1 @@\n-x\n+x\n")
                   '("a.py")))))

;;; req-2026-10-01-011: template stable ids

(ert-deftest code-review-criteria-test/req-2026-10-01-011-template-ids ()
  "Template insertion: the id is date-numbered past the
existing criteria of the SAME date (a template file already
carries its id, so drafting never collides), the file name
carries it as the stem head, the skeleton is EARS-shaped with
the paths prefilled."
  (let* ((today (format-time-string "%Y-%m-%d"))
         (repo (code-review-criteria-test--make-repo
                (list (cons (format "specs/%s-047-existing.md" today)
                            "WHEN a THE SYSTEM SHALL b.\n"))))
         (file (code-review-criteria-insert-template repo)))
    (should (string-match-p
             (format "\\`%s-048-new-requirement\\.md\\'" today)
             (file-name-nondirectory file)))
    (should (file-exists-p file))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (should (search-forward
               "paths: [<paths this requirement governs>]" nil t))
      (goto-char (point-min))
      (should (search-forward
               "WHEN <precondition> THE SYSTEM SHALL <postcondition>."
               nil t)))
    ;; paths prefill (pure helper)
    (should (string-match-p
             "paths: \\[src/a\\.py, src/b\\.py\\]"
             (code-review-criteria--template-text
              '("src/a.py" "src/b.py"))))
    ;; other dates never count toward the sequence
    (with-temp-file (expand-file-name "specs/2025-12-31-999-old.md" repo)
      (insert "WHEN a THE SYSTEM SHALL b.\n"))
    (should (equal (code-review-criteria--next-id repo)
                   (format "%s-049" today)))
    ;; run INTERACTIVELY the command also VISITS the drafted file
    ;; (other window): the direct-call engine stays visit-free
    (let* ((file (let ((default-directory repo))
                  (call-interactively
                   #'code-review-criteria-insert-template)))
           (buf (get-file-buffer file)))
      (should (string-match-p
               (format "\\`%s-049-new-requirement\\.md\\'" today)
               (file-name-nondirectory file)))
      (should buf)
      (kill-buffer buf))))

;;; req-2026-10-01-012: the require-mode refusal

(ert-deftest code-review-criteria-test/req-2026-10-01-012-require-refusal ()
  "REQUIRE mode: a change no criteria cover refuses the local
review with a user-error; a covered change opens; the nil/warn
modes never refuse."
  (let* ((repo (code-review-criteria-test--make-repo
                '(("lib.py" . "def one():\n    return 1\n")
                  ("specs/2026-10-01-001-lib.md"
                   . "---\npaths: [lib.py]\n---\n\nWHEN a THE SYSTEM SHALL b.\n"))))
         (uncovered "diff --git a/other.py b/other.py\n--- a/other.py\n+++ b/other.py\n@@ -0,0 +1,1 @@\n+x = 1\n")
         (covered "diff --git a/lib.py b/lib.py\n--- a/lib.py\n+++ b/lib.py\n@@ -1,1 +1,1 @@\n-x = 1\n+x = 2\n"))
    (let ((code-review-criteria-required 'require))
      (should-error (code-review-criteria--enforce-local repo uncovered)
                    :type 'user-error)
      (should (null (code-review-criteria--enforce-local repo covered))))
    ;; nil/warn: never refused
    (dolist (mode '(nil warn))
      (let ((code-review-criteria-required mode))
        (should (null (code-review-criteria--enforce-local
                       repo uncovered)))))))

;;; The AI-draft hook (soft dependency, never implemented here)

(ert-deftest code-review-criteria-test/draft-calls-generate-function ()
  "The AI-draft hook is CALLED with (FILE PATHS) and drafts
into the file; without a hook the template stands and nothing
errors."
  (let* ((today (format-time-string "%Y-%m-%d"))
         (repo (code-review-criteria-test--make-repo
                '(("lib.py" . "x = 1\n"))))
         (calls nil)
         (code-review-repo-worktree repo)
         (code-review-criteria-generate-function
          (lambda (file paths)
            (push (list (file-name-nondirectory file) paths) calls)
            (with-temp-file file
              (insert "WHEN drafted THE SYSTEM SHALL land with tests.\n")))))
    (let ((file (code-review-criteria-draft)))
      (should (= 1 (length calls)))
      ;; no review at point: no paths prefilled
      (should (equal (car calls)
                     (list (format "%s-001-new-requirement.md" today)
                           nil)))
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (should (search-forward "WHEN drafted" nil t))))
    ;; without a hook: template only, no error
    (let ((code-review-criteria-generate-function nil))
      (should (file-exists-p (code-review-criteria-draft))))))

;;; End-to-end: local review renders

(ert-deftest code-review-criteria-test/req-2026-10-01-013-warn-banner ()
  "warn mode with no covering criteria: the local review
renders a warning banner naming the template escape hatch.
(render-level: worktree setup on, isolated db.)"
  (let* ((repo (code-review-criteria-test--make-repo
                '(("lib.py" . "def one():\n    return 1\n"))))
         (code-review-criteria-required 'warn)
         ;; hermetic: no async history-harvest child whose sentinel
         ;; would re-render this buffer AFTER the test's db reset
         (code-review-history-enabled nil))
    ;; working-tree change: the review's diff
    (with-temp-file (expand-file-name "lib.py" repo)
      (insert "def one():\n    return 2\n"))
    (code-review-test--with-db
     (let ((default-directory repo))
       (code-review-review-local-diff 1))
     (let ((deadline (+ (float-time) 15))
           (buf nil))
       (while (and (not buf) (< (float-time) deadline))
         (dolist (b (buffer-list))
           (when (and (string-match-p "Code Review: local:"
                                      (buffer-name b))
                      (with-current-buffer b
                        (goto-char (point-min))
                        (save-excursion
                          (search-forward "no criteria cover the files"
                                          nil t))))
             (setq buf b)))
         (unless buf (sit-for 0.05)))
       (unless buf (ert-fail "warn banner did not render"))
       (unwind-protect (with-current-buffer buf
         (goto-char (point-min))
         (should (search-forward "Criteria" nil t))
         (goto-char (point-min))
         (should (search-forward "no criteria cover the files in this change"
                                 nil t))
         (goto-char (point-min))
         (should (search-forward
                  "M-x code-review-criteria-insert-template"
                  nil t)))
         ;; leave no review buffer behind for later tests, EVEN ON
         ;; FAILURE: a leaked buffer's async sentinel re-render can
         ;; delete the current LOCAL row and stall later tests
         (kill-buffer buf))))))

(ert-deftest code-review-criteria-test/req-2026-10-01-014-checklist-next-to-diff ()
  "A local review renders the criteria checklist next to the
diff: the covering requirement with its id (jump button), EARS
sentence, covered paths, and the pending state marker — which
RET cycles pending -> satisfied -> violated -> pending.
(render-level: worktree setup on, isolated db.)"
  (let* ((repo (code-review-criteria-test--make-repo
                '(("lib.py" . "def one():\n    return 1\n")
                  ("specs/2026-10-01-001-lib.md"
                   . "---\npaths: [lib.py]\n---\n\nWHEN a change touches lib.py\nTHE SYSTEM SHALL show its requirement next to the diff.\n"))))
         (code-review-criteria-required 'warn)
         ;; hermetic: no async history-harvest child whose sentinel
         ;; would re-render this buffer AFTER the test's db reset
         (code-review-history-enabled nil))
    (with-temp-file (expand-file-name "lib.py" repo)
      (insert "def one():\n    return 2\n"))
    (code-review-test--with-db
     (let ((default-directory repo))
       (code-review-review-local-diff 1))
     (let ((deadline (+ (float-time) 15))
           (buf nil))
       (while (and (not buf) (< (float-time) deadline))
         (dolist (b (buffer-list))
           (when (and (string-match-p "Code Review: local:"
                                      (buffer-name b))
                      (with-current-buffer b
                        (goto-char (point-min))
                        (save-excursion
                          (search-forward "2026-10-01-001" nil t))))
             (setq buf b)))
         (unless buf (sit-for 0.05)))
       (unless buf (ert-fail "criteria checklist did not render"))
       (unwind-protect (with-current-buffer buf
         (goto-char (point-min))
         (should (search-forward "Criteria" nil t))
         ;; the requirement row: marker, id, sentence, covered path
         (goto-char (point-min))
         (should (search-forward "2026-10-01-001" nil t))
         (goto-char (point-min))
         (should (search-forward
                  "WHEN a change touches lib.py THE SYSTEM SHALL show its requirement next to the diff."
                  nil t))
         (goto-char (point-min))
         (should (search-forward "[ ] 2026-10-01-001" nil t))
         (goto-char (point-min))
         (should (search-forward "(lib.py)" nil t))
         ;; RET cycles the state marker in place — via the ACTUAL
         ;; keypress (execute-kbd-macro), not a direct call: the
         ;; keymap must live as a TEXT PROPERTY on the item line
         ;; (the section class's keymap slot alone does not route
         ;; keys), and only the keypress proves the routing
         (goto-char (point-min))
         (search-forward "2026-10-01-001")
         (beginning-of-line)
         (execute-kbd-macro "\r")
         (goto-char (point-min))
         (should (search-forward "[x] 2026-10-01-001" nil t))
         ;; second cycle THROUGH THE MARKER ITSELF: point ON the x
         ;; of the freshly rewritten [x] — replace-match writes the
         ;; new marker without the old text's keymap property, so
         ;; the toggle must re-put it or RET falls back to the mode
         ;; map exactly here
         (goto-char (point-min))
         (search-forward "[x]")
         (backward-char 2)
         (should (looking-at "x"))
         (execute-kbd-macro "\r")
         (goto-char (point-min))
         (should (search-forward "[!] 2026-10-01-001" nil t))
         (goto-char (point-min))
         (search-forward "2026-10-01-001")
         (beginning-of-line)
         (code-review-criteria-toggle)
         (goto-char (point-min))
         (should (search-forward "[ ] 2026-10-01-001" nil t)))
         ;; leave no review buffer behind for later tests, EVEN ON
         ;; FAILURE: a leaked buffer's async sentinel re-render
         ;; DELETES the current LOCAL row and stalls later local
         ;; render tests (one LOCAL row exists at a time)
         (kill-buffer buf))))))

(provide 'code-review-criteria-test)
