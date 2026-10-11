;;; code-review-criteria-trace-test.el --- ERT tests for the phase 24 criteria surface -*- lexical-binding: t; -*-
;;
;; Phase 24 tests: SPEC classification + the Specs section, the
;; traceability pass, comment-to-criterion (C-c C-t), the verdict
;; sidecar.  Test names carry the req-ID segment binding each
;; requirement to the ERT test(s) that guard it.

(require 'ert)
(require 'cl-lib)
(require 'code-review-criteria)
(require 'code-review-section-wash)
(require 'code-review-diff)
(require 'code-review-testimpact)
(require 'code-review-comment)
(require 'code-review-section)
(require 'code-review-local)
(require 'code-review-test-helpers)
;; the phase 23 test file provides the temp-repo helper
(require 'code-review-criteria-test)

(defconst code-review-criteria-trace-test--noise-rules
  '((:match "\\.md\\'" :tag "DOC" :collapse t :hide nil)
    (:match "\\.txt\\'" :tag "GEN" :collapse t))
  "One deterministic noise-rule set for the SPEC classification
tests: with these bound, EVERY `.md' file would be DOC (tagged,
collapsed) — a criteria file must escape the rule entirely.")

;;; req-2026-10-01-015: specs classified FIRST

(ert-deftest code-review-criteria-trace-test/req-2026-10-01-015-spec-classification ()
  "A criteria file classifies SPEC — FIRST, before any noise rule
(every `.md' would otherwise be DOC: tagged, collapsed), and with
neither :collapse nor :hide (focus mode never hides the
reviewed-once artifact).  A non-criteria `.md' stays DOC under
the same rules."
  (let ((code-review-diff-noise-rules
         code-review-criteria-trace-test--noise-rules))
    ;; the criteria file escapes the DOC rule entirely
    (should (equal (code-review--diff--classify-path
                    "specs/2026-10-01-001-anything.md")
                   '(:tag "SPEC")))
    (should (equal (code-review--diff--classify-path
                    "requirements/req-1.feature")
                   '(:tag "SPEC")))
    ;; a plain `.md' under the same rules: DOC + collapsed
    (should (equal (code-review--diff--classify-path "docs/readme.md")
                   '(:tag "DOC" :collapse t)))
    ;; code files keep their default (no rule matches)
    (should (null (code-review--diff--classify-path "src/x.py")))))

(ert-deftest code-review-criteria-trace-test/req-2026-10-01-015-specs-first-identity ()
  "`specs-first' is IDENTITY on a diff with no criteria file
(the text returns untouched, no join) and LIFTS the criteria
blocks to the head otherwise, blocks keeping their trailing
newlines (a block missing one glues onto the next header)."
  (let* ((code-diff "diff --git a/src/x.py b/src/x.py\n--- a/src/x.py\n+++ b/src/x.py\n@@ -1 +1 @@\n-a\n+b\n")
         (spec-diff "diff --git a/specs/2026-10-01-001-x.md b/specs/2026-10-01-001-x.md\n--- a/specs/2026-10-01-001-x.md\n+++ b/specs/2026-10-01-001-x.md\n@@ -1 +1 @@\n-WHEN a\n+WHEN b\n"))
    ;; no criteria: identity
    (should (equal (code-review-section--specs-first code-diff)
                   code-diff))
    ;; criteria LAST in the text: lifted to the head, the code
    ;; block follows, no glued headers
    (let ((reordered (code-review-section--specs-first
                      (concat code-diff spec-diff))))
      (should (string-prefix-p
               "diff --git a/specs/2026-10-01-001-x.md" reordered))
      (should (> (string-match "diff --git a/src/x.py" reordered) 0))
      (should (not (string-match-p "[^\n]diff --git " reordered)))
      (should (string-suffix-p "\n" reordered)))))

;;; req-2026-10-01-016: the traceability pass

(ert-deftest code-review-criteria-trace-test/req-2026-10-01-016-strong-and-hint ()
  "STRONG: a criteria file in the diff no test hunk references.
HINT: a test hunk referencing a req id whose criteria file the
diff does not touch.  A criteria file AND its referencing test
together: no finding at all."
  (let* ((spec-block "diff --git a/specs/2026-10-01-001-atomic.md b/specs/2026-10-01-001-atomic.md\n--- a/specs/2026-10-01-001-atomic.md\n+++ b/specs/2026-10-01-001-atomic.md\n@@ -1 +1 @@\n-WHEN a\n+WHEN b\n")
         (code-block "diff --git a/src/x.py b/src/x.py\n--- a/src/x.py\n+++ b/src/x.py\n@@ -1 +1 @@\n-a\n+b\n")
         (test-block "diff --git a/tests/test_x.py b/tests/test_x.py\n--- a/tests/test_x.py\n+++ b/tests/test_x.py\n@@ -1 +1 @@\n-def test_old():\n+def test_new():  # req-2026-10-01-001\n+    pass\n"))
    ;; spec modified, no test references it: STRONG
    (should (equal (code-review-criteria--traceability
                    (concat code-block spec-block))
                   '((:kind strong :id "2026-10-01-001"
                            :path "specs/2026-10-01-001-atomic.md"))))
    ;; test references an untouched criterion: HINT
    (should (equal (code-review-criteria--traceability
                    (concat code-block test-block))
                   '((:kind hint :id "2026-10-01-001"
                            :path "tests/test_x.py"))))
    ;; spec AND its test change together: no finding
    (should (null (code-review-criteria--traceability
                   (concat spec-block test-block))))
    ;; nil diff: nothing
    (should (null (code-review-criteria--traceability nil)))))

(ert-deftest code-review-criteria-trace-test/req-2026-10-01-016-run-is-cached ()
  "`--traceability-run' caches per (PR-ID, md5 of diff): a second
run with the same pair does NOT recompute (the compute is faked
with `fset' and counted; `fset' + `unwind-protect', never
`cl-letf' — the documented trap), a NEW diff does."
  (let* ((diff-a "diff --git a/specs/2026-10-01-002-a.md b/specs/2026-10-01-002-a.md\n--- a/specs/2026-10-01-002-a.md\n+++ b/specs/2026-10-01-002-a.md\n@@ -1 +1 @@\n-a\n+b\n")
         (diff-b "diff --git a/specs/2026-10-01-003-b.md b/specs/2026-10-01-003-b.md\n--- a/specs/2026-10-01-003-b.md\n+++ b/specs/2026-10-01-003-b.md\n@@ -1 +1 @@\n-a\n+b\n")
         (calls 0)
         (real (symbol-function 'code-review-criteria--traceability)))
    (unwind-protect
        (progn
          (fset 'code-review-criteria--traceability
                (lambda (_diff)
                  (setq calls (1+ calls))
                  '((:kind strong :id "faked" :path "faked"))))
          ;; the cache is global: use a probe-only PR id so other
          ;; tests can never collide with these keys
          (should (equal (code-review-criteria--traceability-run
                          "p24-cache-probe" diff-a)
                         '((:kind strong :id "faked" :path "faked"))))
          (should (= calls 1))
          ;; same (pr, diff): cached, no recompute
          (should (equal (code-review-criteria--traceability-run
                          "p24-cache-probe" diff-a)
                         '((:kind strong :id "faked" :path "faked"))))
          (should (= calls 1))
          ;; new diff: recomputed
          (code-review-criteria--traceability-run "p24-cache-probe" diff-b)
          (should (= calls 2))
          ;; nil diff: never signals (the value is whatever the
          ;; compute returns — under this fake, the fake result)
          (should (code-review-criteria--traceability-run
                   "p24-cache-probe" nil)))
      (fset 'code-review-criteria--traceability real))))

;;; req-2026-10-01-017: comment-to-criterion

(ert-deftest code-review-criteria-trace-test/req-2026-10-01-017-from-comment ()
  "`--from-comment': the first paragraph becomes the SHALL clause
(assumed: the comment states the gap, the criterion states the
invariant), the id is date-numbered from the worktree (computed
from TODAY, never hardcoded — a hardcoded date passes on the day
it was written and fails the next), the path is anchored in the
front matter, the body is EARS-shaped.  No sentence: nil."
  (let* ((today (format-time-string "%Y-%m-%d"))
         (repo (code-review-criteria-test--make-repo
                '(("lib.py" . "x = 1\n"))))
         (criterion (code-review-criteria--from-comment
                     "The retry loop drops the last batch. When a batch fails the system shall retry it three times.\n\n(Seen on the incident of last week.)"
                     "lib.py" repo)))
    (should criterion)
    (should (equal (plist-get criterion :id) (concat today "-001")))
    (should (equal (plist-get criterion :sentence)
                   "The retry loop drops the last batch. When a batch fails the system shall retry it three times."))
    (should (equal (plist-get criterion :paths) '("lib.py")))
    (should (equal (file-name-nondirectory (plist-get criterion :file))
                   (concat today "-001-from-review.md")))
    (should (string-match-p "paths: \\[lib\\.py\\]"
                             (plist-get criterion :text)))
    (should (string-match-p "WHEN a change touches lib\\.py THE SYSTEM SHALL"
                            (plist-get criterion :text)))
    ;; whitespace-only comment: no criterion
    (should-not (code-review-criteria--from-comment "  \n\n" "lib.py" repo))))

(ert-deftest code-review-criteria-trace-test/req-2026-10-01-017-strategy-chain ()
  "The submission chain, FIRST MATCH WINS over the ordered
`code-review-criteria-submit-strategies' list: agent (a wired
agent function and the PR carries one), then push; `suggest' is
terminal (always fires); an EMPTY list fires nothing (the caller
still records the sidecar).  The sub-strategies are faked with
`fset' + `unwind-protect' (never `cl-letf')."
  (let ((real-agent (symbol-function 'code-review-criteria--submit-via-agent))
        (real-push (symbol-function 'code-review-criteria--submit-via-push))
        ;; :file is ABSOLUTE and in a temp dir: the `local' leg of
        ;; this test runs the REAL --submit-via-local, which writes
        ;; the file — a relative :file here once leaked a 1-byte
        ;; specs/x.md into the real repo on every `make test' (the
        ;; batch suite runs in the repo root)
        (criterion (list :id "2026-10-01-001"
                         :file (expand-file-name
                                "x.md" (make-temp-file "cr-chain-" t))
                         :sentence "S" :paths nil :text "T")))
    (unwind-protect
        (progn
          ;; the default chain: agent first
          (fset 'code-review-criteria--submit-via-agent (lambda (&rest _) t))
          (fset 'code-review-criteria--submit-via-push (lambda (&rest _) t))
          (should (eq (code-review-criteria--submit criterion "/tmp") 'agent))
          ;; agent fails or is not wired: push
          (fset 'code-review-criteria--submit-via-agent (lambda (&rest _) nil))
          (should (eq (code-review-criteria--submit criterion "/tmp") 'push))
          ;; both fail: the terminal suggest
          (fset 'code-review-criteria--submit-via-push (lambda (&rest _) nil))
          (should (eq (code-review-criteria--submit criterion "/tmp")
                      'suggest))
          ;; an EMPTY list: nothing fires
          (let ((code-review-criteria-submit-strategies '()))
            (should (null (code-review-criteria--submit criterion "/tmp"))))
          ;; a local-only list ignores agent/push entirely
          (let ((code-review-criteria-submit-strategies '(local)))
            (should (eq (code-review-criteria--submit criterion "/tmp")
                        'local))))
      (fset 'code-review-criteria--submit-via-agent real-agent)
      (fset 'code-review-criteria--submit-via-push real-push))))

(ert-deftest code-review-criteria-trace-test/req-2026-10-01-021-local-strategy ()
  "The `local' strategy (the UNVERSIONED solo model): the
criteria file lands in the worktree, NOTHING ELSE happens (no
git add, no commit, no push), and it always fires.  The criteria
directory is not under version control in this model."
  (let* ((repo (make-temp-file "cr-local-strategy-" t))
         (file (expand-file-name "specs/2026-10-01-042-x.md" repo))
         (criterion (list :id "2026-10-01-042" :file file
                          :sentence "S" :paths '("lib.py")
                          :text "WHEN a THE SYSTEM SHALL b.\n")))
    (should (code-review-criteria--submit-via-local criterion))
    (should (file-exists-p file))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (should (search-forward "WHEN a THE SYSTEM SHALL b." nil t)))
    ;; a failing write logs and falls through (never signals):
    ;; a FILE where the criteria directory should be
    (with-temp-file (expand-file-name "blocker" repo) (insert "x"))
    (should-not (code-review-criteria--submit-via-local
                 (list :id "x" :file (expand-file-name "blocker/y.md" repo)
                       :sentence "S" :paths nil :text "T")))))

(ert-deftest code-review-criteria-trace-test/req-2026-10-01-017-suggestion-and-key ()
  "Strategy 3 drafts the FULL criteria file as a fenced
suggestion block (the AUTHOR applies it — the reviewer authors
criteria, never code), and C-c C-t is bound in the comment
buffer to the command that runs the chain."
  (let* ((criterion (list :id "2026-10-01-001" :file "specs/x.md"
                          :sentence "S" :paths '("lib.py")
                          :text "---\npaths: [lib.py]\n---\n\nWHEN x THE SYSTEM SHALL y.\n"))
         (suggestion (code-review-criteria--suggestion-text criterion)))
    (should (string-prefix-p "```suggestion\n" suggestion))
    (should (string-suffix-p "```\n" suggestion))
    (should (string-match-p "WHEN x THE SYSTEM SHALL y" suggestion)))
  ;; the comment-buffer key
  (should (eq (lookup-key code-review-comment-mode-map (kbd "C-c C-t"))
              'code-review-comment-to-criterion)))

;;; req-2026-10-01-018: the verdict sidecar

(ert-deftest code-review-criteria-trace-test/req-2026-10-01-018-verdict-sidecar ()
  "Every captured criterion leaves a GREPPABLE verdict entry in
the sidecar: date, req id, file:line anchor, the sentence, the
original comment.  Entries APPEND (nothing is overwritten); the
call never signals."
  (let* ((today (format-time-string "%Y-%m-%d"))
         (repo (make-temp-file "cr-verdict-" t))
         (criterion (list :id (concat today "-007")
                          :file "specs/x.md" :sentence "SHALL retry"
                          :paths '("lib.py") :text "T"))
         (file (code-review-criteria--verdict-sidecar
                criterion "drop the batch" "lib.py" 42 repo)))
    (should (equal file (expand-file-name
                         code-review-criteria-verdicts-file repo)))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (should (search-forward (concat "* " today " req-" (concat today "-007")
                                      " lib.py:42")
                              nil t))
      (goto-char (point-min))
      (should (search-forward "SHALL retry" nil t))
      (goto-char (point-min))
      (should (search-forward "drop the batch" nil t)))
    ;; appends: a second verdict keeps the first
    (code-review-criteria--verdict-sidecar
     (list :id (concat today "-008") :sentence "SHALL log"
           :file "specs/y.md" :paths nil :text "T")
     "nothing logged" nil nil repo)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (should (search-forward (concat "req-" today "-007") nil t))
      (goto-char (point-min))
      (should (search-forward "SHALL log" nil t)))
    ;; a nil line/path never explodes the anchor
    (should (string-match-p
             (concat "req-" today "-008")
             (with-temp-buffer
               (insert-file-contents file)
               (buffer-string))))))

;;; req-2026-10-01-019: specs are prose to the CI-gaming detector

(ert-deftest code-review-criteria-trace-test/req-2026-10-01-019-specs-are-prose ()
  "A SPEC-classified criteria file counts as a DOC file to the
CI-gaming detector: a requirements file is prose, its CI-marker
mentions are claims, not code (the detector once flagged its own
release notes)."
  (let ((code-review-diff-noise-rules
         code-review-criteria-trace-test--noise-rules))
    (should (code-review-testimpact--doc-file-p
             "specs/2026-10-01-001-ci.md"))
    (should (code-review-testimpact--doc-file-p "docs/ci.md"))
    (should-not (code-review-testimpact--doc-file-p "src/x.py"))))

;;; End-to-end: the rendered surface

(ert-deftest code-review-criteria-trace-test/req-2026-10-01-015-016-render ()
  "A local review of a change touching BOTH a criteria file and
covered code renders: the Specs section ABOVE Files changed (with
the EARS keywords fontified by overlays), and the strong
traceability finding (the criteria file changed, no test hunk
references it) in the Analysis section with a jump button.
(render-level: worktree setup on, isolated db, HERMETIC — no
async history child — and LEAK-PROOF — the buffer is killed even
on failure.)"
  (let* ((repo (code-review-criteria-test--make-repo
                '(("lib.py" . "def one():\n    return 1\n")
                  ("tests/test_lib.py" . "def test_one():\n    assert True\n")
                  ("specs/2026-10-01-001-lib.md"
                   . "---\npaths: [lib.py]\n---\n\nWHEN a change touches lib.py\nTHE SYSTEM SHALL show its requirement next to the diff.\n"))))
         (code-review-criteria-required 'warn)
         ;; hermetic: no async history-harvest child whose sentinel
         ;; would re-render this buffer AFTER the test's db reset
         (code-review-history-enabled nil))
    ;; working-tree change: BOTH the code and the criteria file
    (with-temp-file (expand-file-name "lib.py" repo)
      (insert "def one():\n    return 2\n"))
    (with-temp-file (expand-file-name "specs/2026-10-01-001-lib.md" repo)
      (insert "---\npaths: [lib.py]\n---\n\nWHEN a change touches lib.py\nTHE SYSTEM SHALL show its requirement next to the diff, fontified.\n"))
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
                          (search-forward "(traceability)" nil t))))
             (setq buf b)))
         (unless buf (sit-for 0.05)))
       (unless buf (ert-fail "traceability findings did not render"))
       (unwind-protect (with-current-buffer buf
         (goto-char (point-min))
         (let ((specs-pos (search-forward "Specs (criteria)" nil t))
               (files-pos (progn (goto-char (point-min))
                                 (search-forward "Files changed" nil t))))
           ;; the Specs section renders ABOVE Files changed
           (should (< specs-pos files-pos)))
         ;; the strong finding: req id, the criteria file, the
         ;; hint that no test hunk references it
         (goto-char (point-min))
         (should (search-forward
                  "(traceability) req-2026-10-01-001 modified in specs/2026-10-01-001-lib.md"
                  nil t))
         (goto-char (point-min))
         (should (search-forward
                  "no test in this diff references it"
                  nil t))
         ;; the EARS keyword is fontified by an OVERLAY (overrides
         ;; the hunk paint's text-property faces, the phase 10
         ;; lesson) somewhere on a SHALL inside the specs section
         (let ((found nil)
               (p (point-min)))
           (while (and (not found)
                       (search-forward "SHALL" nil t))
             (let ((ovs (overlays-at (1- (point)))))
               (dolist (ov ovs)
                 (when (eq (overlay-get ov 'face) 'font-lock-keyword-face)
                   (setq found t)))))
           (should found)))
         ;; leave no review buffer behind for later tests, EVEN ON
         ;; FAILURE: a leaked buffer's async sentinel re-render
         ;; deletes the current LOCAL row and stalls later tests
         (kill-buffer buf))))))

(provide 'code-review-criteria-trace-test)
