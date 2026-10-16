;;; code-review-criteria-home-test.el --- ERT tests for the criteria home (phase 29) -*- lexical-binding: t; -*-
;;
;; Phase 29 tests: the test-block scanner, the local index, the
;; merged provider seam, the trigger table's pure parts, the
;; lift command, and the 30-file migration invariants (ids and
;; test names unchanged).

(require 'ert)
(require 'cl-lib)
(require 'code-review-criteria)
(require 'code-review-criteria-home)
(require 'code-review-section-criteria)
(require 'code-review-analysis)

;;; Fixtures

(defun code-review-criteria-home-test--make-repo (files-alist)
  "Create a temp git repository with FILES-ALIST committed at HEAD.
Return its directory (with trailing slash)."
  (let* ((dir (file-name-as-directory (make-temp-file "cr-home-" t)))
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

(defconst code-review-criteria-home-test--block
  ";;; req-2026-10-01-001: export is atomic
;; EARS: WHEN a payment export runs
;; THE SYSTEM SHALL write it atomically.
;; covers: src/export.py, src/
;; covers-fn: export
;; enforcement: test
;; incident: 2026-047
;; incident: 2026-051
"
  "One complete req block fixture.")

;;; Pure parsing

(ert-deftest code-review-criteria-home-test/parse-block-keys ()
  (let ((p (code-review-criteria-home--parse-block
            (split-string code-review-criteria-home-test--block "\n" t))))
    (should (equal (plist-get p :id) "2026-10-01-001"))
    (should (equal (plist-get p :title) "export is atomic"))
    (should (equal (plist-get p :sentence)
                   "WHEN a payment export runs THE SYSTEM SHALL write it atomically."))
    (should (equal (plist-get p :paths) '("src/export.py" "src/")))
    (should (equal (plist-get p :fns) '("export")))
    (should (equal (plist-get p :enforcement) "test"))
    (should (equal (plist-get p :incidents) '("2026-047" "2026-051")))))

(ert-deftest code-review-criteria-home-test/parse-block-continuations ()
  "An UNKNOWN `key:' line and a plain line both continue the
previous key (an EARS sentence may legitimately contain a
colon)."
  (let ((p (code-review-criteria-home--parse-block
            '(";;; req-2026-10-01-002: continuation"
              ";; EARS: WHEN x: the edge case THE SYSTEM SHALL"
              ";; still: parse the sentence correctly"
              ";; and keep going"))))
    (should (equal (plist-get p :sentence)
                   "WHEN x: the edge case THE SYSTEM SHALL still: parse the sentence correctly and keep going")))
  ;; a header with no id shape: no block
  (should-not (code-review-criteria-home--parse-block
               '(";;; req-not-an-id: no"
                 ";; EARS: WHEN x THE SYSTEM SHALL y."))))

(ert-deftest code-review-criteria-home-test/scan-text-blocks ()
  "Consecutive blocks FLUSH (the second header ends the first),
a block with no EARS falls back to the next deftest's
DOCSTRING, and a block with neither is skipped (a lift
candidate, not a criterion)."
  (let* ((text (concat
                code-review-criteria-home-test--block
                "(ert-deftest x/req-2026-10-01-001-export ()\n"
                "  \"docstring\"\n  (should t))\n\n"
                ";;; req-2026-10-01-002: no ears here\n"
                ";; covers: src/y.py\n"
                "(ert-deftest x/no-ears ()\n"
                "  \"WHEN the scanner reads a docstring THE SYSTEM SHALL use it as the definition.\"\n"
                "  (should t))\n\n"
                ";;; req-2026-10-01-003: neither\n"
                "(defun not-a-test ()\n  nil)\n"))
         (res (code-review-criteria-home--scan-text
               text "test/foo-test.el")))
    (should (equal (mapcar (lambda (r) (plist-get r :id)) res)
                   '("2026-10-01-001" "2026-10-01-002")))
    (should (cl-some (lambda (r)
                        (and (equal (plist-get r :id) "2026-10-01-002")
                             (equal (plist-get r :sentence)
                                    "WHEN the scanner reads a docstring THE SYSTEM SHALL use it as the definition.")))
                      res))
    (should (cl-every (lambda (r) (eq (plist-get r :source) 'test)) res))
    (should (cl-every (lambda (r) (equal (plist-get r :file)
                                         "test/foo-test.el"))
                      res))))

(ert-deftest code-review-criteria-home-test/unlifted-deftests ()
  (let ((names (code-review-criteria-home--unlifted-deftests
                "(ert-deftest x/req-2026-10-01-001-atomic ()\n  nil)\n
\(ert-deftest x/plain-thing ()\n  nil)\n
\(ert-deftest x/another ()\n  nil)\n")))
    (should (equal names '("x/plain-thing" "x/another")))))

;;; The local index

(ert-deftest code-review-criteria-home-test/index-parse ()
  (let* ((text (concat "# header prose\n"
                       "## 2026-10-01-001\n"
                       "priority: P1\n"
                       "## 2026-10-01-002\n"
                       "priority: P9\n"     ; invalid: ignored
                       "definition: WHEN x THE SYSTEM SHALL y\n"
                       "and keep going\n"
                       "## 2026-10-01-003\n"
                       "not-colocated: true\n"))
         (idx (code-review-criteria-home--parse-index-text text)))
    (should (equal (car (assoc "2026-10-01-001" idx)) "2026-10-01-001"))
    (should (equal (plist-get (cdr (assoc "2026-10-01-001" idx)) :priority)
                   "P1"))
    (should (null (plist-get (cdr (assoc "2026-10-01-002" idx)) :priority)))
    (should (equal (plist-get (cdr (assoc "2026-10-01-002" idx)) :definition)
                   "WHEN x THE SYSTEM SHALL y and keep going"))
    (should (plist-get (cdr (assoc "2026-10-01-003" idx)) :not-colocated))))

(ert-deftest code-review-criteria-home-test/index-rewrite ()
  (let* ((base "## 2026-10-01-001\npriority: P1\nnote prose here\n\n## 2026-10-01-002\npriority: P2\n")
         (appended (code-review-criteria-home--index-rewrite
                    base "2026-10-01-009" :priority "P3"))
         (replaced (code-review-criteria-home--index-rewrite
                    base "2026-10-01-001" :priority "P2"))
         (removed (code-review-criteria-home--index-rewrite
                   base "2026-10-01-001" :priority nil)))
    ;; a fresh stanza is appended, everything else untouched
    (should (string-match-p
             "priority: P1\nnote prose here\n\n## 2026-10-01-002\npriority: P2\n\n## 2026-10-01-009\npriority: P3\n"
             appended))
    (should (string-match-p "## 2026-10-01-002\npriority: P2" replaced))
    (should (not (string-match-p "priority: P1" removed)))
    ;; prose survives every rewrite
    (should (string-match-p "note prose here" replaced))
    (should (string-match-p "note prose here" removed))
    ;; ensure: an existing stanza is untouched, a missing one added
    (should (equal (code-review-criteria-home--index-ensure
                    base "2026-10-01-001")
                   base))
    (should (string-match-p "## 2026-10-01-009\n\\'"
                            (code-review-criteria-home--index-ensure
                             base "2026-10-01-009")))))

(ert-deftest code-review-criteria-home-test/index-rewrite-definition ()
  "Rewriting a `definition:' drops its continuation lines with
it and never disturbs the other keys."
  (let* ((base "## 2026-10-01-001\ndefinition: WHEN x\nTHE SYSTEM SHALL y\npriority: P1\n")
         (rewritten (code-review-criteria-home--index-rewrite
                     base "2026-10-01-001"
                     :definition "WHEN z THE SYSTEM SHALL w")))
    (should (string-match-p "definition: WHEN z THE SYSTEM SHALL w" rewritten))
    (should (not (string-match-p "SHALL y" rewritten)))
    (should (string-match-p "priority: P1" rewritten))))

;;; The merged provider (the seam)

(ert-deftest code-review-criteria-home-test/home-criteria-merge ()
  (let* ((repo (code-review-criteria-home-test--make-repo
                (list (cons "test/foo-test.el"
                            (concat code-review-criteria-home-test--block
                                    "(ert-deftest x/req-2026-10-01-001-export ()\n"
                                    "  \"doc\"\n  (should t))\n")))))
         (index (expand-file-name "specs/index.md" repo)))
    ;; the index overlay: priority rides the tool state
    (code-review-criteria-home--index-set
     repo "2026-10-01-001" :priority "P2")
    ;; a local-only definition (a C-u lift): definition + flag
    (code-review-criteria-home--index-set
     repo "2026-10-12-001" :definition
     "WHEN a lift is local-only THE SYSTEM SHALL key it to the index.")
    (code-review-criteria-home--index-set
     repo "2026-10-12-001" :not-colocated t)
    (let* ((home (code-review-criteria-home--criteria repo))
           (one (cl-find "2026-10-01-001" home
                         :key (lambda (r) (plist-get r :id)) :test #'equal))
           (two (cl-find "2026-10-12-001" home
                         :key (lambda (r) (plist-get r :id)) :test #'equal)))
      (should (equal (length home) 2))
      (should (equal (plist-get one :priority) "P2"))
      (should (plist-get one :index))
      (should (eq (plist-get one :source) 'test))
      (should (equal (plist-get one :file) "test/foo-test.el"))
      (should (eq (plist-get two :source) 'index))
      (should (plist-get two :not-colocated))
      (should (equal (plist-get two :file) "specs/index.md"))
      ;; id-sorted
      (should (equal (mapcar (lambda (r) (plist-get r :id)) home)
                     '("2026-10-01-001" "2026-10-12-001"))))))

(ert-deftest code-review-criteria-home-test/criteria-seam-home-wins ()
  "`--criteria' appends the home provider's result and keeps
the HOME entry on a shared id (the definition-in-test ownership
split)."
  (let* ((repo (code-review-criteria-home-test--make-repo
                (list (cons "specs/2026-10-01-001-shadow.md"
                            "---\npaths: [src/export.py]\n---\n\nWHEN the legacy file exists THE SYSTEM SHALL still lose to the block.\n")
                         (cons "specs/2026-10-01-010-legacy.md"
                              "---\npaths: [src/old.py]\n---\n\nWHEN no block carries the id THE SYSTEM SHALL keep the spec file.\n")
                         (cons "test/foo-test.el"
                              (concat code-review-criteria-home-test--block
                                      "(ert-deftest x/req-2026-10-01-001-export ()\n"
                                      "  \"doc\"\n  (should t))\n")))))
         (criteria (code-review-criteria--criteria repo)))
    (should (equal (mapcar (lambda (r) (plist-get r :id)) criteria)
                   '("2026-10-01-001" "2026-10-01-010")))
    (should (equal (plist-get (car criteria) :sentence)
                   "WHEN a payment export runs THE SYSTEM SHALL write it atomically."))
    (should (equal (plist-get (cadr criteria) :sentence)
                   "WHEN no block carries the id THE SYSTEM SHALL keep the spec file."))))

;;; Function-level covers

(ert-deftest code-review-criteria-home-test/changed-fns-and-matching ()
  "The covers-fn seam: a criterion declares `covers-fn:' and
matches the change at FUNCTION granularity; elisp VARIABLE
definitions are not behavior (the phase 17 rule)."
  (let* ((diff (concat
                "diff --git a/src/x.el b/src/x.el\n"
                "--- a/src/x.el\n+++ b/src/x.el\n"
                "@@ -1 +1,3 @@\n"
                "-old\n"
                "+(defun new-behavior ()\n"
                "+  nil)\n"
                "+(defvar new-variable nil)\n"
                "diff --git a/README.md b/README.md\n"
                "--- a/README.md\n+++ b/README.md\n"
                "@@ -1 +1 @@\n"
                "-a\n+b\n"))
         (fns (code-review-criteria--changed-fns diff)))
    (should (equal fns '("new-behavior")))
    ;; matching: a path-covered criterion and a fn-covered one
    (let* ((req-path '(:id "2026-10-01-001" :paths ("README.md") :fns nil))
           (req-fn '(:id "2026-10-01-002" :paths nil :fns ("new-behavior")))
           (req-none '(:id "2026-10-01-003" :paths nil :fns ("other-fn")))
           (m (code-review-criteria--matching
               (list req-path req-fn req-none)
               '("README.md") fns)))
      (should (equal (mapcar (lambda (x)
                               (plist-get (plist-get x :req) :id))
                             m)
                     '("2026-10-01-001" "2026-10-01-002")))
      ;; the fn-only match covers no path (row shows no buttons)
      (should (equal (plist-get (cadr m) :paths) nil))
      (should (equal (plist-get (car m) :paths) '("README.md"))))))

;;; The trigger table's pure parts

(ert-deftest code-review-criteria-home-test/spec-change-p ()
  (let ((req '(:id "2026-10-01-001" :file "test/foo-test.el")))
    (should (code-review-criteria-home--spec-change-p
             req '("src/x.py" "test/foo-test.el")))
    (should-not (code-review-criteria-home--spec-change-p
                 req '("src/x.py")))
    (should-not (code-review-criteria-home--spec-change-p req nil))))

(ert-deftest code-review-criteria-home-test/lift-candidates ()
  "The changed test files carrying NO req blocks are the lift
candidates."
  (let* ((repo (code-review-criteria-home-test--make-repo
                (list (cons "test/plain-test.el"
                            "(ert-deftest x/plain ()\n  \"doc\"\n  nil)\n")
                         (cons "test/blocky-test.el"
                              (concat code-review-criteria-home-test--block
                                      "(ert-deftest x/req-2026-10-01-001 ()\n"
                                      "  \"doc\"\n  nil)\n")))))
         (cands (code-review-criteria-home--lift-candidates
                 repo '("test/plain-test.el" "test/blocky-test.el"
                        "src/x.py"))))
    (should (equal cands '("test/plain-test.el")))))

;;; The lift command

(ert-deftest code-review-criteria-home-test/lift-co-located ()
  "The lift drafts the EARS sentence from the test's own
DOCSTRING, writes the block CO-LOCATED (above the deftest), and
ensures the index stanza.  The id is date-numbered from today.
The fixture binds the WORKTREE (the lift's repo resolution) to
the temp repo — never the ambient default-directory."
  (let* ((repo (code-review-criteria-home-test--make-repo
                '(("test/plain-test.el" . "(ert-deftest x/plain-thing ()\n  \"writes the export atomically\"\n  nil)\n"))))
         (file (expand-file-name "test/plain-test.el" repo))
         (id (let ((code-review-repo-worktree repo))
               (code-review-criteria-lift-test "test/plain-test.el" nil))))
    (should (equal id (concat (format-time-string "%Y-%m-%d") "-001")))
    (let ((text (with-temp-buffer
                  (insert-file-contents file)
                  (buffer-string))))
      ;; the block carries the machine keys only — the sentence is
      ;; the test's own DESCRIPTION, never a comment line
      (should (string-match-p
               (concat ";;; req-" id ": x/plain-thing (plain-test)\n"
                       ";; enforcement: test\n"
                       "(ert-deftest x/plain-thing ()\n")
               text))
      (should-not (string-match-p ";; EARS:" text))
      ;; the docstring's FIRST PARAGRAPH is the reshaped sentence
      (should (string-match-p
               (concat "  \"WHEN x/plain-thing THE SYSTEM SHALL"
                       " writes the export atomically\"")
               text)))
    ;; the block scans as a criterion; the index stanza exists
    (let* ((scanned (code-review-criteria-home--scan-text
                     (with-temp-buffer
                       (insert-file-contents file)
                       (buffer-string))
                     "test/plain-test.el")))
      (should (equal (mapcar (lambda (r) (plist-get r :id)) scanned)
                     (list id))))
    (with-temp-buffer
      (insert-file-contents (expand-file-name "specs/index.md" repo))
      (should (search-forward (concat "## " id) nil t)))))

(ert-deftest code-review-criteria-home-test/lift-local-only ()
  "With LOCAL-ONLY the definition is stored in the index, flagged
not-colocated, and the test file is NOT touched."
  (let* ((repo (code-review-criteria-home-test--make-repo
                '(("test/plain-test.el" . "(ert-deftest x/plain-thing ()\n  \"docstring\"\n  nil)\n"))))
         (file (expand-file-name "test/plain-test.el" repo))
         (before (with-temp-buffer
                   (insert-file-contents file)
                   (buffer-string)))
         (id (let ((code-review-repo-worktree repo))
               (code-review-criteria-lift-test "test/plain-test.el" t))))
    (should id)
    ;; the test file untouched
    (should (equal (with-temp-buffer
                     (insert-file-contents file)
                     (buffer-string))
                   before))
    ;; the definition in the index, flagged
    (let* ((text (with-temp-buffer
                   (insert-file-contents
                    (expand-file-name "specs/index.md" repo))
                   (buffer-string)))
           (idx (code-review-criteria-home--parse-index-text text))
           (entry (cdr (assoc id idx))))
      (should (plist-get entry :definition))
      (should (plist-get entry :not-colocated)))))

(ert-deftest code-review-criteria-home-test/insert-block-replaces-header ()
  "`--insert-block' replaces an existing `;;; req-' block
directly above the deftest (the migration shape) and inserts
above the deftest otherwise."
  (let* ((file (make-temp-file "cr-insblock-" nil ".el"))
         (bare ";;; req-2026-10-01-001: old title\n")
         (body "(ert-deftest x/req-2026-10-01-001-a ()\n  nil)\n"))
    (with-temp-file file (insert bare body))
    (code-review-criteria-home--insert-block
     file "x/req-2026-10-01-001-a"
     ";;; req-2026-10-01-001: new title\n;; EARS: WHEN x THE SYSTEM SHALL y.")
    (let ((text (with-temp-buffer
                  (insert-file-contents file)
                  (buffer-string))))
      (should (string-match-p
               ";;; req-2026-10-01-001: new title\n;; EARS: WHEN x THE SYSTEM SHALL y\.\n(ert-deftest"
               text))
      (should (not (string-match-p "old title" text))))
    ;; no deftest: nil, nothing written
    (should-not (code-review-criteria-home--insert-block
                 file "x/nonexistent"
                 ";;; req-2026-10-01-002: nope"))))

;;; The migration invariants (dogfood: this repository's own ledger)

(ert-deftest code-review-criteria-home-test/migration-invariants ()
  "The 30 migrated requirements scan from THIS repository's test
blocks with the ids, sentences and guard-test names unchanged:
every criterion id still has an ERT test whose name carries it,
every block declares its enforcement, and the local index holds
the priorities."
  (let* ((root (expand-file-name
                ".."
                (file-name-directory (or load-file-name buffer-file-name))))
         (home (code-review-criteria-home--criteria root))
         (ids (mapcar (lambda (r) (plist-get r :id)) home))
         (index (code-review-criteria-home--parse-index-text
                 (with-temp-buffer
                   (insert-file-contents
                    (expand-file-name "specs/index.md" root))
                   (buffer-string)))))
    ;; New requirements may be added without changing the migrated set.
    (should (equal (length (cl-remove-if-not
                           (lambda (id) (string-lessp id "2026-10-16"))
                           ids))
                   30))
    ;; sorted, unique
    (should (equal (delete-dups (copy-sequence ids)) ids))
    (dolist (req home)
      (let ((id (plist-get req :id)))
        ;; the guard test still carries the id in its NAME
        (let ((text (with-temp-buffer
                      (insert-file-contents
                       (expand-file-name (plist-get req :file) root))
                      (buffer-string))))
          (should (string-match-p (concat "req-" id) text)))
        ;; the definition made it whole
        (should (plist-get req :sentence))
        (should (string-match-p "SHALL" (plist-get req :sentence)))
        (should (equal (plist-get req :enforcement) "test"))
        (should (equal (plist-get req :source) 'test))
        ;; the priority rides the local index
        (should (equal (plist-get (cdr (assoc id index)) :priority)
                       (plist-get req :priority)))))))

;;; code-review-criteria-home-test.el ends here
