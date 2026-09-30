;;; code-review-registry-test.el --- ERT tests for the incident registry -*- lexical-binding: t; -*-
;;
;; Phase 22 tests: pure-function tests for the trailer/front-matter
;; parsing, the entry template, id sequencing, keyword detection and
;; the idempotent conventions append, plus git integration tests
;; (trailer `git log' scan, entry generation, cache invalidation, the
;; incidents/ fallback) and the consumption wiring (phase 15 badge
;; heat, phase 5 dead never-flag).  Pure parts run anywhere; the git
;; tests must run in a fresh batch emacs.

(require 'ert)
(require 'cl-lib)
(require 'code-review-registry)
(require 'code-review-analysis)
(require 'code-review-local)
(require 'code-review-dossier)
(require 'code-review-test-helpers)

;;; Git integration: one temp repository

(defun code-review-registry-test--make-repo (commits)
  "Create a temp git repository; return its directory.
COMMITS are ((MSG FILES-ALIST)...): each commit writes FILES-ALIST
\((PATH . TEXT)...) and commits with MSG — incident trailers belong
in MSG's last paragraph (git's own trailer rule)."
  (let* ((dir (file-name-as-directory (make-temp-file "cr-registry-" t)))
         (default-directory dir))
    (call-process "git" nil nil nil "init" ".")
    (call-process "git" nil nil nil "config" "user.email" "test@test.test")
    (call-process "git" nil nil nil "config" "user.name" "test")
    (pcase-dolist (`(,msg . ,files) commits)
      (pcase-dolist (`(,path . ,text) files)
        (let ((full (expand-file-name path dir)))
          (make-directory (file-name-directory full) t)
          (with-temp-file full (insert text))))
      (call-process "git" nil nil nil "add" "-A")
      (call-process "git" nil nil nil "commit" "-m" msg))
    dir))

(defconst code-review-registry-test--msg
  "Fix the export race

Incident: 2026-047
Invariant-Ref: req-14
Regression-Test: test_export_is_atomic
Paths: src/payments/export.py
"
  "A fix-commit message carrying the phase 22 trailer convention.")

;;; Trailer parsing (git's own rule: last paragraph only)

(ert-deftest code-review-registry/parse-trailers ()
  ;; the last paragraph, all `Key: value' lines
  (should (equal (code-review-registry--parse-trailers
                  "Fix the export race

Incident: 2026-047
Invariant-Ref: req-14
Regression-Test: test_export_is_atomic
Paths: src/payments/export.py
")
                 '(("Incident" . "2026-047")
                   ("Invariant-Ref" . "req-14")
                   ("Regression-Test" . "test_export_is_atomic")
                   ("Paths" . "src/payments/export.py"))))
  ;; continuation lines (leading whitespace) fold into the value
  (should (equal (code-review-registry--parse-trailers
                  "Fix

Paths: a.py
  b.py
")
                 '(("Paths" . "a.py b.py"))))
  ;; a last paragraph that is not all trailers: no trailers
  (should-not (code-review-registry--parse-trailers
               "Fix the thing

Some prose here
"))
  ;; the subject line alone never counts
  (should-not (code-review-registry--parse-trailers "Fix the export race
")))

(ert-deftest code-review-registry/split-values ()
  (should (equal (code-review-registry--split-values " a.py, b.py ")
                 '("a.py" "b.py")))
  (should (equal (code-review-registry--split-values "[x, y]") '("x" "y")))
  (should (equal (code-review-registry--split-values "a.py\nb.py")
                 '("a.py" "b.py")))
  (should (null (code-review-registry--split-values "   ")))
  (should (null (code-review-registry--split-values nil)))
  (should (equal (code-review-registry--first-value "a, b") "a")))

(ert-deftest code-review-registry/incident-from-trailers ()
  (let ((inc (code-review-registry--incident-from-trailers
              '(("Incident" . "2026-047")
                ("Invariant-Ref" . "req-14")
                ("Regression-Test" . "test_export_is_atomic, test_other")
                ("Paths" . "src/payments/export.py"))
              "a1b2c3" "Fix the export race" "2026-04-11")))
    (should (equal inc
                   '(:id "2026-047"
                     :title "Fix the export race"
                     :date "2026-04-11"
                     :invariant "req-14"
                     :tests ("test_export_is_atomic" "test_other")
                     :paths ("src/payments/export.py")
                     :sha "a1b2c3"))))
  ;; no Incident trailer: no incident
  (should-not (code-review-registry--incident-from-trailers
               '(("Paths" . "src/x.py")))))

;; \x1f in a string literal greedily reads up to FOUR hex digits:
;; join the fields with a standalone "\x1f" via concat.
(ert-deftest code-review-registry/log-records-and-incidents ()
  (let* ((sep "\x1f")
         (rec (concat "a1b2c3" sep "Fix the export race" sep "2026-04-11"
                      sep "2026-047\n" sep "req-14\n"
                      sep "test_export_is_atomic\n" sep "src/x.py\n"))
         (log (concat rec "\x1e"
                      "d4e5f6" sep "unrelated" sep "2026-04-10"
                      sep "\n" sep "\n" sep "\n" sep "\n"
                      "\x1e")))
    ;; both records parse (7 fields each); only the trailer-carrying
    ;; one becomes an incident
    (should (= 2 (length (code-review-registry--log-records log))))
    (let ((incidents (code-review-registry--incidents-from-log log)))
      (should (= 1 (length incidents)))
      (should (equal (plist-get (car incidents) :id) "2026-047"))
      (should (equal (plist-get (car incidents) :sha) "a1b2c3"))
      (should (equal (plist-get (car incidents) :invariant) "req-14"))
      (should (equal (plist-get (car incidents) :paths)
                     '("src/x.py"))))))

;;; Front matter (the generated incidents/*.md files)

(ert-deftest code-review-registry/parse-front-matter ()
  (let ((fm (code-review-registry--parse-front-matter
             "---
id: 2026-047
title: Export race
tests: [a, b]
paths: [src/x.py]
---
body text
")))
    (should (equal (plist-get fm :id) "2026-047"))
    (should (equal (plist-get fm :title) "Export race"))
    (should (equal (plist-get fm :tests) "[a, b]")))
  ;; unclosed front matter: nil
  (should-not (code-review-registry--parse-front-matter
               "---
id: 2026-047
"))
  ;; no front matter at all: nil
  (should-not (code-review-registry--parse-front-matter "# plain")))

(ert-deftest code-review-registry/incident-from-front-matter ()
  (let ((inc (code-review-registry--incident-from-front-matter
              (code-review-registry--parse-front-matter
               "---
id: 2026-047
title: Export race
invariant: req-14
tests: [test_export_is_atomic]
paths: [src/x.py, src/y.py]
pr: 762
---
body
"))))
    (should (equal (plist-get inc :id) "2026-047"))
    (should (equal (plist-get inc :invariant) "req-14"))
    (should (equal (plist-get inc :tests) '("test_export_is_atomic")))
    (should (equal (plist-get inc :paths) '("src/x.py" "src/y.py")))
    (should (equal (plist-get inc :pr) "762"))))

;;; The entry template (generated from trailers, never hand-curated)

(ert-deftest code-review-registry/entry-text-round-trip ()
  (let* ((inc (list :id "2026-047" :title "Export race" :date "2026-04-11"
                    :invariant "req-14" :tests '("test_x") :paths '("src/x.py")
                    :pr "762"))
         (text (code-review-registry--entry-text inc))
         (fm (code-review-registry--parse-front-matter text)))
    ;; the machine keys of the front matter round-trip
    (should fm)
    (should (equal (plist-get fm :id) "2026-047"))
    (should (equal (plist-get fm :invariant) "req-14"))
    (should (equal (plist-get fm :pr) "762"))
    (let ((inc2 (code-review-registry--incident-from-front-matter fm)))
      (should (equal (plist-get inc2 :tests) '("test_x")))
      (should (equal (plist-get inc2 :paths) '("src/x.py"))))
    ;; the human body sections exist
    (should (string-match-p "## What happened" text))
    (should (string-match-p "## Invariant" text))
    (should (string-match-p "## Guard" text))))

(ert-deftest code-review-registry/slug-and-file-name ()
  (should (equal (code-review-registry--slug "Export race in Payments!")
                 "export-race-in-payments"))
  (should (equal (code-review-registry--slug nil) ""))
  (should (equal (code-review-registry--entry-file-name
                  (list :date "2026-04-11" :title "Export race!"))
                 "2026-04-11-export-race.md")))

(ert-deftest code-review-registry/next-id-sequencing ()
  (should (equal (code-review-registry--next-id nil "2026") "2026-001"))
  ;; counted from the incidents of the SAME year only
  (should (equal (code-review-registry--next-id
                  (list (list :id "2026-003")
                        (list :id "2026-047")
                        (list :id "2025-099"))
                  "2026")
                 "2026-048")))

;;; Detection (keywords, once per PR)

(ert-deftest code-review-registry/keyword-detection ()
  (should (code-review-registry--keyword-match-p "Fix outage in payments"))
  (should (code-review-registry--keyword-match-p "chore" "cleanup after SEV1"))
  (should (code-review-registry--keyword-match-p "CVE-2026-1234 fixed"))
  (should (code-review-registry--keyword-match-p "Rollback the deploy"))
  (should-not (code-review-registry--keyword-match-p "Add feature" "cleanup"))
  (should-not (code-review-registry--keyword-match-p nil nil)))

;; `--detect' is a no-op in batch emacs (no interactive prompt can
;; fire from a render inside a test or a daemon script)
(ert-deftest code-review-registry/detect-batch-noop ()
  (should-not (code-review-registry--detect)))

;;; Conventions installer (idempotent, sentinel-marked)

(ert-deftest code-review-registry/conventions-install-idempotent ()
  (let* ((dir (make-temp-file "cr-registry-conventions-" t))
         (file (expand-file-name "AGENTS.md" dir)))
    (with-temp-file file (insert "# Project\n\nSome rules.\n"))
    (should (eq (code-review-registry--install-into file) 'installed))
    ;; second install: present, nothing appended
    (should (eq (code-review-registry--install-into file) 'present))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (should (= 1 (how-many (regexp-quote
                              code-review-registry--conventions-sentinel)
                             (point-min) (point-max))))
      (goto-char (point-min))
      (should (= 1 (how-many "^# Project" (point-min) (point-max))))
      ;; the block documents the trailer convention
      (should (search-forward "Incident: <year-numbered id>" nil t)))))

;;; Git integration: the trailer scan, generation, cache

(ert-deftest code-review-registry/log-incidents ()
  (let* ((repo (code-review-registry-test--make-repo
                (list (cons code-review-registry-test--msg
                            '(("src/payments/export.py" . "def export():\n    pass\n"))))))
         (incidents (code-review-registry--log-incidents repo)))
    (should (= 1 (length incidents)))
    (should (equal (plist-get (car incidents) :id) "2026-047"))
    (should (equal (plist-get (car incidents) :invariant) "req-14"))
    (should (equal (plist-get (car incidents) :tests)
                   '("test_export_is_atomic")))
    (should (equal (plist-get (car incidents) :paths)
                   '("src/payments/export.py")))))

(ert-deftest code-review-registry/generate-writes-and-preserves ()
  (let* ((repo (code-review-registry-test--make-repo
                (list (cons code-review-registry-test--msg
                            '(("src/payments/export.py" . "def export():\n    pass\n"))))))
         (dir (expand-file-name code-review-registry-dir repo)))
    ;; first generate: the entry is written from the trailers
    (should (= 1 (code-review-registry-generate repo)))
    (let* ((inc (car (code-review-registry--log-incidents repo)))
           (file (expand-file-name
                  (code-review-registry--entry-file-name inc) dir)))
      (should (file-exists-p file))
      (let ((fm (code-review-registry--file-front-matter file)))
        (should (equal (plist-get fm :id) "2026-047"))
        (should (equal (plist-get fm :invariant) "req-14")))
      ;; the human body is never clobbered: edit it, regenerate,
      ;; same id -> the file is left alone
      (with-temp-file file
        (insert-file-contents file)
        (goto-char (point-max))
        (insert "\nPostmortem notes by a human.\n"))
      (should (= 0 (code-review-registry-generate repo)))
      (with-temp-buffer
        (insert-file-contents file)
        (should (search-forward "Postmortem notes by a human." nil t))))))

(ert-deftest code-review-registry/cache-invalidated-by-new-commit ()
  (let* ((repo (code-review-registry-test--make-repo
                (list (cons code-review-registry-test--msg
                            '(("src/payments/export.py" . "def export():\n    pass\n")))))))
    (should (= 1 (length (code-review-registry--incidents repo))))
    ;; a second trailer commit: new HEAD, the cache invalidates
    ;; naturally and the scan sees both incidents
    (with-temp-file (expand-file-name "src/payments/core.py" repo)
      (insert "def core():\n    pass\n"))
    (call-process "git" nil nil nil "-C" repo "add" "-A")
    (call-process "git" nil nil nil "-C" repo "commit" "-m"
                  "Second incident

Incident: 2026-048
Invariant-Ref: req-15
Regression-Test: test_core
Paths: src/payments/core.py
")
    (let ((incidents (code-review-registry--incidents repo)))
      (should (= 2 (length incidents)))
      (should (equal (sort (mapcar (lambda (i) (plist-get i :id)) incidents)
                           #'string<)
                     '("2026-047" "2026-048"))))))

(ert-deftest code-review-registry/file-fallback-when-no-trailers ()
  ;; a repository without trailer commits falls back to the
  ;; incidents/ front matter (byte-capped, entry-capped)
  (let* ((repo (code-review-registry-test--make-repo
                (list (cons "init" '(("README.md" . "nothing\n"))))))
         (dir (expand-file-name code-review-registry-dir repo)))
    (make-directory dir t)
    (with-temp-file (expand-file-name "2026-04-11-export-race.md" dir)
      (insert "---
id: 2026-047
title: Export race
paths: [src/x.py]
---
body
"))
    (let ((incidents (code-review-registry--incidents repo)))
      (should (= 1 (length incidents)))
      (should (equal (plist-get (car incidents) :id) "2026-047"))
      (should (equal (plist-get (car incidents) :paths) '("src/x.py"))))))

;;; Consumption wiring

(ert-deftest code-review-registry/hunk-entry-incident-heat ()
  ;; incidents are permanent review heat: a single incident clears
  ;; the phase 15 delicacy threshold on its own, and the badge
  ;; carries the incident count.  The added line is definition-free
  ;; so the score isolates the incident ingredient.
  (let* ((refs (make-hash-table :test #'equal))
         (hunk (list :ranges "-1,1 +1,1"
                     :old nil
                     :added '((1 . "x = 1"))
                     :deleted nil))
         (entry (code-review-analysis--hunk-entry "a.py" hunk refs nil
                                                  '("2026-047"))))
    (should (equal (plist-get entry :incidents) 1))
    (should (equal (plist-get entry :score) 0.5))
    (should (equal (code-review-analysis--hunk-reasons entry)
                   '("1 incident")))
    (should (equal (code-review-analysis--hunk-badge entry)
                   "  (risk: 1 incident)"))
    ;; two incidents: plural, saturated heat
    (let ((entry2 (code-review-analysis--hunk-entry "a.py" hunk refs nil
                                                    '("2026-047"
                                                      "2026-048"))))
      (should (equal (plist-get entry2 :score) 1.0))
      (should (equal (code-review-analysis--hunk-reasons entry2)
                     '("2 incidents"))))
    ;; no incidents: no reason, no badge, no heat
    (let ((entry3 (code-review-analysis--hunk-entry "a.py" hunk refs nil)))
      (should (equal (plist-get entry3 :incidents) 0))
      (should (equal (plist-get entry3 :score) 0.0))
      (should-not (code-review-analysis--hunk-reasons entry3))
      (should-not (code-review-analysis--hunk-badge entry3)))))

(ert-deftest code-review-registry/dead-never-flags-incident-paths ()
  ;; phase 5: an incident-touched path is never reported possibly
  ;; dead — an unreferenced definition there is heat, not death
  (let* ((refs (make-hash-table :test #'equal))
         (block "diff --git a/Foo.py b/Foo.py
--- a/Foo.py
+++ b/Foo.py
@@ -0,0 +1,1 @@
+class Foo:
")
         (incident-paths (make-hash-table :test #'equal))
         (analyze
          (lambda (paths)
            (nth 1 (code-review-analysis--analyze-one-file
                    nil refs "Foo.py" block paths)))))
    (should (equal (funcall analyze nil) '(("Foo" "Foo.py" 1))))
    (puthash "Foo.py" '("2026-047") incident-paths)
    (should (null (funcall analyze incident-paths)))))

(ert-deftest code-review-registry/file-tag-counts-incidents ()
  ;; phase 22: incident count on file headings, from the
  ;; (REPO . HEAD)-cached registry scan
  (let* ((repo (code-review-registry-test--make-repo
                (list (cons code-review-registry-test--msg
                            '(("src/payments/export.py" . "def export():\n    pass\n"))))))
         (code-review-repo-worktree repo))
    (should (equal (code-review-registry--file-tag
                    "src/payments/export.py")
                   "[1 incident]"))
    (should-not (code-review-registry--file-tag "other.py"))))

(ert-deftest code-review-registry/incident-paths-lookup ()
  ;; the consumers' lookup table: path -> incident ids
  (let* ((inc1 (list :id "2026-047" :paths '("a.py" "b.py")))
         (inc2 (list :id "2026-048" :paths '("b.py")))
         (table (code-review-registry--incident-paths (list inc1 inc2))))
    (should (equal (gethash "a.py" table) '("2026-047")))
    ;; newest first (the scan is newest-first)
    (should (equal (gethash "b.py" table) '("2026-048" "2026-047")))
    (should (null (gethash "c.py" table)))
    (should (null (gethash "c.py"
                           (code-review-registry--incident-paths nil))))))

;;; End-to-end: a local review render of an incident-shaped change

(ert-deftest code-review-registry/local-review-renders-incident-heat ()
  "A local review of working-tree changes touching an incident
path renders the incident count on the file heading, the
incident reason in the hunk delicacy badge, and feeds the
dossier its incidents (the whole phase 22 consumption chain in
one real render, on an isolated db)."
  (let* ((repo (code-review-registry-test--make-repo
                (list (cons code-review-registry-test--msg
                            '(("src/payments/export.py"
                               . "def export():\n    return 1\n")))))))
    ;; working-tree change on the incident path: the review's diff
    (with-temp-file (expand-file-name "src/payments/export.py" repo)
      (insert "def export():\n    return 2\n"))
    (code-review-test--with-db
     (let ((default-directory repo))
       (code-review-review-local-diff 1))
     ;; pump the deferred chain until a render produced the
     ;; incident tag (only the NEW render can write it)
     (let ((deadline (+ (float-time) 15))
           (buf nil))
       (while (and (not buf) (< (float-time) deadline))
         (dolist (b (buffer-list))
           (when (and (string-match-p "Code Review: local:"
                                      (buffer-name b))
                      (with-current-buffer b
                        (goto-char (point-min))
                        (save-excursion
                          (search-forward "[1 incident]" nil t))))
             (setq buf b)))
         (unless buf (sit-for 0.05)))
       (unless buf
         (ert-fail "local review render did not produce the \
incident file tag"))
       (with-current-buffer buf
         ;; file heading: the incident count tag
         (goto-char (point-min))
         (should (search-forward "[1 incident]" nil t))
         ;; hunk heading: the incident reason in the delicacy badge
         (goto-char (point-min))
         (should (search-forward "(risk: 1 incident" nil t))
         ;; the analysis entry carries the incident count
         (let* ((res (code-review-analysis-run))
                (entry (car (plist-get res :hunks))))
           (should (= 1 (plist-get entry :incidents)))
           ;; ...and the dossier lists the touching incident
           (let ((dossier (code-review-dossier--get
                           (plist-get entry :path)
                           (plist-get entry :ranges))))
             (should dossier)
             (should (= 1 (length (plist-get dossier :incidents))))
             (should (equal (plist-get (car (plist-get dossier :incidents))
                                       :id)
                            "2026-047")))))))))
