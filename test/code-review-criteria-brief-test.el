;;; code-review-criteria-brief-test.el --- phase 27 station 1  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Andrea <andrea-dev@hotmail.com>

;; This file is part of code-review.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'code-review-criteria)
(require 'code-review-criteria-brief)
(require 'code-review-agents-shell)

;;; req-2026-10-12-001: criteria-priorities
;; covers: code-review-criteria-brief.el, code-review-section-criteria.el
;; enforcement: test
(ert-deftest code-review-criteria-brief-test/req-2026-10-12-001-priority-cycle ()
  "WHEN the reviewer edits the brief THE SYSTEM SHALL keep each
criterion's P1..P3 priority in the criteria file's front matter
(durable, re-prioritizable in place) and WHEN the P1 list exceeds the
configured budget THE SYSTEM SHALL flag the change as under-specified.

The priority cycle: unset -> P1 -> P2 -> P3 -> unset.  Pure."
  (should (equal (code-review-criteria-brief--next nil) "P1"))
  (should (equal (code-review-criteria-brief--next "P1") "P2"))
  (should (equal (code-review-criteria-brief--next "P2") "P3"))
  (should (null (code-review-criteria-brief--next "P3"))))

(ert-deftest code-review-criteria-brief-test/req-2026-10-12-001-front-matter-rewrite ()
  "The priority rewrite touches ONLY the front matter: a file
with no front matter gains the minimal block, an existing block
gains or replaces the priority line, nil removes it, and the
body is never touched."
  (let ((body "WHEN x THE SYSTEM SHALL y.\n"))
    ;; no front matter: gains the block
    (should (string-prefix-p "---\npriority: P1\n---\n\nWHEN x"
                            (code-review-criteria-brief--rewrite
                             body "P1")))
    ;; existing front matter without priority: the line is added,
    ;; the body untouched
    (let ((fm "---\npaths: [src/a.el]\n---\n\nWHEN x THE SYSTEM SHALL y.\n"))
      (let ((out (code-review-criteria-brief--rewrite fm "P2")))
        (should (string-match-p "^priority: P2$" out))
        (should (string-match-p "^paths: \\[src/a.el\\]$" out))
        (should (string-suffix-p "WHEN x THE SYSTEM SHALL y.\n" out))
        ;; the priority line belongs INSIDE the block: one appended
        ;; after the closing --- is invisible to the parser and
        ;; re-appends on every cycle (the 2026-10-13 spam bug)
        (should (string-match-p "^priority: P2\n---" out)))
      ;; replace an existing priority
      (let ((out (code-review-criteria-brief--rewrite
                  "---\npaths: [src/a.el]\npriority: P3\n---\n\nWHEN x\n"
                  "P1")))
        (should (string-match-p "^priority: P1$" out))
        (should-not (string-match-p "P3" out)))
      ;; nil removes the line, keeps the rest
      (let ((out (code-review-criteria-brief--rewrite
                  "---\npaths: [src/a.el]\npriority: P1\n---\n\nWHEN x\n"
                  nil)))
        (should-not (string-match-p "priority:" out))
        (should (string-match-p "^paths: \\[src/a.el\\]$" out))))))

(ert-deftest code-review-criteria-brief-test/req-2026-10-12-001-set-writes-file ()
  "`--set' writes the file (durable) and returns the updated
plist; a missing file leaves everything alone (never signals)."
  (let* ((dir (make-temp-file "cr-brief-" t))
         (file (expand-file-name "specs/2026-10-12-090-x.md" dir))
         (req (list :id "2026-10-12-090" :file file
                    :sentence "WHEN x THE SYSTEM SHALL y."
                    :paths '("src/a.el") :priority nil
                    :text "---\npaths: [src/a.el]\n---\n\nWHEN x THE SYSTEM SHALL y.\n")))
    (make-directory (expand-file-name "specs" dir) t)
    (with-temp-file file (insert (plist-get req :text)))
    (let ((out (code-review-criteria-brief-set req "P1")))
      (should (equal (plist-get out :priority) "P1"))
      (with-temp-buffer
        (insert-file-contents file)
        (should (string-match-p "^priority: P1$"
                                (buffer-substring-no-properties
                                 (point-min) (point-max))))))
    ;; a deleted criterion file: no signal, nil
    (delete-file file)
    (should (null (code-review-criteria-brief-set req "P2")))))

(ert-deftest code-review-criteria-brief-test/req-2026-10-12-001-priority-parsed-from-front-matter ()
  "The engine carries the front-matter P1..P3 into the
requirement plist (and drops invalid values)."
  (should (equal (plist-get (code-review-criteria--requirement-from-text
                             "2026-10-12-091-x.md"
                             "---\npaths: [a]\npriority: P1\n---\n\nWHEN x THE SYSTEM SHALL y.\n")
                            :priority)
                 "P1"))
  (should (null (plist-get (code-review-criteria--requirement-from-text
                            "2026-10-12-091-x.md"
                            "---\npaths: [a]\npriority: P9\n---\n\nWHEN x THE SYSTEM SHALL y.\n")
                           :priority))))

;;; The state machine + the budget

(ert-deftest code-review-criteria-brief-test/req-2026-10-12-001-verdict-count-and-next-action ()
  "The verdict counter and the one-next-action stations."
  (let ((states (make-hash-table :test #'equal)))
    (puthash "a" 'satisfied states)
    (puthash "b" 'pending states)
    (should (= 1 (code-review-criteria-brief--verdicts
                  states '("a" "b" "c"))))
    (should (= 0 (code-review-criteria-brief--verdicts nil '("a"))))
    ;; station 1: no ledger
    (should (string-match-p
             "station 1"
             (let ((code-review-criteria-grill--run? nil))
               (code-review-criteria-brief--next-action 0 0))))
    ;; station 3: ledger, no verdicts
    (should (string-match-p
             "station 3"
             (let ((code-review-criteria-grill--run? nil))
               (code-review-criteria-brief--next-action 5 0))))
    ;; station 4: verdicts recorded, grill not run
    (should (string-match-p
             "station 4"
             (let ((code-review-criteria-grill--run? nil))
               (code-review-criteria-brief--next-action 5 2))))
    ;; loop complete
    (should (string-match-p
             "loop complete"
             (let ((code-review-criteria-grill--run? t))
               (code-review-criteria-brief--next-action 5 2))))))

;;; The reference drafter prompts (pure)

(ert-deftest code-review-criteria-brief-test/req-2026-10-12-001-brief-prompt-contract ()
  "The brief prompt asks the agent to draft the criteria as GUARD
TESTS in the worktree's test suite — the requirement id in the
test name, the EARS sentence in the docstring, the machine keys in
the comment block above the test — never specs files, with the
draft-not-push rule intact."
  (let ((prompt (code-review-agents-shell-brief-prompt
                 "Add export" "Adds the export button."
                 '("src/export.el" "src/ui.el") "/wt/")))
    (should (string-match-p "/wt/" prompt))
    ;; the criteria ARE tests, keyed by the machine-key block
    (should (string-match-p "ert-deftest" prompt))
    (should (string-match-p "req-YYYY-MM-DD-NNN" prompt))
    (should (string-match-p "WHEN .* THE SYSTEM SHALL" prompt))
    (should (string-match-p "covers:" prompt))
    (should (string-match-p "covers-fn:" prompt))
    (should (string-match-p "enforcement: test" prompt))
    ;; the old specs-file shape is explicitly forbidden
    (should (string-match-p "Do NOT write specs" prompt))
    ;; ranking stays reviewer state (in the reply, not the tests)
    (should (string-match-p "rank them in your reply" prompt))
    (should (string-match-p "no[[:space:]\n]+interface" prompt))
    (should (string-match-p "Do NOT[[:space:]\n]+commit" prompt))
    (should (string-match-p "uncommitted" prompt))
    (should (string-match-p "src/export.el" prompt))
    (should (string-match-p "Add export" prompt))))

(provide 'code-review-criteria-brief-test)
;;; code-review-criteria-brief-test.el ends here
