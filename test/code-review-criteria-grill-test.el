;;; code-review-criteria-grill-test.el --- phase 27 station 4  -*- lexical-binding: t; -*-

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
(require 'code-review-criteria-grill)
(require 'code-review-agents-shell)

(defun code-review-criteria-grill-test--req (id prio sentence)
  (list :id id :priority prio :sentence sentence
        :file (format "specs/%s-x.md" id) :paths '("src/a.el") :text "T"))

;;; req-2026-10-12-002: the-grill
;; covers: code-review-criteria-grill.el
;; enforcement: test
(ert-deftest code-review-criteria-grill-test/req-2026-10-12-002-vague-p ()
  "WHEN verdicts are recorded and the reviewer runs the grill THE SYSTEM
SHALL report ledger suggestions awaiting disposition —
satisfied-but-contradicted criteria, unfalsifiable wording,
budget-carrying criteria nothing covers — in the review buffer, never
as review comments.

Vague = no SHALL clause or a vague qualifier in the sentence."
  (should (code-review-criteria-grill--vague-p
           "WHEN x the system handles errors appropriately."))
  (should (code-review-criteria-grill--vague-p
           "WHEN x THE SYSTEM handles it properly."))
  (should-not (code-review-criteria-grill--vague-p
               "WHEN x THE SYSTEM SHALL reject the payload.")))

(ert-deftest code-review-criteria-grill-test/req-2026-10-12-002-findings ()
  "The mechanical findings: a [x] whose covered paths the diff
does not touch is a DISAGREEMENT; a P1 nothing covers is flagged
even when pending; unfalsifiable wording is flagged; a clean
satisfied criterion with coverage is silent."
  (let* ((matching
          (list `(:req ,(code-review-criteria-grill-test--req
                         "id-a" "P1" "WHEN a THE SYSTEM SHALL do a.")
                     :paths ("src/a.el"))
                `(:req ,(code-review-criteria-grill-test--req
                         "id-b" "P2" "WHEN b the system works properly.")
                     :paths ("src/b.el"))
                `(:req ,(code-review-criteria-grill-test--req
                         "id-c" "P2" "WHEN c THE SYSTEM SHALL do c.")
                     :paths ("src/c.el"))))
         (states (make-hash-table :test #'equal)))
    (puthash "id-a" 'satisfied states)
    (puthash "id-c" 'satisfied states)
    (let ((fs (code-review-criteria-grill--findings
               matching states '("src/other.el"))))
      ;; id-a: satisfied AND P1 but nothing covered -> both
      ;; disagreement and uncovered-p1; id-b: vague; id-c: ALSO a
      ;; disagreement (marked [x], its covered path untouched)
      (should (= 4 (length fs)))
      (should (cl-some (lambda (f)
                         (and (eq (plist-get f :kind) 'disagreement)
                              (equal (plist-get f :id) "id-a")))
                       fs))
      (should (cl-some (lambda (f)
                         (and (eq (plist-get f :kind) 'uncovered-p1)
                              (equal (plist-get f :id) "id-a")))
                       fs))
      (should (cl-some (lambda (f)
                         (and (eq (plist-get f :kind) 'vague)
                              (equal (plist-get f :id) "id-b")))
                       fs))
      (should (cl-some (lambda (f)
                         (and (eq (plist-get f :kind) 'disagreement)
                              (equal (plist-get f :id) "id-c")))
                       fs)))
    ;; with coverage, the satisfied criteria are silent
    (let ((fs (code-review-criteria-grill--findings
               matching states '("src/a.el" "src/b.el" "src/c.el"))))
      (should (= 1 (length fs)))
      ;; id-b's vague wording still flags even with coverage (it is
      ;; unfalsifiable, not dispositioned); id-a and id-c are clean
      (should (eq (plist-get (car fs) :kind) 'vague)))))

(ert-deftest code-review-criteria-grill-test/req-2026-10-12-002-grill-finds-nothing-clean-ledger ()
  "A fully dispositioned, covered, falsifiable ledger: zero
findings (the grill must not manufacture signal)."
  (let* ((matching
          (list `(:req ,(code-review-criteria-grill-test--req
                         "id-a" "P2" "WHEN a THE SYSTEM SHALL do a.")
                     :paths ("src/a.el"))))
         (states (make-hash-table :test #'equal)))
    (puthash "id-a" 'satisfied states)
    (should (null (code-review-criteria-grill--findings
                   matching states '("src/a.el"))))))

;;; req-2026-10-12-003: review-by-execution
;; covers: code-review-criteria-grill.el
;; enforcement: test
(ert-deftest code-review-criteria-grill-test/req-2026-10-12-003-probe-interpretation ()
  "WHEN a contested criterion converts to an executable probe THE SYSTEM
SHALL run it at HEAD and at the BASE rev on the bounded fake-fix
substrate and interpret the two outcomes (met / contradicts — flip the
verdict or emit / probe does not detect).

The two-outcome reading: HEAD pass + BASE fail = met; HEAD
fail = the code contradicts the criterion (flip or emit); BASE
pass = a probe that detects nothing."
  (should (string-match-p
           "MET"
           (code-review-criteria-grill--probe-interpret 'pass 'fail)))
  (should (string-match-p
           "CONTRADICTS"
           (code-review-criteria-grill--probe-interpret 'fail 'fail)))
  (should (string-match-p
           "does not DETECT"
           (code-review-criteria-grill--probe-interpret 'pass 'pass))))

;;; The reference drafter prompts (pure)

(ert-deftest code-review-criteria-grill-test/req-2026-10-12-002-grill-prompt-contract ()
  "The grill prompt lists the criteria and the mechanical
findings, and demands ledger EDITS — never file edits, never a
commit (the reviewer dispositions by hand)."
  (let* ((reqs (list (code-review-criteria-grill-test--req
                      "id-a" "P1" "WHEN a THE SYSTEM SHALL do a.")))
         (prompt (code-review-agents-shell-grill-prompt
                  (list (list :kind 'disagreement :id "id-a"
                              :evidence "marked [x] but uncovered"))
                  reqs "/wt/")))
    (should (string-match-p "id-a" prompt))
    (should (string-match-p "WHEN a THE SYSTEM SHALL do a." prompt))
    (should (string-match-p "marked \\[x\\] but uncovered" prompt))
    (should (string-match-p "do NOT edit any file" prompt))))

(ert-deftest code-review-criteria-grill-test/req-2026-10-12-003-probe-prompt-contract ()
  "The probe prompt pins the probe file path, the
fails-when-violated contract, and the draft-not-push rule."
  (let ((prompt (code-review-agents-shell-probe-prompt
                 (code-review-criteria-grill-test--req
                  "2026-10-12-050" "P1" "WHEN p THE SYSTEM SHALL q.")
                 "/wt/")))
    (should (string-match-p "test/2026-10-12-050-probe-test.el" prompt))
    (should (string-match-p "WHEN p THE SYSTEM SHALL q." prompt))
    (should (string-match-p "FAIL when the criterion" prompt))
    (should (string-match-p "Do NOT commit" prompt))))

(provide 'code-review-criteria-grill-test)
;;; code-review-criteria-grill-test.el ends here
