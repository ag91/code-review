;;; code-review-budget-focus-test.el --- ERT tests for the focus tracker -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'a)
(require 'code-review-db)
(require 'code-review-github)
(require 'code-review-budget)
(require 'code-review-budget-focus)
(require 'code-review-test-helpers)

;;; The focus tracker (phase 25b follow-up): measured minutes replace
;;; the booking

(ert-deftest code-review-budget-focus/req-2026-10-30-004-measured-booking ()
  "WHEN a review buffer holds the selected window and the
reviewer submits the review THE SYSTEM SHALL book the MEASURED
focus minutes for that PR instead of the diff-shape estimate,
falling back to the estimate only for rows the tracker never
saw."
  (let ((code-review-budget-state-file (make-temp-file "cr-focus-"))
        (code-review-history-enabled nil))
    (code-review-test--with-db
      ;; two finished rows, same diff (estimate 2.8): one tracked, one not
      (let ((tracked (code-review-budget-test--make-pr
                      "o" "r" "1" code-review-budget-test--diff "T"))
            (untracked (code-review-budget-test--make-pr
                        "o" "r" "2" code-review-budget-test--diff "U")))
        (dolist (pr (list tracked untracked))
          (oset pr finished t)
          (oset pr finished-at (current-time-string))
          (code-review-db-update pr))
        ;; 110 focused seconds on o/r#1, ticked in two intervals
        ;; inside the idle threshold
        (let ((code-review-budget-focus--last nil)
              (code-review-budget-focus--accrued nil)
              (code-review-budget-focus--last-flush 0.0))
          (code-review-budget-focus--note 1030.0 "o/r#1")
          (code-review-budget-focus--note 1090.0 "o/r#1") ; +60s
          (code-review-budget-focus--note 1140.0 "o/r#1") ; +50s
          ;; a gap over the threshold is a walk-away: discarded
          (code-review-budget-focus--note 1500.0 "o/r#1")
          (code-review-budget-focus--flush)
          ;; the state file carries the measured seconds: 110s
          (let ((measured (code-review-budget-focus-minutes-for "o/r#1")))
            (should (< (- measured (/ 110.0 60.0)) 0.001))
            (should (null (code-review-budget-focus-minutes-for "o/r#2"))))
          ;; the booking: measured replaces the estimate for the
          ;; tracked row, the untracked row falls back to 2.8
          (let ((spent (code-review-budget--spent-today)))
            (should (< (- spent (+ (/ 110.0 60.0) 2.8)) 0.01))))))))

(ert-deftest code-review-budget-focus/req-2026-10-30-004-idle-and-non-review ()
  "WHEN focus leaves the review buffer for longer than the idle
threshold, or the buffer is not a code-review buffer at all, THE
SYSTEM SHALL count no focus time."
  (let ((code-review-budget-state-file (make-temp-file "cr-focus2-")))
    ;; a long gap between two ticks of the same pr: discarded
    (setq code-review-budget-focus--last nil
          code-review-budget-focus--accrued nil
          code-review-budget-focus--last-flush 0.0)
    (setq code-review-budget-focus--last (list :time 100.0 :pr "o/r#9"))
    (code-review-budget-focus--note 300.0 "o/r#9")
    (should (null code-review-budget-focus--accrued))
    ;; a non-review buffer is unattributable
    (with-temp-buffer
      (should (null (code-review-budget-focus--pr-of-buffer
                     (current-buffer)))))
    ;; a code-review-mode buffer with a pr id is attributable
    (let ((buf (generate-new-buffer " *cr-focus-test*")))
      (unwind-protect
          (progn
            (with-current-buffer buf
              (delay-mode-hooks (code-review-mode))
              (setq code-review-review-buffer-pr-id "o/r#1"))
            (should (equal (code-review-budget-focus--pr-of-buffer buf)
                           "o/r#1")))
        (kill-buffer buf)))))

;;; code-review-budget-focus-test.el ends here
