;;; code-review-lazy-test.el --- ERT tests for the lazy wash -*- lexical-binding: t; -*-

(require 'ert)
(require 'code-review-lazy)
(require 'code-review-test-helpers)

;;; req-2026-11-09-011: the lazy drain must not leave point at the bottom
;; covers: code-review-lazy.el
;; covers-fn: code-review-lazy--expand-step
;; enforcement: test
(ert-deftest code-review-lazy-test/drain-restores-pre-render-point ()
  "WHEN the lazy drain finishes THE SYSTEM SHALL rest point where
the render intended — not at the last expanded block (the bottom
of the PR).  A fresh render opens at `point-min'."
  (code-review-test--with-db
    (with-temp-buffer
      (magit-section-mode)
      (let ((inhibit-read-only t))
        (insert "PR header\n\ndiff body\n"))
      (setq code-review-lazy--epoch (list 'epoch)
            code-review-lazy--queue nil
            code-review-lazy--end-fixups nil
            code-review-comment-cursor-pos nil
            code-review-lazy--pre-render-point 1)
      (code-review-lazy--expand-step (current-buffer) code-review-lazy--epoch)
      ;; pump the deferred queue: the terminal branch runs on the
      ;; first tick when the queue starts empty
      (sit-for 0.2)
      (should (= (point) 1)))))

(ert-deftest code-review-lazy-test/drain-restores-cursor-pos ()
  "WHEN a submit flow captured a cursor position THE SYSTEM SHALL
restore IT over the pre-render point."
  (code-review-test--with-db
    (with-temp-buffer
      (magit-section-mode)
      (let ((inhibit-read-only t))
        (insert "PR header\n\ndiff body\n"))
      (goto-char (point-max))
      (setq code-review-lazy--epoch (list 'epoch)
            code-review-lazy--queue nil
            code-review-lazy--end-fixups nil
            code-review-comment-cursor-pos 3
            code-review-lazy--pre-render-point 1)
      (code-review-lazy--expand-step (current-buffer) code-review-lazy--epoch)
      (sit-for 0.2)
      (should (= (point) 3)))))

(ert-deftest code-review-lazy-test/drain-ignores-stale-epoch ()
  "WHEN the epoch moved on (a newer render took over) THE SYSTEM
SHALL not touch the buffer at all."
  (code-review-test--with-db
    (with-temp-buffer
      (magit-section-mode)
      (let ((inhibit-read-only t))
        (insert "PR header\n\ndiff body\n"))
      (goto-char (point-max))
      (setq code-review-lazy--epoch '(new) code-review-lazy--queue nil)
      (code-review-lazy--expand-step (current-buffer) '(old))
      (sit-for 0.2)
      ;; no drain ran: point untouched at the bottom
      (should (= (point) (point-max))))))

;;; code-review-lazy-test.el ends here
