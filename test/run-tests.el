;;; run-tests.el --- Batch ERT runner for code-review -*- lexical-binding: t; -*-
;;
;; Invoked by `make test' as:
;;   emacs -Q --batch -L . -L test [-L <installed package dirs>...] \
;;     -l test/run-tests.el
;;
;; Loads every test file in this directory and runs all ERT tests.

(let ((default-directory
       (file-name-directory (or load-file-name buffer-file-name))))
  ;; repository root first, so we test the checkout, not an installed copy
  (add-to-list 'load-path (expand-file-name ".."))
  (add-to-list 'load-path default-directory))

;; fail fast if any dependency is missing
(require 'code-review)

;; render tests assert SYNCHRONOUS content: the deferred analysis
;; (placeholder + background child) has its own dedicated test,
;; which binds the defcustom t around its render.
(setq-default code-review-analysis-deferred? nil)

(dolist (file (directory-files
               (file-name-directory (or load-file-name buffer-file-name))
               t "-test\\.el\\'"))
  (load file nil t))

(ert-run-tests-batch-and-exit)
