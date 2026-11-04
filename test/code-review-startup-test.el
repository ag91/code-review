;;; code-review-startup-test.el --- Fresh package startup tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'dash)

;;; req-2026-11-03-006: PR-links-open-in-code-review-after-startup
;; covers: code-review-browse.el
;; covers-fn: code-review-browse-url-install
;; enforcement: test
(ert-deftest code-review-startup/req-2026-11-03-006-link-handler-load-order ()
  "WHEN Emacs starts THE SYSTEM SHALL route PR links to code review regardless of Forge load order."
  (let* ((autoload-file (locate-library "code-review-autoloads"))
         (executable (expand-file-name invocation-name invocation-directory))
         (paths (-mapcat (lambda (dir) (list "-L" dir))
                         (-filter #'stringp load-path))))
    (--each '(nil browse-url forge)
      (let* ((program
              `(progn
                 ,@(when it `((require ',it)))
                 (load ,autoload-file nil t)
                 (require 'browse-url)
                 (require 'forge)
                 (unless (eq (cdar browse-url-default-handlers) 'code-review-browse-url)
                   (error "PR handler lost precedence"))))
             (args (append
                    '("-Q" "--batch" "--eval" "(setq native-comp-jit-compilation nil)")
                    paths (list "--eval" (prin1-to-string program)))))
        (with-temp-buffer
          (let ((status (apply #'call-process executable nil t nil args)))
            (unless (eq status 0)
              (ert-fail (buffer-string)))))))))

;;; code-review-startup-test.el ends here
