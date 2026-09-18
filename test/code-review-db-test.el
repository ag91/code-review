;;; code-review-db-test.el --- ERT tests for the code-review database -*- lexical-binding: t; -*-

(require 'ert)
(require 'eieio)
(require 'dash)
(require 'a)
(require 'code-review-db)
(require 'code-review-github)
(require 'code-review-test-helpers)

(defun code-review-db-test--sample-pr-obj ()
  "Return a fresh sample pullreq object.
Must be rebuilt per test: closql writes through `oset', so a shared
object would accumulate ids and a stale database pointer."
  (code-review-github-repo
   :owner "owner"
   :repo "repo"
   :number "num"))

(ert-deftest code-review-db-test/create-pullreq ()
  "We should be able to create a pullreq db obj from a pr-obj."
  (code-review-test--with-db
    (code-review-db--pullreq-create (code-review-db-test--sample-pr-obj))
    (let ((pr (code-review-db-get-pullreq)))
      (should (oref pr id))
      (should (equal (oref pr repo) "repo"))
      (should (equal (oref pr owner) "owner"))
      (should (equal (oref pr number) "num")))))

(ert-deftest code-review-db-test/get-back-original-fields ()
  "From the pullreq db obj we can get back the original fields."
  (code-review-test--with-db
    (code-review-db--pullreq-create (code-review-db-test--sample-pr-obj))
    (let ((pr (code-review-db-get-pullreq)))
      (should (equal (oref pr owner) "owner"))
      (should (equal (oref pr repo) "repo"))
      (should (equal (oref pr number) "num")))))

(ert-deftest code-review-db-test/pullreq-sha-update ()
  "Update the sha value of a pullreq."
  (code-review-test--with-db
    (code-review-db--pullreq-create (code-review-db-test--sample-pr-obj))
    (code-review-db--pullreq-sha-update "SHA")
    (should (equal (oref (code-review-db-get-pullreq) sha)
                   "SHA"))))

(ert-deftest code-review-db-test/curr-path-update-creates-buffer-with-paths ()
  "Updating the current path should create a buffer with paths."
  (code-review-test--with-db
    (code-review-db--pullreq-create (code-review-db-test--sample-pr-obj))
    (code-review-db--curr-path-update "github.el")
    (let* ((paths (oref (code-review-db-get-buffer) paths))
           (path (-first-item paths)))
      (should (equal (oref path name) "github.el"))
      (should (null (oref path head-pos)))
      (should (oref path at-pos-p)))))

(ert-deftest code-review-db-test/curr-path-update-disables-previous-at-pos-p ()
  "Updating current path should disable `at-pos-p' of previous paths."
  (code-review-test--with-db
    (code-review-db--pullreq-create (code-review-db-test--sample-pr-obj))
    (code-review-db--curr-path-update "github.el")
    (code-review-db--curr-path-update "gitlab.el")
    (let* ((paths (oref (code-review-db-get-buffer) paths)))
      (dolist (path paths)
        (cond
         ((string-equal (oref path name) "github.el")
          (should (null (oref path at-pos-p))))
         ((string-equal (oref path name) "gitlab.el")
          (should (oref path at-pos-p))))))))

(ert-deftest code-review-db-test/curr-path-head-pos-update ()
  "Update path head-pos value."
  (code-review-test--with-db
    (code-review-db--pullreq-create (code-review-db-test--sample-pr-obj))
    (code-review-db--curr-path-update "github.el")
    (code-review-db--curr-path-head-pos-update "github.el" 42)
    (let* ((paths (oref (code-review-db-get-buffer) paths)))
      (dolist (p paths)
        (when (string-equal (oref p name) "github.el")
          (should (equal (oref p head-pos) 42)))))))

;;; code-review-db-test.el ends here
