;;; code-review-db-test.el --- Test our utility functions
;;; Commentary:
;;; Code:

(require 'a)
(require 'dash)
(require 'uuidgen)
(require 'buttercup)
(require 'code-review-db)
(require 'code-review-gitlab)
(require 'code-review-github)

(defconst sample-pr-obj
  (code-review-github-repo
   :owner "owner"
   :repo "repo"
   :number "num"))

(defconst random-test-db
  (format "/tmp/code-review-test-db-%s.sqlite" (uuidgen-4)))

(describe "pullreq"
  :var (code-review-database-file
        code-review--db-connection)
  (before-all
    (setf code-review-database-file random-test-db
          code-review--db-connection nil))

  (it "we should be able to create a pullreq db obj from a pr-alist"
    (code-review-db--pullreq-create sample-pr-obj)
    (let ((pr (code-review-db-get-pullreq)))
      (expect (oref pr id) :to-be-truthy)
      (expect (oref pr repo) :to-equal "repo")
      (expect (oref pr owner) :to-equal "owner")
      (expect (oref pr number) :to-equal "num")))

  (it "from the pullreq db obj we can get back the original fields"
    (let ((pr (code-review-db-get-pullreq)))
      (expect (oref pr owner) :to-equal "owner")
      (expect (oref pr repo) :to-equal "repo")
      (expect (oref pr number) :to-equal "num")))

  (it "update the sha value of a pullreq"
    (code-review-db--pullreq-sha-update "SHA")
    (expect (oref (code-review-db-get-pullreq) sha)
            :to-equal "SHA"))

  (it "update the value of current path should create a buffer with paths"
    (code-review-db--curr-path-update "github.el")
    (let* ((paths (oref (code-review-db-get-buffer) paths))
           (path (-first-item paths)))
      (expect (oref path name)
              :to-equal "github.el")
      (expect (oref path head-pos)
              :to-be nil)
      (expect (oref path at-pos-p)
              :to-be t)))

  (it "update the value of current path should disable `at-pos-p' of previous paths and append new one"
    (code-review-db--curr-path-update "github.el")
    (code-review-db--curr-path-update "gitlab.el")
    (let* ((paths (oref (code-review-db-get-buffer) paths)))
      (-map
       (lambda (path)
         (cond
          ((string-equal (oref path name) "github.el")
           (expect (oref path at-pos-p) :to-be nil))

          ((string-equal (oref path name) "gitlab.el")
           (expect (oref path at-pos-p) :to-be t))

          (t
           (throw "Test error" ""))))
       paths))))

(describe "path"
  :var (code-review-database-file
        code-review--db-connection)
  (before-all
    (setf code-review-database-file random-test-db
          code-review--db-connection nil)
    (code-review-db--pullreq-create sample-pr-obj))

  (it "update path head-pos value"
    (code-review-db--curr-path-update "github.el")
    (code-review-db--curr-path-head-pos-update "github.el" 42)
    (let* ((paths (oref (code-review-db-get-buffer) paths)))
      (dolist (p paths)
        (when (string-equal (oref p name) "github.el")
          (expect (oref p head-pos) :to-equal 42))))))

(provide 'code-review-db-test)
;;; code-review-db-test.el ends here
