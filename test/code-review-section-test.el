;;; code-review-section-test.el --- ERT tests for section functions -*- lexical-binding: t; -*-

(require 'ert)
(require 'a)
(require 'code-review-db)
(require 'code-review-github)
(require 'code-review-section)
(require 'code-review-test-helpers)

(defun code-review-section-test--sample-pr-obj ()
  "Return a fresh sample pullreq object.
Must be rebuilt per test: closql writes through `oset', so a shared
object would accumulate ids and a stale database pointer."
  (code-review-github-repo
   :owner "owner"
   :repo "repo"
   :number "num"))

(defmacro code-review-section-test--with-section-env (&rest body)
  "Run BODY with a fresh test db and deterministic section variables."
  (declare (indent 0))
  `(let ((code-review-section-indent-width 2)
         (code-review-section-image-scaling 0.8)
         (code-review-fill-column 70))
     (code-review-test--with-db
       (code-review-db--pullreq-create (code-review-section-test--sample-pr-obj))
       ,@body)))

(ert-deftest code-review-section-test/title ()
  "Available in raw-infos should be added."
  (code-review-section-test--with-section-env
    (code-review-db--pullreq-raw-infos-update `((title . "My title")))
    (code-review-test--sections-match
     (lambda () (code-review-section-insert-title))
     `(((type . code-review-title-section)
        (value . "My title"))))))

(ert-deftest code-review-section-test/title-missing ()
  "Missing title: should not break and not add anything to the buffer."
  (code-review-section-test--with-section-env
    (code-review-db--pullreq-raw-infos-update nil)
    (code-review-test--sections-match
     (lambda () (code-review-section-insert-title))
     nil t)))

(ert-deftest code-review-section-test/state ()
  "Available raw-infos state should be added to the buffer."
  (code-review-section-test--with-section-env
    (code-review-db--pullreq-raw-infos-update `((state . "OPEN")))
    (code-review-test--sections-match
     (lambda () (code-review-section-insert-state))
     `(((type . code-review-state-section)
        (value . "OPEN"))))))

(ert-deftest code-review-section-test/milestone ()
  "Available raw-infos milestone should be added to the buffer."
  (code-review-section-test--with-section-env
    (let ((obj (code-review-milestone-section :title "Milestone Title" :perc 50)))
      (code-review-db--pullreq-raw-infos-update
       `((milestone (title . "Milestone Title")
                    (progressPercentage . 50))))
      (code-review-test--sections-match
       (lambda () (code-review-section-insert-milestone))
       `(((type . code-review-milestone-section)
          (value . ,obj))))
      (should (equal (code-review-pretty-milestone obj)
                     "Milestone Title (50.00%)")))))

(ert-deftest code-review-section-test/milestone-missing-title ()
  "If milestone title is missing, add default msg."
  (code-review-section-test--with-section-env
    (let ((obj (code-review-milestone-section :title nil :perc "50")))
      (code-review-db--pullreq-raw-infos-update
       `((milestone (title . nil) (progressPercentage . "50"))))
      (code-review-test--sections-match
       (lambda () (code-review-section-insert-milestone))
       `(((type . code-review-milestone-section)
          (value . ,obj))))
      (should (equal (code-review-pretty-milestone obj)
                     "No milestone")))))

(ert-deftest code-review-section-test/milestone-missing-progress ()
  "If milestone progress is missing, leave it out."
  (code-review-section-test--with-section-env
    (let ((obj (code-review-milestone-section :title "My title" :perc nil)))
      (code-review-db--pullreq-raw-infos-update `((milestone (title . "My title"))))
      (code-review-test--sections-match
       (lambda () (code-review-section-insert-milestone))
       `(((type . code-review-milestone-section)
          (value . ,obj))))
      (should (equal (code-review-pretty-milestone obj)
                     "My title")))))

(ert-deftest code-review-section-test/top-level-comments ()
  "Inserting general comments in the buffer."
  (code-review-section-test--with-section-env
    (let ((obj (code-review-comment-section
                :author "Code Review"
                :msg "Comment 1"
                :id 1234
                :reactions nil)))
      (code-review-db--pullreq-raw-infos-update
       `((comments (nodes ((author (login . "Code Review"))
                           (bodyHTML . "<p>Comment 1</p>")
                           (databaseId . 1234)
                           (createdAt . "2021-11-08T00:24:09Z"))))))
      (code-review-test--sections-match
       (lambda () (code-review-section-insert-top-level-comments))
       `(((type . code-review-comment-header-section))
         ((type . code-review-comment-section)
          (value . ,obj)))))))

;;; code-review-section-test.el ends here
