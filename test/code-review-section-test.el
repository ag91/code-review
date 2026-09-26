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

(defconst code-review-section-test--wash-diff-text
  (concat
   "diff --git a/dbt/macros/mrt.sql b/dbt/macros/mrt.sql\n"
   "index 111..222 100644\n"
   "--- a/dbt/macros/mrt.sql\n"
   "+++ b/dbt/macros/mrt.sql\n"
   "@@ -1,3 +1,4 @@\n"
   " context line\n"
   "-removed line\n"
   "+added line\n"
   "diff --git a/dbt/models/old.sql b/dbt/models/new_v1.sql\n"
   "similarity index 91%\n"
   "rename from dbt/models/old.sql\n"
   "rename to dbt/models/new_v1.sql\n"
   "index c7aeae11f..ee8409930 100644\n"
   "--- a/dbt/models/old.sql\n"
   "+++ b/dbt/models/new_v1.sql\n"
   "@@ -1,5 +1,16 @@\n"
   " {#\n"
   "-   old name\n"
   "+   new name (v1) -- reference oracle\n")
  "Raw diff text with a same-path block and a rename block.")

(ert-deftest code-review-section-test/wash-diff-rename-block ()
  "The wash entry regex must accept rename blocks (two different
paths on the `diff --git' line).  A transcription of magit's
washer once dropped the optional backreference group, so the
pattern matched no `diff --git' line at all: the wash stopped at
the first block and the whole diff landed as raw, uncolored
text."
  (code-review-section-test--with-section-env
    (with-temp-buffer
      (magit-section-mode)
      (let ((inhibit-read-only t))
        (insert code-review-section-test--wash-diff-text)
        (goto-char (point-min))
        (magit-insert-section (code-review--root-section)
          (magit-insert-section (code-review-files-chnged)
            (save-restriction
              (narrow-to-region (point) (point-max))
              (magit-wash-sequence #'code-review-wash-diff)))))
      ;; every block consumed: no raw diff text left behind
      (should (zerop (count-matches "^diff --git "
                                    (point-min) (point-max))))
      ;; both blocks became file sections, rename included
      (let (files (walk nil))
        (setq walk (lambda (sec)
                     (dolist (c (oref sec children))
                       (when (eq (oref c type) 'file)
                         (push (substring-no-properties (oref c value))
                               files))
                       (funcall walk c))))
        (funcall walk magit-root-section)
        (should (equal (sort files #'string-lessp)
                       '("b/dbt/macros/mrt.sql"
                         "b/dbt/models/new_v1.sql"))))
        ;; the rename heading shows the old -> new pair
        (should (save-excursion
                  (goto-char (point-min))
                  (search-forward "old.sql -> " nil t)))
        ;; faces are painted on the rename block's added line
        (should (eq (save-excursion
                      (goto-char (point-min))
                      (search-forward "+   new name" nil t)
                      (get-text-property (line-beginning-position)
                                         'font-lock-face))
                    'magit-diff-added)))))

;;; Phase 15: delicate hunk badge, hunk key, cycling

(ert-deftest code-review-section-test/wash-hunk-delicate-badge-and-key ()
  "The wash paints the risk badge on a delicate hunk's heading and
records the raw ranges text in the section value (the hunk key the
jump list and `C-c C-d' cycle by).  Hunks with no entry get no
badge.  The analysis run is faked: the wash only READS its result
(cache hit at wash time by section-hook ordering)."
  (code-review-section-test--with-section-env
    (let ((orig (symbol-function 'code-review-analysis-run)))
      (fset 'code-review-analysis-run
            (lambda ()
              (list :hunks
                    (list (list :path "dbt/macros/mrt.sql"
                                :ranges "-1,3 +1,4"
                                :score 1.0
                                :callers 40
                                :dead '("deadfn")
                                :median-age 1500
                                :authors 1
                                :cplx 5)))))
      (unwind-protect
          (with-temp-buffer
            (magit-section-mode)
            (let ((inhibit-read-only t))
              (insert code-review-section-test--wash-diff-text)
              (goto-char (point-min))
              (magit-insert-section (code-review--root-section)
                (magit-insert-section (code-review-files-chnged)
                  (save-restriction
                    (narrow-to-region (point) (point-max))
                    (magit-wash-sequence #'code-review-wash-diff)))))
            ;; the badge rides the delicate hunk's heading line
            (should (save-excursion
                      (goto-char (point-min))
                      (search-forward
                       "@@ -1,3 +1,4 @@  (risk: 40 callers; lines 4y old; +5 branches; 1 dead def)"
                       nil t)))
            ;; the untracked-by-analysis hunk (rename block) is bare:
            ;; no badge text right after its heading
            (should-not (save-excursion
                          (goto-char (point-min))
                          (search-forward "@@ -1,5 +1,16 @@" nil t)
                          (looking-back "(risk:" (line-beginning-position))))
            ;; the hunk key rides the section value
            (should (code-review-section--find-hunk-section
                     "dbt/macros/mrt.sql" "-1,3 +1,4"))
            (should-not (code-review-section--find-hunk-section
                         "dbt/macros/mrt.sql" "-1,5 +1,16"))
            ;; C-c C-d cycles to the delicate hunk, wrapping from the
            ;; end of the buffer back to it.  NB: the @@ ranges text
            ;; in a REGEXP needs the `+' escaped ("+1,4" is a
            ;; quantifier otherwise).
            (goto-char (point-min))
            (code-review-next-delicate-hunk)
            (should (looking-at "^@@ -1,3 \\+1,4"))
            (goto-char (point-max))
            (code-review-next-delicate-hunk)
            (should (looking-at "^@@ -1,3 \\+1,4"))
            ;; from inside the hunk: wraps around to itself (only
            ;; target in the list)
            (code-review-next-delicate-hunk)
            (should (looking-at "^@@ -1,3 \\+1,4")))
        (fset 'code-review-analysis-run orig)))))

;;; code-review-section-test.el ends here
