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

;;; Characterization tests: header inserters (refactor safety net)
;;;
;;; They pin the rendered TEXT and the section tree of each inserter
;;; so a behavior-preserving refactor of code-review-section.el can
;;; be verified mechanically.

(defun code-review-section-test--section-types ()
  "Collect the types of all sections under `magit-root-section'."
  (let (types)
    (let ((walk nil))
      (setq walk (lambda (sec)
                   (dolist (c (oref sec children))
                     (push (oref c type) types)
                     (funcall walk c))))
      (funcall walk magit-root-section))
    types))

(ert-deftest code-review-section-test/author ()
  "Author line renders the login."
  (code-review-section-test--with-section-env
    (code-review-db--pullreq-raw-infos-update
     `((author (login . "octocat")
               (url . "https://example.com/octocat"))))
    (code-review-test--sections-match
     (lambda () (code-review-section-insert-author))
     `(((type . code-review-author-section)
        (value . ,(code-review-author-section
                   :login "octocat"
                   :url "https://example.com/octocat")))))))

(ert-deftest code-review-section-test/author-missing ()
  "Missing author login: nothing inserted."
  (code-review-section-test--with-section-env
    (code-review-db--pullreq-raw-infos-update nil)
    (code-review-test--sections-match
     (lambda () (code-review-section-insert-author))
     nil t)))

(ert-deftest code-review-section-test/is-draft ()
  "Draft flag renders true/false."
  (code-review-section-test--with-section-env
    (code-review-db--pullreq-raw-infos-update `((isDraft . t)))
    (with-temp-buffer
      (let ((inhibit-read-only t))
        (code-review-section-insert-is-draft))
      (should (string-match-p "Draft: +true" (buffer-string))))
    (code-review-db--pullreq-raw-infos-update `((isDraft)))
    (with-temp-buffer
      (let ((inhibit-read-only t))
        (code-review-section-insert-is-draft))
      (should (string-match-p "Draft: +false" (buffer-string)))))

(ert-deftest code-review-section-test/labels ()
  "Labels render each name; missing labels show the placeholder."
  (code-review-section-test--with-section-env
    (let ((pr (code-review-db-get-pullreq)))
      (oset pr labels (list '((name . "bug") (color . "ff0000"))))
      (closql-insert (code-review-db) pr t))
    (with-temp-buffer
      (let ((inhibit-read-only t))
        (magit-insert-section (code-review--root-section)
          (code-review-section-insert-labels)))
      (should (string-match-p "Labels: +bug" (buffer-string)))
      (should (= 1 (length (overlays-in (point-min) (point-max))))))
    ;; no labels at all: the dimmed placeholder
    (with-temp-buffer
      (let ((inhibit-read-only t))
        (code-review-section-insert-labels))
      (should (string-match-p "Labels: +None yet" (buffer-string))))))

(ert-deftest code-review-section-test/assignees-none ()
  "No assignees: the assign-yourself placeholder."
  (code-review-section-test--with-section-env
    (with-temp-buffer
      (let ((inhibit-read-only t))
        (code-review-section-insert-assignee))
      (should (string-match-p
               "Assignees: +No one — Assign yourself"
               (buffer-string))))))

(ert-deftest code-review-section-test/assignees ()
  "Assignees render the set-new-assignee button and each name."
  (code-review-section-test--with-section-env
    (let ((pr (code-review-db-get-pullreq)))
      (oset pr assignees
            (list '((name . "Ana") (login . "ana")
                    (url . "https://example.com/ana"))))
      (closql-insert (code-review-db) pr t))
    (with-temp-buffer
      (magit-section-mode)
      (let ((inhibit-read-only t))
        (magit-insert-section (code-review--root-section)
          (code-review-section-insert-assignee)))
      (let ((text (buffer-string)))
        (should (string-match-p "Assignees: " text))
        (should (string-match-p "Set new assignee" text))
        (should (string-match-p "Ana (@ana)" text)))
      (let ((types (code-review-section-test--section-types)))
        (should (member 'code-review-assignees-section types))
        (should (member 'code-review-assignee-section types))))))

(ert-deftest code-review-section-test/suggested-reviewers-none ()
  "No suggested reviewers: the dimmed placeholder."
  (code-review-section-test--with-section-env
    (code-review-db--pullreq-raw-infos-update nil)
    (with-temp-buffer
      (let ((inhibit-read-only t))
        (code-review-section-insert-suggested-reviewers))
      (should (string-match-p
               "Suggested-Reviewers: No suggestions"
               (buffer-string))))))

(ert-deftest code-review-section-test/suggested-reviewers-filter-requested ()
  "Suggested reviewers already requested/reviewed are filtered out."
  (code-review-section-test--with-section-env
    (code-review-db--pullreq-raw-infos-update
     `((suggestedReviewers
        . (((reviewer (login . "alice")))
           ((reviewer (login . "bob")))))
       (reviewRequests
        (nodes ((requestedReviewer (login . "bob")
                                    (url . "https://example.com/bob")))))
       (latestOpinionatedReviews (nodes))))
    (with-temp-buffer
      (magit-section-mode)
      (let ((inhibit-read-only t))
        (magit-insert-section (code-review--root-section)
          (code-review-section-insert-suggested-reviewers)))
      (let ((text (buffer-string)))
        (should (string-match-p "Suggested-Reviewers:" text))
        (should (string-match-p "Request Review - @alice" text))
        ;; bob is already requested: not offered again
        (should-not (string-match-p "@bob" text))))))

(ert-deftest code-review-section-test/reviewers ()
  "Reviewers render grouped by status (pending and reviewed)."
  (code-review-section-test--with-section-env
    (code-review-db--pullreq-raw-infos-update
     `((reviewRequests
        (nodes ((requestedReviewer (login . "bob")
                                    (url . "https://example.com/bob")))))
       (latestOpinionatedReviews
        (nodes ((author (login . "alice")
                        (url . "https://example.com/alice")
                        (state . "APPROVED")
                        (createdAt . "2021-11-08T00:24:09Z")))))))
    (with-temp-buffer
      (magit-section-mode)
      (let ((inhibit-read-only t))
        (magit-insert-section (code-review--root-section)
          (code-review-section-insert-reviewers)))
      (let ((text (buffer-string)))
        (should (string-match-p "Reviewers:" text))
        (should (string-match-p "PENDING - @bob" text))
        (should (string-match-p "APPROVED - @alice" text)))
      (let ((types (code-review-section-test--section-types)))
        (should (member 'code-review-reviewers-section types))
        (should (= 2 (cl-count 'code-review-reviewer-section types)))))))

(ert-deftest code-review-section-test/pr-description-html ()
  "Description renders bodyHTML via shr."
  (code-review-section-test--with-section-env
    (code-review-db--pullreq-raw-infos-update
     `((databaseId . 42)
       (bodyHTML . "<p>Some description text</p>")
       (bodyText . "Some description text")
       (reactions (nodes))))
    (with-temp-buffer
      (magit-section-mode)
      (let ((inhibit-read-only t))
        (magit-insert-section (code-review--root-section)
          (code-review-section-insert-pr-description)))
      (let ((text (buffer-string)))
        (should (string-match-p "Description" text))
        ;; shr may wrap the line: allow whitespace between words
        (should (string-match-p
                 "Some[[:space:]\n]*description[[:space:]\n]*text" text))))))

(ert-deftest code-review-section-test/pr-description-empty ()
  "Empty description renders the dimmed placeholder."
  (code-review-section-test--with-section-env
    (code-review-db--pullreq-raw-infos-update
     `((databaseId . 42)
       (bodyHTML . "")
       (bodyText . "")))
    (with-temp-buffer
      (magit-section-mode)
      (let ((inhibit-read-only t))
        (magit-insert-section (code-review--root-section)
          (code-review-section-insert-pr-description)))
      (should (string-match-p "No description provided." (buffer-string))))))

(ert-deftest code-review-section-test/commits-plain ()
  "Commit without CI checks: sha + message only."
  (code-review-section-test--with-section-env
    (code-review-db--pullreq-raw-infos-update
     `((commits (nodes ((commit (abbreviatedOid . "abc1234")
                                (message . "plain subject"))))))))
    (with-temp-buffer
      (magit-section-mode)
      (let ((inhibit-read-only t))
        (magit-insert-section (code-review--root-section)
          (code-review-section-insert-commits)))
      (should (string-match-p "Commits:" (buffer-string)))
      (should (string-match-p "abc1234 plain subject" (buffer-string)))
      (should-not (string-match-p "CI Checks" (buffer-string)))
      (let ((types (code-review-section-test--section-types)))
        (should (= 1 (cl-count 'code-review-commits-header-section types)))
        (should (= 1 (cl-count 'code-review-commit-section types)))))))

(ert-deftest code-review-section-test/commits-with-checks ()
  "Commit with a statusCheckRollup: heading icon, multiline message
body, the CI Checks section and one detail section per check
(success with workflow name and elapsed time, failure with summary)."
  (code-review-section-test--with-section-env
    (code-review-db--pullreq-raw-infos-update
     `((commits
        (nodes
         ((commit (abbreviatedOid . "abc1234")
                  (message . "subject line

body line")
                  (statusCheckRollup
                   (state . "FAILURE")
                   (contexts
                    (nodes
                     ((conclusion . "SUCCESS")
                      (name . "build")
                      (checkSuite (app (name . "gh"))
                                  (workflowRun (workflow (name . "CI"))))
                      (startedAt . "2021-11-08T00:00:00Z")
                      (completedAt . "2021-11-08T00:01:00Z")
                      (detailsUrl . "https://example.com/build"))
                     ((conclusion . "FAILURE")
                      (summary . "tests exploded")
                      (context . "coverage")))))))))))
    (with-temp-buffer
      (magit-section-mode)
      (let ((inhibit-read-only t))
        (magit-insert-section (code-review--root-section)
          (code-review-section-insert-commits)))
      (let ((text (buffer-string)))
        (should (string-match-p "Commits (1)" text))
        (should (string-match-p "abc1234 subject line :x:" text))
        (should (string-match-p "Expand for Details (1)" text))
        (should (string-match-p "body line" text))
        (should (string-match-p "CI Checks (2)" text))
        (should (string-match-p "CI / build" text))
        (should (string-match-p "Successful in" text))
        (should (string-match-p ":white_check_mark: Details" text))
        (should (string-match-p "coverage - tests exploded" text))
        (should (string-match-p ":x: Details" text)))
      (let ((types (code-review-section-test--section-types)))
        (should (= 1 (cl-count 'code-review-commits-header-section types)))
        (should (= 1 (cl-count 'code-review-commit-section types)))
        (should (= 1 (cl-count 'code-review-commit-checks-section types)))
        (should (= 2 (cl-count
                       'code-review-commit-check-detail-section types)))))))

(ert-deftest code-review-section-test/insert-analysis-content ()
  "The Analysis section renders similar/dead/dangling findings and
the delicate-hunk jump list (threshold-filtered, score printed)."
  (code-review-section-test--with-section-env
    (let ((orig (symbol-function 'code-review-analysis-run))
          (code-review-repo-worktree "/tmp/wt")
          (code-review-analysis-delicacy-threshold 0.5)
          (code-review-analysis-delicacy-top-k 5))
      (fset 'code-review-analysis-run
            (lambda ()
              (list :similar (list (list "new.py" 10 "old.py" 3 5 9))
                    :dead (list (list "deadfn" "old.py" 7))
                    :dangling (list (list "gonefn" "old.py"
                                          (list (list "u.py" 3 "use"))))
                    :hunks (list (list :path "p.py" :ranges "-1,3 +1,4"
                                       :score 0.9 :callers 2
                                       :dead nil :median-age nil
                                       :authors 1 :cplx 0)
                                 (list :path "q.py" :ranges "-9 +9"
                                       :score 0.1 :callers 0
                                       :dead nil :median-age nil
                                       :authors 1 :cplx 0)))))
      (unwind-protect
          (with-temp-buffer
            (magit-section-mode)
            (let ((inhibit-read-only t))
              (magit-insert-section (code-review--root-section)
                (code-review-section-insert-analysis)))
            (let ((text (buffer-string)))
              (should (string-match-p "Analysis (heuristic)" text))
              (should (string-match-p
                       "similar: new.py: 3/10 added lines also in" text))
              (should (string-match-p "old.py:5-9" text))
              (should (string-match-p
                       "possibly dead: deadfn (added in old.py:7; no references found in the worktree)"
                       text))
              (should (string-match-p
                       "dangling: gonefn (deleted in old.py; still used at u.py:3)"
                       text))
              (should (string-match-p "delicate: p.py -1,3 \\+1,4" text))
              (should (string-match-p "0.90" text))
              ;; below the threshold: not listed
              (should-not (string-match-p "delicate: q.py" text))))
        (fset 'code-review-analysis-run orig)))))

(ert-deftest code-review-section-test/outdated-comment-group ()
  "Outdated comments render grouped by hunk: one hunk section with
the washed hunk, one nested heading+body section pair per comment,
and the written-count bookkeeping records the comment path."
  (code-review-section-test--with-section-env
    (code-review-db--pullreq-raw-infos-update
     `((author (login . "pr-author"))))
    (code-review-db--curr-path-update "README.md")
    (let* ((hunk "@@ -5,3 +5,5 @@
 - old line
 + new line")
           (c1 (code-review-code-comment-section
                :author "alice" :state "COMMENTED"
                :msg "<p>First outdated</p>"
                :path "README.md" :diffHunk hunk :id 101
                :createdAt "2021-11-08T00:24:09Z"
                :reactions nil))
           (c2 (code-review-code-comment-section
                :author "bob" :state "REQUEST_CHANGES"
                :msg "<p>Second outdated</p>"
                :path "README.md" :diffHunk hunk :id 102
                :createdAt "2021-11-08T00:24:09Z"
                :reactions nil)))
      (oset c1 outdated? t)
      (oset c2 outdated? t)
      (let ((code-review-section-hold-written-comment-count nil))
        (with-temp-buffer
          (magit-section-mode)
          (let ((inhibit-read-only t))
            (magit-insert-section (code-review--root-section)
              (code-review-section-insert-outdated-comment (list c1 c2) 0)))
          (let ((text (buffer-string)))
            (should (string-match-p "Reviewed - \\[OUTDATED\\]" text))
            (should (string-match-p "old line" text))
            (should (string-match-p "new line" text))
            ;; magit appends the child count to the heading: the
            ;; heading regexes must not require the trailing colon
            (should (string-match-p "Reviewed by alice\\[COMMENTED\\]" text))
            (should (string-match-p
                     "Reviewed by bob\\[REQUEST_CHANGES\\]" text))
            ;; shr may wrap the rendered body: allow line breaks
            (should (string-match-p "First[[:space:]\n]*outdated" text))
            (should (string-match-p "Second[[:space:]\n]*outdated" text)))
          (let ((types (code-review-section-test--section-types)))
            (should (= 1 (cl-count 'code-review-outdated-hunk-section types)))
            ;; heading + body section per comment
            (should (= 4 (cl-count 'code-review-outdated-comment-section
                                    types))))
          ;; the amount-loc bookkeeping lands on the comment objects
          (should (numberp (oref c1 amount-loc)))
          (should (numberp (oref c2 amount-loc)))
          ;; and the written-count alist records the path
          (should (numberp (alist-get "README.md"
                                      code-review-section-hold-written-comment-count
                                      nil nil 'equal))))))))

(ert-deftest code-review-section-test/wash-hunk-interleaves-comments ()
  "The hunk wash interleaves grouped comments inline: the
position-keyed comment lands after its anchor line, the
side/line-keyed one after its new-side line, and the hunk itself is
still painted."
  (code-review-section-test--with-section-env
    (code-review-db--pullreq-raw-infos-update
     `((author (login . "pr-author"))))
    (let* ((pos-c (code-review-code-comment-section
                   :author "alice" :state "COMMENTED"
                   :msg "<p>at position one</p>"
                   :path "github.el" :position 1 :id 201
                   :createdAt "2021-11-08T00:24:09Z"
                   :reactions nil))
           (line-c (code-review-code-comment-section
                    :author "bob" :state "COMMENTED"
                    :msg "<p>at right line two</p>"
                    :path "github.el" :id 202
                    :createdAt "2021-11-08T00:24:09Z"
                    :reactions nil))
           (code-review-section-grouped-comments
            (list (cons (code-review-utils--comment-key "github.el" 1)
                        (list pos-c))
                  (cons (code-review-utils--comment-key-from-line
                         "github.el" "RIGHT" 2)
                        (list line-c))))
           (code-review-section-hold-written-comment-count nil)
           (code-review-section-hold-written-comment-ids nil))
      (with-temp-buffer
        (magit-section-mode)
        (let ((inhibit-read-only t))
          (insert "diff --git a/github.el b/github.el\n"
                  "index 111..222 100644\n"
                  "--- a/github.el\n"
                  "+++ b/github.el\n"
                  "@@ -1,3 +1,4 @@\n"
                  " context\n"
                  "-old\n"
                  "+added\n")
          (goto-char (point-min))
          (magit-insert-section (code-review--root-section)
            (magit-insert-section (code-review-files-chnged)
              (save-restriction
                (narrow-to-region (point) (point-max))
                (magit-wash-sequence #'code-review-wash-diff)))))
        (let ((text (buffer-string)))
          ;; shr may wrap the rendered body: allow line breaks
          (should (string-match-p
                   "at[[:space:]\n]*position[[:space:]\n]*one" text))
          (should (string-match-p
                   "at[[:space:]\n]*right[[:space:]\n]*line[[:space:]\n]*two"
                   text))
          ;; the comment headings use the non-outdated format
          (should (string-match-p "Reviewed by @alice" text))
          (should (string-match-p "Reviewed by @bob" text))
          ;; the hunk itself is still washed and painted
          (should (save-excursion
                    (goto-char (point-min))
                    (search-forward "+added" nil t)
                    (eq (get-text-property (line-beginning-position)
                                           'font-lock-face)
                        'magit-diff-added)))
          ;; both keys were marked written
          (should (member (code-review-utils--comment-key "github.el" 1)
                          code-review-section-hold-written-comment-ids))
          (should (member (code-review-utils--comment-key-from-line
                           "github.el" "RIGHT" 2)
                          code-review-section-hold-written-comment-ids)))))))

;;; code-review-section-test.el ends here
