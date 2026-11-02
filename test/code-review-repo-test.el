;;; code-review-repo-test.el --- ERT tests for code-review-repo -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'code-review-db)
(require 'code-review-github)
(require 'code-review-repo)
(require 'code-review-test-helpers)

(defun code-review-repo-test--git (dir &rest args)
  "Run git ARGS in DIR, signaling with stderr on failure."
  (with-temp-buffer
    (let ((exit (apply #'call-process magit-git-executable nil t nil
                       (append (list "--no-pager" "-C" dir) args))))
      (unless (zerop exit)
        (error "git %s failed (%d): %s" args exit
               (buffer-substring-no-properties (point-min)
                                               (point-max)))))))

(defun code-review-repo-test--make-repo ()
  "A throwaway git repository with one commit.  Return its directory."
  (let ((dir (make-temp-file "cr-repo-cleanup-" t)))
    (code-review-repo-test--git dir "init" "-q" "-b" "main")
    (code-review-repo-test--git dir "config" "user.name" "t")
    (code-review-repo-test--git dir "config" "user.email" "t@t")
    (with-temp-file (expand-file-name "f.txt" dir)
      (insert "one\n"))
    (code-review-repo-test--git dir "add" "f.txt")
    (code-review-repo-test--git dir "-c" "user.name=t"
                                "-c" "user.email=t@t"
                                "commit" "-m" "one" "--no-verify")
    dir))

(defun code-review-repo-test--make-row (num finished?)
  "A saved row for owner/repo NUM, finished or not."
  (let ((pr (code-review-github-repo :owner "o" :repo "r"
                                     :number num)))
    (code-review-db--pullreq-create pr)
    (oset pr saved t)
    (when finished?
      (oset pr finished t)
      (oset pr finished-at (current-time-string)))
    (code-review-db-update pr)
    pr))

;;; req-2026-10-31-001: worktree-cleanup-prunes-finished-stale
;; covers: code-review-repo.el
;; covers-fn: code-review-repo-cleanup, code-review-repo--cleanup-plan
;; enforcement: test
(ert-deftest code-review-repo/req-2026-10-31-001-cleanup-prunes-stale-finished-worktrees ()
  "WHEN a review is finished and its worktree's head has not moved
for more than `code-review-repo-cleanup-days' days THE SYSTEM
SHALL prune that worktree — and never prune an UNFINISHED
review's worktree (however old) or a finished one whose head
moved recently: an active review's worktree never ages out."
  (let ((code-review-history-enabled nil)
        (code-review-review-budget-minutes nil)
        (code-review-budget-state-file (make-temp-file "cr-wt-"))
        (code-review-repo-cleanup-days 14))
    (code-review-test--with-db
      (let* ((dir (code-review-repo-test--make-repo))
             (head (code-review-repo--git dir "rev-parse" "HEAD")))
        ;; three worktrees: 1 stale+finished, 2 fresh+finished,
        ;; 3 stale+unfinished
        (dolist (num '("1" "2" "3"))
          (should (code-review-repo--ensure-worktree dir num head)))
        (let ((stale (days-to-seconds -20)))
          (set-file-times
           (code-review-repo--worktree-head-file dir "1")
           (seconds-to-time stale))
          (set-file-times
           (code-review-repo--worktree-head-file dir "3")
           (seconds-to-time stale)))
        ;; rows: #1 and #2 finished, #3 unfinished
        (code-review-repo-test--make-row "1" t)
        (code-review-repo-test--make-row "2" t)
        (code-review-repo-test--make-row "3" nil)
        ;; plan: only #1 (stale AND finished)
        (let ((plan (code-review-repo--cleanup-plan dir "o" "r")))
          (should (equal (mapcar #'car plan) '("1")))
          ;; execute: the worktree is gone, the others stay
          (let ((res (code-review-repo--cleanup-execute dir plan)))
            (should (equal res '(1 nil))))
          (should-not (file-directory-p
                       (code-review-repo--worktree-dir dir "1")))
          (should (file-directory-p
                   (code-review-repo--worktree-dir dir "2")))
          (should (file-directory-p
                   (code-review-repo--worktree-dir dir "3")))
          ;; the branch refs survive the removal
          (should (equal head (code-review-repo--git
                               dir "rev-parse" "HEAD"))))))))

(defun days-to-seconds (d) (* d 86400.0))

(provide 'code-review-repo-test)
;;; code-review-repo-test.el ends here
