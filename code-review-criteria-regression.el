;;; code-review-criteria-regression.el --- ship an explored draft  -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: code-review contributors
;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Phase 27, the APPROVAL step of the reviewer-side loop (C-c C-g,
;; `code-review-criteria-request-regression').  The flow it closes:
;; C-c C-t captures a criterion (local strategy: ledger + DRAFTER
;; handoff), the reviewer's agent drafts the regression suite as
;; UNCOMMITTED changes in the review worktree (draft-not-push),
;; the reviewer EXPLORES the draft (diff it, run the T command's
;; mapped subset) — and only then presses C-c C-g.  This command is
;; the decision-selection moment: automate information acquisition
;; and analysis, never decision selection (Parasuraman/Sheridan/
;; Wickens 2000; the explicit keypress IS the approval).
;;
;; What it does, on the capability ladder:
;;   L0 (any repo, no forge): branch `review/req-<id>', commit the
;;     criteria file + the drafted TEST files (never a blanket
;;     `git add -A' on the user's own working tree), trailers
;;     `Invariant-Ref:' and `Regression-Test:'.  Nothing leaves
;;     the machine.
;;   L1 (GitHub PR): push the branch and open a STACKED PR with
;;     base = the PR's own branch — the CONTRIBUTOR decides
;;     whether the suite is useful, the reviewer never pushes to
;;     the contributor's branch and never to main.  GitLab and
;;     Bitbucket degrade to L0 with a message (phase 27 L1 is
;;     GitHub first).
;;
;; The forge worktree is a DETACHED CHECKOUT at the PR head
;; (`code-review-repo-setup'), so the branch starts exactly at
;; the PR head and the stacked diff is the drafted suite alone.

;;; Code:

(require 'cl-lib)
(require 'magit-section)
(require 'code-review-utils)
(require 'code-review-criteria)
(require 'code-review-registry)
(defvar code-review-repo-worktree)         ; code-review-repo.el
(defvar code-review-auth-login-marker)     ; code-review-github.el
(defvar code-review-github-host)           ; code-review-github.el
(declare-function code-review-db-local-pr-p "code-review-db")
(declare-function code-review-db-get-pullreq "code-review-db")
(declare-function code-review-repo--forge "code-review-repo")
(declare-function code-review-github--graphql-data "code-review-github")
(declare-function ghub-post "ghub")

(defcustom code-review-criteria-regression-branch-prefix "review/req-"
  "Branch prefix for shipped regression suites (phase 27)."
  :type 'string
  :group 'code-review)

(defcustom code-review-criteria-regression-test-file-regexp
  "\\(?:-test\\.el\\'\\|\\.el\\.gz\\'\\|_test\\|/tests?/\\|\\.spec\\.\\|\\`tests?/\\|test_\\)"
  "Regexp matching DRAFTED files that are the regression suite.
Used by `code-review-criteria-request-regression' in the L0
local-review case, where the working tree is the user's own: only
the criteria file and files matching this are committed, a
blanket `git add -A' would sweep unrelated WIP.  Also selects the
`Regression-Test:' trailer's contents in every mode."
  :type 'regexp
  :group 'code-review)

(defun code-review-criteria-regression--branch-name (id)
  "The shipped-suite branch name for criterion ID.  Pure."
  (concat code-review-criteria-regression-branch-prefix id))

(defun code-review-criteria-regression--commit-message (criterion test-files)
  "Commit message for a shipped suite: CRITERION + its TEST-FILES.
Pure.  Trailers per the phase 22 conventions: `Invariant-Ref'
always, `Regression-Test' with the drafted test files (best
effort — an empty suite ships without it and the trailer
graduates at merge time)."
  (concat
   (format "regression suite for %s (from review)\n\n"
           (plist-get criterion :id))
   (format "Invariant-Ref: %s\n" (plist-get criterion :id))
   (and test-files
        (format "Regression-Test: %s\n" (string-join test-files ", ")))))

(defun code-review-criteria-regression--pr-payload (criterion branch pr-branch)
  "The stacked-PR creation payload (pure).
BRANCH is the pushed head, PR-BRANCH the contributor's PR branch
(the PR's base — the contributor decides, never main)."
  (let ((id (plist-get criterion :id)))
    (list (cons 'title
                (format "Regression suite for criterion %s (from review)" id))
          (cons 'head branch)
          (cons 'base pr-branch)
          (cons 'maintainer_can_modify t)
          (cons 'body
                (format
                 "Drafted by the reviewer's agent from review criterion %s.
The contributor decides whether this is useful — close freely.

Requirement (EARS): %s

Invariant-Ref: %s
"
                 id (plist-get criterion :sentence) id)))))

(defun code-review-criteria-regression--git-ok (worktree &rest args)
  "Run git ARGS in WORKTREE; non-nil when it exited 0.
The mutation twin of `code-review-registry--git' (which reads)."
  (zerop (apply #'call-process "git" nil nil nil
                "-C" (directory-file-name worktree) args)))

(defun code-review-criteria-regression--test-files (worktree)
  "The STAGED files matching the test regexp in WORKTREE.
Reads `git diff --cached --name-only' (call AFTER staging)."
  (let ((out (code-review-registry--git
              worktree "diff" "--cached" "--name-only")))
    (and out
         (cl-remove-if-not
          (lambda (path)
            (string-match-p
             code-review-criteria-regression-test-file-regexp path))
          (split-string out "\n" t)))))

(defun code-review-criteria-regression--existing-branch-p (worktree branch)
  "Non-nil when BRANCH already exists in WORKTREE."
  (and (code-review-registry--git
        worktree "rev-parse" "--verify"
        (concat "refs/heads/" branch))
       t))

(defun code-review-criteria-regression--branch-and-commit
    (criterion worktree mode)
  "Create the suite branch and commit the draft.  Returns
\(BRANCH . TEST-FILES) on success, nil on failure (never signals
— the caller messages; TEST-FILES are the staged files matching
the test regexp, read from the index BEFORE the commit empties
it).  MODE selects staging:
  `:all'  the forge worktree case — disposable, its whole delta
          IS the draft, so `git add -A';
  `:tests' the local-review case — the user's own working tree,
          so ONLY the criteria file and test-matching files are
          staged (never a blanket add)."
  (let* ((branch (code-review-criteria-regression--branch-name
                  (plist-get criterion :id)))
         (rel (file-relative-name (plist-get criterion :file) worktree)))
    (when (and (not (code-review-criteria-regression--existing-branch-p
                     worktree branch))
               (code-review-criteria-regression--git-ok
                worktree "checkout" "-b" branch))
      (let ((staged
             (cond
              ((eq mode :all)
               (code-review-criteria-regression--git-ok
                worktree "add" "-A"))
              ((eq mode :tests)
               (let* ((status (code-review-registry--git
                               worktree "status" "--porcelain"))
                      (paths (and status
                                  (append
                                   (list rel)
                                   (cl-remove-if-not
                                    (lambda (path)
                                      (string-match-p
                                       code-review-criteria-regression-test-file-regexp
                                       path))
                                    (mapcar
                                     (lambda (line) (substring line 3))
                                     (split-string status "\n" t)))))))
                 (and paths
                      (apply
                       #'code-review-criteria-regression--git-ok
                       worktree "add" "--"
                       (delete-dups (copy-sequence paths))))))
              (t nil))))
        (if (not staged)
            ;; nothing staged and the branch is empty: undo the
            ;; checkout so a retry is not blocked by a dangling
            ;; branch (the rerun-refusal trap)
            (progn
              (code-review-criteria-regression--git-ok
               worktree "checkout" "-")
              (code-review-criteria-regression--git-ok
               worktree "branch" "-D" branch)
              nil)
          (let ((tests (code-review-criteria-regression--test-files worktree)))
            (and (code-review-criteria-regression--git-ok
                  worktree "commit" "-m"
                  (code-review-criteria-regression--commit-message
                   criterion tests))
                 (cons branch tests))))))))

(defun code-review-criteria-regression--push (worktree branch)
  "Push BRANCH from WORKTREE to origin.  Non-nil on success.
A failure (no write access, fork PR head the reviewer cannot
push to) degrades: the branch stays local, the caller messages."
  (code-review-criteria-regression--git-ok
   worktree "push" "-u" "origin" branch))

(defun code-review-criteria-regression--github-open-pr (criterion branch)
  "Open the stacked PR on GitHub for CRITERION's review.
Returns the new PR's html_url, or nil on failure (never
signals).  Base is the review PR's own head branch — the
CONTRIBUTOR decides; the reviewer never targets main."
  (condition-case err
      (let* ((pr (code-review-db-get-pullreq))
             (owner (and pr (oref pr owner)))
             (repo (and pr (oref pr repo)))
             (pr-branch (and pr (oref pr head-ref-name))))
        (unless (and owner repo pr-branch)
          (user-error "no PR context for the stacked PR"))
        (require 'code-review-github)
        (let* ((res (ghub-post
                     (format "/repos/%s/%s/pulls" owner repo)
                     nil
                     :auth code-review-auth-login-marker
                     :payload (code-review-criteria-regression--pr-payload
                               criterion branch pr-branch)
                     :headers '(("Accept" . "application/vnd.github.v3+json"))
                     :host code-review-github-host
                     :synchronous t))
               (data (code-review-github--graphql-data res)))
          (cdr (assq 'html_url data))))
    (error
     (code-review-utils--log
      "code-review-criteria"
      (format "stacked-PR creation failed: %S" err))
     nil)))

(defun code-review-criteria-regression--req-at-point ()
  "The criterion plist at point (checklist row), or nil."
  (let ((sec (magit-current-section)))
    (and sec
         (slot-exists-p sec 'req)
         (slot-boundp sec 'req)
         (oref sec req))))

(defun code-review-criteria-regression--read-criterion (worktree)
  "The criterion to ship: the checklist row at point, else
completing-read over the worktree's criteria ledger.
The candidates are the criteria FILE names alone (the name
carries the id and the subject slug — one scannable token, the
file IS the index); the EARS sentence rides as an ANNOTATION on
its own indented line while choosing, the inline preview of what
you would ship against (the full text is the file itself — C-u
on the command opens it)."
  (or (code-review-criteria-regression--req-at-point)
      (let* ((criteria (or (code-review-criteria--criteria worktree) '()))
             (table (mapcar (lambda (req)
                              (cons (file-name-nondirectory
                                     (plist-get req :file))
                                    req))
                            criteria))
             (sentences (mapcar (lambda (entry)
                                  (cons (car entry)
                                        (plist-get (cdr entry) :sentence)))
                                table))
             (annotate (lambda (name)
                         ;; its own line, indented — a bare suffix
                         ;; glues onto the candidate and reads as
                         ;; one long wrapped token
                         (let ((sentence (cdr (assoc name sentences))))
                           (and sentence (concat "\n    " sentence)))))
             (collection
              (lambda (string pred action)
                (cond
                 ((eq action 'metadata)
                  `(metadata (annotation-function . ,annotate)))
                 ((eq action 'lambda) t)
                 (t (complete-with-action action table string pred))))))
        (if (not table)
            (user-error "No criteria under %s — capture one with C-c C-t"
                        worktree)
          (cdr (assoc (completing-read "Ship regression suite for (file): "
                                       collection nil t)
                      table))))))

;;;###autoload
(defun code-review-criteria-request-regression (criterion criterion-prefix)
  "Ship the explored regression draft for CRITERION (C-c C-g,
phase 27 approval step).  After C-c C-t (local strategy) handed
the criterion to the reviewer's agent and the agent drafted the
regression suite as uncommitted changes in the review worktree,
and the reviewer EXPLORED the draft — this is the explicit
approval keypress:

  branch `review/req-<id>' off the current HEAD (the PR head in
  the detached forge worktree), commit the criteria file + the
  drafted tests with `Invariant-Ref:' and `Regression-Test:'
  trailers;

  a LOCAL review (L0) stops there — nothing leaves the machine
  (only the criteria file and test-matching files are staged,
  never a blanket add on your own working tree; other WIP stays
  untouched, you return with `git checkout -');

  a GitHub PR review (L1) pushes the branch and opens a STACKED
  PR whose BASE is the contributor's PR branch — the contributor
  decides whether the suite is useful, the reviewer never pushes
  to their branch and never to main.  GitLab/Bitbucket degrade
  to L0 (message says so).  A push without write access keeps
  the branch locally and says so.

The branch name prefix and the test-file regexp are defcustoms.
CRITERION is the requirement plist (interactive: the checklist
row at point, else completing-read over the worktree ledger —
candidates are the criteria FILE names, the EARS sentence shown
as an annotation).  With a C-u prefix the command instead
PREVIEWS the criterion: the full criteria file opens in another
window and nothing ships — the read-before-ship step."
  (interactive
   (list (code-review-criteria-regression--read-criterion
          (or (bound-and-true-p code-review-repo-worktree)
              (code-review-registry--repo-dir)))
         current-prefix-arg))
  (when criterion-prefix
    (find-file-other-window (plist-get criterion :file))
    (message "code-review: previewing %s — press C-c C-g without a prefix to ship"
             (plist-get criterion :id)))
  (code-review-criteria-regression--ship criterion))

(defun code-review-criteria-regression--ship (criterion)
  "The shipping core of `code-review-criteria-request-regression':
branch, commit (trailers), and per context push + stacked PR.
Separated so the preview prefix is a thin branch above it."
  (let* ((worktree (file-name-as-directory
                    (or (bound-and-true-p code-review-repo-worktree)
                        (code-review-registry--repo-dir))))
         (branch (code-review-criteria-regression--branch-name
                  (plist-get criterion :id)))
         (localp (and (fboundp 'code-review-db-local-pr-p)
                      (code-review-db-local-pr-p))))
    (unless worktree
      (user-error "No worktree/repository at point"))
    (when (code-review-criteria-regression--existing-branch-p
           worktree branch)
      (user-error "Branch %s already exists — inspect or delete it first"
                  branch))
    (if (not localp)
        ;; forge review: the detached worktree's whole delta IS the
        ;; draft — blanket add, then push + stacked PR when possible
        (let ((res (code-review-criteria-regression--branch-and-commit
                    criterion worktree :all)))
          (if (not res)
              (message "code-review: nothing to ship — no draft in %s?"
                       worktree)
            (let ((tests (cdr res)))
              (if (eq (ignore-errors
                        (code-review-repo--forge
                         (code-review-db-get-pullreq)))
                      'github)
                  (if (code-review-criteria-regression--push
                       worktree branch)
                      (let ((url
                             (code-review-criteria-regression--github-open-pr
                              criterion branch)))
                        (if url
                            (message "code-review: stacked PR %s -> %s (%s)"
                                     branch url
                                     (or (car tests) "no test files matched"))
                          (message
                           "code-review: branch %s pushed but the stacked PR failed — create it from %s"
                           branch branch)))
                    (message
                     "code-review: branch %s kept locally (push failed: no access?)"
                     branch))
                (message
                 "code-review: branch %s committed (%s) — stacked PR for this forge not yet supported, L0"
                 branch
                 (if tests (string-join tests ", ")
                   "no test files matched"))))))
      ;; local review (L0): the user's own working tree — stage ONLY
      ;; the criteria file + test-matching files, other WIP untouched
      (let ((res (code-review-criteria-regression--branch-and-commit
                  criterion worktree :tests)))
        (if (not res)
            (message "code-review: nothing to ship — no draft in %s?"
                     worktree)
          (message
           "code-review: branch %s committed (%s) — L0, nothing leaves the machine; return with git checkout -"
           branch
           (if (cdr res) (string-join (cdr res) ", ")
             "no test files matched")))))))

(provide 'code-review-criteria-regression)
;;; code-review-criteria-regression.el ends here
