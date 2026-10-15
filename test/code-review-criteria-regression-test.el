;;; code-review-criteria-regression-test.el --- phase 27 slice  -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: code-review contributors

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

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'code-review-criteria)
(require 'code-review-criteria-regression)
(require 'code-review-agents-shell)

;;; Fixture: one temp repository per test

(defun code-review-criteria-regression-test--make-repo ()
  "A temp git repository with one committed file.  Returns its
directory (trailing slash)."
  (let* ((dir (file-name-as-directory (make-temp-file "cr-reg-" t)))
         (default-directory dir))
    (call-process "git" nil nil nil "init" ".")
    (call-process "git" nil nil nil "config" "user.email" "t@t.tt")
    (call-process "git" nil nil nil "config" "user.name" "test")
    (with-temp-file (expand-file-name "base.txt" dir) (insert "base\n"))
    (call-process "git" nil nil nil "add" "-A")
    (call-process "git" nil nil nil "commit" "-m" "init")
    dir))

(defun code-review-criteria-regression-test--criterion (dir)
  "The criterion plist for a fixture criteria file in DIR."
  (let ((file (expand-file-name "specs/2026-10-10-002-x.md" dir)))
    (list :id "2026-10-10-002"
          :file file
          :sentence "WHEN x THE SYSTEM SHALL y"
          :paths '("src/a.el")
          :text "---\npaths: [src/a.el]\n---\n\nWHEN x THE SYSTEM SHALL y\n")))

;;; req-2026-10-10-001: drafter-handoff
;; covers: code-review-criteria.el, code-review-agents-shell.el
;; enforcement: test
(ert-deftest code-review-criteria-regression-test/req-2026-10-10-001-local-strategy-fires-drafter ()
  "WHEN the `local' strategy fires with a reviewer-side drafter wired THE
SYSTEM SHALL hand the criterion to the drafter after the ledger file
has landed (a drafter failure never loses the criterion) with a
DRAFT-NOT-PUSH contract — the regression suite is drafted as
uncommitted changes in the review worktree, and no login probe gates
the handoff.

The local strategy fires the REVIEWER-SIDE drafter AFTER the
ledger file has landed: the criterion never evaporates on a
drafter failure, and no login probe gates the handoff (the
phase 27 drafter/transport split)."
  (let* ((dir (code-review-criteria-regression-test--make-repo))
         (criterion (code-review-criteria-regression-test--criterion dir))
         (calls nil)
         (code-review-criteria-submit-strategies '(local))
         (code-review-criteria-drafter-function
          (lambda (c) (push c calls))))
    (should (eq (code-review-criteria--submit criterion dir) 'local))
    (should (file-exists-p (plist-get criterion :file)))
    (should (equal (length calls) 1))
    (should (equal (car calls) criterion))))

(ert-deftest code-review-criteria-regression-test/req-2026-10-10-001-drafter-failure-keeps-criterion ()
  "A SIGNALING drafter never loses the criterion: the file is
already written when the handoff runs, the strategy still fired
`local', and nothing signals out of the chain."
  (let* ((dir (code-review-criteria-regression-test--make-repo))
         (criterion (code-review-criteria-regression-test--criterion dir))
         (code-review-criteria-submit-strategies '(local))
         (code-review-criteria-drafter-function
          (lambda (_c) (error "drafter exploded"))))
    (should (eq (code-review-criteria--submit criterion dir) 'local))
    (should (file-exists-p (plist-get criterion :file)))))

(ert-deftest code-review-criteria-regression-test/req-2026-10-10-001-prompt-is-draft-not-push ()
  "The handoff prompt carries the DRAFT-NOT-PUSH contract: the
worktree, the requirement, the do-not-commit/do-not-push rule,
and the ship command the reviewer presses after exploring."
  (let* ((dir (code-review-criteria-regression-test--make-repo))
         (criterion (code-review-criteria-regression-test--criterion dir))
         (prompt (code-review-agents-shell-prompt criterion dir)))
    (should (string-match-p (regexp-quote dir) prompt))
    (should (string-match-p "2026-10-10-002" prompt))
    (should (string-match-p "WHEN x THE SYSTEM SHALL y" prompt))
    (should (string-match-p "Do NOT commit" prompt))
    (should (string-match-p "do\nNOT push" prompt))
    (should (string-match-p "code-review-criteria-request-regression"
                            prompt))))

;;; req-2026-10-10-002: approval-stacked-pr
;; covers: code-review-criteria-regression.el
;; enforcement: test
(ert-deftest code-review-criteria-regression-test/req-2026-10-10-002-pure-parts ()
  "WHEN the reviewer approves an explored regression draft THE SYSTEM
SHALL branch `review/req-<id>' off the PR head, commit the criteria
file and the drafted tests with `Invariant-Ref:' and
`Regression-Test:' trailers, and on a GitHub PR open a STACKED PR
whose base is the contributor's PR branch (the contributor decides;
never the reviewer's push to the contributor's branch, never main) —
while a local review ships L0-only, staging the criteria file and
test-matching files rather than a blanket add on the user's own
working tree.

Branch name, commit message trailers, and the stacked-PR
payload: base is the CONTRIBUTOR's PR branch (never main), head
is the shipped suite branch, the body carries the EARS sentence
and the Invariant-Ref, and the contributor may modify."
  (let ((criterion (code-review-criteria-regression-test--criterion
                    "/nowhere/")))
    (should (equal (code-review-criteria-regression--branch-name
                    "2026-10-10-002")
                   "review/req-2026-10-10-002"))
    (let ((msg (code-review-criteria-regression--commit-message
                criterion '("test/foo-test.el"))))
      (should (string-match-p "Invariant-Ref: 2026-10-10-002" msg))
      (should (string-match-p "Regression-Test: test/foo-test.el" msg)))
    (should (not (string-match-p
                  "Regression-Test"
                  (code-review-criteria-regression--commit-message
                   criterion nil))))
    (let ((payload (code-review-criteria-regression--pr-payload
                    criterion "review/req-2026-10-10-002"
                    "contributor/feature")))
      (should (equal (cdr (assq 'base payload)) "contributor/feature"))
      (should (equal (cdr (assq 'head payload)) "review/req-2026-10-10-002"))
      (should (cdr (assq 'maintainer_can_modify payload)))
      (should (string-match-p "WHEN x THE SYSTEM SHALL y"
                              (cdr (assq 'body payload))))
      (should (string-match-p "Invariant-Ref: 2026-10-10-002"
                              (cdr (assq 'body payload))))
      (should (string-match-p "2026-10-10-002" (cdr (assq 'title payload)))))))

(ert-deftest code-review-criteria-regression-test/req-2026-10-10-002-l0-stages-criteria-and-tests-only ()
  "L0 (a LOCAL review, the user's own working tree): the approval
commits the criteria file + the drafted TEST files with the
trailers, and NEVER a blanket add — unrelated WIP stays
untouched in the working tree."
  (let* ((dir (code-review-criteria-regression-test--make-repo))
         (criterion (code-review-criteria-regression-test--criterion dir)))
    ;; the draft: an agent left these uncommitted
    (make-directory (expand-file-name "specs" dir) t)
    (with-temp-file (plist-get criterion :file)
      (insert (plist-get criterion :text)))
    (make-directory (expand-file-name "test" dir) t)
    (with-temp-file (expand-file-name "test/foo-test.el" dir)
      (insert "(ert-deftest foo () (should t))\n"))
    (with-temp-file (expand-file-name "notes-wip.txt" dir)
      (insert "unrelated WIP\n"))
    (let ((res (code-review-criteria-regression--branch-and-commit
                criterion dir :tests)))
      (should (equal (car res) "review/req-2026-10-10-002"))
      (should (equal (cdr res) '("test/foo-test.el")))
      (should (equal (string-trim
                      (code-review-registry--git
                       dir "rev-parse" "--abbrev-ref" "HEAD"))
                     "review/req-2026-10-10-002"))
      (let ((names (code-review-registry--git
                    dir "show" "--name-only" "--format=" "HEAD")))
        (should (string-match-p "specs/2026-10-10-002-x.md" names))
        (should (string-match-p "test/foo-test.el" names))
        (should (not (string-match-p "notes-wip.txt" names))))
      (let ((msg (code-review-registry--git dir "log" "-1" "--format=%B")))
        (should (string-match-p "Invariant-Ref: 2026-10-10-002" msg))
        (should (string-match-p "Regression-Test: test/foo-test.el" msg)))
      ;; the WIP file is still untracked, not committed
      (should (string-match-p
               "?? notes-wip.txt"
               (or (code-review-registry--git dir "status" "--porcelain")
                   ""))))))

(ert-deftest code-review-criteria-regression-test/req-2026-10-10-002-forge-worktree-commits-the-whole-delta ()
  "The forge worktree is disposable and its whole delta IS the
draft: `:all' commits everything (the blanket add is correct
there, never on the user's own tree)."
  (let* ((dir (code-review-criteria-regression-test--make-repo))
         (criterion (code-review-criteria-regression-test--criterion dir)))
    (make-directory (expand-file-name "specs" dir) t)
    (with-temp-file (plist-get criterion :file)
      (insert (plist-get criterion :text)))
    (with-temp-file (expand-file-name "anything-drafted.txt" dir)
      (insert "draft\n"))
    (let ((res (code-review-criteria-regression--branch-and-commit
                criterion dir :all)))
      (should (equal (car res) "review/req-2026-10-10-002"))
      (should (string-match-p
               "anything-drafted.txt"
               (code-review-registry--git
                dir "show" "--name-only" "--format=" "HEAD")))
      (should (string-match-p
               "specs/2026-10-10-002-x.md"
               (code-review-registry--git
                dir "show" "--name-only" "--format=" "HEAD"))))))

(ert-deftest code-review-criteria-regression-test/req-2026-10-10-002-existing-branch-refused ()
  "A branch that already exists is REFUSED without a signal (the
rerun trap: a dangling branch must not block the retry)."
  (let* ((dir (code-review-criteria-regression-test--make-repo))
         (criterion (code-review-criteria-regression-test--criterion dir)))
    (call-process "git" nil nil nil "-C" dir "branch"
                  "review/req-2026-10-10-002")
    (should (null (code-review-criteria-regression--branch-and-commit
                   criterion dir :tests)))))

(ert-deftest code-review-criteria-regression-test/req-2026-10-10-002-nothing-to-ship-cleans-up ()
  "Nothing staged (no criteria file, no draft): the branch
created by the checkout is DELETED again — the retry is not
blocked by a dangling empty branch."
  (let* ((dir (code-review-criteria-regression-test--make-repo))
         (criterion (code-review-criteria-regression-test--criterion dir)))
    (should (null (code-review-criteria-regression--branch-and-commit
                   criterion dir :tests)))
    (should (null (code-review-registry--git
                   dir "rev-parse" "--verify"
                   "refs/heads/review/req-2026-10-10-002")))
    (should (not (equal
                  (string-trim
                   (code-review-registry--git
                    dir "rev-parse" "--abbrev-ref" "HEAD"))
                  "review/req-2026-10-10-002")))))

;;; code-review-criteria-regression-test.el ends here
