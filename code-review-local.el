;;; code-review-local.el --- Review local diffs as if they were PRs -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026 Andrea <andrea-dev@hotmail.com>
;;
;; This file is part of code-review.
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either under version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; Commentary:
;;
;;  Review the working tree (or any local range) with the full
;;  code-review rendering: classification tags, folding, difftastic
;;  drill-down.  The "PR" is a local diff; there is no forge, so
;;  the review's OUTCOMES are local too (phase 25b): pending local
;;  comments dispatch to the attached AI agent session as fix work
;;  items, and finishing books the budget and hands off to
;;  magit-status for the commit.
;;
;;  The pullreq row uses state "LOCAL" as its marker.  It has NO
;;  extra slots: the git diff args ride the existing
;;  `base-ref-name' column, the repo root is the `host' column,
;;  and the worktree is derived from `magit-toplevel' at render
;;  time.
;;
;;; Code:

(require 'magit-git)
(require 'magit-section)
(require 'deferred)
(require 'a)
(require 'code-review-db)
(require 'code-review-utils)
(require 'code-review-interfaces)
(require 'code-review-section)
(require 'code-review-criteria)
(require 'code-review-budget)

(declare-function code-review--build-buffer "code-review-section")

(defun code-review-local--diff-text (obj)
  "Return the output of the local `git diff' for OBJ.
The repo root comes from OBJ's `host' slot, so this is safe to run
from a timer where `default-directory' is arbitrary."
  (let ((default-directory (file-name-as-directory (oref obj host))))
    (magit-git-output "diff" "--no-color" (oref obj base-ref-name))))

(defun code-review-local--cleanup-rows (&optional root diff-args)
  "Delete stale LOCAL rows, except the review of ROOT and DIFF-ARGS.
Keeping the matching row lets reopening preserve its viewed marks."
  (let ((db (code-review-db)))
    (dolist (row (emacsql db [:select [id host base-ref-name] :from 'pullreq
                                    :where (= state "LOCAL")]))
      (unless (and root (equal root (nth 1 row))
                   (equal diff-args (nth 2 row)))
        (code-review-db--delete-pullreq-tree db (car row))))))

(defun code-review-local--commit-args (rev)
  "Return (DIFF-ARGS TITLE) reviewing REV against its first parent.
\"REV^..REV\" for a commit with a parent, \"REV^!\" for a ROOT
commit (\"REV^..REV\" fails there: git exit 128, verified with
real git)."
  (list (if (magit-rev-verify (concat rev "^"))
            (format "%s^..%s" rev rev)
          (format "%s^!" rev))
        (format "Commit %s (%s)"
                (magit-git-string "rev-parse" "--short" rev)
                (magit-git-string "log" "-1" "--format=%s" rev))))

(defconst code-review-local--empty-tree
  "4b825dc642cb6eb9a060e54bf8d69288fbee4904"
  "The git EMPTY TREE oid.
It is the same sha in every repository (the hash of an empty
entry list), so it is a valid `git diff' range endpoint even
though the object does not exist.  Used when the OLDEST commit
in a log-region review is the repository's ROOT commit: the
range starts at the empty tree so the root commit's changes are
in the diff too (\"ROOT^..NEW\" fails there: git exit 128).")

(defun code-review-local--magit-log-args ()
  "Return (DIFF-ARGS TITLE) for what a magit log buffer selects.
The commit at point; with a region marked, ALL the commits in the
region: their COMBINED diff, from the OLDEST selected commit's
parent to the NEWEST (\"OLD^..NEW\" — a region of one commit is
just that commit's diff; when the oldest is the ROOT commit the
range starts at the git empty tree instead, since ROOT has no
parent).  nil when nothing commit-like is at point."
  (let ((revs (magit-region-values '(commit branch) t)))
    (cond
     ((and revs (cdr revs))
      (deactivate-mark)
      ;; region values are listed newest first: the oldest is last
      (let* ((newest (car revs))
             (oldest (car (last revs)))
             (range (if (magit-rev-verify (concat oldest "^"))
                        (format "%s^..%s" oldest newest)
                      (format "%s..%s" code-review-local--empty-tree newest))))
        (list range
              (format "Commits %s..%s"
                      (magit-git-string "rev-parse" "--short" oldest)
                      (magit-git-string "rev-parse" "--short" newest)))))
     (revs (deactivate-mark)
      (code-review-local--commit-args (car revs)))
     ((let ((rev (magit-commit-at-point)))
        (and rev (code-review-local--commit-args rev))))
     (t nil))))

(defun code-review-local--magit-diff-args ()
  "Return (DIFF-ARGS TITLE) describing the magit diff at point.
In a `magit-revision-mode' buffer: the shown COMMIT, diffed
against its first parent (\"REV^..REV\"; \"REV^!\" for a root
commit, which has no parent to diff against — \"REV^..REV\"
fails there).  In a `magit-log-mode' buffer: the commit at point,
or — with a region marked — the COMBINED diff of all the commits
in the region (see `code-review-local--magit-log-args').  In a
plain `magit-diff-mode' buffer showing a RANGE: the range as-is.
nil anywhere else: the caller falls back to the working tree.
Runs in the magit buffer, so its `default-directory' is the
repository."
  (cond
   ((and (derived-mode-p 'magit-revision-mode)
         magit-buffer-revision)
    (code-review-local--commit-args magit-buffer-revision))
   ((derived-mode-p 'magit-log-mode)
    (code-review-local--magit-log-args))
   ((and (derived-mode-p 'magit-diff-mode)
         (not (derived-mode-p 'magit-revision-mode))
         magit-buffer-diff-range)
    (list magit-buffer-diff-range
          (format "Diff %s" magit-buffer-diff-range)))
   (t nil)))

;;;###autoload
(defun code-review-review-local-diff (&optional arg)
  "Review the local diff as if it were a PR.
With no prefix ARG, review what the current buffer is looking at:
in a `magit-revision-mode' buffer the shown COMMIT (diffed against
its first parent), in a `magit-log-mode' buffer the commit at
point — or, with a region marked, the COMBINED diff of all the
commits in the region — in a `magit-diff-mode' buffer showing a
range the RANGE, and the uncommitted changes (git diff HEAD)
anywhere else.  With one prefix ARG review only staged changes
(git diff --cached).  With two prefix ARGs prompt for a ref and
review changes since it.

Only ONE local review row exists at a time (rendering a new one
deletes the previous LOCAL row), so an older local review buffer
stops responding to G (full reload)."
  (interactive "p")
  (let* ((root (or (magit-toplevel)
                   (user-error "Not inside a git repository")))
         ;; `interactive "p"' passes 1 — NOT nil — for no prefix:
         ;; no-prefix and single-prefix-free both mean "the buffer's
         ;; own diff", only 4 (C-u) and 16 (C-u C-u) are explicit.
         (magit-review (and (member arg '(nil 1))
                            (code-review-local--magit-diff-args)))
         (diff-args (cond
                     ((eq arg 4) "--cached")
                     ((eq arg 16) (read-string "Diff since ref: " "HEAD"))
                     ((and magit-review (car magit-review)))
                     (t "HEAD")))
         (title (or (and magit-review (cadr magit-review))
                    (format "Local changes (%s)" diff-args)))
         (diff (let ((default-directory root))
                 (magit-git-output "diff" "--no-color" diff-args))))
    (if (or (not diff) (string-empty-p diff))
        (message "code-review: no local changes (git diff %s)" diff-args)
      ;; This command is autoloaded WITHOUT `code-review.el' (which
      ;; requires this file, so we cannot require it at the top:
      ;; circular loads signal "Recursive load").  The render path
      ;; needs what lives ONLY there: `code-review-mode' and the
      ;; `code-review-sections-hook' defcustom (the header
      ;; sections).  A runtime require is a no-op once loaded and
      ;; never recursive (by then this file's provide has run).
      (require 'code-review)
      ;; phase 23: REQUIRE mode — the local review refuses to open
      ;; until at least one criteria file covers the touched area
      ;; (the change's REQUIREMENTS must exist before the diff is
      ;; worth reading)
      (code-review-criteria--enforce-local root diff)
      (let ((code-review-section-full-refresh? t))
        (code-review-local--cleanup-rows root diff-args)
        (let ((obj (code-review-local-diff
                    :owner "local"
                    :repo (file-name-nondirectory (directory-file-name root))
                    :number 0 :url nil)))
          (oset obj host root)
          (oset obj base-ref-name diff-args)
          (code-review-db--pullreq-create obj))
        (let ((pr (code-review-db-get-pullreq)))
          (oset pr state "LOCAL")
          (oset pr title title)
          ;; the git diff args ride this column; difftastic uses them
          (oset pr base-ref-name diff-args)
          ;; the ref header inserts this unconditionally
          (oset pr head-ref-name "working tree")
          ;; the repo root: this review may be fetched/rendered from
          ;; a timer where default-directory is arbitrary
          (oset pr host root)
          ;; oset is memory-only for these classes: persist the row
          ;; now so build-buffer's buffer-name lookup sees the state
          (closql-insert (code-review-db) pr t))
        (code-review--build-buffer)))))

;;; The row class.  Every slot maps to a pullreq schema column:
;;; the base class's slot order is the schema column order, and
;;; closql-insert fails loudly on any count mismatch.

(defclass code-review-local-diff (code-review-db-pullreq)
  ()
  "A local diff treated as a PR.  `state' is \"LOCAL\".")

;;; Render machinery: mirror the forge classes so the standard
;;; build-buffer path (including full reload, G) works unchanged.

(cl-defmethod code-review-diff-deferred ((obj code-review-local-diff))
  "Return the local diff for OBJ as an immediately resolved deferred."
  (deferred:succeed
   `((message . ,(code-review-local--diff-text obj)))))

(cl-defmethod code-review-infos-deferred ((_obj code-review-local-diff) _fallback?)
  "Local diffs have no forge infos: return an empty resolved deferred."
  (deferred:succeed nil))

(cl-defmethod code-review--auth-token-set? ((_local code-review-local-diff) _res)
  "Local diffs never need auth tokens."
  nil)

(cl-defmethod code-review--internal-build ((obj code-review-local-diff) progress res &optional buff-name msg)
  "Build the review buffer for a LOCAL diff from the fetched RES."
  ;; This runs in a timer: the db's current pullreq may have been
  ;; switched to another buffer's PR meanwhile, so re-assert it.
  (setq code-review-db--pullreq-id (oref obj id))
  (progress-reporter-update progress 3)
  (let* ((new-diff (code-review-utils--clean-diff-prefixes
                    (a-get (-first-item res) 'message)))
         ;; phase 13: local reviews have no infos blob — the diff
         ;; comparison still applies (re-reviewing a working tree
         ;; highlights what changed since the last run); the news
         ;; value is stashed in the render-time defvar because
         ;; trigger-hooks only re-derives it from persisted infos.
         (news (code-review-section--compute-new-since-last-review
                new-diff nil)))
    (setq code-review-section--new-since-last-review news)
    (oset obj raw-diff new-diff)
    (closql-insert (code-review-db) obj t)
    ;; phase 13 slice 2: expire the read-tracking marks the new diff
    ;; invalidated (local reviews: the state is local by definition)
    (code-review-viewed--expire (a-get news 'files)))
  (progress-reporter-update progress 5)
  (let ((code-review-section--just-refetched t))
    (code-review--trigger-hooks buff-name nil msg))
  (progress-reporter-done progress))

;;; Local review outcomes (phase 25b): the two honest exits of a
;;; self-review are (1) issues found -> dispatch the pending local
;;; comments to the attached AI agent session as work items, and
;;; (2) complete -> book the budget and commit via magit.  Both are
;;; pure git (L0); the agent transport is a hook, nil = no AI.

(defcustom code-review-local-agent-dispatch-function nil
  "Transport for dispatching local review comments to an agent.
Called with (PROMPT REPO-DIR); nil (the default) means no AI:
the work item falls back to a buffer + the kill ring, so it can
go to any agent by hand.  `code-review-agents-shell.el' fills
this with its reference dispatcher when agent-shell is loaded."
  :group 'code-review
  :type '(choice function (const nil)))

(defun code-review-local--pending-comments ()
  "Pending local comments of the current LOCAL review.
Plists (:path :line :side :body :diffhunk) from the row's
raw-comments (the nodes the local comment flow writes; replies
and fetched comments are not local-review material)."
  (let ((pr (code-review-db-get-pullreq)))
    (cl-loop for c in (oref pr raw-comments)
             when (equal (a-get c 'state) "LOCAL COMMENT")
             append (cl-loop for n in (a-get-in c '(comments nodes))
                             unless (a-get n 'reply?)
                             collect (list :path (a-get n 'path)
                                           :line (a-get n 'line)
                                           :side (a-get n 'side)
                                           :body (a-get n 'bodyText)
                                           :diffhunk (a-get n 'diffHunk))))))

(defun code-review-local-agent--prompt (comments repo)
  "The agent WORK-ITEM prompt for the local COMMENTS of REPO.
Pure: one batched item, each comment anchored at path:line with
the reviewer's text and the diff hunk context.  DRAFT-NOT-PUSH:
the agent fixes UNCOMMITTED in the working tree, the reviewer
inspects and commits via magit.  No transport here."
  (let ((items nil))
    (dolist (c comments)
      (setq items
            (append items
                    (list
                     (format "- %s%s:\n%s\n  Reviewer's finding: %s"
                             (plist-get c :path)
                             (if (plist-get c :line)
                                 (format ":%s" (plist-get c :line))
                               "")
                             (or (and (plist-get c :diffhunk)
                                      (concat "  Hunk context:\n"
                                              (code-review-local--indent
                                               (plist-get c :diffhunk)
                                               "  ")))
                                 "")
                             (plist-get c :body))))))
    (format
     "Review-fix work item from a local code review.

Working tree: %s
Fix HERE, and only here.

The reviewer left the findings below on the uncommitted changes.
Address each one as UNCOMMITTED changes in that working tree.
Do NOT commit, do NOT push, do NOT create or touch any branch:
the reviewer inspects the fixes and commits via magit.

Findings:

%s

When a finding is wrong or unclear, say so in your reply instead
of guessing: the reviewer reads the session."
     repo
     (string-join (or items '("- none")) "\n"))))

(defun code-review-local--indent (text prefix)
  "Every line of TEXT indented by PREFIX."
  (string-join
   (mapcar (lambda (l) (concat prefix l))
           (split-string text "\n"))
   "\n"))

;;;###autoload
(defun code-review-local-comments-send ()
  "Send the pending local comments to the attached AI agent session.
One batched work item; the transport is
`code-review-local-agent-dispatch-function' (agent-shell when
wired).  Without any agent, the work item lands in a buffer and
the kill ring — paste it wherever your agent lives.  This does
NOT book the budget or finish the review: dispatching is not
completing."
  (interactive)
  (unless (code-review-db-local-pr-p)
    (user-error "Only local diff reviews dispatch comments locally"))
  (let* ((comments (code-review-local--pending-comments))
         (pr (code-review-db-get-pullreq))
         (repo (oref pr host)))
    (if (null comments)
        (user-error "No pending local comments to send (RET on a hunk line writes one)")
      (let ((prompt (code-review-local-agent--prompt comments repo)))
        (if code-review-local-agent-dispatch-function
            (progn
              (funcall code-review-local-agent-dispatch-function
                       prompt repo)
              (message "code-review: %d finding(s) handed to the agent session (draft-not-push; finish with code-review-local-finish)"
                       (length comments)))
          ;; L0: no agent wired — nothing evaporates
          (kill-new prompt)
          (with-current-buffer
              (get-buffer-create "*Local Review Findings*")
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert prompt)
              (special-mode)))
          (pop-to-buffer "*Local Review Findings*")
          (message "code-review: no agent wired — the work item is in *Local Review Findings* and the kill ring"))))))

(defun code-review-local--finish ()
  "Stamp the current LOCAL review finished (the budget booking).
The row's estimated cost then counts as spent today via the
budget's finished-today scan.  Idempotent."
  (unless (code-review-db-local-pr-p)
    (user-error "Only local diff reviews finish locally"))
  (let ((pr (code-review-db-get-pullreq)))
    (oset pr finished t)
    (oset pr finished-at (current-time-string))
    (code-review-db-update pr)
    pr))

;;;###autoload
(defun code-review-local-finish ()
  "Complete the local review: book the budget, then magit-status.
The review's estimated cost counts as spent today (honest
accounting: the attention was spent); the review leaves the
unfinished queue.  Then opens magit-status on the repository —
you commit the changes yourself."
  (interactive)
  (let* ((pr (code-review-local--finish))
         (root (oref pr host)))
    (message "code-review: local review booked (%.1f min estimated); committing via magit"
             (code-review-budget--cost
              (code-review-budget--shape
               (oref pr raw-diff))))
    ;; magit loads transitively with the package; require lazily so
    ;; this file never depends on it at load time
    (require 'magit-status nil t)
    (if (fboundp 'magit-status)
        (magit-status root)
      (user-error "magit-status unavailable; open the repository yourself"))))

(provide 'code-review-local)
;;; code-review-local.el ends here
