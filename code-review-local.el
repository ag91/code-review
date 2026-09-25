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
;;  drill-down.  The "PR" is a local diff; comments and reactions
;;  are read-only because there is no forge to send them to.
;;
;;  The pullreq row uses state "LOCAL" as its marker.  It has NO
;;  extra slots: the closql schema is fixed, so the git diff args
;;  ride the existing `base-ref-name' column and the worktree is
;;  derived from `magit-toplevel' at render time.
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

(declare-function code-review--build-buffer "code-review-section")

(defun code-review-local--diff-text (obj)
  "Return the output of the local `git diff' for OBJ.
The repo root comes from OBJ's `host' slot, so this is safe to run
from a timer where `default-directory' is arbitrary."
  (let ((default-directory (file-name-as-directory (oref obj host))))
    (magit-git-output "diff" "--no-color" (oref obj base-ref-name))))

(defun code-review-local--cleanup-rows ()
  "Delete stale LOCAL rows from the database."
  (let ((db (code-review-db)))
    (dolist (row (emacsql db [:select [id] :from 'pullreq
                                    :where (= state "LOCAL")]))
      (ignore-errors
        (closql-delete (closql-get db (car row) 'code-review-db-pullreq))))))

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
      (let ((code-review-section-full-refresh? t))
        (code-review-local--cleanup-rows)
        (code-review-db--pullreq-create
         (code-review-local-diff
          :owner "local"
          :repo (file-name-nondirectory (directory-file-name root))
          :number 0
          :url nil))
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

;;; The row class.  NO extra slots: every slot must map to a pullreq
;;; schema column or closql-insert fails with a column count error.

(defclass code-review-local-diff (code-review-db-pullreq)
  ((callback :initform nil))
  "A local diff treated as a PR.  `state' is \"LOCAL\".
The `callback' slot exists only to match the pullreq schema column
layout shared with the forge classes; it is never used.")

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
  (oset obj raw-diff
        (code-review-utils--clean-diff-prefixes (a-get (-first-item res) 'message)))
  (closql-insert (code-review-db) obj t)
  (progress-reporter-update progress 5)
  (code-review--trigger-hooks buff-name nil msg)
  (progress-reporter-done progress))

(provide 'code-review-local)
;;; code-review-local.el ends here
