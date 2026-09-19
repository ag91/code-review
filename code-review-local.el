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

;;;###autoload
(defun code-review-review-local-diff (&optional arg)
  "Review the local diff as if it were a PR.
With no prefix ARG review all uncommitted changes (git diff HEAD).
With one prefix arg review only staged changes (git diff --cached).
With two prefix args prompt for a ref and review changes since it."
  (interactive "p")
  (let* ((root (or (magit-toplevel)
                   (user-error "Not inside a git repository")))
         (diff-args (pcase arg
                      (4 "--cached")
                      (16 (read-string "Diff since ref: " "HEAD"))
                      (_ "HEAD")))
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
          (oset pr title (format "Local changes (%s)" diff-args))
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
