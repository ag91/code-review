;;; code-review-viewed.el --- Phase 13 read tracking: mark files/hunks viewed -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026 Andrea
;;
;; Author: Wanderson Ferreira <https://github.com/wandersoncferreira>
;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Phase 13 slice 2: the explicit read tracking.  `v' marks the file
;; or hunk at point viewed (C-u unmarks).  State lives in the db's
;; `reviewed' table (one row per file or hunk), so it survives
;; re-renders and restarts and works for every forge backend and
;; local diff reviews.
;;
;; GitHub file marks mirror viewerViewedState.  Hunk marks and other
;; backends are local.  File marks expire on changed diff blocks; hunk
;; marks expire when their headers change.  Completing a file's hunks
;; also marks it viewed on GitHub.  Sync errors restore the previous
;; state without touching another review or a newer marking operation.

;;; Code:

(require 'cl-lib)
(require 'a)
(require 'magit-section)
(require 'code-review-db)
(require 'code-review-diff)
(require 'code-review-interfaces)

(declare-function code-review--build-buffer "code-review-section")
(declare-function code-review-utils--log "code-review-utils")
(declare-function code-review-section--file-collapse-p "code-review-section-shared")
(defvar code-review-review-buffer-pr-id)
(defvar code-review-section--viewed)
(defvar code-review-section-full-refresh?)
(defvar code-review-section--new-since-last-review)
(defvar code-review-section--just-refetched)
(defvar code-review-commit-minor-mode)
(defvar code-review-section--file-classifications)

(defvar-local code-review-hide-viewed-files nil
  "Hide files marked viewed in this review buffer.")
(put 'code-review-hide-viewed-files 'permanent-local t)

(defvar code-review-viewed--changed-target nil
  "Mark or unmark target whose visibility should change during this render.")
(defvar code-review-viewed--pending (make-hash-table :test #'equal)
  "Latest outstanding GitHub mutation, keyed by (PR-ID PATH).")
(defvar code-review-viewed--hunk-key-cache nil
  "One cached (DIFF . HUNK-KEYS) snapshot, shared across index rebuilds.")

;;;###autoload
(defun code-review-viewed-toggle (&optional unmark?)
  "Mark the file or hunk at point viewed.
With UNMARK? (\\[universal-argument]) unmark instead of mark.
At a file section this marks the whole file viewed; at a hunk
section, only that hunk (keyed by the hunk's diff header, so the
mark expires when the diff changes it).  File-level marks on
GitHub PRs sync with GitHub's Viewed checkbox optimistically;
when the last hunk of a file is marked, the file's sync fires
too.  Local diff reviews and other forges keep the state in the
db only."
  (interactive "P")
  (when (bound-and-true-p code-review-commit-minor-mode)
    (user-error "Read tracking applies to the full review, not a commit view"))
  (when code-review-review-buffer-pr-id
    (setq code-review-db--pullreq-id code-review-review-buffer-pr-id))
  (let* ((section (magit-current-section))
         (target (code-review-viewed--target-at section)))
    (unless target
      (user-error "Point is not on a file or hunk section"))
    (let* ((path (plist-get target :path))
           (hunk-key (plist-get target :hunk-key))
           (index (code-review-viewed--index))
           (info (gethash path index))
           (file-viewed? (code-review-viewed--file-p info))
           (viewed? (if hunk-key
                        (or (plist-get info :file)
                            (member hunk-key (plist-get info :hunks)))
                      file-viewed?))
           (before (code-review-viewed--path-rows path))
           (mark? (not unmark?)))
      (cond
       ((and mark? viewed?)
        (message "%s is already marked viewed" path))
       ((and (not mark?) (if hunk-key (not viewed?) (null before)))
        (message "%s is not marked viewed" path))
       (t
        (when (gethash (list code-review-db--pullreq-id path)
                       code-review-viewed--pending)
          (user-error "Viewed state for %s is syncing; wait before changing it"
                      path))
        (code-review-viewed--set-state path hunk-key mark? info)
        (let* ((after-info (gethash path (code-review-viewed--index)))
               (after-viewed? (code-review-viewed--file-p after-info))
               (code-review-viewed--changed-target
                (append target (list :unmark unmark?))))
          (code-review-viewed--refresh (current-buffer))
          (when (and (cl-typep (code-review-db-get-pullreq)
                              'code-review-github-repo)
                     (not (eq (not file-viewed?) (not after-viewed?))))
            (code-review-viewed--sync-file path after-viewed? before))))))))

(defun code-review-viewed--path-rows (path)
  "Return the current review's marks for PATH."
  (cl-remove-if-not (lambda (row) (equal path (plist-get row :path)))
                    (code-review-db--reviewed-rows)))

(defun code-review-viewed--set-state (path key mark? info)
  "Set PATH or KEY to MARK?, using its previous index entry INFO."
  (closql-with-transaction (code-review-db)
    (cond
     ((null key)
      (code-review-db--reviewed-delete-path path)
      (when mark? (code-review-db--reviewed-insert path)))
     (mark? (code-review-db--reviewed-insert path key))
     (t
      ;; A file-level mark implicitly covers every hunk.  Split it
      ;; before unmarking one, preserving the other hunks' progress.
      (when (plist-get info :file)
        (code-review-db--reviewed-delete path)
        (dolist (other (plist-get info :keys))
          (unless (equal key other)
            (code-review-db--reviewed-insert path other))))
      (code-review-db--reviewed-delete path key)))))

(defun code-review-viewed--refresh (buffer)
  "Re-render BUFFER from cached data, without fetching or losing point."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((code-review-section-full-refresh? nil)
            (position (point)))
        (code-review--build-buffer (buffer-name))
        (goto-char (min position (point-max)))))))

(defun code-review-viewed--target-at (section)
  "The read-tracking target, a (:path .. :hunk-key ..) plist, for
the magit SECTION at point, or nil."
  (cond
   ((and section (slot-boundp section 'value)
         (stringp (oref section value))
         (cl-typep section 'magit-file-section))
    (list :path (or (and (slot-boundp section 'start)
                        (oref section start)
                        (get-text-property (oref section start)
                                           'code-review-path))
                   (code-review-viewed--strip-path (oref section value)))
          :hunk-key nil))
   ((and section (slot-boundp section 'value)
         (cl-typep section 'magit-hunk-section))
    (let ((v (oref section value)))
      (when (and (listp v)
                 (cdr (assq 'path v))
                 (cdr (assq 'ranges v)))
        ;; The owned washer stores an already canonical hunk path.
        (list :path (substring-no-properties (cdr (assq 'path v)))
              :hunk-key (cdr (assq 'ranges v))))))))

(defun code-review-viewed--strip-path (path)
  "Strip the a/ b/ diff prefixes from PATH, if present."
  (substring-no-properties
   (cond ((string-prefix-p "a/" path) (substring path 2))
        ((string-prefix-p "b/" path) (substring path 2))
        (t path))))

;;; Render index

(defun code-review-viewed--diff-keys ()
  "Return current hunk keys, parsing only when the diff snapshot changes."
  (let ((diff (or (code-review-db--pullreq-raw-diff) "")))
    (unless (equal diff (car code-review-viewed--hunk-key-cache))
      (setq code-review-viewed--hunk-key-cache
            (cons diff (code-review-diff--hunk-keys diff))))
    (cdr code-review-viewed--hunk-key-cache)))

(defun code-review-viewed--index ()
  "Index current diff paths, excluding stale marks for removed files.
Each entry has :keys (current headers), :file (a whole-file mark),
:hunks (viewed headers), and :all-hunks (the completed rollup)."
  (let* ((diff-keys (code-review-viewed--diff-keys))
         (index (make-hash-table :test 'equal)))
    (pcase-dolist (`(,path . ,keys) diff-keys)
      (puthash path (list :keys keys) index))
    (dolist (row (code-review-db--reviewed-rows))
      (let* ((path (plist-get row :path))
             (key (plist-get row :hunk-key))
             (info (gethash path index)))
        (when info
          (if key
              (when (member key (plist-get info :keys))
                (setq info (plist-put info :hunks
                                      (cons key (plist-get info :hunks)))))
            (setq info (plist-put info :file t)))
          (puthash path info index))))
    (maphash
     (lambda (path info)
       (puthash path
                (plist-put info :all-hunks
                           (and (plist-get info :keys)
                                (cl-every
                                 (lambda (key)
                                   (member key (plist-get info :hunks)))
                                 (plist-get info :keys))))
                index))
     index)
    index))

(defun code-review-viewed--file-p (info)
  "Return non-nil when the indexed file INFO is fully viewed."
  (or (plist-get info :file) (plist-get info :all-hunks)))

(defun code-review-viewed--count-files ()
  "How many of the diff's files are viewed (the heading counter)."
  (when (hash-table-p code-review-section--viewed)
    (let ((viewed 0))
      (maphash (lambda (_path vinfo)
                 (when (code-review-viewed--file-p vinfo)
                   (setq viewed (1+ viewed))))
               code-review-section--viewed)
      viewed)))

;;; Expiry: GitHub's reset semantics, locally

(defun code-review-viewed--expire (news-files)
  "Drop the read-tracking rows that the new diff invalidated.
NEWS-FILES = the files whose diff blocks changed in this fetch
(the phase 13 news).  Hunk rows whose hunk-key left the diff and
file rows whose file changed are dropped; for GitHub PRs the
seed (the same fetch) rewrites the file rows afterwards anyway."
  (let ((keys (code-review-viewed--diff-keys)))
    (maphash (lambda (key _)
               (when (equal (car key) code-review-db--pullreq-id)
                 (remhash key code-review-viewed--pending)))
             code-review-viewed--pending)
    (code-review-db--reviewed-delete-invalid
     (lambda (path) (not (member path news-files)))
     keys)))

;;; GitHub sync

(defun code-review-viewed--seed-github ()
  "Reconcile fetched files' viewerViewedState with local marks.
GitHub wins for known paths.  Missing fields or unfetched pages
leave local state alone.  Partial hunk progress remains local."
  (let* ((infos (code-review-db--pullreq-raw-infos))
         (files (a-get-in infos '(files nodes)))
         (index (code-review-viewed--index)))
    (closql-with-transaction (code-review-db)
      (dolist (file files)
        (let* ((path (a-get file 'path))
               (state (a-get file 'viewerViewedState))
               (info (gethash path index)))
          (when (and path (member state '("VIEWED" "UNVIEWED" "DISMISSED")))
            (if (equal state "VIEWED")
                (unless (plist-get info :file)
                  (code-review-db--reviewed-insert path))
              ;; Remote resets must not be undone by an old local rollup.
              ;; Partial progress survives an UNVIEWED file; DISMISSED
              ;; means the reviewed content changed, invalidating it all.
              (when (or (and (equal state "DISMISSED")
                             (plist-get info :hunks))
                        (code-review-viewed--file-p info))
                (code-review-db--reviewed-delete-path path)))))))))

(defun code-review-viewed--sync-file (path mark? before)
  "Push PATH's viewed state (MARK? non-nil = viewed) to the forge
optimistically.  An error reverts the local row and messages the
user; the review keeps working offline."
  (let* ((pr (code-review-db-get-pullreq))
         (pr-id code-review-db--pullreq-id)
         (database-file code-review-db-database-file)
         (buffer (current-buffer))
         (after (code-review-viewed--path-rows path))
         (key (list pr-id path))
         (token (make-symbol "viewed-sync"))
         (success (lambda (&rest _)
                    (when (eq token (gethash key code-review-viewed--pending))
                      (remhash key code-review-viewed--pending))))
         (failure
          (lambda (&rest _)
            (when (and (equal database-file code-review-db-database-file)
                       (eq token (gethash key code-review-viewed--pending)))
              (remhash key code-review-viewed--pending)
              (let ((code-review-db--pullreq-id pr-id))
                (when (and (code-review-db-get-pullreq)
                           (equal after (code-review-viewed--path-rows path)))
                  (code-review-db--reviewed-restore-path path before)
                  (when (and (buffer-live-p buffer)
                             (equal pr-id
                                    (buffer-local-value
                                     'code-review-review-buffer-pr-id buffer)))
                    (save-window-excursion
                      (save-current-buffer
                        (code-review-viewed--refresh buffer))))))
              (code-review-utils--log "code-review-viewed"
                                      (format "Viewed sync failed for %s" path))
              (message "code-review: could not %s %s on GitHub; local state restored"
                       (if mark? "mark" "unmark") path)))))
    (when (gethash key code-review-viewed--pending)
      (user-error "Viewed state for %s is already syncing" path))
    (puthash key token code-review-viewed--pending)
    (condition-case nil
        (code-review-toggle-file-viewed pr path mark? success failure)
      (error (funcall failure)))))

;;; Visibility

;;;###autoload
(defun code-review-toggle-hide-viewed-files ()
  "Hide or show reviewed files without changing their marks."
  (interactive)
  (when (bound-and-true-p code-review-commit-minor-mode)
    (user-error "Read tracking applies to the full review, not a commit view"))
  (when code-review-review-buffer-pr-id
    (setq code-review-db--pullreq-id code-review-review-buffer-pr-id))
  (setq code-review-hide-viewed-files (not code-review-hide-viewed-files))
  (code-review-viewed--refresh (current-buffer))
  (message "code-review: viewed files %s"
           (if code-review-hide-viewed-files "hidden" "shown")))

(defun code-review-viewed--filter-diff (diff classifications)
  "Omit viewed files from DIFF when requested by this buffer.
CLASSIFICATIONS retains all paths, so the counter still includes hidden files."
  (if (not code-review-hide-viewed-files)
      diff
    (let* ((start (string-match "^diff --git " diff))
           (prefix (if start (substring diff 0 start) diff)))
      (concat prefix
              (mapconcat
               #'cdr
               (cl-remove-if
                (lambda (block)
                  (plist-get (gethash (car block) classifications) :viewed))
                (code-review--diff--split-by-files diff))
               "")))))

(defun code-review-viewed--apply-visibility (root defaults?)
  "Apply read-tracking folds under ROOT.
DEFAULTS? enables initial or post-fetch folding.  Ordinary re-renders
preserve deliberate expansions; only a just-changed target overrides them."
  (when (and root (hash-table-p code-review-section--viewed))
    (magit-map-sections
     (lambda (section)
       (when-let* ((target (code-review-viewed--target-at section)))
         (let* ((path (plist-get target :path))
                (key (plist-get target :hunk-key))
                (info (gethash path code-review-section--viewed))
                (changed code-review-viewed--changed-target)
                (same-path (equal path (plist-get changed :path)))
                (selected (and same-path
                               (or (null (plist-get changed :hunk-key))
                                   (equal key (plist-get changed :hunk-key)))))
                (viewed? (if key
                             (or (plist-get info :file)
                                 (member key (plist-get info :hunks)))
                           (code-review-viewed--file-p info))))
           (cond
            ((and selected (plist-get changed :unmark))
             (magit-section-show section))
            ((and same-path (null key) (plist-get changed :unmark))
             (magit-section-show section))
            ((and viewed? (or defaults? selected
                              (and same-path (null key) changed)))
             (magit-section-hide section))
            ((and defaults? code-review-section--just-refetched
                  (member path
                          (a-get code-review-section--new-since-last-review
                                 'files))
                  (or key
                      (not (code-review-section--file-collapse-p
                            path (gethash path
                                          code-review-section--file-classifications))))
                  (not viewed?))
             (magit-section-show section))))))
     root)))

(provide 'code-review-viewed)
;;; code-review-viewed.el ends here
