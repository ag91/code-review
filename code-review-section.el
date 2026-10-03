;;; code-review-section.el --- UI -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Wanderson Ferreira
;;
;; Author: Wanderson Ferreira <https://github.com/wandersoncferreira>
;; Maintainer: Wanderson Ferreira <wand@hey.com>
;; Version: 0.0.7
;; Homepage: https://github.com/wandersoncferreira/code-review
;;
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
;;  Code to build the UI.
;;
;;; Code:

(require 'emojify)
(require 'deferred)
(require 'magit-section)
(require 'magit-diff)
(require 'cl-lib)
(require 'shr)

(require 'code-review-faces)
(require 'code-review-db)
(require 'code-review-utils)
(require 'code-review-repo)
(require 'code-review-diff)
(require 'code-review-analysis)
(require 'code-review-history)
(require 'code-review-hunkhighlight)
(require 'code-review-reactions)

(declare-function code-review--diff--classify-diff "code-review-diff")
(declare-function code-review--maybe-reorder-diff "code-review-diff")
(declare-function code-review-browse--reveal "code-review-browse")
(declare-function code-review-browse--strip-diff-prefix "code-review-browse")

(require 'code-review-interfaces)
(require 'code-review-github)
(require 'code-review-gitlab)
(require 'code-review-bitbucket)

;;; Part files (phase 15c split): required here so every
;;; `code-review-section' require site sees all of it.
(require 'code-review-section-shared)
(require 'code-review-section-header)
(require 'code-review-section-analysis)
(require 'code-review-section-criteria)
(require 'code-review-section-comment)
(require 'code-review-section-wash)


(defcustom code-review-buffer-name "*Code Review*"
  "Fallback name of the code review main buffer.
Buffers are normally named after the reviewed PR
\(see `code-review-pr-buffer-name') so that several reviews can
stay open at once."
  :group 'code-review
  :type 'string)

(defvar-local code-review-review-buffer-pr-id nil
  "Database id of the pull request displayed in this review buffer.")
(put 'code-review-review-buffer-pr-id 'permanent-local t)

(defvar-local code-review-comment-review-buffer nil
  "Review buffer a comment buffer was opened for.")
(put 'code-review-comment-review-buffer 'permanent-local t)

(defun code-review-local--buffer-name-hint (args)
  "Compact buffer-name hint for a LOCAL review's diff ARGS.
Empty for the classic args (HEAD, --cached) so existing local
review buffer names stay unchanged; \"@ REV\" for a commit review
(7-char short form when REV is a full sha), \"@ A..B\" for a range
review (each full-sha side shortened, the first-parent \"^\"
dropped: \"OLD^..NEW\" shows as \"OLD..NEW\"), \"@ ARGS\"
otherwise."
  (cond
   ((or (not args)
        (member args '("HEAD" "--cached")))
    "")
   ;; REV^..REV / REV^!: the commit-review args, same REV both sides
   ((string-match "\\`\\(.+\\)\\^\\.\\.\\1\\'" args)
    (format " @ %s"
            (substring args (match-beginning 1)
                       (min (match-end 1) (+ (match-beginning 1) 7)))))
   ((string-match "\\`\\(.+\\)\\^!\\'" args)
    (format " @ %s"
            (substring args (match-beginning 1)
                       (min (match-end 1) (+ (match-beginning 1) 7)))))
   ;; a range: OLD^..NEW (log-region review) or any other A..B.
   ;; Bind both sides BEFORE the side-shrinking matches: the global
   ;; match data is clobbered by any nested string-match.
   ((string-match "\\`\\(.+\\)\\.\\.\\(.+\\)\\'" args)
    (let* ((a (match-string 1 args))
           (b (match-string 2 args))
           (shrink (lambda (side)
                     (if (string-match
                          "\\`\\([0-9a-f]\\{7\\}\\)[0-9a-f]*\\'" side)
                         (match-string 1 side)
                       side))))
      (when (string-suffix-p "^" a)
        (setq a (substring a 0 -1)))
      (format " @ %s..%s" (funcall shrink a) (funcall shrink b))))
   (t (format " @ %s" args))))

(defun code-review-pr-buffer-name (&optional pr)
  "Return the review buffer name for PR (defaults to the DB current pullreq).
Falls back to `code-review-buffer-name' when the PR cannot be
determined."
  (let ((pr (or pr (ignore-errors (code-review-db-get-pullreq)))))
    (if (and pr
             (slot-boundp pr 'owner)
             (slot-boundp pr 'repo)
             (slot-boundp pr 'number))
        (if (equal (oref pr state) "LOCAL")
            (format "*Code Review: local: %s%s*"
                    (replace-regexp-in-string "%2F" "/" (oref pr repo))
                    (code-review-local--buffer-name-hint
                     (and (slot-boundp pr 'base-ref-name)
                          (oref pr base-ref-name))))
          (format "*Code Review: %s/%s#%s*"
                  (oref pr owner)
                  (replace-regexp-in-string "%2F" "/" (oref pr repo))
                  (oref pr number)))
      code-review-buffer-name)))

(defun code-review-review-buffer ()
  "Return the review buffer for the PR being acted on, or nil.
Prefer the current buffer when it already is a review buffer, then
the review buffer recorded in a comment buffer, then the buffer
named after the DB's current pullreq."
  (cond ((and (bound-and-true-p code-review-review-buffer-pr-id)
              (buffer-live-p (current-buffer)))
         (current-buffer))
        ((and (bound-and-true-p code-review-comment-review-buffer)
              (buffer-live-p code-review-comment-review-buffer))
         code-review-comment-review-buffer)
        (t (get-buffer (code-review-pr-buffer-name)))))

(defun code-review--sync-db-pullreq ()
  "Make the DB current pullreq match the PR shown in this buffer."
  (when (and code-review-review-buffer-pr-id
             (not (equal code-review-db--pullreq-id
                         code-review-review-buffer-pr-id)))
    (setq code-review-db--pullreq-id code-review-review-buffer-pr-id)))

(defcustom code-review-commit-buffer-name "*Code Review Commit*"
  "Name of the code review commit buffer."
  :group 'code-review
  :type 'string)

(defcustom code-review-new-buffer-window-strategy
  #'switch-to-buffer-other-window
  "Function used after create a new Code Review buffer."
  :group 'code-review
  :type 'function)

;; fix unbound symbols
(defvar magit-root-section)
(defvar code-review-comment-commit-buffer?)
(defvar code-review-comment-cursor-pos)


(declare-function code-review-promote-comment-to-new-issue "code-review")
(declare-function code-review-utils--visit-binary-file-at-remote "code-review-utils")
(declare-function code-review-utils--visit-binary-file-at-point "code-review-utils")
(declare-function code-review-toggle-resolved "code-review-interfaces"
  (obj thread-id resolve? callback))

;;; * build buffer

(defclass code-review--root-section (magit-section)
  ((body :initform nil)))

(defun code-review-section--setup-worktree ()
  "Bind `code-review-repo-worktree' and `default-directory' for the
render.  Local repository context: worktree checked out at PR
head; for a local diff review the repository itself is the worktree."
  (if (code-review-db-local-pr-p)
      (when (and code-review-repo-enable
                 (not code-review-repo-worktree))
        (setq code-review-repo-worktree
              (or (let ((root (oref (code-review-db-get-pullreq) host)))
                    (and root (file-name-as-directory root)))
                   (magit-toplevel))))
    (when (and code-review-repo-enable
               (or (not code-review-repo-worktree)
                   code-review-section-full-refresh?))
      (condition-case err
          (code-review-repo-setup (code-review-db-get-pullreq))
        (error (message "code-review: repo setup failed: %s"
                        (error-message-string err))))))
  (when code-review-repo-worktree
    (setq default-directory
          (file-name-as-directory code-review-repo-worktree))))

(defun code-review-section--insert-pr-text ()
  "Insert the (reordered) raw PR diff text into the review buffer.
Phase 14 heat is computed (or loaded) before the wash: a cold
cache kicks the async harvest and stays nil here."
  (save-excursion
    (code-review-history-prepare
     (code-review-db--pullreq-raw-diff)
     code-review-repo-worktree)
    (setq code-review-section--file-classifications
          (code-review--diff--classify-diff
           (code-review-db--pullreq-raw-diff)))
    (erase-buffer)
    (insert (code-review--maybe-reorder-diff
             (code-review-db--pullreq-raw-diff)
             code-review-section--file-classifications))
    (insert ?\n)))

(defun code-review-section--render-diff-tree (commit-focus?)
  "Render the section tree over the inserted diff text: the header
sections (via `code-review-sections-hook', or
`code-review-sections-commit-hook' with COMMIT-FOCUS?) and the
washed diff under the files-report container."
  (magit-insert-section section (code-review--root-section)
    (magit-insert-section (code-review)
      (magit-run-section-hook (if commit-focus?
                                  'code-review-sections-commit-hook
                                'code-review-sections-hook)))
    (magit-insert-section (code-review-files-report-section)
      (code-review-section-insert-files-changed)
      (magit-insert-section (code-review-files-chnged)
        (save-restriction
          (narrow-to-region (point) (point-max))
          (run-hooks 'magit-diff-wash-diffs-hook)
          (magit-wash-sequence
           #'code-review-wash-diff))))))

(defun code-review-section--fold-and-mark (fresh-render?)
  "Fold bot-authored comment threads (AI chatter) and add the
fringe markers on diff lines carrying a thread.  The fold runs
only on a FRESH render (FRESH-RENDER? non-nil): on re-render magit
inherits each section's previous visibility, and re-folding would
clobber sections the user deliberately expanded.  The fringe
markers are needed on EVERY render (erasing the buffer kills the
overlays), and are cheap enough (one section-tree walk) to always
be worth it."
  (when (and code-review-collapse-bot-comments
             fresh-render?)
    (code-review--collapse-bot-comments magit-root-section))
  (when code-review-comment-fringe-markers
    (code-review--mark-comment-lines)))

(defun code-review-section--activate-review-buffer (buff-name window ws
                                                    commit-focus?)
  "Display BUFF-NAME (restoring WINDOW's start WS) and switch it
into `code-review-mode' with its per-buffer bookkeeping.  Done in
this order because `code-review-mode' kills local variables and
hooks."
  (if window
      (progn
        (pop-to-buffer buff-name)
        (set-window-start window ws))
    (progn
      (funcall code-review-new-buffer-window-strategy buff-name)
      (goto-char (point-min))))
  (code-review-mode)
  ;; per-PR review buffers: remember which PR this buffer
  ;; shows, and make commands issued here act on that PR.
  (setq code-review-review-buffer-pr-id code-review-db--pullreq-id)
  (add-hook 'post-command-hook #'code-review--sync-db-pullreq nil t)
  ;; sticky file name while reading deep inside a hunk
  (setq header-line-format nil)
  (add-hook 'post-command-hook #'code-review--update-header-line
            nil t)
  (add-hook 'window-scroll-functions #'code-review--update-header-line
            nil t)
  (when commit-focus?
    (code-review-commit-minor-mode 1))
  (code-review-section-insert-header-title))

(defun code-review--trigger-hooks (buff-name &optional commit-focus? msg)
  "Trigger magit section hooks and draw BUFF-NAME.
Run code review commit buffer hook when COMMIT-FOCUS? is non-nil.
If you want to display a minibuffer MSG in the end."
  (setq code-review-section-grouped-comments
        (code-review-utils-make-group
         (code-review-db--pullreq-raw-comments))
        code-review-section-hold-written-comment-count nil
        code-review-section-hold-written-comment-ids nil)
  (with-current-buffer (get-buffer-create buff-name)
    (code-review-section--setup-worktree)
    (let* ((window (get-buffer-window buff-name))
           (ws (window-start window))
           (inhibit-read-only t)
           ;; before the render replaces it: t when this buffer
           ;; has never been rendered before
           (fresh-render? (not magit-root-section)))
      (code-review-section--insert-pr-text)
      (code-review-section--render-diff-tree commit-focus?)
      (code-review-section--fold-and-mark fresh-render?)
      (code-review-section--activate-review-buffer
       buff-name window ws commit-focus?)
      (when code-review-comment-cursor-pos
        (goto-char code-review-comment-cursor-pos))
      (when msg
        (message nil)
        (message msg))
      ;; Run post hook after everything is rendered and mode is active
      (run-hooks 'code-review-post-hook))))

(cl-defmethod code-review--auth-token-set? ((_github code-review-github-repo) res)
  "Check if the RES has a message for auth token not set for GITHUB."
  (string-prefix-p "Required Github token" (-first-item (a-get res 'error))))

(cl-defmethod code-review--auth-token-set? ((_gitlab code-review-gitlab-repo) res)
  "Check if the RES has a message for auth token not set for GITLAB."
  (string-prefix-p "Required Gitlab token" (-first-item (a-get res 'error))))

(cl-defmethod code-review--auth-token-set? ((_bitbucket code-review-bitbucket-repo) res)
  "Check if the RES has a message for auth token not set for BITBUCKET."
  (string-prefix-p "Required Bitbucket token" (-first-item (a-get res 'error))))

(cl-defmethod code-review--auth-token-set? (obj res)
  "Default catch all unknown values passed to this function as OBJ and RES."
  (code-review-utils--log
   "code-review--auth-token-set?"
   (string-join (list
                 (prin1-to-string obj)
                 (prin1-to-string res))
                " <->"))
  (error "Unknown backend obj created.  Look at `code-review-log-file' and report the bug upstream"))

(cl-defmethod code-review--internal-build ((obj code-review-github-repo) progress res &optional buff-name msg)
  "Helper function to build process for GITHUB based on the fetched RES informing PROGRESS."
  ;; This runs in a timer: the db's current pullreq may have been
  ;; switched to another buffer's PR meanwhile, so re-assert it.
  (setq code-review-db--pullreq-id (oref obj id))
  (let* ((errors-complete-query (alist-get 'errors (-second-item res)))
         (raw-infos-complete (a-get-in (cdr (-second-item res)) (list 'repository 'pullRequest)))
         (raw-infos-fallback (a-get-in (cdr (-third-item res)) (list 'repository 'pullRequest)))
         (raw-infos
          (if (not raw-infos-complete)
              raw-infos-fallback
            raw-infos-complete)))

    (when errors-complete-query
      (code-review-utils--log "code-review--internal-build"
                              (format "Data returned by GraphQL API: \n %s" (prin1-to-string res)))
      (message "GraphQL Github data contains errors. See `code-review-log-file' for details."))

    ;; verify must have value!
    (let-alist raw-infos
      (when (not .headRefOid)
        (code-review-utils--log "code-review--internal-build"
                                "Commit SHA not returned by GraphQL Github API. See `code-review-log-file' for details")
        (code-review-utils--log "code-review--internal-build"
                                (format "Data returned by GraphQL API: \n %s" (prin1-to-string res)))
        (error "Missing required data")))

    ;; 1. save raw diff data
    (progress-reporter-update progress 3)
    (code-review-db--pullreq-raw-diff-update
     (code-review-utils--clean-diff-prefixes
      (a-get (-first-item res) 'message)))

    ;; 1.1 save raw info data e.g. data from GraphQL API
    (progress-reporter-update progress 4)
    (code-review-db--pullreq-raw-infos-update
     (code-review-github-fix-infos raw-infos))

    ;; 1.2 trigger renders
    (progress-reporter-update progress 5)
    (code-review--trigger-hooks buff-name nil msg)
    (progress-reporter-done progress)))

(cl-defmethod code-review--internal-build ((obj code-review-gitlab-repo) progress res &optional buff-name msg)
  "Helper function to build process for GITLAB based on the fetched RES informing PROGRESS."
  ;; This runs in a timer: the db's current pullreq may have been
  ;; switched to another buffer's PR meanwhile, so re-assert it.
  (setq code-review-db--pullreq-id (oref obj id))

  (when-let (err (a-get (-second-item res) 'errors))
    (code-review-utils--log
     "code-review--internal-build"
     (format "Data returned by GraphQL API: \n%s" (prin1-to-string err))))

  ;; 1. save raw diff data
  (progress-reporter-update progress 3)
  (code-review-db--pullreq-raw-diff-update
   (code-review-gitlab-fix-diff
    (a-get (-first-item res) 'changes)))

  ;; 1.1. compute position line numbers to diff line numbers
  (progress-reporter-update progress 4)
  (code-review-gitlab-pos-line-number->diff-line-number
   (a-get (-first-item res) 'changes))

  ;; 1.2. save raw info data e.g. data from GraphQL API
  (progress-reporter-update progress 5)
  (code-review-db--pullreq-raw-infos-update
   (code-review-gitlab-fix-infos
    (code-review-github-fix-infos
     (a-get-in (-second-item res) (list 'data 'repository 'pullRequest)))))

  ;; 1.3. trigger renders
  (progress-reporter-update progress 6)
  (code-review--trigger-hooks buff-name nil msg)
  (progress-reporter-done progress))

(cl-defmethod code-review--internal-build ((obj code-review-bitbucket-repo) progress res &optional buff-name msg)
  "Helper function to build process for BITBUCKET based on the fetched RES informing PROGRESS."
  ;; This runs in a timer: the db's current pullreq may have been
  ;; switched to another buffer's PR meanwhile, so re-assert it.
  (setq code-review-db--pullreq-id (oref obj id))
  (prin1 (format "RESULT:%s\n" (-second-item res)))
  (let* ((raw-infos (let-alist (-second-item res)
                      `((title . ,.title)
                        (author . ((login . ,.author.nickname)
                                   (url . ,(format "https://%s/%s"
                                                   code-review-bitbucket-base-url
                                                   .author.nickname))))
                        (number . ,.id)
                        (state . ,.state)
                        (bodyHTML . ,.rendered.description.html)
                        (headRef (target (oid . ,.source.commit.hash)))
                        (baseRefName . ,.destination.branch.name)
                        (headRefName . ,.source.branch.name)
                        (comments (nodes . ,.comments.nodes))
                        (commits . ,.commits)
                        (reviews (nodes . ,.reviews.nodes))))))

    ;; 1. save raw diff data
    (progress-reporter-update progress 3)
    (code-review-db--pullreq-raw-diff-update
     (code-review-utils--clean-diff-prefixes
      (a-get (-first-item res) 'message)))

    ;; 1.1. compute position line numbers to diff line numbers
    (progress-reporter-update progress 4)
    (code-review-bitbucket-pos-line-number->diff-line-number
     (a-get (-first-item res) 'message))

    ;; 1.2 save raw info data e.g. data from GraphQL API
    (progress-reporter-update progress 4)
    (code-review-db--pullreq-raw-infos-update
     (code-review-bitbucket--fix-diff-comments raw-infos))

    ;; 1.3 trigger renders
    (progress-reporter-update progress 5)
    (code-review--trigger-hooks buff-name nil msg)
    (progress-reporter-done progress)))

(defcustom code-review-log-raw-request-responses nil
  "Log the Raw request responses from your VC provider."
  :type 'string
  :group 'code-review)

(defun code-review--build-buffer (&optional buf-name commit-focus? msg)
  "Build BUF-NAME set COMMIT-FOCUS? mode to use commit list of hooks.
If you want to provide a MSG for the end of the process.
BUF-NAME defaults to the per-PR buffer name."
  (let ((buff-name (or buf-name (code-review-pr-buffer-name))))
    (if (not code-review-section-full-refresh?)
        (code-review--trigger-hooks buff-name commit-focus? msg)
      (let ((obj (code-review-db-get-pullreq))
            (progress (make-progress-reporter "Fetch diff PR..." 1 6)))
        (progress-reporter-update progress 1)
        (deferred:$
         (deferred:parallel
          (lambda () (code-review-diff-deferred obj))
          (lambda () (code-review-infos-deferred obj))
          (lambda () (code-review-infos-deferred obj t)))
         (deferred:nextc it
                         (lambda (x)
                           (when code-review-log-raw-request-responses
                             (code-review-utils--log
                              "code-review--build-buffer: [DIFF]"
                              (prin1-to-string (-first-item x)))
                             (code-review-utils--log
                              "code-review--build-buffer: [INFOS_main]"
                              (prin1-to-string (-second-item x)))
                             (code-review-utils--log
                              "code-review--build-buffer: [INFOS_fallback]"
                              (prin1-to-string (-third-item x))))

                           (progress-reporter-update progress 2)
                           (if (code-review--auth-token-set? obj x)
                               (progn
                                 (progress-reporter-done progress)
                                 (message "Required %s token. Look at the README for how to setup your Personal Access Token"
                                          (cond
                                           ((code-review-github-repo-p obj)
                                            "Github")
                                           ((code-review-gitlab-repo-p obj)
                                            "Gitlab")
                                           (t "Unknown"))))
                             (code-review--internal-build obj progress x buff-name msg))))
         (deferred:error it
                         (lambda (err)
                           (code-review-utils--log
                            "code-review--build-buffer"
                            (prin1-to-string err))
                           (if (and (sequencep err)
                                    (stringp (-second-item err))
                                    (string-prefix-p "BUG: Unknown extended header:" (-second-item err)))
                               (message "Your PR might have diffs too large. Currently not supported.")
                             (message "Got an error from your VC provider. Check `code-review-log-file'.")))))))))

;;; * commit buffer
;;; Rebuild commit buffer focusing on a single commit's diff.
(defun code-review-section--build-commit-buffer (buff-name)
  "Build commit buffer review given by BUFF-NAME.
Fetches the commit diff using the backend and renders the buffer
with commit-focused hooks and keybindings."
  (let* ((obj (code-review-db-get-pullreq))
         (progress (make-progress-reporter "Fetch commit diff..." 1 3)))
    (progress-reporter-update progress 1)
    (deferred:$
     (code-review-commit-diff-deferred obj)
     (deferred:nextc it
                     (lambda (res)
                       ;; Save raw diff data into DB and render using commit hooks
                       (progress-reporter-update progress 2)
                       (code-review-db--pullreq-raw-diff-update
                        (code-review-utils--clean-diff-prefixes
                         (a-get res 'message)))
                       (progress-reporter-update progress 3)
                       (code-review--trigger-hooks buff-name t)
                       (progress-reporter-done progress)))
     (deferred:error it
                     (lambda (err)
                       (code-review-utils--log
                        "code-review-section--build-commit-buffer"
                        (prin1-to-string err))
                       (message "Got an error while fetching commit diff. See `code-review-log-file'."))))))






;;; Patch line helpers (skip review UI lines inside hunks)

(defun code-review--section-in-review-subsection-p (&optional section)
  "Return non-nil when SECTION (or current section) is inside a review UI subsection.
Detects comment/reply/local/reactions/outdated comment sections which render
non-patch lines inside hunks."
  (let ((cur (or section (magit-current-section)))
        found)
    (while (and cur (not found))
      (setq found (or (and (fboundp 'code-review-code-comment-section-p)
                           (code-review-code-comment-section-p cur))
                      (and (fboundp 'code-review-reply-comment-section-p)
                           (code-review-reply-comment-section-p cur))
                      (and (fboundp 'code-review-local-comment-section-p)
                           (code-review-local-comment-section-p cur))
                      (and (fboundp 'code-review-reactions-section-p)
                           (code-review-reactions-section-p cur))
                      (and (fboundp 'code-review-outdated-comment-section-p)
                           (code-review-outdated-comment-section-p cur))))
      (setq cur (and (slot-boundp cur 'parent) (oref cur parent))))
    found))

(defun code-review--patch-line-p ()
  "Return non-nil if point is on a patch line inside a Magit hunk.
Skips lines that belong to code-review comment UI subsections."
  (and (magit-hunk-section-p (magit-current-section))
       (not (code-review--section-in-review-subsection-p))
       (let ((c (char-after (line-beginning-position))))
         (or (eq c ?\s) (eq c ?+) (eq c ?-)))))

(defun code-review--hunk-content-start (hunk)
  "Return buffer position of first content line for HUNK (line after header)."
  (save-excursion
    (goto-char (marker-position (oref hunk start)))
    (forward-line 1)
    (line-beginning-position)))

(defun code-review--nearest-patch-line-in-hunk (hunk &optional pos)
  "Find nearest patch line position (BOL) within HUNK from POS (default point).
Prefers previous patch line; if none, uses next patch line; returns nil if none."
  (let ((here (or pos (point)))
        prev next)
    (save-excursion
      ;; Search backward within hunk bounds
      (goto-char here)
      (while (and (>= (point) (marker-position (oref hunk start)))
                  (not prev))
        (when (code-review--patch-line-p)
          (setq prev (line-beginning-position)))
        (forward-line -1))
      ;; Search forward within hunk bounds
      (goto-char here)
      (while (and (< (point) (marker-position (oref hunk end)))
                  (not next))
        (when (code-review--patch-line-p)
          (setq next (line-beginning-position)))
        (forward-line 1)))
    (or prev next)))

(defun code-review--count-patch-advances (hunk start-pos end-pos)
  "Count old/new advances across patch lines from START-POS up to END-POS in HUNK.
Returns cons (OLD . NEW) where OLD counts lines with ' ' or '-', and NEW counts
lines with ' ' or '+'. END-POS is exclusive. Review UI lines are ignored."
  (let ((old 0) (new 0))
    (save-excursion
      (goto-char start-pos)
      (while (< (point) end-pos)
        (when (code-review--patch-line-p)
          (pcase (char-after (line-beginning-position))
            (?\s (setq old (1+ old) new (1+ new)))
            (?+  (setq new (1+ new)))
            (?-  (setq old (1+ old)))))
        (forward-line 1)))
    (cons old new)))








(declare-function difftastic-git-diff-range "difftastic"
                  (&optional rev-or-range args files))

(provide 'code-review-section)
;;; code-review-section.el ends here
