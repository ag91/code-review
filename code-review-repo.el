;;; code-review-repo.el --- Local repository context for code-review -*- lexical-binding: t; -*-

;; Copyright (C) 2026 code-review contributors
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License see
;; <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Resolve the repository a PR belongs to on the local filesystem,
;; clone it on demand when missing, fetch the PR refs, and maintain
;; a git worktree checked out at the PR head.  This gives the Code
;; Review buffer real project context: visiting files, xref, and any
;; project-aware command operate on the exact version of the code
;; being reviewed.

;;; Code:

(require 'magit-git)
(require 'code-review-utils)
(require 'code-review-db)
(require 'code-review-github)
(require 'code-review-gitlab)

(defgroup code-review-repo nil
  "Local repository integration for code-review."
  :group 'code-review)

(defcustom code-review-repo-enable t
  "When non-nil, set up a local worktree for reviewed PRs.
When the repository cannot be located, cloned or fetched, the
review silently falls back to the remote-only behavior."
  :type 'boolean
  :group 'code-review-repo)

(defcustom code-review-projects-root nil
  "Default directory where your project clones live.
Used to locate a local clone of the PR repository.  Candidate
directories are matched by git remote URL, so only clones whose
origin points at the PR repository are used.

Example: \"~/src\"."
  :type '(choice (const :tag "Disabled" nil)
                 (directory :tag "Directory"))
  :group 'code-review-repo)

(defcustom code-review-repo-cache-dir
  (expand-file-name "code-review" (or (getenv "XDG_CACHE_HOME") "~/.cache"))
  "Cache directory for repos cloned on demand and review worktrees.
Subdirectories \"repos\" (on-demand clones) and \"worktrees\"
(per-PR checkouts) are created below this directory as needed."
  :type 'directory
  :group 'code-review-repo)

(defcustom code-review-repo-clone-confirm t
  "When non-nil, ask before cloning a repository on demand."
  :type 'boolean
  :group 'code-review-repo)

;; Buffer-local state, set in the Code Review buffer by
;; `code-review-repo-setup'.

(defvar-local code-review-repo-worktree nil
  "Worktree directory checked out at the PR head, or nil.")

(defvar-local code-review-repo-dir nil
  "Local git repository directory for the reviewed PR, or nil.")

;;; Git plumbing

(defun code-review-repo--git (dir &rest args)
  "Run git with ARGS in directory DIR.
Return trimmed stdout on success, nil otherwise."
  (when dir
    (let ((default-directory (file-name-as-directory dir)))
      (with-temp-buffer
        (when (zerop (apply #'call-process magit-git-executable
                            nil t nil "--no-pager" args))
          (string-trim (buffer-substring-no-properties (point-min)
                                                      (point-max))))))))

(defun code-review-repo--remote-url (dir &optional remote)
  "Return URL of REMOTE (default \"origin\") of git repository DIR, or nil."
  (code-review-repo--git dir "remote" "get-url" (or remote "origin")))

(defun code-review-repo--remote-names (dir)
  "Return the list of remote names of git repository DIR."
  (split-string (or (code-review-repo--git dir "remote") "") "\n" t))

(defun code-review-repo--url-matches-p (url host owner repo)
  "Return non-nil when git remote URL matches HOST/OWNER/REPO.
Supports both https://host/owner/repo(.git) and git@host:owner/repo(.git)."
  (and url
       (string-match-p (concat (regexp-quote host) "[/:]"
                               (regexp-quote owner) "/"
                               (regexp-quote repo) "\\(?:\\.git\\)?/?$")
                       url)))

(defun code-review-repo--fork-url-p (url host repo)
  "Return non-nil when URL points at a fork of HOST repo named REPO."
  (and url
       (string-match-p (concat (regexp-quote host) "[/:][^/:]+/"
                               (regexp-quote repo) "\\(?:\\.git\\)?/?$")
                       url)))

;;; Repository resolution

(defun code-review-repo--clone-dir (owner repo)
  "Return cache directory for an on-demand clone of OWNER/REPO."
  (expand-file-name (concat "repos/" owner "/" repo)
                    code-review-repo-cache-dir))

(defun code-review-repo--candidate-dirs (owner repo)
  "Return candidate directories that may host a clone of OWNER/REPO.
Directories inside `code-review-repo-cache-dir' (e.g. our own
review worktrees) are excluded."
  (let* ((repo-name (file-name-nondirectory (directory-file-name repo)))
         (cache (and code-review-repo-cache-dir
                     (file-name-as-directory
                      (expand-file-name code-review-repo-cache-dir))))
         (cands (list (ignore-errors (magit-toplevel)))))
    (when code-review-projects-root
      (setq cands
            (nconc cands
                   (file-expand-wildcards
                    (expand-file-name repo-name code-review-projects-root))
                   (file-expand-wildcards
                    (expand-file-name (concat "*/" repo-name)
                                      code-review-projects-root)))))
    (let ((cached (code-review-repo--clone-dir owner repo)))
      (when (file-exists-p cached)
        (setq cands (nconc cands (list cached)))))
    (seq-uniq
     (seq-remove (lambda (dir)
                   (and cache
                        (string-prefix-p cache
                                         (file-name-as-directory
                                          (expand-file-name dir)))))
                 (delq nil cands))
     #'equal)))

(defun code-review-repo--find-existing (host owner repo)
  "Return a local directory of repository HOST/OWNER/REPO, or nil.
A fork clone whose remote points at another owner's copy of REPO
also matches, so that reviewing upstream PRs works from a fork."
  (let ((cands (code-review-repo--candidate-dirs owner repo)))
    (or (cl-some (lambda (dir)
                   (and (code-review-repo--url-matches-p
                         (code-review-repo--remote-url dir) host owner repo)
                        dir))
                 cands)
        (cl-some (lambda (dir)
                   (and (code-review-repo--fork-url-p
                         (code-review-repo--remote-url dir) host repo)
                        dir))
                 cands))))

(defun code-review-repo--clone (host owner repo)
  "Clone https://HOST/OWNER/REPO into the cache on demand.
Return the clone directory, or nil."
  (let* ((dest (code-review-repo--clone-dir owner repo))
         (url (format "https://%s/%s/%s.git" host owner repo)))
    (cond
     ((file-exists-p dest)
      (if (code-review-repo--url-matches-p (code-review-repo--remote-url dest)
                                           host owner repo)
          dest
        (message "code-review: ignoring stale cache clone in %s" dest)
        nil))
     ((or (not code-review-repo-clone-confirm)
          (y-or-n-p (format "No local clone of %s/%s found.  Clone to %s? "
                            owner repo dest)))
      (message "code-review: cloning %s ..." url)
      (make-directory (file-name-directory dest) t)
      (if (code-review-repo--git (file-name-directory dest)
                                 "clone" "--filter=blob:none" url dest)
          (progn (message "code-review: clone done") dest)
        (message "code-review: cloning %s failed" url)
        nil))
     (t nil))))

(defun code-review-repo-find (host owner repo)
  "Return a local directory of repository HOST/OWNER/REPO.
Clone it on demand when no local clone exists."
  (or (code-review-repo--find-existing host owner repo)
      (code-review-repo--clone host owner repo)))

;;; PR refs and worktree

(defun code-review-repo--fetch-remote (dir host owner repo)
  "Return the remote name or URL used to fetch PR refs from DIR.
Prefers a remote pointing exactly at HOST/OWNER/REPO, then an
\"upstream\" remote, and falls back to the https URL of the
repository (works for public repos and via git credentials)."
  (catch 'found
    (dolist (remote (code-review-repo--remote-names dir))
      (when (code-review-repo--url-matches-p
             (code-review-repo--remote-url dir remote) host owner repo)
        (throw 'found remote)))
    (when (and (member "upstream" (code-review-repo--remote-names dir))
               (code-review-repo--url-matches-p
                (code-review-repo--remote-url dir "upstream") host owner repo))
      (throw 'found "upstream"))
    (format "https://%s/%s/%s.git" host owner repo)))

(defun code-review-repo--fetch-refs (dir remote forge num base-ref)
  "Fetch PR and BASE-REF refs for DIR into dedicated local refs.
REMOTE is a remote name or URL to fetch from, FORGE is `github' or
`gitlab' and NUM the PR number (string).  Return the head oid on
success, nil otherwise."
  (let ((head-refspec
         (pcase forge
           ('github (format "+refs/pull/%s/head:refs/remotes/code-review/%s/head"
                            num num))
           ('gitlab (format "+refs/merge-requests/%s/head:refs/remotes/code-review/%s/head"
                            num num))
           (_ nil))))
    (when head-refspec
      (when (code-review-repo--git dir "fetch" remote head-refspec)
        ;; base ref is optional (only needed for local diff generation)
        (when (and base-ref (not (string-empty-p base-ref)))
          (ignore (code-review-repo--git
                   dir "fetch" remote
                   (format "+refs/heads/%s:refs/remotes/code-review/%s/base"
                           base-ref num))))
        (code-review-repo--git dir "rev-parse"
                               (format "refs/remotes/code-review/%s/head" num))))))

(defun code-review-repo--worktree-dir (owner repo num)
  "Return worktree path for OWNER/REPO PR NUM."
  (expand-file-name (format "worktrees/%s/%s/%s" owner repo num)
                    code-review-repo-cache-dir))

(defun code-review-repo--ensure-worktree (repo-dir owner repo num head-oid)
  "Return a worktree of REPO-DIR checked out at HEAD-OID, or nil.
The worktree is created under the cache dir for OWNER/REPO PR NUM
and is hard-updated when the PR head moved."
  (let ((wt (code-review-repo--worktree-dir owner repo num)))
    (cond
     ;; existing worktree: update to the current PR head
     ((and (file-exists-p (expand-file-name ".git" wt)) head-oid)
      (ignore (code-review-repo--git wt "reset" "--hard"))
      (if (code-review-repo--git wt "checkout" "--detach" head-oid)
          wt
        (message "code-review: updating worktree %s failed" wt)
        nil))
     ;; first time: create it
     (head-oid
      (message "code-review: creating worktree for PR %s ..." num)
      (make-directory (file-name-directory wt) t)
      (if (code-review-repo--git repo-dir "worktree" "add" "--detach" wt head-oid)
          (progn (message "code-review: worktree ready at %s" wt) wt)
        (message "code-review: creating worktree failed")
        nil))
     (t nil))))

;;; Entry points

(defun code-review-repo--forge (pr)
  "Return forge symbol (`github' or `gitlab') for PR object, or nil."
  (cond ((code-review-github-repo-p pr) 'github)
        ((code-review-gitlab-repo-p pr) 'gitlab)
        (t nil)))

(defun code-review-repo-setup (pr)
  "Set up local repo context for PR object.
Stores the repository dir in `code-review-repo-dir' and a worktree
checked out at the PR head in `code-review-repo-worktree' (both
buffer-local).  Return the worktree directory, or nil on failure."
  (when code-review-repo-enable
    (let* ((forge (code-review-repo--forge pr))
           (host (pcase forge
                   ('github code-review-github-base-url)
                   ('gitlab code-review-gitlab-base-url)
                   (_ nil)))
           (owner (and (slot-boundp pr 'owner) (oref pr owner)))
           (repo (replace-regexp-in-string
                  "%2F" "/"
                  (and (slot-boundp pr 'repo) (oref pr repo))))
           (num (and (slot-boundp pr 'number)
                     (format "%s" (oref pr number))))
           (base-ref (and (slot-boundp pr 'base-ref-name)
                         (oref pr base-ref-name))))
      (when (and host owner repo num)
        (let ((dir (code-review-repo-find host owner repo)))
          (when dir
            (let* ((fetch-remote (code-review-repo--fetch-remote dir host owner repo))
                   (head (code-review-repo--fetch-refs
                          dir fetch-remote forge num base-ref))
                   (wt (and head
                            (code-review-repo--ensure-worktree
                             dir owner repo num head))))
              (when wt
                (setq code-review-repo-dir dir
                      code-review-repo-worktree wt)
                wt))))))))

(defun code-review-repo-insert-header ()
  "Insert a Worktree header line when local repo context is set up."
  (when code-review-repo-worktree
    (insert (format "%-17s" "Worktree: "))
    (insert-button code-review-repo-worktree
                   'face 'code-review-url-header-face
                   'help-echo "Open the PR worktree in Dired"
                   'action (lambda (&rest _)
                             (dired code-review-repo-worktree)))
    (insert ?\n)))

(defun code-review-repo-open-worktree ()
  "Open the PR worktree for this review in Dired."
  (interactive)
  (if code-review-repo-worktree
      (dired code-review-repo-worktree)
    (user-error "No local worktree for this review")))

(provide 'code-review-repo)
;;; code-review-repo.el ends here
