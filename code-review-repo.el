;;; code-review-repo.el --- Local repository context for code-review -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Andrea <andrea-dev@hotmail.com>
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

It is also where repositories are cloned on demand when no local
clone is found (under the repository name: ROOT/REPO).  When nil,
\"/tmp\" is used instead.

Example: \"~/src\"."
  :type '(choice (const :tag "Disabled" nil)
                 (directory :tag "Directory"))
  :group 'code-review-repo)

(defcustom code-review-repo-clone-confirm t
  "When non-nil, ask before cloning a repository on demand."
  :type 'boolean
  :group 'code-review-repo)

;; Buffer-local state, set in the Code Review buffer by
;; `code-review-repo-setup'.

(defvar-local code-review-repo-worktree nil
  "Worktree directory checked out at the PR head, or nil.")
(put 'code-review-repo-worktree 'permanent-local t)

(defvar-local code-review-repo-dir nil
  "Local git repository directory for the reviewed PR, or nil.")
(put 'code-review-repo-dir 'permanent-local t)

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

(defun code-review-repo--base-dir ()
  "Root directory for on-demand clones.
`code-review-projects-root' when set, \"/tmp\" otherwise."
  (expand-file-name (or code-review-projects-root "/tmp")))

(defun code-review-repo--clone-dir (repo)
  "Return directory for an on-demand clone of REPO.
The repository is cloned under its own name
\(BASE-DIR/REPO) directly."
  (expand-file-name repo
                    (code-review-repo--base-dir)))

(defun code-review-repo--candidate-dirs (owner repo)
  "Return candidate directories that may host a clone of OWNER/REPO."
  (let* ((repo-name (file-name-nondirectory (directory-file-name repo)))
         (cands (list (ignore-errors (magit-toplevel)))))
    (when code-review-projects-root
      (setq cands
            (nconc cands
                   (file-expand-wildcards
                    (expand-file-name repo-name code-review-projects-root))
                   (file-expand-wildcards
                    (expand-file-name (concat "*/" repo-name)
                                      code-review-projects-root)))))
    (let ((clone (code-review-repo--clone-dir repo)))
      (when (file-exists-p clone)
        (setq cands (nconc cands (list clone)))))
    (seq-uniq (delq nil cands) #'equal)))

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
  "Clone https://HOST/OWNER/REPO on demand under the base directory.
Return the clone directory, or nil."
  (let* ((dest (code-review-repo--clone-dir repo))
         (url (format "https://%s/%s/%s.git" host owner repo)))
    (cond
     ((file-exists-p dest)
      (if (code-review-repo--url-matches-p (code-review-repo--remote-url dest)
                                           host owner repo)
          dest
        (message "code-review: ignoring unrelated directory in %s" dest)
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

(defun code-review-repo--worktree-dir (repo-dir num)
  "Return worktree path for PR NUM of the repository at REPO-DIR.
The worktree lives inside the repository's git directory
\(BASE-DIR/OWNER/REPO/.git/code-review-worktrees/NUM), so it never
pollutes the working tree and survives `git clean'."
  (let ((gitdir (or (code-review-repo--git repo-dir
                                          "rev-parse" "--git-common-dir")
                    ".git")))
    (expand-file-name (format "code-review-worktrees/%s" num)
                      (if (file-name-absolute-p gitdir)
                          gitdir
                        (expand-file-name gitdir repo-dir)))))

(defun code-review-repo--ensure-worktree (repo-dir num head-oid)
  "Return a worktree of REPO-DIR checked out at HEAD-OID, or nil.
The worktree is hard-updated when the PR head moved."
  (let ((wt (code-review-repo--worktree-dir repo-dir num)))
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
                             dir num head))))
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

(defun code-review-repo-substantive-files (num)
  "Return the list of files that really changed in PR NUM.
This compares the PR base ref with the PR head ignoring
whitespace-only changes, so files whose only change is
reindentation or trailing spaces are NOT in the returned list.
Return nil when the worktree or the base ref are unavailable."
  (let ((wt code-review-repo-worktree)
        (dir code-review-repo-dir)
        (base (format "refs/remotes/code-review/%s/base" num)))
    (when (and wt
               (code-review-repo--git (or dir wt)
                                      "rev-parse" "--verify" "--quiet" base))
      (let ((out (code-review-repo--git
                  wt "diff" "--no-renames" "-w" "--ignore-blank-lines"
                  "--numstat" (format "%s...HEAD" base))))
        (when out
          (delq nil
                (mapcar (lambda (line)
                          (let ((cols (split-string line "\t")))
                            (when (= 3 (length cols))
                              (let ((adds (nth 0 cols))
                                    (dels (nth 1 cols)))
                                ;; keep the file when lines were truly
                                ;; added/removed, or it is binary ("-")
                                (when (or (string= adds "-")
                                          (string= dels "-")
                                          (not (string= adds "0"))
                                          (not (string= dels "0")))
                                  (nth 2 cols))))))
                        (split-string out "\n" t))))))))

(provide 'code-review-repo)
;;; code-review-repo.el ends here
