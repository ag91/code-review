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

(require 'dash)
(require 'magit-git)
(require 'code-review-utils)
(require 'code-review-db)
(require 'code-review-github)
(require 'code-review-gitlab)

(declare-function code-review-utils--log "code-review-utils")

(defgroup code-review-repo nil
  "Local repository integration for code-review."
  :group 'code-review)

(defcustom code-review-repo-enable t
  "When non-nil, set up a local worktree for reviewed PRs.
When the repository cannot be located, cloned or fetched, the
review silently falls back to the remote-only behavior."
  :type 'boolean
  :group 'code-review-repo)

(defcustom code-review-projects-roots nil
  "Default directories where your project clones live.
The first is the default.
Used to locate a local clone of the PR repository.  Candidate
directories are matched by git remote URL, so only clones whose
origin points at the PR repository are used.

It is also where repositories are cloned on demand when no local
clone is found (under the repository name: ROOT/REPO).  When nil,
\"/tmp\" is used instead.

Example: \"~/src\"."
  :type '(choice (const :tag "Disabled" nil)
                 (list :tag "Directories"))
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
First of `code-review-projects-roots' when set, \"/tmp\" otherwise."
  (expand-file-name (or (nth 0 code-review-projects-roots) "/tmp")))

(defun code-review-repo--clone-dir (repo)
  "Return directory for an on-demand clone of REPO.
The repository is cloned under its own name
\(BASE-DIR/REPO) directly."
  (expand-file-name repo
                    (code-review-repo--base-dir)))

(defun code-review-repo--candidate-dirs (owner repo)
  "Return candidate directories that may host a clone of OWNER/REPO."
  (let* ((repo-name (file-name-nondirectory (directory-file-name repo)))
         (current-repo (ignore-errors (magit-toplevel))))
    (-some--> code-review-projects-roots
      (--map (append
              (file-expand-wildcards
               (expand-file-name repo-name it))
              (file-expand-wildcards
               (expand-file-name (concat "*/" repo-name) it)))
             code-review-projects-roots)
      ;; we keep the current repository first to optimize
      (cons current-repo (-flatten it))
      (-distinct it)
      (-non-nil it))))

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

;;; Worktree cleanup

(defcustom code-review-repo-cleanup-days 14
  "Prune finished reviews' worktrees older than this many days.
Age is the last time the review updated the worktree's head (the
gitdir's HEAD file mtime): a re-render hard-updates it, so an
active review's worktree never ages out.  Worktrees of UNFINISHED
reviews are never pruned, regardless of age."
  :group 'code-review
  :type 'natnum)

(defun code-review-repo--git-common-dir (repo-dir)
  "The repository REPO-DIR's git common directory, absolute."
  (let ((gitdir (or (code-review-repo--git repo-dir
                                          "rev-parse" "--git-common-dir")
                    ".git")))
    (if (file-name-absolute-p gitdir)
        (directory-file-name gitdir)
      (expand-file-name gitdir repo-dir))))

(defun code-review-repo--worktree-head-file (repo-dir num)
  "The gitdir HEAD file of worktree NUM of REPO-DIR.
Rewritten on every head update (create, `reset --hard',
`checkout --detach'): its mtime is the worktree's age signal."
  (expand-file-name (format "worktrees/%s/HEAD" num)
                    (code-review-repo--git-common-dir repo-dir)))

(defun code-review-repo--worktree-age-days (repo-dir num)
  "Days since worktree NUM of REPO-DIR was last touched, or nil."
  (let* ((head (code-review-repo--worktree-head-file repo-dir num))
         (attr (and (file-exists-p head) (file-attributes head))))
    (and attr
         (/ (- (float-time) (float-time (nth 5 attr))) 86400.0))))

(defun code-review-repo--unfinished-worktree-nums (owner repo)
  "Numbers (strings) of UNFINISHED review rows protected from cleanup.
With OWNER and REPO, only that repository's rows count; without
them (cleanup on an arbitrary directory) this is deliberately
conservative: every unfinished row's number is protected, whatever
repository it belongs to.  Never signals: a db problem returns
the symbol `all' (protect everything, prune nothing)."
  (condition-case err
      (let* ((rows (code-review-db-all-unfinished))
             ;; a row's worktree is protected when no identity is
             ;; given (protect everything), or the row belongs to the
             ;; repository being cleaned
             (protected-row?
              (lambda (row)
                (and (slot-boundp row 'number)
                     (or (and (null owner) (null repo))
                         (and (slot-boundp row 'owner)
                              (equal (oref row owner) owner)
                              (slot-boundp row 'repo)
                              (equal (oref row repo) repo)))))))
        (->> rows
             (-filter protected-row?)
             (-map (lambda (row) (format "%s" (oref row number))))
             (-distinct)))
    (error
     (code-review-utils--log
      "code-review-repo"
      (format "cleanup unfinished scan failed: %s"
              (error-message-string err)))
     'all)))

(defun code-review-repo--cleanup-plan (dir owner repo)
  "The stale worktrees of repository DIR: (NUM AGE-DAYS) pairs.
Stale = past `code-review-repo-cleanup-days' since its head was
last updated, and its review (when identifiable) is finished."
  (let* ((protected (code-review-repo--unfinished-worktree-nums owner repo))
         (wtdir (expand-file-name "code-review-worktrees"
                                  (code-review-repo--git-common-dir dir)))
         (nums (and (file-directory-p wtdir)
                    (directory-files wtdir nil
                                     directory-files-no-dot-files-regexp))))
    (nreverse
     (->> nums
          (--map (list it (code-review-repo--worktree-age-days dir it)))
          (-filter (lambda (entry)
                     (and (nth 1 entry)
                          (> (nth 1 entry) code-review-repo-cleanup-days)
                          (not (or (eq protected 'all)
                                   (member (nth 0 entry) protected))))))))))

(defun code-review-repo--cleanup-execute (dir plan)
  "Prune PLAN's worktrees of repository DIR.  Return (PRUNED FAILED)."
  ;; stale metadata first: dirs deleted out from under git
  (code-review-repo--git dir "worktree" "prune")
  (let ((results
         (--map (cons (nth 0 it)
                      (let ((wt (expand-file-name
                                 (format "code-review-worktrees/%s"
                                         (nth 0 it))
                                 (code-review-repo--git-common-dir dir))))
                        (cond
                         ((not (file-directory-p wt)) 'gone)
                         ((and (file-exists-p (expand-file-name ".git" wt))
                               (code-review-repo--git
                                dir "worktree" "remove" "--force" wt))
                          'removed)
                         ;; git does not know the dir: it is a leftover,
                         ;; not a worktree — delete it as a directory
                         ((progn (ignore-errors (delete-directory wt t)) t)
                          'removed))))
                plan)))
    (list (--count (eq (cdr it) 'removed) results)
          (--map (car it)
                 (-filter (lambda (it) (eq (cdr it) 'failed)) results)))))

;;;###autoload
(defun code-review-repo-cleanup (dir)
  "Prune old review worktrees of the repository at DIR.
A worktree is pruned when its review's head was last updated more
than `code-review-repo-cleanup-days' days ago and its review is
finished (unfinished reviews' worktrees are never pruned).  Worktree
branches' commits live on their refs and survive the removal; the
worktrees themselves are re-created by the next render."
  (interactive
   (list (or (and (derived-mode-p 'code-review-mode)
                  code-review-repo-dir)
             (read-directory-name
              "Repository: "
              (or (ignore-errors (magit-toplevel)) default-directory)))))
  (let* ((dir (directory-file-name (expand-file-name dir)))
         ;; exact unfinished-row protection when run from the review
         ;; buffer whose repository this is; conservative otherwise
         (in-review? (and (derived-mode-p 'code-review-mode)
                          code-review-repo-dir
                          (equal (directory-file-name
                                  (expand-file-name code-review-repo-dir))
                                 dir)))
         (pr (and in-review? (ignore-errors (code-review-db-get-pullreq))))
         (owner (and pr (slot-boundp pr 'owner) (oref pr owner)))
         (repo (and pr (slot-boundp pr 'repo) (oref pr repo)))
         (plan (code-review-repo--cleanup-plan dir owner repo)))
    (if (null plan)
        (message "code-review: no stale review worktrees in %s" dir)
      (if (y-or-n-p
           (format "Prune %d review worktree(s) older than %d days (%s)? "
                   (length plan)
                   code-review-repo-cleanup-days
                   (mapconcat (lambda (e)
                                (format "#%s (%.0fd)"
                                        (nth 0 e) (nth 1 e)))
                              plan ", ")))
          (let* ((res (code-review-repo--cleanup-execute dir plan))
                 (pruned (nth 0 res))
                 (failed (nth 1 res)))
            (message
             "code-review: pruned %d worktree(s)%s"
             pruned
             (if failed
                 (format ", FAILED: %s"
                         (mapconcat #'identity failed ", "))
               "")))
        (message "code-review: cleanup cancelled")))))

(provide 'code-review-repo)
;;; code-review-repo.el ends here
