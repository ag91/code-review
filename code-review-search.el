;;; code-review-search.el --- Find open PRs by author via the search API -*- lexical-binding: t; -*-

;;
;; Copyright (C) 2026 Andrea <andrea-dev@hotmail.com>
;;
;; Author: Andrea <andrea-dev@hotmail.com>
;; Keywords: git, tools, vc
;; Homepage: https://github.com/wandersoncferreira/code-review
;;
;; This file is not part of GNU Emacs

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

;; Phase 33, the search half: the PRs of the people you review,
;; pooled into one queue.  The org AND the author list derive from
;; your own clones: the orgs from the origin remotes of the
;; repositories under `code-review-projects-roots', the authors from
;; those repositories' last 100 commits (most frequent committers
;; first; `code-review-search-authors' adds extras).  The reviewer's
;; recipe — open PRs authored by those people, NOT yet approved,
;; created inside a rolling window — run as one search per author
;; (parallel, deferred), pooled into a single tabulated-list view
;; (the phase-25 queue template: sortable columns, RET opens the
;; review, `g' refreshes).
;;
;; GitHub-only (L1, needs the forge).  The query builder and the
;; response parser are pure (tested L0); only the fetch touches the
;; network, and its callbacks never signal.

;;; Code:

(require 'a)
(require 'dash)
(require 'deferred)
(require 'ghub)
(require 'tabulated-list)

(require 'code-review-utils)
(require 'code-review-people)
(require 'code-review-repo)

;; forward-declared VALUELESS: the owning files' defcustoms install
;; their defaults; a valued defvar here would swallow them
(defvar code-review-auth-login-marker)
(defvar code-review-github-host)

(defcustom code-review-search-authors nil
  "EXTRA GitHub logins whose open PRs `code-review-search' pools.
The author list is DERIVED by default: the author emails of the
first 100 commits of every clone under `code-review-projects-roots'
(GitHub noreply emails resolve to logins, plain emails search
directly), most frequent first.  This list is added to the derived
one for the people the scan misses (yourself included)."
  :group 'code-review
  :type '(repeat string))

(defcustom code-review-search-window-days 5
  "Only PRs created within this many days are searched."
  :group 'code-review
  :type 'integer)

(defcustom code-review-search-per-page 30
  "How many hits per author search to keep."
  :group 'code-review
  :type 'integer)

(defcustom code-review-search-max-authors 15
  "Cap on the derived author searches per refresh.
The most frequent committers of the project clones win."
  :group 'code-review
  :type 'integer)

;;; The query (pure)

(defun code-review-search--window-start (days as-of)
  "The created:>= date DAYS before AS-OF, as YYYY-MM-DD in UTC.
GitHub evaluates search dates in UTC, so the window must not shift
with the local time zone."
  (format-time-string
   "%Y-%m-%d"
   (time-subtract as-of (days-to-time days))
   t))

(defun code-review-search--query (org author &optional days as-of)
  "The search qualifier string for ORG and AUTHOR.
The reviewer's recipe: open PRs by AUTHOR, not yet approved,
created inside the rolling DAYS window (default
`code-review-search-window-days'; AS-OF pins now for tests)."
  (let ((days (or days code-review-search-window-days)))
    (format "org:%s is:pr author:%s -review:approved state:open created:>=%s"
            org author (code-review-search--window-start days as-of))))

(defun code-review-search--parse-item (item)
  "Parse one REST search ITEM into a row plist, nil when unusable."
  (let* ((url (cdr (assq 'html_url item)))
         (repo (when-let* ((rurl (cdr (assq 'repository_url item))))
                 (if (string-match "/repos/\\([^/]+/[^/]+\\)\\'" rurl)
                     (match-string 1 rurl)
                   rurl))))
    (when (and url repo)
      (list :key url
            :url url
            :title (or (cdr (assq 'title item)) "")
            :author (a-get (cdr (assq 'user item)) 'login)
            :number (cdr (assq 'number item))
            :repo repo
            :created-ts (code-review-search--iso->ts
                         (cdr (assq 'created_at item)))
            :draft? (cdr (assq 'draft item))))))

(defun code-review-search--iso->ts (iso)
  "Parse the API's ISO8601 instant ISO into a time value, or nil."
  (when iso
    (condition-case nil
        (parse-iso8601-time-string iso)
      (error nil))))

;;; The fetch (L1, async, never signals)

(defun code-review-search--fetch (author callback)
  "Run ONE author search for AUTHOR, async; CALLBACK gets the rows."
  (ghub-get
   "/search/issues"
   `((q . ,(code-review-search--query
            code-review-search--current-org author))
     (sort . "created")
     (order . "desc")
     (per_page . ,(number-to-string code-review-search-per-page)))
   :auth code-review-auth-login-marker
   :host code-review-github-host
   :callback
   (lambda (response)
     (condition-case e
         (funcall callback
                  (delq nil
                        (mapcar #'code-review-search--parse-item
                                (cdr (assq 'items response)))))
       (error
        (code-review-utils--log
         "search" (format "parse failed: %s" (error-message-string e)))
        (funcall callback nil))))
   :errorback
   (lambda (&rest args)
     (message "Code Review search failed: %s" args)
     (funcall callback nil))))

(defun code-review-search--fetch-authors (authors callback)
  "Pool the searches for AUTHORS; CALLBACK gets the deduped rows.
One deferred per author inside a `deferred:parallel'; a failed
author search contributes nothing instead of killing the pool."
  (let ((fetched nil))
    (let ((deferreds
           (mapcar
            (lambda (author)
              (let ((d (deferred:new #'identity)))
                (code-review-search--fetch
                 author
                 (lambda (rows)
                   (setq fetched (append fetched rows))
                   (deferred:callback-post d t)))
                d))
            authors)))
      (deferred:nextc
          (deferred:parallel deferreds)
        (lambda (_)
          (funcall callback
                   (delete-dups
                    (copy-sequence fetched))))))))

;;; The org (derived from the project roots, free string allowed)

(defvar code-review-search--current-org nil
  "The org the queue is currently scanning; nil until chosen.
Prompted once per session (C-u re-prompts); `g' reuses it.")

(defun code-review-search--url-owner (url)
  "The GitHub OWNER segment of a clone URL, or nil.
Both https://host/owner/repo(.git) and git@host:owner/repo(.git)."
  (when (and url (string-match "[/:]\\([^/:]+\\)/[^/:]+\\(?:\\.git\\)?/?$" url))
    (match-string 1 url)))

(defun code-review-search--project-repos ()
  "The clone directories under `code-review-projects-roots'.
One and two levels deep, each holding a .git directory."
  (let ((dirs nil))
    (dolist (root code-review-projects-roots)
      (dolist (dir (append
                    (file-expand-wildcards (expand-file-name "*" root))
                    (file-expand-wildcards (expand-file-name "*/*" root))))
        (when (and (file-directory-p dir)
                   (file-exists-p (expand-file-name ".git" dir)))
          (push dir dirs))))
    (nreverse dirs)))

(defun code-review-search--org-candidates ()
  "The GitHub orgs found among the `code-review-projects-roots' clones.
Takes the owner segment of every clone's origin remote.  Distinct,
order stable."
  (let ((orgs nil))
    (dolist (dir (code-review-search--project-repos) (nreverse orgs))
      (let ((owner (code-review-search--url-owner
                    (code-review-repo--remote-url dir))))
        (when owner
          (cl-pushnew owner orgs :test #'equal))))))

(defun code-review-search--email->author (email)
  "The search author value for a committer EMAIL.
A GitHub noreply email (12345+login@users.noreply.github.com, or
the old login@ form) resolves to the LOGIN; anything else is used
as-is — the search's author: qualifier accepts commit emails too.
Bot committers (dependabot, github-actions, ...) return nil —
machine churn does not belong in a review queue."
  (when (and email (not (string-match-p "\\[bot\\]" email)))
    (cond
     ((string-match
       (concat "\\`[0-9]+\\(?:\\+\\)?\\([a-zA-Z0-9-]+\\)"
               "@users\\.noreply\\.github\\.com\\'")
       email)
      (match-string 1 email))
     ((string-match
       "\\`\\([a-zA-Z0-9-]+\\)@users\\.noreply\\.github\\.com\\'"
       email)
      (match-string 1 email))
     (email))))

(defun code-review-search--rank-authors (emails)
  "The derived author list for a flat list of committer EMAILS.
Noreply emails resolve to logins, every email is a candidate,
ordered by frequency (most commits first), capped at
`code-review-search-max-authors'."
  (let ((freq (make-hash-table :test #'equal)))
    (dolist (email emails)
      (let ((author (code-review-search--email->author email)))
        (when author
          (puthash author
                   (1+ (gethash author freq 0))
                   freq))))
    (-take code-review-search-max-authors
           (mapcar #'car
                   (--sort (> (cdr it) (cdr other))
                           (code-review-search--hash->alist freq))))))

(defun code-review-search--hash->alist (table)
  "The hash TABLE as an (KEY . VALUE) alist."
  (let (res)
    (maphash (lambda (k v) (push (cons k v) res)) table)
    res))

(defun code-review-search--derive-authors ()
  "Derive the author list from the project clones' recent commits.
The author emails of the first 100 commits of every clone
(=code-review-repo--git=) ranked by
`code-review-search--rank-authors'."
  (let ((emails nil))
    (dolist (dir (code-review-search--project-repos))
      (setq emails
            (append emails
                    (split-string
                     (or (code-review-repo--git
                          dir "log" "-n" "100" "--format=%ae")
                         "")
                     "\n" t))))
    (code-review-search--rank-authors emails)))

(defun code-review-search--org (&optional re-prompt)
  "The org to scan: asked once, then remembered.
The candidates come from the project clones' remotes; ANY string is
accepted (the completing-read requires no match).  RE-PROMPT
re-asks even when remembered."
  (when (or re-prompt (not code-review-search--current-org))
    (let* ((orgs (code-review-search--org-candidates))
           (prompt (if orgs
                       (format "Organization to scan (candidates: %s): "
                               (string-join orgs ", "))
                     "Organization to scan: "))
           (org (completing-read prompt orgs nil nil nil nil (car orgs))))
      (if (and (stringp org) (not (string-empty-p org)))
          (setq code-review-search--current-org org)
        (user-error "No organization given"))))
  code-review-search--current-org)

(defvar code-review-search--table nil
  "The pooled search rows, keyed by their html_url.")

(defun code-review-search--age (ts)
  "Render the age of TS (a time value) in days."
  (if ts
      (format "%dd" (/ (float-time (time-subtract nil ts)) 86400))
    ""))

(defun code-review-search--row (entry)
  "One tabulated row for the pooled ENTRY."
  (list (plist-get entry :key)
        (vector (code-review-search--age (plist-get entry :created-ts))
                (plist-get entry :title)
                (format "%s#%s" (plist-get entry :repo)
                        (plist-get entry :number))
                (or (plist-get entry :author) "")
                (if (plist-get entry :draft?) "draft" ""))))

(defun code-review-search--sort-age (a b)
  "Sorter: youngest first, by the entries' created-ts."
  (let ((ta (plist-get (gethash (car a) code-review-search--table) :created-ts))
        (tb (plist-get (gethash (car b) code-review-search--table) :created-ts)))
    (cond ((null ta) nil)
          ((null tb) t)
          (t (time-less-p tb ta)))))

(defun code-review-search--refresh (&optional re-prompt)
  "Re-run the searches and repaint the queue.
RE-PROMPT re-asks the org."
  (let* ((org (code-review-search--org re-prompt))
         ;; the derived committers first (most frequent win), the
         ;; explicit defcustom list added on top
         (authors (delete-dups
                   (append code-review-search-authors
                           (code-review-search--derive-authors)))))
    (when (not authors)
      ;; nothing derivable (empty roots? cold clone?): fall back to
      ;; one author picked from the people cache
      (let* ((people (code-review-people-ensure org))
             (cands (and people (code-review-people-candidates people))))
        (when cands
          (setq authors
                (list (code-review-people-login-of
                       (completing-read "Author to search: " cands nil nil)
                       cands))))))
    (unless authors
      (user-error "No authors found: set `code-review-search-authors' or point `code-review-projects-roots' at your clones"))
    (message "Code Review: searching %s (%d author(s))..." org (length authors))
    (code-review-search--fetch-authors
     authors
     (lambda (rows)
       (setq code-review-search--table
             (let ((table (make-hash-table :test #'equal)))
               (dolist (e rows table) (puthash (plist-get e :key) e table)))
             tabulated-list-entries
             (mapcar #'code-review-search--row rows)
             header-line-format
             (format " Open PRs (%s, not approved, last %dd): %d result(s)"
                     org
                     code-review-search-window-days
                     (length rows)))
       (tabulated-list-print)
       (message "Code Review: %d open PR(s)" (length rows))))))

(define-derived-mode code-review-search-mode tabulated-list-mode
  "Code Review Search"
  "The pooled open-PR queue by author (phase 33)."
  (setq tabulated-list-format
        [("Age" 6 code-review-search--sort-age)
         ("Title" 55 nil)
         ("Repo/PR" 24 nil)
         ("Author" 16 nil)
         ("Flags" 7 nil)]
        tabulated-list-sort-key '("Age" . nil))
  (tabulated-list-init-header)
  ;; g re-runs the searches
  (add-hook 'tabulated-list-revert-hook
            #'code-review-search--refresh nil t))

(defun code-review-search--entry-at-point ()
  "The pooled ENTRY at point, or user-error."
  (let ((key (tabulated-list-get-id)))
    (or (and key code-review-search--table
             (gethash key code-review-search--table))
        (user-error "No PR at point"))))

(defun code-review-search--open-at-point ()
  "Open the review of the PR at point."
  (interactive)
  (let ((url (plist-get (code-review-search--entry-at-point) :url)))
    (code-review-start url)))

(defvar code-review-search-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'code-review-search--open-at-point)
    map))

;;;###autoload
(defun code-review-search (&optional re-prompt)
  "Open the pooled open-PR queue for `code-review-search-authors'.
The GitHub-only half of phase 33: one search per author, run in
parallel, pooled into one sortable view.  The org comes from the
`code-review-projects-roots' clones' remotes (any string is
accepted); with C-u, re-prompt for it."
  (interactive "P")
  (code-review-search--org re-prompt)
  (pop-to-buffer (get-buffer-create "*Code Review Search*"))
  (with-current-buffer "*Code Review Search*"
    (unless (derived-mode-p 'code-review-search-mode)
      (code-review-search-mode))
    (code-review-search--refresh)))

(provide 'code-review-search)
;;; code-review-search.el ends here
