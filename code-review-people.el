;;; code-review-people.el --- Org people cache: login + display name -*- lexical-binding: t; -*-

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

;; The people cache (phase 33): the org's members fetched ONCE,
;; cached on disk with a TTL, refreshed in the background.  The
;; reviewer knows the PEOPLE, not the usernames — every entry keeps
;; the display NAME next to the login so searches can match either.
;;
;; Feeds: `code-review-search''s author filters, the C-c @ mention
;; completion, and (later) author rendering.
;;
;; GitHub-only (L1, needs the forge): a non-GitHub forge degrades to
;; an empty cache that says so.  The fetch is ASYNC (ghub-graphql,
;; manual cursor walk) and its callbacks never signal — an error is
;; logged and the stale cache keeps serving (the sentinel lesson).

;;; Code:

(require 'a)
(require 'ghub)
(require 'dash)

(require 'code-review-utils)

;; forward-declared VALUELESS: the owning files' defcustoms install
;; their defaults; a valued defvar here would swallow them
(defvar code-review-auth-login-marker)
(defvar code-review-github-graphql-host)

(defcustom code-review-people-cache-file
  (expand-file-name "code-review-people-cache.el" user-emacs-directory)
  "Where the org people cache lives (a versioned plist)."
  :group 'code-review
  :type 'file)

(defcustom code-review-people-ttl-days 7
  "The people cache is refreshed in the background after this many days."
  :group 'code-review
  :type 'integer)

(defconst code-review-people--version 1
  "The cache format version; old caches re-fetch once.")

(defvar code-review-people--state nil
  "The in-memory cache: plist with :org, :people, :fetched-at, :refreshing?.
:people is an alist of (LOGIN . NAME), NAME possibly nil.")

(defvar code-review-people--refresh-hook nil
  "Run after a background refresh lands (view buffers re-read the cache).")

(defvar code-review-search--current-org nil
  "Forward-declared VALUELESS: defined in code-review-search.el.")

(defun code-review-people--log (msg)
  "Log MSG to the package log without ever signaling."
  (ignore-errors
    (code-review-utils--log "people-cache" msg)))

(defun code-review-people--load (file)
  "Read the cache plist from FILE, nil when absent or unreadable."
  (condition-case e
      (let ((state (with-temp-buffer
                     (insert-file-contents file)
                     (read (current-buffer)))))
        (and (plistp state)
             (>= (or (plist-get state :version) 0)
                 code-review-people--version)
             state))
    (error
     (code-review-people--log (format "cache load failed: %s"
                                      (error-message-string e)))
     nil)))

(defun code-review-people--save (state file)
  "Write the cache STATE plist to FILE, atomically."
  (condition-case e
      (let ((print-length nil)
            (dir (file-name-directory file)))
        (make-directory dir t)
        (with-temp-file file
          (prin1 state (current-buffer))))
    (error
     (code-review-people--log (format "cache save failed: %s"
                                      (error-message-string e)))
     nil)))

(defun code-review-people--fresh-p (state)
  "Non-nil when the cache STATE is within its TTL."
  (and state
       (plist-get state :people)
       (< (- (float-time) (or (plist-get state :fetched-at) 0))
          (* 86400 code-review-people-ttl-days))))

(defun code-review-people--merge-page (people nodes)
  "Fold the GraphQL NODES into the PEOPLE alist, returning it."
  (dolist (n nodes people)
    (let ((login (cdr (assq 'login n)))
          (name (cdr (assq 'name n))))
      (when login
        (setq people (a-assoc people login name))))))

(defun code-review-people--walk (org cursor people done)
  "Walk the membersWithRole pages of ORG from CURSOR.
PEOPLE pools the pages so far; DONE runs with the result."
  (code-review-people--graphql
   org cursor
   (lambda (nodes next-cursor)
     (setq people (if nodes
                      (code-review-people--merge-page people nodes)
                    people))
     (if next-cursor
         (code-review-people--walk org next-cursor people done)
       (funcall done people)))))

(defun code-review-people--refresh (org done)
  "Refresh the people cache for ORG in the background.
DONE runs with the pooled people alist (or nil) when the walk ends."
  (code-review-people--walk org nil nil done))

(defun code-review-people--parse-response (response)
  "The (NODES CURSOR) for one ghub-graphql RESPONSE.
(Nil nil) on junk.  ghub 5.1 delivers the data contents SPLICED
after the `data' symbol — (data (organization ...)) with the data
symbol as a BARE ELEMENT, so (assq 'data response) is nil and
every downstream lookup silently yields nil; unwrap with cdr."
  (let* ((data (cdr response))
         (org-node (cdr (assq 'organization data)))
         (conn (cdr (assq 'membersWithRole org-node)))
         (page-info (cdr (assq 'pageInfo conn)))
         (nodes (cdr (assq 'nodes conn))))
    (list nodes
          (and (cdr (assq 'hasNextPage page-info))
               (cdr (assq 'endCursor page-info))))))

(defun code-review-people--graphql (org cursor callback)
  "Fetch ONE membersWithRole page for ORG at CURSOR, async.
CALLBACK receives (nodes next-cursor) or nil on error."
  (ghub-query
   "query($org:String!, $cursor:String) {
      organization(login:$org) {
        membersWithRole(first:100, after:$cursor) {
          pageInfo { endCursor hasNextPage }
          nodes { login name }
        }
      }
    }"
   `((org . ,org)
     ,@(and cursor `((cursor . ,cursor))))
   :auth code-review-auth-login-marker
   :host code-review-github-graphql-host
   :callback
   (lambda (response)
     (condition-case e
         (apply callback (code-review-people--parse-response response))
       (error
        (code-review-people--log (format "page parse failed: %s"
                                         (error-message-string e)))
        (funcall callback nil nil))))
   :errorback
   (lambda (&rest args)
     (code-review-people--log (format "fetch failed: %s" args))
     (funcall callback nil nil))))

;;;###autoload
(defun code-review-people-ensure (org)
  "Serve the people cache for ORG, refreshing in the background when stale.
Returns the people alist ((LOGIN . NAME) ...) when one is known —
the FRESH one, or the stale one while a refresh is in flight — nil
when nothing is known yet.  Never blocks on the network."
  (unless (and code-review-people--state
               (equal (plist-get code-review-people--state :org) org))
    (setq code-review-people--state
          (or (code-review-people--load code-review-people-cache-file)
              `(:version ,code-review-people--version :org ,org
                         :people nil :fetched-at nil :refreshing? nil)))
    (unless (equal (plist-get code-review-people--state :org) org)
      (setq code-review-people--state
            `(:version ,code-review-people--version :org ,org
                       :people nil :fetched-at nil :refreshing? nil))))
  (let ((state code-review-people--state))
    (when (and (not (plist-get state :refreshing?))
               (not (code-review-people--fresh-p state)))
      (plist-put state :refreshing? t)
      (code-review-people--refresh
       org
       (lambda (people)
         (plist-put code-review-people--state :refreshing? nil)
         (when people
           (plist-put code-review-people--state :fetched-at (float-time))
           (plist-put code-review-people--state :people people)
           (code-review-people--save code-review-people--state
                                     code-review-people-cache-file))
         (run-hooks 'code-review-people--refresh-hook))))
    (plist-get code-review-people--state :people)))

(defun code-review-people-match (people query)
  "The PEOPLE alist entries matching QUERY.
Substring, case-insensitive, on the display NAME or the LOGIN."
  (let* ((q (downcase query))
         (hit (lambda (entry)
                (let ((login (car entry))
                      (name (cdr entry)))
                  (or (and name
                           (string-match-p (regexp-quote q) (downcase name)))
                      (string-match-p (regexp-quote q) (downcase login)))))))
    (-filter hit people)))

(defun code-review-people--display (entry)
  "Render the people alist ENTRY as a completion string."
  (let ((login (car entry))
        (name (cdr entry)))
    (if (and name (not (string-empty-p name)))
        (format "%s (%s)" name login)
      login)))

(defun code-review-people-candidates (people)
  "Completion candidates (STRING . LOGIN) for the PEOPLE alist."
  (mapcar (lambda (e)
            (cons (code-review-people--display e) (car e)))
          people))

(defun code-review-people-login-of (picked candidates)
  "The LOGIN behind the picked completion string among CANDIDATES."
  (or (cdr (assoc picked candidates))
      ;; a typed-not-completed pick IS the login
      picked))

;;;###autoload
(defun code-review-people-refresh (org)
  "Force a background refresh of the ORG people cache."
  (interactive
   (list (or code-review-search--current-org
             (read-string "Organization: "))))
  (setq code-review-people--state
        `(:version ,code-review-people--version :org ,org
                   :people nil :fetched-at nil :refreshing? nil))
  (code-review-people-ensure org)
  (message "Refreshing the %s people cache in the background" org))

(provide 'code-review-people)
;;; code-review-people.el ends here
