;;; code-review-browse.el --- Open pull request URLs inside Emacs -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026 Andrea <andrea-dev@hotmail.com>
;;
;; Author: Wanderson Ferreira <https://github.com/wandersoncferreira>
;; Maintainer: Andrea <andrea-dev@hotmail.com>
;; Keywords: git, tools, vc
;; Homepage: https://github.com/wandersoncferreira/code-review
;; Package-Requires: ((emacs "28.1"))

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Phase 7: browse-url integration.  A handler is registered in
;; `browse-url-default-handlers' so GitHub pull request links from any
;; buffer (notmuch emails, eww, org, chat) open as a code-review buffer
;; instead of the browser.  GitHub file/line anchors are understood:
;;
;;   #diff-<sha256-of-path>       jump to the file section
;;   #diff-<sha256-of-path>L42    jump to old-side line 42 in the hunk
;;   #diff-<sha256-of-path>R42    jump to new-side line 42 in the hunk
;;   #discussion_r<id>            jump to the review-thread comment
;;   #issuecomment-<id>           jump to the top-level comment
;;   #pullrequestreview-<id>      best effort (reviews are not
;;                                rendered as sections)
;;
;; The sha256 anchor is GitHub's stable path digest (see
;; `code-review-jump-to-gh' and
;; https://github.com/orgs/community/discussions/55764); here it is
;; used in reverse: file sections are hashed until one matches the
;; anchor.  When a review buffer for the PR is already open it is
;; reused as-is (no refetch); otherwise a fresh review is fetched and
;; the anchor jump runs from `code-review-post-hook' once the buffer
;; is rendered.

;;; Code:

(require 'browse-url)
(require 'cl-lib)
(require 'magit-section)
(require 'magit-diff)
(require 'thingatpt)
(require 'code-review-utils)

(declare-function code-review-start "code-review" (url))
(declare-function code-review--patch-line-p "code-review-section")
(declare-function code-review--hunk-content-start "code-review-section" (hunk))

(defconst code-review-browse-url-regexp
  "\\`https?://github\\.com/[^/?#]+/[^/?#]+/pull/[0-9]+"
  "Regexp of the GitHub PR URLs handled inside Emacs.
Only github.com: the anchors this feature understands
\(#diff-<sha>L42 and friends) are GitHub-specific.  GitHub
Enterprise hosts are not handled.")

(defconst code-review-browse-pr-url-scan-regexp
  (concat "\\(?:https?://github\\.com/"
          "[^[:space:]<>\"'()]+/pull/[0-9]+\\(?:#[-[:alnum:]_]+\\)?"
          "\\|https?://gitlab\\.com/"
          "[^[:space:]<>\"'()]+/merge_requests/[0-9]+\\(?:#[-[:alnum:]_]+\\)?"
          "\\|https?://bitbucket\\.org/"
          "[^[:space:]<>\"'()]+/pull-requests/[0-9]+\\(?:#[-[:alnum:]_]+\\)?\\)")
  "Regexp to find pull request URLs in arbitrary buffer text.
The optional trailing fragment keeps #diff-/#discussion_r anchors
from notification emails intact.  Used by
`code-review-open-pr-at-point'.  The open path itself
\(`code-review-browse-url') is forge-generic: any URL
`code-review-utils-pr-from-url' accepts.")

(defvar code-review-browse--pending nil
  "Alist (BUFFER-NAME . ANCHOR) awaiting a rendered review buffer.
Populated by `code-review-browse-url' when a fresh review must be
fetched; consumed by `code-review-browse--after-render'.")

;;; URL parsing

(defun code-review-browse--canonical-url (url)
  "Return URL without fragment, query and trailing view suffixes.
GitHub/GitLab link to sub views (/files, /commits, /diffs, ...)
and carry anchors; the review machinery wants the bare PR URL,
and the URL stored on the PR object should stay clean (it is used
for outbound links by `code-review-kill-pr-url' and
`code-review-jump-to-gh')."
  ;; Property-free: chat buffers (slack lui buttons) hand
  ;; propertized URLs to `browse-url', and `match-string' keeps
  ;; the properties — downstream db slots must never carry them
  ;; (see `code-review-utils-pr-from-url').
  (substring-no-properties
   (or (and (string-match
             "\\`\\(https?://github\\.com/[^/]+/[^/]+/pull/[0-9]+\\)" url)
            (match-string 1 url))
       (and (string-match
             "\\`\\(https?://gitlab\\.com/.+/-/merge_requests/[0-9]+\\)" url)
            (match-string 1 url))
       (and (string-match
             "\\`\\(https?://bitbucket\\.org/[^/]+/[^/]+/pull-requests/[0-9]+\\)"
             url)
            (match-string 1 url))
       ;; unknown shape: at least drop fragment and query
       (replace-regexp-in-string "[?#].*\\'" "" url))))

(defun code-review-browse--parse-fragment (url)
  "Return an anchor plist describing URL's fragment, or nil.
Supported: #diff-<hash>[L<n>[-R<n>]|R<n>] with :kind `diff',
:hash STRING, :line NUMBER and :side `left'|`right';
#discussion_r<id>, #issuecomment-<id> and #pullrequestreview-<id>
with :kind `comment' and :id NUMBER.  Anything else (or no
fragment) returns nil: the PR opens at the top."
  (let ((frag (save-match-data
                (and (string-match "\\`[^#]*\\(#.*\\)\\'" url)
                     (match-string 1 url)))))
    (cond
     ((not frag) nil)
     ;; NOTE: match data must be read in the branch bodies directly
     ;; after each string-match; nothing may match in between.
     ((string-match
       "diff-\\([0-9a-fA-F]+\\)\\(?:L\\([0-9]+\\)\\)?\\(?:-R\\([0-9]+\\)\\)?\\(?:R\\([0-9]+\\)\\)?"
       frag)
      (let* ((hash (downcase (match-string 1 frag)))
             (l (match-string 2 frag))
             (r (or (match-string 3 frag) (match-string 4 frag))))
        (if (or l r)
            ;; when both sides appear (L42-R50) the new side is the
            ;; useful target in a review buffer
            (list :kind 'diff
                  :hash hash
                  :line (string-to-number (or r l))
                  :side (if r 'right 'left))
          ;; file-only anchor: no line at all
          (list :kind 'diff :hash hash))))
     ((string-match
       "\\`#\\(?:discussion_r\\|issuecomment-\\|pullrequestreview-\\)\\([0-9]+\\)\\'"
       frag)
      (list :kind 'comment :id (string-to-number (match-string 1 frag))))
     (t nil))))

(defun code-review-browse--buffer-name (pr-alist)
  "Review buffer name for PR-ALIST from `code-review-utils-pr-from-url'.
Same shape `code-review-pr-buffer-name' derives from the PR object."
  (format "*Code Review: %s/%s#%s*"
          (alist-get 'owner pr-alist)
          (replace-regexp-in-string "%2F" "/" (alist-get 'repo pr-alist))
          (alist-get 'num pr-alist)))

;;; Opening

;;;###autoload
(defun code-review-browse-url (url &rest _args)
  "Open the pull request at URL inside Emacs.
Registered in `browse-url-default-handlers', so GitHub PR links
clicked in any buffer (email, eww, org, chat) open as a
code-review buffer instead of the browser.  GitHub file/line
anchors (#diff-<sha>L42, #discussion_r<id>, #issuecomment-<id>)
jump to the matching file section, diff line or comment section.
When a review buffer for the PR is already open, it is reused
as-is (no refetch).  GitLab and Bitbucket URLs work too when
passed explicitly, but only GitHub links are registered with
`browse-url' (their anchor formats are GitHub-specific)."
  (interactive "sPR URL: ")
  (let* ((canonical (code-review-browse--canonical-url url))
         (pr-alist (code-review-utils-pr-from-url canonical))
         (anchor (code-review-browse--parse-fragment url)))
    (unless pr-alist
      (error "code-review: not a supported pull request URL: %s" url))
    (let* ((buff-name (code-review-browse--buffer-name pr-alist))
           (existing (get-buffer buff-name)))
      (if (and existing
               (buffer-live-p existing)
               (with-current-buffer existing
                 (derived-mode-p 'code-review-mode)))
          (progn
            (pop-to-buffer buff-name)
            (when anchor
              (code-review-browse--jump anchor))
            (message "Reusing open review buffer %s" buff-name))
        (when anchor
          (push (cons buff-name anchor) code-review-browse--pending))
        (code-review-start canonical)))))

(defun code-review-open-pr-at-point ()
  "Open a pull request link from this buffer inside Emacs.
The email/notmuch workflow: uses the URL at point when there is
one, otherwise scans the whole buffer for pull request URLs
(GitHub, GitLab, Bitbucket).  With exactly one hit it opens it;
with several it prompts with completion.  Opens via
`code-review-browse-url', so GitHub file/line anchors jump to
the right spot."
  (interactive)
  (let ((url (code-review-browse--url-in-buffer)))
    (if url
        (code-review-browse-url url)
      (user-error "No pull request URL found in this buffer"))))

(defun code-review-browse--url-in-buffer ()
  "Return a PR URL at point or from this buffer; prompt when ambiguous."
  (or (let ((u (thing-at-point 'url t)))
        (and u (string-match-p code-review-browse-pr-url-scan-regexp u) u))
      (let ((urls (code-review-browse--urls-in-buffer)))
        (cond ((null urls) nil)
              ((= 1 (length urls)) (car urls))
              (t (completing-read "Open PR: " urls nil 'require-match))))))

(defun code-review-browse--urls-in-buffer ()
  "Collect the PR URLs in the current buffer, in order, deduped."
  (let (urls)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward code-review-browse-pr-url-scan-regexp nil t)
        (let ((u (match-string-no-properties 0)))
          (unless (member u urls)
            (push u urls)))))
    (nreverse urls)))

;;; Jumping (must run in the review buffer)

(defun code-review-browse--after-render ()
  "Run the pending browse anchor jump for this buffer, if any.
Installed on `code-review-post-hook': a fresh fetch from
`code-review-browse-url' renders asynchronously, so the jump has
to wait until the buffer is fully built."
  (when (and code-review-browse--pending
             (derived-mode-p 'code-review-mode)
             (assoc (buffer-name) code-review-browse--pending #'equal))
    (let* ((entry (assoc (buffer-name) code-review-browse--pending #'equal))
           (anchor (cdr entry)))
      (setq code-review-browse--pending
            (cl-remove-if (lambda (e) (equal (car e) (buffer-name)))
                          code-review-browse--pending))
      (code-review-browse--jump anchor))))

(defun code-review-browse--jump (anchor)
  "Move point to ANCHOR (a plist from `code-review-browse--parse-fragment').
Must run in the review buffer.  Unknown anchors leave point at
the top of the review."
  (unless (derived-mode-p 'code-review-mode)
    (error "code-review-browse: %s is not a code-review buffer"
           (buffer-name)))
  (pcase (plist-get anchor :kind)
    ('diff (code-review-browse--jump-to-diff-anchor anchor))
    ('comment (code-review-browse--jump-to-comment-id anchor))
    (_ (goto-char (point-min))))
  (when (get-buffer-window (current-buffer))
    (recenter 0)))

(defun code-review-browse--reveal (section)
  "Reveal SECTION: show its ancestors, then SECTION itself last.
`magit-section-show' re-folds hidden children, so the target
must be shown after every ancestor."
  (let ((chain nil)
        (cur section))
    (while cur
      (push cur chain)
      (setq cur (and (slot-boundp cur 'parent) (oref cur parent))))
    (dolist (sec chain)
      (magit-section-show sec))))

(defun code-review-browse--path-hash (path)
  "GitHub's stable diff anchor for PATH: sha256 of the repo-relative path.
Mirror of `code-review-jump-to-gh', which builds #diff-<hash>
links outbound."
  (secure-hash 'sha256 path))

(defun code-review-browse--strip-diff-prefix (path)
  "Strip an a/ or b/ prefix from a diff file PATH."
  (cond ((string-prefix-p "a/" path) (substring path 2))
        ((string-prefix-p "b/" path) (substring path 2))
        (t path)))

(defun code-review-browse--walk-sections (fn)
  "Call FN for every section under `magit-root-section' until it returns non-nil.
FN receives a section; its value becomes the result.  Returns nil
when no section satisfies FN, or when there is no root section
(buffer not rendered)."
  (when (bound-and-true-p magit-root-section)
    (let (found)
      (cl-labels ((walk (sec)
                     (dolist (c (oref sec children))
                       (unless found
                         (when-let* ((res (funcall fn c)))
                           (setq found res))
                         (unless found
                           (walk c))))))
        (walk magit-root-section))
      found)))

(defun code-review-browse--find-file-section (hash)
  "Return the file section whose anchor hash matches HASH, or nil."
  (code-review-browse--walk-sections
   (lambda (sec)
     (when (and (magit-file-section-p sec)
                (slot-boundp sec 'value)
                (stringp (oref sec value)))
       (let ((h (code-review-browse--path-hash
                 (code-review-browse--strip-diff-prefix
                  (substring-no-properties (oref sec value))))))
         ;; GitHub may truncate the digest in links; accept prefix
         ;; matches in either direction.
         (when (or (string-prefix-p hash h)
                   (string-prefix-p h hash))
           sec))))))

(defun code-review-browse--jump-to-diff-anchor (anchor)
  "Move point to the file (and line) a #diff- ANCHOR plist targets."
  (let* ((hash (plist-get anchor :hash))
         (line (plist-get anchor :line))
         (side (or (plist-get anchor :side) 'right))
         (file-section (code-review-browse--find-file-section hash)))
    (cond
     ((not file-section)
      (goto-char (point-min))
      (message "code-review-browse: no file in this diff matches anchor %s"
               hash))
     ((not line)
      (code-review-browse--reveal file-section)
      (magit-section-goto file-section))
     (t
      (let ((pos (code-review-browse--line-position file-section side line)))
        (cond
         (pos (goto-char pos))
         (t
          ;; the line is unchanged context: not part of any hunk
          (code-review-browse--reveal file-section)
          (magit-section-goto file-section)
          (message "code-review-browse: line %d (%s side) is not part of \
this diff's hunks; moved to the file section"
                   line side))))))))

(defun code-review-browse--line-position (file-section side line)
  "Position (beginning of line) of diff LINE on SIDE in FILE-SECTION.
SIDE is `left' (old file) or `right' (new file); LINE is the
1-based file line number from the GitHub anchor.  Returns nil
when LINE falls outside every hunk of the file."
  (let ((hunk (cl-loop for c in (oref file-section children)
                       when (and (magit-hunk-section-p c)
                                 (slot-boundp c 'from-range)
                                 (slot-boundp c 'to-range)
                                 (code-review-browse--hunk-covers-line-p
                                  c side line))
                       return c)))
    (when hunk
      (code-review-browse--reveal hunk)
      (code-review-browse--hunk-line-position hunk side line))))

(defun code-review-browse--hunk-covers-line-p (hunk side line)
  "Return non-nil when HUNK's SIDE range contains diff line LINE."
  (let* ((range (if (eq side 'left)
                    (oref hunk from-range)
                  (oref hunk to-range)))
         (start (and (consp range) (numberp (car range)) (car range)))
         (count (and (consp range) (numberp (cadr range)) (cadr range))))
    (and start count
         (>= line start)
         (< line (+ start count)))))

(defun code-review-browse--hunk-line-position (hunk side line)
  "Beginning of the patch line for diff LINE on SIDE within HUNK.
Falls back to the nearest patch line when the exact line is not
in the diff (e.g. an unchanged line inside a hunk's context).
Review UI lines (inline comments) are skipped, like the comment
machinery does."
  (let* ((from (oref hunk from-range))
         (to (oref hunk to-range))
         (old-line (and (consp from) (numberp (car from)) (car from)))
         (new-line (and (consp to) (numberp (car to)) (car to)))
         (end (marker-position (oref hunk end)))
         (pos nil)
         (fallback nil))
    (save-excursion
      (goto-char (code-review--hunk-content-start hunk))
      (while (and (< (point) end) (not pos))
        (when (code-review--patch-line-p)
          (unless fallback
            (setq fallback (line-beginning-position)))
          (let ((ch (char-after (line-beginning-position))))
            ;; A line exists on the left side only when it is
            ;; context or removed, on the right side only when it
            ;; is context or added: the line counters must be
            ;; compared only for lines that exist on that side.
            (when (and (eq side 'left) (memq ch '(?\s ?-))
                       old-line (eq line old-line))
              (setq pos (line-beginning-position)))
            (when (and (eq side 'right) (memq ch '(?\s ?+))
                       new-line (eq line new-line))
              (setq pos (line-beginning-position)))
            (pcase ch
              (?\s (setq old-line (and old-line (1+ old-line))
                         new-line (and new-line (1+ new-line))))
              (?+ (setq new-line (and new-line (1+ new-line))))
              (?- (setq old-line (and old-line (1+ old-line)))))))
        (forward-line 1)))
    (or pos fallback)))

(defun code-review-browse--section-id (section)
  "Return the comment databaseId carried by SECTION, or nil.
The dual-role comment sections store their data object (which
holds the id) in the `value' slot, so both the section itself and
its value are candidates."
  (let ((candidates
         (list section
               (and (slot-boundp section 'value)
                    (eieio-object-p (oref section value))
                    (oref section value)))))
    (cl-loop for obj in candidates
             when (and obj
                       (slot-exists-p obj 'id)
                       (slot-boundp obj 'id))
             return (oref obj id))))

(defun code-review-browse--jump-to-comment-id (anchor)
  "Move point to the comment section with the id ANCHOR carries.
Matches any section (or section value object, for the dual-role
comment classes) with a numeric `id' slot equal to the anchor's
id: review-thread comments (#discussion_r<id>), top-level
comments (#issuecomment-<id>) and the PR description.
`#pullrequestreview-<id>' ids belong to the review as a whole,
which is not rendered as a section; those fall back to the top
with a message."
  (let ((id (plist-get anchor :id)))
    (let ((section (code-review-browse--walk-sections
                    (lambda (sec)
                      (let ((cid (code-review-browse--section-id sec)))
                        (when (or (equal cid id)
                                  (and (numberp cid) (stringp id)
                                       (string-equal (number-to-string cid) id))
                                  (and (stringp cid) (numberp id)
                                       (string-equal cid (number-to-string id))))
                          sec))))))
      (if section
          (progn
            (code-review-browse--reveal section)
            (magit-section-goto section))
        (goto-char (point-min))
        (message "code-review-browse: no comment with id %s in this review" id)))))

;;; Handler registration

(defun code-review-browse-url-install ()
  "Register the GitHub PR handler in `browse-url-default-handlers'.
Idempotent.  Run automatically when this file loads; also run at
startup through the package autoloads, so PR links open inside
Emacs even before any code-review command has been used."
  (interactive)
  (unless (rassoc 'code-review-browse-url browse-url-default-handlers)
    (push (cons code-review-browse-url-regexp 'code-review-browse-url)
          browse-url-default-handlers)))

(defun code-review-browse-url-uninstall ()
  "Remove the code-review handler from `browse-url-default-handlers'."
  (interactive)
  (setq browse-url-default-handlers
        (rassq-delete-all 'code-review-browse-url
                          browse-url-default-handlers)))

;; Register on load.  Idempotent, so re-loading in a daemon is safe.
;; (rassoc, not the regexp: the autoloads file may already have added
;; an equal entry carrying its own regexp string.)
(code-review-browse-url-install)

;; Register at startup as well: the package autoloads execute this
;; form when Emacs starts, before `browse-url' itself is loaded (the
;; `with-eval-after-load' defers until the first link is opened).
;;;###autoload
(with-eval-after-load 'browse-url
  (add-to-list 'browse-url-default-handlers
               '("\\`https?://github\\.com/[^/?#]+/[^/?#]+/pull/[0-9]+"
                 . code-review-browse-url)))

(provide 'code-review-browse)
;;; code-review-browse.el ends here
