;;; code-review-section-comment.el --- the comment section classes and renderers -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026 Andrea <andrea-dev@hotmail.com>
;;
;; Author: Wanderson Ferreira <https://github.com/wandersoncferreira>
;; Maintainer: Andrea <andrea-dev@hotmail.com>
;; Keywords: git, tools, vc
;; Homepage: https://github.com/wandersoncferreira/code-review
;;
;; Split from code-review-section.el (phase 15c, see Improvements.org):
;; the code is mostly Wanderson Ferreira's original
;; code-review-section.el, moved verbatim by the split; the fork and
;; this file are maintained by Andrea.

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; the comment section classes and renderers (part of the code-review section rendering, split from
;; code-review-section.el in phase 15c).

;;; Code:

(require 'code-review-section-shared)
(require 'magit-section)
(require 'magit-diff)
(require 'cl-lib)
(require 'a)
(require 'code-review-faces)
(require 'code-review-db)
(require 'code-review-utils)
(require 'code-review-reactions)

(declare-function code-review-wash-hunk "code-review-section-wash")
(declare-function code-review--patch-line-p "code-review-section")
(declare-function code-review-comment-insert-reactions "code-review-reactions")



;;; general comments - top level comments

(defclass code-review-comment-header-section (magit-section)
  (()))
(defclass code-review-comment-section (magit-section)
  ((keymap :initform 'code-review-comment-section-map)
   (author :initarg :author
           :type string)
   (msg    :initarg :msg
           :type string)
   (body   :initarg :body
           :initform nil
           :type (or null string)
           :documentation "Raw (markdown) body as written by the author.
Used when editing a submitted comment.")
   (id     :initarg :id)
   (reactions :initarg :reactions)
   (typename :initarg :typename)
   (face   :initform 'magit-log-author)))
(defvar code-review-comment-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "e") 'code-review-edit-remote-comment-at-point)
    (define-key map (kbd "C-c C-r") 'code-review-conversation-reaction-at-point)
    (define-key map (kbd "C-c C-i") 'code-review-promote-comment-at-point-to-new-issue)
    map)
  "Keymaps for comment section.")
(cl-defmethod code-review--insert-conversation-section ((github code-review-github-repo))
  "Function to insert conversation section for GITHUB PRs."
  (let-alist (oref github raw-infos)
    (let ((list-of-comments (->> .reviews.nodes
                                 (-filter
                                  (lambda (n)
                                    (not (string-empty-p (a-get n 'bodyHTML)))))
                                 (append .comments.nodes)
                                 (--sort
                                  (< (time-to-seconds (date-to-time (a-get it 'createdAt)))
                                     (time-to-seconds (date-to-time (a-get other 'createdAt))))))))
      (when (not list-of-comments)
        (insert (propertize "No conversation found." 'font-lock-face 'magit-dimmed))
        (insert ?\n))

      (dolist (c list-of-comments)
        (let* ((reactions (a-get-in c (list 'reactions 'nodes)))
               (reaction-objs (when reactions
                                (-map
                                 (lambda (r)
                                   (code-review-reaction-section
                                    :id (a-get r 'id)
                                    :content (a-get r 'content)))
                                 reactions)))
               (obj (code-review-comment-section
                     :author (a-get-in c (list 'author 'login))
                     :msg (a-get c 'bodyHTML)
                     :body (a-get c 'body)
                     :id (a-get c 'databaseId)
                     :typename (a-get c 'typename)
                     :reactions reaction-objs)))
          (magit-insert-section (code-review-comment-section obj)
            (insert (concat
                     (propertize (format "@%s" (oref obj author)) 'font-lock-face (oref obj face))
                     " - "
                     (propertize (code-review-utils--format-timestamp (a-get c 'createdAt)) 'face 'code-review-timestamp-face)))
            (magit-insert-heading)
            (insert ?\n)
            (code-review-insert-comment-lines obj)
            (when reactions
              (code-review-comment-insert-reactions
               reaction-objs
               "comment"
               (a-get c 'databaseId)))
            (insert ?\n))))
      (insert ?\n))))
(cl-defmethod code-review--insert-conversation-section ((gitlab code-review-gitlab-repo))
  "Function to insert conversation section for GITLAB PRs."
  (let-alist (oref gitlab raw-infos)
    (let ((thread-groups (-group-by
                          (lambda (it)
                            (a-get-in it (list 'discussion 'id)))
                          .comments.nodes)))
      (dolist (g (a-keys thread-groups))
        (magit-insert-section (code-review-comment-thread-section)
          (insert (propertize "New Thread" 'font-lock-face 'code-review-thread-face))
          (magit-insert-heading)
          (let ((thread-comments (alist-get g thread-groups nil nil 'equal)))
            (dolist (c thread-comments)
              (let* ((obj (code-review-comment-section
                           :author (a-get-in c (list 'author 'login))
                           :msg (a-get c 'bodyHTML)
                           :id (a-get c 'databaseId))))
                (magit-insert-section (code-review-comment-section obj)
                  (insert (concat
                           (propertize (format "@%s" (oref obj author)) 'font-lock-face (oref obj face))
                           " - "
                           (propertize (code-review-utils--format-timestamp (a-get c 'createdAt)) 'face 'code-review-timestamp-face)))
                  (magit-insert-heading)
                  (code-review-insert-comment-lines obj)
                  (insert ?\n))))))))))
(cl-defmethod code-review--insert-conversation-section ((bitbucket code-review-bitbucket-repo))
  (let-alist (oref bitbucket raw-infos)
    (let ((list-of-comments (--sort
                             (< (time-to-seconds (date-to-time (a-get it 'createdAt)))
                                (time-to-seconds (date-to-time (a-get other 'createdAt))))
                             .comments.nodes)))
      (dolist (c list-of-comments)
        (let* ((obj (code-review-comment-section
                     :author (a-get-in c (list 'author 'login))
                     :msg (a-get c 'bodyHTML)
                     :id (a-get c 'databaseId)
                     :typename (a-get c 'typename))))
          (magit-insert-section (code-review-comment-section obj)
            (insert (concat
                     (propertize (format "@%s" (oref obj author)) 'font-lock-face (oref obj face))
                     " - "
                     (propertize (code-review-utils--format-timestamp (a-get c 'createdAt)) 'face 'code-review-timestamp-face)))
            (magit-insert-heading)
            (insert ?\n)
            (code-review-insert-comment-lines obj)
            (insert ?\n)))))))
(defun code-review-section-insert-top-level-comments ()
  "Insert general comments for the PULL-REQUEST in the buffer."
  (when-let (pr (and (not (code-review-db-local-pr-p))
                     (code-review-db-get-pullreq)))
    (code-review-section--hide-if-hidden
     (magit-insert-section (code-review-comment-header-section
                           nil code-review-fold-header-sections)
       (insert (propertize "Conversation" 'font-lock-face 'magit-section-heading))
       (magit-insert-heading)
       (when code-review-section--display-top-level-comments
         (code-review--insert-conversation-section pr))))))

;; -

(defclass code-review-base-comment-section (magit-section)
  ((state      :initarg :state
               :type string)
   (author     :initarg :author
               :type string)
   (msg        :initarg :msg
               :type string)
   (body       :initarg :body
               :initform nil
               :type (or null string)
               :documentation "Raw (markdown) body as written by the author.
nil for local comments rendered before the data was available; the
raw body is what `e' (edit submitted comment) pre-fills the
comment buffer with.")
   (position   :initarg :position
               :initform nil
               :type (or null number))
   (side         :initarg :side
                 :documentation "GitHub side for comment: LEFT or RIGHT")
   (line         :initarg :line
                 :initform nil
                 :documentation "GitHub line number for side")
   (start-side   :initarg :start-side
                 :initform nil
                 :documentation "GitHub start_side for multi-line comments")
   (start-line   :initarg :start-line
                 :initform nil
                 :documentation "GitHub start_line for multi-line comments")
   (thread-id   :initarg :thread-id
                :initform nil
                :documentation "Forge review thread id (e.g. GraphQL node id) when known.")
   (resolved?   :initarg :resolved?
                :initform nil
                :type boolean
                :documentation "Whether the review thread is resolved.")
   (reactions  :initarg :reactions
               :type (or null
                         (satisfies
                          (lambda (it)
                            (-all-p #'code-review-reaction-section-p it)))))
   (path       :initarg :path
               :type string)
   (diffHunk   :initarg :diffHunk
               :type (or null string))
   (id         :initarg :id
               :documentation "ID that identifies the comment in the Forge.")
   (internalId :initarg :internalId)
   (amount-loc :initform nil)
   (outdated?  :initform nil
               :type boolean)
   (reply?     :initform nil
               :type boolean)
   (local?     :initform nil
               :type boolean)
   (createdAt  :initarg :createdAt)
   (updatedAt  :initarg :updatedAt)))

;; Comment classes are dual purpose: the same class instantiates the
;; prebuilt data objects AND the inserted magit sections (the section
;; wraps the data object in its `value' slot).  `magit-section-ident'
;; therefore calls this generic twice with the same class: first on
;; the section, then (via magit's fallback) on the data object.
;; Without these methods both calls return nil, every comment
;; section gets the SAME ident, and magit's visibility cache plus
;; old-tree matching treat all comments as one section: hiding one
;; bot comment then re-hides every comment on the next render, human
;; ones included.  The dispatch below covers both roles.
(cl-defmethod magit-section-ident-value ((obj code-review-base-comment-section))
  "Identify a comment by its forge id, falling back to author/msg.
When OBJ is the inserted section (its `value' wraps the data
object), delegate to that data object."
  (if (and (slot-boundp obj 'value)
           (eieio-object-p (oref obj value)))
      (magit-section-ident-value (oref obj value))
    (or (and (slot-boundp obj 'id) (oref obj id))
        (and (slot-boundp obj 'author) (oref obj author))
        (and (slot-boundp obj 'msg) (oref obj msg)))))
(cl-defmethod magit-section-ident-value ((obj code-review-comment-section))
  "Identify a conversation comment by id, falling back to author.
When OBJ is the inserted section (its `value' wraps the data
object), delegate to that data object."
  (if (and (slot-boundp obj 'value)
           (eieio-object-p (oref obj value)))
      (magit-section-ident-value (oref obj value))
    (or (and (slot-boundp obj 'id) (oref obj id))
        (and (slot-boundp obj 'author) (oref obj author))
        (and (slot-boundp obj 'msg) (oref obj msg)))))
(defclass code-review-code-comment-section (code-review-base-comment-section)
  ((keymap     :initform 'code-review-code-comment-section-map)
   (diffHunk   :initarg :diffHunk)
   (id         :initarg :id
               :documentation "ID that identifies the comment in the Forge.")
   (amount-loc :initform nil)
   (outdated?  :initform nil
               :type boolean)
   (reply?     :initform nil
               :type boolean)
   (local?     :initform nil
               :type boolean)))
(defclass code-review-local-comment-section (code-review-base-comment-section)
  ((keymap       :initform 'code-review-local-comment-section-map)
   (local?       :initform t)
   (reply?       :initform nil)
   (edit?        :initform nil)
   (send?        :initarg :send?)
   (outdated?    :initform nil)
   (heading-face :initform 'code-review-recent-comment-heading)
   (body-face    :initform nil)
   (diffHunk     :initform nil)
   (line-type    :initarg :line-type)
   (render-heading           :initform "Comment by YOU: " :allocation :class)
   (render-extra-newline?    :initform nil                :allocation :class)))
(defclass code-review-reply-comment-section (code-review-base-comment-section)
  ((keymap       :initform 'code-review-reply-comment-section-map)
   (reply?       :initform t)
   (local?       :initform t)
   (edit?        :initform nil)
   (outdated?    :initform nil)
   (heading-face :initform 'code-review-recent-comment-heading)
   (body-face    :initform nil)
   (render-heading           :initform "Reply by YOU: " :allocation :class)
   (render-extra-newline?    :initform t               :allocation :class)))
(defclass code-review-outdated-comment-section (code-review-base-comment-section)
  ((keymap       :initform 'code-review-outdated-comment-section-map)
   (local?       :initform t)
   (outdated?    :initform t)))
(defclass code-review-check-section (magit-section)
  ((details :initarg :details)))
(defclass code-review-binary-file-section (magit-section)
  ((keymap :initform 'code-review-binary-file-section-map)))
(defvar code-review-code-comment-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'code-review-comment-add-or-edit)
    (define-key map (kbd "e") 'code-review-edit-remote-comment-at-point)
    (define-key map (kbd "C-c C-r") 'code-review-code-comment-reaction-at-point)
    (define-key map (kbd "C-c C-n") 'code-review-promote-comment-at-point-to-new-issue)
    (define-key map (kbd "K") 'code-review-section-delete-comment-remote)
    map)
  "Keymaps for code-comment sections.")
(defvar code-review-local-comment-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'code-review-comment-add-or-edit)
    (define-key map (kbd "C-c C-k") 'code-review-section-delete-comment)
    (define-key map (kbd "K") 'code-review-section-delete-comment-remote)
    map)
  "Keymaps for local-comment sections.")
(defvar code-review-reply-comment-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'code-review-comment-add-or-edit)
    (define-key map (kbd "C-c C-k") 'code-review-section-delete-comment)
    (define-key map (kbd "K") 'code-review-section-delete-comment-remote)
    map)
  "Keymaps for reply-comment sections.")
(defvar code-review-outdated-comment-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'code-review-comment-add-or-edit)
    (define-key map (kbd "e") 'code-review-edit-remote-comment-at-point)
    (define-key map (kbd "K") 'code-review-section-delete-comment-remote)
    map)
  "Keymaps for outdated-comment sections.")
(defvar code-review-binary-file-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'code-review-utils--visit-binary-file-at-point)
    (define-key map [mouse-2] 'code-review-utils--visit-binary-file-at-point)
    (define-key map [follow-link] 'code-review-utils--visit-binary-file-at-point)
    (define-key map [mouse-3] 'code-review-utils--visit-binary-file-at-remote)
    (define-key map (kbd "C-c C-v") 'code-review-utils--visit-binary-file-at-remote)
    map)
  "Keymaps for binary files sections.")
(cl-defmethod code-review-insert-comment-lines ((obj code-review-comment-section))
  "Insert the comment lines given in the OBJ with colored background."
  (let ((start (point))
        (face (code-review-section--comment-bg-face obj)))
    (code-review--insert-html (oref obj msg) (* 3 code-review-section-indent-width))
    (let ((ov (make-overlay start (point))))
      (overlay-put ov 'face face)
      (overlay-put ov 'priority 100))))
(cl-defgeneric code-review-comment-insert-lines (obj)
  "Insert comment lines in the code section based on section type denoted by OBJ.")
(cl-defmethod code-review-comment-insert-lines ((obj code-review-local-comment-section))
  "Insert local comment lines present in the OBJ."
  (code-review-comment--insert-you-lines obj 'code-review-local-comment-section))
(cl-defmethod code-review-comment-insert-lines ((obj code-review-reply-comment-section))
  "Insert reply comment lines present in the OBJ."
  (code-review-comment--insert-you-lines obj 'code-review-reply-comment-section))
(defun code-review-comment--insert-you-lines (obj type)
  "Insert the local or reply comment OBJ as a section of TYPE.
The heading text and the trailing blank line come from the class
slots `render-heading' and `render-extra-newline?'."
  (magit-insert-section ((eval type) obj)
    (let ((heading (oref obj render-heading)))
      (add-face-text-property 0 (length heading) (oref obj heading-face) t heading)
      (magit-insert-heading heading))
    (magit-insert-section ((eval type) obj)
      (let ((start (point)))
        (dolist (l (code-review-utils--split-comment
                    (code-review-utils--wrap-text
                     (oref obj msg)
                     code-review-fill-column)))
          (insert l)
          (insert ?\n))
        (when (oref obj render-extra-newline?)
          (insert ?\n))
        (let ((ov (make-overlay start (point))))
          (overlay-put ov 'face 'code-review-comment-self-bg)
          (overlay-put ov 'priority 1))))))
(cl-defmethod code-review-comment-insert-lines (obj)
  "Default insert comment lines in the OBJ."
  (magit-insert-section (code-review-code-comment-section obj)
    (let ((bgface (code-review-section--comment-bg-face obj))
          (heading (concat
                    (propertize "Reviewed by " 'face 'magit-section-heading)
                    (propertize (concat "@" (oref obj author)) 'face 'code-review-author-face)
                    " - "
                    (code-review--propertize-keyword (oref obj state))
                    " - "
                    (propertize (code-review-utils--format-timestamp (oref obj createdAt)) 'face 'code-review-timestamp-face)
                    (when (and (slot-boundp obj 'resolved?) (oref obj resolved?))
                      (concat " - " (code-review--propertize-keyword "RESOLVED"))))))
      (add-face-text-property 0 (length heading) 'code-review-recent-comment-heading t heading)
      (magit-insert-heading heading)
      ;; point is now at the FIRST BODY line's bol (magit-insert-heading
      ;; inserts the heading's own newline), so the heading line is the
      ;; line ABOVE.  Never take the eol at the body bol: the wash
      ;; interleaves comments into already-inserted hunk text, and the
      ;; line-end-position there belongs to a HUNK line — the overlay
      ;; would swallow the whole body, the next comment and a hunk line.
      ;; Cover the heading's newline too so the paint runs to the edge,
      ;; contiguous with the body overlay below.
      (save-excursion
        (forward-line -1)
        (let ((bol (point))
              (eol (line-end-position)))
          (let ((ov (make-overlay bol (1+ eol))))
            (overlay-put ov 'face bgface)
            (overlay-put ov 'priority 1))))
      (magit-insert-section (code-review-code-comment-section obj)
        (let ((start (point)))
          (code-review--insert-html
           (oref obj msg)
           (* 3 code-review-section-indent-width))
          (when-let (reactions-obj (oref obj reactions))
            (code-review-comment-insert-reactions
             reactions-obj
             "code-comment"
             (oref obj id)))
          (let ((ov (make-overlay start (point))))
            (overlay-put ov 'face bgface)
            (overlay-put ov 'priority 100)))))))
(defun code-review-section--comment-bg-face (c)
  "Background face for comment C: self, PR author, or other."
  (let* ((infos (code-review-db--pullreq-raw-infos))
         (pr-author (and infos (a-get-in infos '(author login))))
         (author (oref c author))
         (my-login (code-review-utils--git-get-user)))
    (cond
     ((and my-login author (string= my-login author))
      'code-review-comment-self-bg)
     ((and pr-author author (string= pr-author author))
      'code-review-comment-author-bg)
     (t 'code-review-comment-other-bg))))
(defun code-review-section--insert-outdated-comment-body (c)
  "Insert one outdated comment C: the nested heading and body
sections with their background overlays."
  (let ((bgface (code-review-section--comment-bg-face c)))
    (magit-insert-section (code-review-outdated-comment-section c)
      (let ((heading (format "Reviewed by %s[%s]:"
                           (oref c author)
                           (oref c state))))
        (magit-insert-heading heading)
        ;; same as the default comment method: point is at the body's
        ;; first bol, the heading line is the line above, and the eol
        ;; must be the HEADING's, not a hunk line's (interleaved wash).
        (save-excursion
          (forward-line -1)
          (let* ((bol (point))
                 (eol (line-end-position))
                 (ov (make-overlay bol (1+ eol))))
            (overlay-put ov 'face bgface)
            (overlay-put ov 'priority 100))))
      (magit-insert-section (code-review-outdated-comment-section c)
        (let ((start (point)))
          (code-review--insert-html
           (oref c msg)
           (* 3 code-review-section-indent-width))
          (when-let (reactions-obj (oref c reactions))
            (code-review-comment-insert-reactions
             reactions-obj
             "outdated-comment"
             (oref c id)))
          (let ((ov (make-overlay start (point))))
            (overlay-put ov 'face bgface)
            (overlay-put ov 'priority 100)))))))
(defun code-review-section--insert-outdated-hunk (hunk metadata hunk-comments
                                                  amount-loc-cell)
  "Insert one outdated HUNK (METADATA its section value) and its
COMMENT sections.  AMOUNT-LOC-CELL is a list cell holding the
running internal amount-loc, advanced here by the hunk text and
each comment body (list cell: the render helpers cannot return
from inside `magit-insert-section' bodies)."
  (let ((first-path (oref (-first-item hunk-comments) path)))
    (code-review-section--hide-if-hidden
     (magit-insert-section (code-review-outdated-hunk-section metadata t)
       (let ((heading (format "Reviewed - [OUTDATED]")))
         (add-face-text-property 0 (length heading)
                                 'code-review-outdated-comment-heading
                                 t heading)
         (magit-insert-heading heading))
       (magit-insert-section ()
         (save-excursion
           (insert hunk))
         (code-review-wash-hunk)
         (insert ?\n)
         (dolist (c hunk-comments)
           (let* ((written-loc (code-review--html-written-loc
                                (oref c msg)
                                (* 3 code-review-section-indent-width)))
                  (amount-new-loc-outdated-partial (+ 1 written-loc))
                  (amount-new-loc-outdated (if (oref c reactions)
                                               (+ 2 amount-new-loc-outdated-partial)
                                             amount-new-loc-outdated-partial)))
             (setcar amount-loc-cell
                     (+ (car amount-loc-cell) amount-new-loc-outdated))
             (setq code-review-section-hold-written-comment-count
                   (code-review-utils--comment-update-written-count
                    code-review-section-hold-written-comment-count
                    first-path
                    amount-new-loc-outdated))
             (oset c amount-loc (car amount-loc-cell))
             (code-review-section--insert-outdated-comment-body c))))))))
(defun code-review-section-insert-outdated-comment (comments amount-loc)
  "Insert outdated COMMENTS in the buffer of PULLREQ-ID considering AMOUNT-LOC.
Safeguards against non-outdated/local comments accidentally passed in."
  ;; Only consider true outdated diff comments that have a diff hunk.
  (let* ((outdated-comments
          (-filter (lambda (el)
                     (and (ignore-errors (oref el outdated?))
                          (oref el outdated?)
                          (ignore-errors (slot-exists-p el 'diffHunk))
                          (oref el diffHunk)))
                   comments)))
    (when outdated-comments
      ;; Hunk groups are necessary because we usually have multiple
      ;; reviews about the same original position across different
      ;; commit snapshots; as the github UI we add those hunks and
      ;; its comments.
      (let* ((hunk-groups (-group-by (lambda (el) (oref el diffHunk))
                                    outdated-comments))
             (hunks (a-keys hunk-groups))
             ;; list cell threading the running internal amount-loc
             ;; through the render helper below
             (amount-loc-cell (list amount-loc)))
        (dolist (hunk hunks)
          (when (not hunk)
            (code-review-utils--log
             "code-review-section-insert-outdated-comment"
             (format "Every outdated comment must have a hunk! Error found for %S"
                     (prin1-to-string hunk)))
            (message "Hunk empty found. A empty string will be used instead. Report this bug please."))
          (let* ((safe-hunk (or hunk ""))
                 (diff-hunk-lines (split-string safe-hunk "\n"))
                 (amount-new-loc (+ 1 (length diff-hunk-lines)))
                 (hunk-comments (alist-get safe-hunk hunk-groups nil nil 'equal))
                 (first-hunk-commit (-first-item hunk-comments))
                 (metadata1 `((comment . ,first-hunk-commit)
                              (amount-loc . ,(+ (car amount-loc-cell) amount-new-loc)))))
            (setcar amount-loc-cell (+ (car amount-loc-cell) amount-new-loc))
            (setq code-review-section-hold-written-comment-count
                  (code-review-utils--comment-update-written-count
                   code-review-section-hold-written-comment-count
                   (oref first-hunk-commit path)
                   amount-new-loc))
            (code-review-section--insert-outdated-hunk
             safe-hunk metadata1 hunk-comments amount-loc-cell)))))))
(defun code-review-section-insert-outdated-comment-missing (path-name missing-paths grouped-comments)
  "Write missing outdated comments in the end of the current path.
We need PATH-NAME, MISSING-PATHS, and GROUPED-COMMENTS to make this work."
  (dolist (path-pos missing-paths)
    (let* ((comment-written-pos
            (or (alist-get path-name code-review-section-hold-written-comment-count nil nil 'equal)
                0))
           (comments (code-review-utils--comment-get grouped-comments path-pos))
           (first (car comments)))
      (when comments
        (if (and (ignore-errors (oref first outdated?))
                 (oref first outdated?))
            (code-review-section-insert-outdated-comment comments comment-written-pos)
          (code-review-section-insert-comment comments comment-written-pos)))
      (push path-pos code-review-section-hold-written-comment-ids))))
(defun code-review-section-insert-comment (comments amount-loc)
  "Insert COMMENTS to PULLREQ-ID keep the AMOUNT-LOC of comments written.
A quite good assumption: every comment in an outdated hunk will be outdated."
  (if (and (oref (-first-item comments) outdated?)
           code-review-section--display-diff-comments)
      (code-review-section-insert-outdated-comment
       comments
       amount-loc)
    (let ((new-amount-loc amount-loc))
      (forward-line)
      (dolist (c comments)
        (when (or (and (code-review-local-comment-section-p c)
                       code-review-section--display-diff-comments)
                  (and (code-review-reply-comment-section-p c)
                       code-review-section--display-diff-comments)
                  (and (code-review-code-comment-section-p c)
                       code-review-section--display-diff-comments))
          (let* ((written-loc (code-review--html-written-loc
                               (oref c msg)
                               (* 3 code-review-section-indent-width)))
                 (amount-loc-incr-partial (+ 1 written-loc))
                 (amount-loc-incr (if (oref c reactions)
                                      (+ 2 amount-loc-incr-partial)
                                    amount-loc-incr-partial)))
            (setq new-amount-loc (+ new-amount-loc amount-loc-incr))
            (oset c amount-loc new-amount-loc)

            (setq code-review-section-hold-written-comment-count
                  (code-review-utils--comment-update-written-count
                   code-review-section-hold-written-comment-count
                   (oref c path)
                   amount-loc-incr))
            (code-review-comment-insert-lines c)))))))
(defun code-review--section-author (section)
  "Return the author string of SECTION, or nil.
Comment sections are created by passing a prebuilt object to
`magit-insert-section', which stores it in the `value' slot, so
the author may live either in the section itself or in its
value object.  Sections of classes with no `author' slot (most
of them) safely return nil instead of signaling
`invalid-slot-name'."
  (or (and (slot-exists-p section 'author)
           (slot-boundp section 'author)
           (let ((author (oref section author)))
             (and (stringp author) author)))
      (let ((val (and (slot-boundp section 'value)
                      (oref section value))))
        (and (eieio-object-p val)
             (slot-exists-p val 'author)
             (slot-boundp val 'author)
             (let ((author (oref val author)))
               (and (stringp author) author))))))
(defun code-review--bot-comment-p (section)
  "Non-nil when SECTION is a comment authored by a bot."
  (let ((author (code-review--section-author section)))
    (and author
         (string-match-p code-review-bot-author-regexp
                         (downcase author))
         t)))
(defun code-review--section-tree-any (section pred)
  "Non-nil when PRED holds for SECTION or one of its descendants."
  (or (funcall pred section)
      (let ((found nil))
        (dolist (c (oref section children))
          (unless found
            (setq found (code-review--section-tree-any c pred))))
        found)))
(defun code-review--collapse-bot-comments (section)
  "Fold bot-authored comment sections under SECTION.
In plain terms: reviews of AI-generated PRs are full of bots
commenting on each other.  Those comments are collapsed to their
one-line \"@author - date\" heading, so what's left reads like a
human conversation.  Threads where a human replied stay expanded,
and TAB unfolds any of them."
  (if (and (code-review--bot-comment-p section)
           (not (code-review--section-tree-any
                 section
                 (lambda (s)
                   (let ((author (code-review--section-author s)))
                     (and author
                          (not (code-review--bot-comment-p s))))))))
      (magit-section-hide section)
    (dolist (c (oref section children))
      (code-review--collapse-bot-comments c))))


;;; Phase 9: fringe markers linking diff lines to their threads

(defvar code-review--fringe-bitmap-defined nil
  "Non-nil after the `code-review-comment-marker' bitmap was registered.")
(defun code-review--ensure-fringe-bitmap ()
  "Register the thread marker fringe bitmap once (no-op on text frames)."
  (when (and (display-graphic-p)
             (fboundp 'define-fringe-bitmap)
             (not code-review--fringe-bitmap-defined))
    (define-fringe-bitmap 'code-review-comment-marker
      (vector 0 128 192 224 224 192 128 0))
    (setq code-review--fringe-bitmap-defined t)))
(defvar code-review-fringe-marker-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] 'code-review-fringe-jump-to-thread)
    map)
  "Keymap for thread-marker lines.
A sparse keymap: only mouse-1 is bound (jump to the thread); any
other key falls through to the buffer's own maps untouched.")
(defun code-review--comment-anchor-position (section)
  "Buffer position (BOL) of the diff line SECTION is anchored to.
The wash inserts each thread right AFTER its anchor line, so scan
backwards from the section start, inside the hunk, until a patch
line is found.  nil when SECTION does not sit inside a hunk."
  (when (and (slot-boundp section 'parent)
             (magit-hunk-section-p (oref section parent)))
    (let ((bound (oref (oref section parent) start)))
      (save-excursion
        (goto-char (oref section start))
        (catch 'found
          (while (> (point) bound)
            (forward-line -1)
            (when (code-review--patch-line-p)
              (throw 'found (line-beginning-position))))
          nil)))))
(defun code-review--mark-comment-line (section)
  "Lay the clickable fringe marker on SECTION's anchor diff line.
Idempotent: any previous marker on that line is removed first."
  (let ((bol (code-review--comment-anchor-position section)))
    (when bol
      (remove-overlays bol (1+ bol) 'cr-comment-marker t)
      (let ((ov (make-overlay bol (min (1+ bol) (point-max)))))
        (overlay-put ov 'cr-comment-marker t)
        (overlay-put ov 'evaporate t)
        (overlay-put ov 'cr-thread-start (copy-marker (oref section start)))
        (overlay-put ov 'before-string
                     (propertize
                      " " 'display
                      (list 'left-fringe 'code-review-comment-marker
                            'code-review-fringe-comment-face)
                      'keymap code-review-fringe-marker-keymap
                      'help-echo "Review thread on this line (mouse-1: jump)"))))))
(defun code-review--mark-comment-lines ()
  "Fringe-mark every review thread anchored inside a hunk.
Sweeps stale marker overlays first (a re-render erases the buffer,
but the walk below is cheap enough to stay idempotent anyway).
Runs from `code-review--trigger-hooks' after every render."
  (dolist (ov (overlays-in (point-min) (point-max)))
    (when (overlay-get ov 'cr-comment-marker)
      (delete-overlay ov)))
  (code-review--ensure-fringe-bitmap)
  (magit-map-sections
   (lambda (section)
     (when (cl-typep section 'code-review-base-comment-section)
       (code-review--mark-comment-line section)))))
(provide 'code-review-section-comment)
;;; code-review-section-comment ends here
