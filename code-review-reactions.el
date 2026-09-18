;;; code-review-reactions.el --- Reactions on PR comments and description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Wanderson Ferreira
;;
;; This file is part of code-review.
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either under version 3, or
;; (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; Commentary:
;;
;;  Reaction sections and the toggle machinery for the three
;;  contexts (pr-description, conversation comment, code comment).
;;  Rendering glue lives in code-review-section.el; the forge API
;;  calls are generic (see code-review-interfaces.el).
;;
;;; Code:

(require 'cl-lib)
(require 'magit-section)
(require 'emojify)
(require 'a)
(require 'dash)
(require 'let-alist)
(require 'code-review-db)
(require 'code-review-interfaces)

(defvar code-review-comment-cursor-pos nil
  "Defined in `code-review-section.el'.")

(declare-function code-review--build-buffer "code-review-section")


(defvar code-review-reaction-types
  `(("THUMBS_UP" . ":+1:")
    ("THUMBS_DOWN" . ":-1:")
    ("LAUGH" . ":laughing:")
    ("CONFUSED" . ":confused:")
    ("HEART" . ":heart:")
    ("HOORAY" . ":tada:")
    ("ROCKET" . ":rocket:")
    ("EYES" . ":eyes:"))
  "All available reactions.")

(defclass code-review-reaction-section ()
  ((id :initarg :id)
   (content :initarg :content)))
(defclass code-review-reactions-section (magit-section)
  ((context-name :initarg :context-name)
   (comment-id :initarg :comment-id)
   (reactions :initarg :reactions
              :type (satisfies
                     (lambda (it)
                       (-all-p #'code-review-reaction-section-p it))))))

(defun code-review-reaction--update-node (node comment-id reaction-id content delete?)
  "Return NODE with its reactions upserted for COMMENT-ID.
REACTION-ID and CONTENT identify the reaction; remove it when
DELETE? is non-nil.  NODEs that don't match COMMENT-ID are
returned untouched."
  (let-alist node
    (when (equal .databaseId comment-id)
      (let ((reactions-nodes
             (if delete?
                 (-filter (lambda (it)
                            (not (string-equal (a-get it 'id) reaction-id)))
                          .reactions.nodes)
               (append .reactions.nodes
                       (list (a-alist 'id reaction-id
                                      'content (upcase content)))))))
        (setf (alist-get 'reactions node) (a-alist 'nodes reactions-nodes)))))
  node)

(defun code-review-reaction--update-flat (infos comment-id reaction-id content delete?)
  "Upsert a reaction in the flat comment and review nodes of INFOS."
  (setf (alist-get 'comments infos)
        (a-alist 'nodes (-map (lambda (c)
                                (code-review-reaction--update-node
                                 c comment-id reaction-id content delete?))
                              (a-get-in infos '(comments nodes)))))
  (setf (alist-get 'reviews infos)
        (a-alist 'nodes (-map (lambda (r)
                                (code-review-reaction--update-node
                                 r comment-id reaction-id content delete?))
                              (a-get-in infos '(reviews nodes)))))
  infos)

(defun code-review-reaction--update-nested (infos comment-id reaction-id content delete?)
  "Upsert a reaction in the nested review comments of INFOS.
Return (INFOS . NEW-REVIEWS)."
  (let ((reviews
         (-map (lambda (r)
                 (setf (alist-get 'comments r)
                       (a-alist 'nodes
                                (-map (lambda (c)
                                        (code-review-reaction--update-node
                                         c comment-id reaction-id content delete?))
                                      (a-get-in r '(comments nodes)))))
                 r)
               (a-get-in infos '(reviews nodes)))))
    (cons infos reviews)))

(defun code-review-reaction--update-description (infos node-id content delete?)
  "Upsert a PR-level reaction in the description reactions of INFOS.
NODE-ID is the reaction node id and CONTENT its reaction content.
Remove it when DELETE? is non-nil."
  (let ((nodes (if delete?
                   (-filter (lambda (it)
                              (not (string-equal node-id (a-get it 'id))))
                            (a-get-in infos '(reactions nodes)))
                 (cons (a-alist 'content (upcase content) 'id node-id)
                       (a-get-in infos (list 'reactions 'nodes))))))
    (setf (alist-get 'reactions infos) (a-alist 'nodes nodes))
    infos))

(defun code-review-reaction--update-context (context comment-id node-id reaction-id content delete?)
  "Persist a reaction add or delete (DELETE?) for CONTEXT in the PR.
COMMENT-ID locates the comment (unused for the PR description),
NODE-ID is the reaction node id and REACTION-ID its database id."
  (let* ((pr (code-review-db-get-pullreq))
         (infos (oref pr raw-infos)))
    (pcase context
      ("pr-description"
       (oset pr raw-infos
             (code-review-reaction--update-description infos node-id content delete?)))
      ("comment"
       (oset pr raw-infos
             (code-review-reaction--update-flat infos comment-id reaction-id content delete?)))
      ("code-comment"
       (let ((res (code-review-reaction--update-nested
                   infos comment-id reaction-id content delete?)))
         (oset pr raw-infos (car res))
         (oset pr raw-comments (cdr res)))))
    (code-review-db-update pr)))

(defun code-review--toggle-reaction-at-point (pr context-name comment-id existing-reactions reaction)
  "Given a PR, use the CONTEXT-NAME to toggle REACTION in COMMENT-ID considering EXISTING-REACTIONS."
  (let* ((res (code-review-send-reaction pr context-name comment-id reaction))
         (reaction-id (a-get res 'id))
         (node-id (a-get res 'node_id))
         (existing-reaction-ids (when existing-reactions
                                  (-map (lambda (r) (oref r id)) existing-reactions))))
    (code-review-reaction--update-context
     context-name comment-id node-id reaction-id reaction
     (-contains-p existing-reaction-ids node-id))
    (code-review--build-buffer)))

(defun code-review-toggle-reaction-at-point (comment-id context-name)
  "Add reaction at point given a COMMENT-ID and CONTEXT-NAME."
  (let* ((allowed-reactions (-map
                             (lambda (it)
                               `(,(cdr it) . ,(car it)))
                             code-review-reaction-types))
         (choice (emojify-completing-read "Reaction: "
                                          (lambda (string-display)
                                            (let ((prefix (car (split-string string-display " -"))))
                                              (-contains-p (a-keys allowed-reactions) prefix)))))
         (pr (code-review-db-get-pullreq))
         (reaction (downcase (alist-get choice allowed-reactions nil nil 'equal))))
    (with-slots (value) (magit-current-section)
      (code-review--toggle-reaction-at-point
       pr
       context-name
       comment-id
       (oref value reactions)
       reaction))))

(defun code-review-reactions-reaction-at-point ()
  "Endorse or remove your reaction at point."
  (interactive)
  (setq code-review-comment-cursor-pos (point))
  (let* ((section (magit-current-section))
         (pr (code-review-db-get-pullreq))
         (obj (oref section value))
         (map-rev (-map
                   (lambda (it)
                     `(,(cdr it) . ,(car it)))
                   code-review-reaction-types))
         (reaction-text (get-text-property (point) 'emojify-text))
         (gh-value (downcase (alist-get reaction-text map-rev nil nil 'equal))))
    (code-review--toggle-reaction-at-point
     pr
     (oref obj context-name)
     (oref obj comment-id)
     (oref obj reactions)
     gh-value)))

(defun code-review-reaction--toggle-in-context (context-name)
  "Toggle a reaction on the comment section at point in CONTEXT-NAME."
  (let ((comment-id (oref (oref (magit-current-section) value) id)))
    (setq code-review-comment-cursor-pos (point))
    (code-review-toggle-reaction-at-point comment-id context-name)))

(defun code-review-description-reaction-at-point ()
  "Toggle reaction in description sections."
  (interactive)
  (code-review-reaction--toggle-in-context "pr-description"))

(defun code-review-conversation-reaction-at-point ()
  "Toggle reaction in conversation sections."
  (interactive)
  (code-review-reaction--toggle-in-context "comment"))

(defun code-review-code-comment-reaction-at-point ()
  "Toggle reaction in code-comment section."
  (interactive)
  (code-review-reaction--toggle-in-context "code-comment"))

(defun code-review-comment-insert-reactions (reactions context-name comment-id)
  "Insert REACTIONS in CONTEXT-NAME identified by COMMENT-ID."
  (let* ((reactions-obj (code-review-reactions-section
                         :comment-id comment-id
                         :reactions reactions
                         :context-name context-name)))
    (magit-insert-section (code-review-reactions-section reactions-obj)
      (let ((reactions-group (-group-by #'identity reactions)))
        (dolist (r (a-keys reactions-group))
          (let ((rit (alist-get r reactions-group nil nil 'equal)))
            (insert (alist-get (oref (-first-item rit) content)
                               code-review-reaction-types
                               nil nil 'equal))
            (insert (format " %S " (length rit)))))
        (insert ?\n)
        (insert ?\n)))))

(provide 'code-review-reactions)
;;; code-review-reactions.el ends here
