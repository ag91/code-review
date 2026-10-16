;;; code-review-db-reviewed.el --- Persistent read tracking -*- lexical-binding: t; -*-

;; This file is part of code-review, distributed under the GNU GPL v3 or later.

;; Copyright (C) 2026 Andrea

;;; Commentary:
;; The reviewed table is separate from the positionally serialized pullreq
;; schema.  Loaded by code-review-db after its core definitions.

;;; Code:

(require 'closql)
(require 'uuidgen)
(require 'cl-lib)

(declare-function code-review-db "code-review-db")
(defvar code-review-db--pullreq-id)

(defclass code-review-db-reviewed (closql-object)
  ((closql-table        :initform 'reviewed)
   (closql-primary-key  :initform 'id)
   (closql-foreign-key  :initform 'pullreq)
   (closql-class-prefix :initform "code-review-")
   (id                  :initarg :id)
   (pullreq             :initarg :pullreq)
   (path                :initarg :path)
   (hunk-key            :initarg :hunk-key)
   (created-at          :initarg :created-at)))

(defun code-review-db--reviewed-create-index (db)
  "Index DB's viewed rows by their operational lookup keys."
  (emacsql db "CREATE INDEX IF NOT EXISTS reviewed_lookup ON reviewed (pullreq, path, hunk_key)"))

(cl-defmethod closql--db-create-schema :after ((db code-review-db-database))
  (code-review-db--reviewed-create-index db))

(cl-defmethod closql--db-update-schema :after ((db code-review-db-database))
  (code-review-db--reviewed-create-index db))

(defun code-review-db--reviewed-rows (&optional pr-id)
  "Return PR-ID's viewed rows, defaulting to the current review.
Each row is a plist (:path PATH :hunk-key KEY); nil KEY marks a file."
  (mapcar (lambda (row) (list :path (car row) :hunk-key (cadr row)))
          (emacsql (code-review-db)
                   [:select [path hunk-key] :from reviewed
                            :where (= pullreq $s1)]
                   (or pr-id code-review-db--pullreq-id))))

(defun code-review-db--reviewed-insert (path &optional hunk-key)
  "Mark PATH or its HUNK-KEY viewed, without inserting duplicate rows."
  (let ((db (code-review-db)))
    (closql-with-transaction db
      (code-review-db--reviewed-delete path hunk-key)
      (closql-insert db
                     (code-review-db-reviewed
                      :id (uuidgen-4) :pullreq code-review-db--pullreq-id
                      :path (substring-no-properties path)
                      :hunk-key (and hunk-key
                                     (substring-no-properties hunk-key))
                      :created-at (float-time))))))

(defun code-review-db--reviewed-delete (path &optional hunk-key)
  "Unmark PATH or its HUNK-KEY in the current review."
  (emacsql (code-review-db)
           [:delete :from reviewed
                    :where (and (= pullreq $s1) (= path $s2)
                                (is hunk-key $s3))]
           code-review-db--pullreq-id path hunk-key))

(defun code-review-db--reviewed-delete-path (path)
  "Remove PATH's file and hunk marks from the current review."
  (emacsql (code-review-db)
           [:delete :from reviewed
                    :where (and (= pullreq $s1) (= path $s2))]
           code-review-db--pullreq-id path))

(defun code-review-db--reviewed-restore-path (path rows)
  "Replace PATH's marks with the snapshot ROWS."
  (closql-with-transaction (code-review-db)
    (code-review-db--reviewed-delete-path path)
    (dolist (row rows)
      (code-review-db--reviewed-insert path (plist-get row :hunk-key)))))

(defun code-review-db--reviewed-replace-file-rows (paths)
  "Replace file-level marks with PATHS, preserving local hunk marks."
  (closql-with-transaction (code-review-db)
    (emacsql (code-review-db)
             [:delete :from reviewed
                      :where (and (= pullreq $s1) (is hunk-key nil))]
             code-review-db--pullreq-id)
    (dolist (path paths)
      (code-review-db--reviewed-insert path))))

(defun code-review-db--reviewed-delete-invalid (keep-file-p keep-hunk-keys)
  "Expire marks absent from the new diff.
KEEP-FILE-P accepts unchanged files.  KEEP-HUNK-KEYS maps current
paths to their hunk headers.  Removed paths always expire."
  (closql-with-transaction (code-review-db)
    (dolist (row (code-review-db--reviewed-rows))
      (let* ((path (plist-get row :path))
             (key (plist-get row :hunk-key))
             (current (assoc path keep-hunk-keys)))
        (unless (and current
                     (if key (member key (cdr current))
                       (funcall keep-file-p path)))
          (code-review-db--reviewed-delete path key))))))

(defun code-review-db--reviewed-file-p (path)
  "Return non-nil when PATH has a file-level mark."
  (cl-some (lambda (row)
             (and (equal path (plist-get row :path))
                  (null (plist-get row :hunk-key))))
           (code-review-db--reviewed-rows)))

(defun code-review-db--reviewed-file-rows ()
  "Return the paths with file-level marks."
  (let (paths)
    (dolist (row (code-review-db--reviewed-rows))
      (unless (plist-get row :hunk-key)
        (cl-pushnew (plist-get row :path) paths :test #'equal)))
    paths))

(defun code-review-db--reviewed-hunk-keys ()
  "Return an alist mapping paths to viewed hunk headers."
  (let (paths)
    (dolist (row (code-review-db--reviewed-rows))
      (when-let* ((key (plist-get row :hunk-key)))
        (let* ((path (plist-get row :path))
               (entry (assoc path paths)))
          (if entry
              (cl-pushnew key (cdr entry) :test #'equal)
            (push (list path key) paths)))))
    paths))

(provide 'code-review-db-reviewed)
;;; code-review-db-reviewed.el ends here
