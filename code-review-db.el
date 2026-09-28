;;; code-review-db.el --- Manage code review database -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Wanderson Ferreira
;;
;; Author: Wanderson Ferreira <https://github.com/wandersoncferreira>
;; Maintainer: Wanderson Ferreira <wand@hey.com>
;; Version: 0.0.7
;; Homepage: https://github.com/wandersoncferreira/code-review
;;
;; This file is not part of GNU Emacs.
;;

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
;;  Description
;;
;;; Code:

(require 'a)
(require 'closql)
(require 'eieio)
(require 'uuidgen)
(require 'dash)

(defcustom code-review-db-database-file
  (expand-file-name "code-review-db-file.sqlite" user-emacs-directory)
  "The file used to store the `code-review' database."
  :group 'code-review
  :type 'file)

(defclass code-review-db-buffer (closql-object)
  ((closql-table        :initform 'buffer)
   (closql-primary-key  :initform 'id)
   (closql-foreign-key  :initform 'pullreq)
   (closql-class-prefix :initform "code-review-")
   (id                  :initarg :id)
   (pullreq             :initarg :pullreq)
   (raw-text            :initform nil)
   (paths               :closql-class code-review-db-path)))

(defclass code-review-db-path (closql-object)
  ((closql-table        :initform 'path)
   (closql-primary-key  :initform 'id)
   (closql-foreign-key  :initform 'buffer)
   (closql-class-prefix :initform "code-review-")
   (id                  :initarg :id)
   (name                :initarg :name)
   (head-pos            :initform nil)
   (buffer              :initarg :buffer)
   (at-pos-p            :initarg :at-pos-p)
   (comments            :closql-class code-review-db-comment)))

(defclass code-review-db-comment (closql-object)
  ((closql-table        :initform 'comment)
   (closql-primary-key  :initform 'id)
   (closql-foreign-key  :initform 'path)
   (closql-class-prefix :initform "code-review-")
   (id                  :initarg :id)
   (path                :initarg :path)
   (loc-written         :initform nil)
   (identifiers         :initarg :identifiers)))

(defclass code-review-db-pullreq (closql-object)
  ((closql-table        :initform 'pullreq)
   (closql-primary-key  :initform 'id)
   (closql-class-prefix :initform "code-review-")
   (closql-order-by     :initform [(desc number)])
   (id                  :initarg :id)
   (base-ref-name       :initform nil)
   (head-ref-name       :initform nil)
   (finished            :initform nil)
   (finished-at         :initform nil)
   (saved               :initform nil)
   (saved-at            :initform nil)
   (raw-infos           :initform nil)
   (raw-diff            :initform nil)
   (raw-comments        :initform nil)
   (owner               :initarg :owner)
   (repo                :initarg :repo)
   (number              :initarg :number)
   (description         :initform nil)
   (title               :initform nil)
   (host                :initform nil)
   (sha                 :initform nil)
   (feedback            :initform nil)
   (state               :initform nil)
   (replies             :initform nil)
   (review              :initform nil)
   (labels              :initform nil)
   (merge               :initform nil)
   (milestones          :initform nil)
   (projects            :initform nil)
   (reviewers           :initform nil)
   (assignees           :initform nil)
   (linked-issues       :initform nil)
   ;; KEEP THIS ORDER IN SYNC with the schema column order: closql
   ;; serializes slots POSITIONALLY (a bare INSERT INTO ... VALUES
   ;; with no column list), so a slot sitting where no same-named
   ;; column sits silently writes into the neighbor's column (the
   ;; v9 url bug this comment guards against).
   (url                 :initarg :url)
   (buffer              :closql-class code-review-db-buffer))
  :abstract t)

(defclass code-review-db-database (closql-database)
  ((name         :initform "code-review-db")
   (object-class :initform 'code-review-db-pullreq)
   (file         :initform 'code-review-db-database-file)
   (schemata     :initform 'code-review-db-table-schema)
   (version      :initform 10)))

(defvar code-review-db--override-connection-class nil)

(defvar code-review-db--sqlite-available-p t)

(defun code-review-db (&optional livep)
  (condition-case err
      (closql-db 'code-review-db-database livep code-review-db--override-connection-class)
    (error (setq code-review-db--sqlite-available-p nil)
           (signal (car err) (cdr err)))))

;; Closql 2.x compatibility: ensure the class symbol passed to closql is correct.
;; We specialize `closql-insert` for our database to avoid relying on Closql internals
;; that expect a specific class representation in the object vector.
(cl-defmethod closql-insert ((db code-review-db-database) obj &optional replace)
  (closql--oset obj 'closql-database db)
  (let (alist)
    (dolist (slot (eieio-class-slots (eieio--object-class obj)))
      (setq slot (cl--slot-descriptor-name slot))
      (when (alist-get :closql-table (closql--slot-properties obj slot))
        (push (cons slot (closql-oref obj slot)) alist)
        (closql--oset obj slot eieio--unbound)))
    (closql-with-transaction db
      (emacsql db
               (if replace
                   [:insert-or-replace-into $i1 :values $v2]
                 [:insert-into $i1 :values $v2])
               (oref-default obj closql-table)
               (pcase-let ((`(,_class ,_db . ,values)
                            (closql--intern-unbound
                             (closql--coerce obj 'list))))
                 ;; Use the class symbol from the object’s class, not the raw vector tag.
                 (vconcat (cons (closql--abbrev-class (eieio-object-class obj))
                                values))))
      (pcase-dolist (`(,slot . ,value) alist)
        (closql-dset obj slot value))))
  obj)

;;; Schema

(defconst code-review-db-table-schema
  '((pullreq
     [(class :not-null)
      (id :not-null :primary-key)
      base-ref-name
      head-ref-name
      finished
      finished-at
      saved
      saved-at
      raw-infos
      raw-diff
      raw-comments
      owner
      repo
      number
      description
      title
      host
      sha
      feedback
      state
      replies
      review
      labels
      merge
      milestones
      projects
      reviewers
      assignees
      linked-issues
      url
      (buffer :default eieio-unbound)])

    (buffer
     [(class :not-null)
      (id :not-null :primary-key)
      pullreq
      raw-text
      (path :default eieio-unbound)]
     (:foreign-key
      [pullreq] :references pullreq [id]
      :on-delete :cascade))

    (path
     [(class :not-null)
      (id :not-null :primary-key)
      name
      head-pos
      buffer
      at-pos-p
      (comment :default eieio-unbound)]
     (:foreign-key
      [buffer] :references buffer [id]
      :on-delete :cascade))

    (comment
     [(class :not-null)
      (id :not-null :primary-key)
      path
      loc-written
      identifiers]
     (:foreign-key
      [path] :references path [id]
      :on-delete :cascade))))

(defconst code-review-db--v10-canonical-columns
  '(class id base_ref_name head_ref_name finished finished_at
    saved saved_at raw_infos raw_diff raw_comments owner repo
    number description title host sha feedback state replies
    review labels merge milestones projects reviewers assignees
    linked_issues url buffer)
  "The canonical (v10) pullreq column order: the schema's order.
Also the insert-value order of the `code-review-db-pullreq'
class (plus its `closql-database' slot), since closql
serializes slots positionally.")

(defun code-review-db--migrate-v10 (db)
  "Migrate the pullreq table of DB to the canonical v10 layout.

The v9 migration added the url COLUMN with ALTER TABLE (sqlite
appends it at the END of the table) while the class serialized
the url SLOT right after number: closql writes slots
positionally, so every slot from url through the tail landed one
column LEFT of its name.  Writes and reads permute identically
(the in-memory roundtrip is self-consistent), but every
NAMED-column SQL predicate read the wrong column: the phase 12
local-row cleanup (WHERE state = LOCAL) silently stopped
matching anything the day phase 7 shipped.

v10 moves the url slot after linked-issues (where the schema
always had the column) and unshifts the VALUES of existing rows,
per row: phase-7+ rows are recognizable by the state-slot values
sitting in the replies column (OPEN/MERGED/...; pre-phase-7
rows have NULL there and their state in the state column) or
the url-slot values in the description column (https URLs).
All-NULL rows are identical under both layouts, so the cycle
leaves them alone.  Two v9 table shapes exist and need
different cycles: migrated tables carry url after callback
(the 18-column cycle runs through the tail), fresh-schema
tables already have it after linked_issues (the cycle stops at
url).  Finally the table is rebuilt in the canonical column
order, which also drops the dead callback column the v9-era
classes carried a slot for."
  (let* ((columns (closql--table-columns db 'pullreq))
         (url-last-p (= 1 (length (member 'url columns))))
         (tail (if url-last-p
                   ;; migrated shape: the buffer and callback
                   ;; columns carry linked-issues/buffer values
                   "linked_issues = buffer, buffer = callback, callback = url, url = description"
                 ;; fresh-schema shape: url already holds
                 ;; linked-issues values; buffer/callback are fine
                 "linked_issues = url, url = description"))
         (cycle (concat "description = title, title = host, "
                       "host = sha, sha = feedback, feedback = state, "
                       "state = replies, replies = review, review = labels, "
                       "labels = merge, merge = milestones, "
                       "milestones = projects, projects = reviewers, "
                       "reviewers = assignees, assignees = linked_issues, "
                       tail)))
    ;; sqlite reads every RHS from the pre-statement row, so this
    ;; single cyclic UPDATE unshifts every classified row.  Literals
    ;; carry the embedded quotes closql stores strings with (prin1
    ;; escaping: symbols are stored bare, strings quoted).  The LIKE
    ;; wildcard must be a RAW literal with a doubled %%: emacsql
    ;; formats prepared raw strings through `format', so %% collapses
    ;; to the % wildcard at execution.  A PARAMETER cannot carry this
    ;; pattern at all: `emacsql-escape-scalar' prin1-escapes args for
    ;; STORAGE, so the arg "https://% becomes the pattern
    ;; "https://% with a literal backslash and never matches; and
    ;; `concat' (not `format') builds the statement so nothing
    ;; collapses the %% before emacsql sees it.
    (emacsql db (concat "UPDATE pullreq SET " cycle
                       " WHERE replies IN ('\"OPEN\"', '\"MERGED\"', '\"CLOSED\"', '\"DRAFT\"', '\"LOCAL\"')"
                       " OR description LIKE '\"https://%%'"))
    ;; rebuild the table in the canonical column order.  PRAGMA
    ;; foreign_keys is a NO-OP inside a transaction, so toggle it
    ;; outside and do the swap in one transaction below.
    (emacsql db [:pragma (= foreign-keys off)])
    (closql-with-transaction db
      (emacsql db [:create-table $i1 $S2]
               'pullreq_v10
               (cdr (assq 'pullreq code-review-db-table-schema)))
      (emacsql db (concat "INSERT INTO pullreq_v10 ("
                          (mapconcat #'symbol-name
                                     code-review-db--v10-canonical-columns
                                     ", ")
                          ") SELECT "
                          (mapconcat #'symbol-name
                                     code-review-db--v10-canonical-columns
                                     ", ")
                          " FROM pullreq"))
      (emacsql db "DROP TABLE pullreq")
      (emacsql db "ALTER TABLE pullreq_v10 RENAME TO pullreq")
      (closql--db-set-version db 10))
    (emacsql db [:pragma (= foreign-keys on)])))

(cl-defmethod closql--db-update-schema ((db code-review-db-database))
  (let ((code-version (oref-default 'code-review-db-database version))
        (version (closql--db-get-version db)))
    (closql-with-transaction db
      (when (= version 7)
        (message "Upgrading Code Review database from version 7 to 8...")
        (emacsql db [:alter-table pullreq :add-column base-ref-name :default nil])
        (emacsql db [:alter-table pullreq :add-column head-ref-name :default nil])
        (closql--db-set-version db (setq version 8))
        (message "Upgrading Code Review database from version 7 to 8...done"))
      (when (= version 8)
        (message "Upgrading Code Review database from version 8 to 9...")
        (emacsql db [:alter-table pullreq :add-column url :default nil])
        (closql--db-set-version db (setq version 9))
        (message "Upgrading Code Review database from version 8 to 9...done")))
    ;; v10 runs OUTSIDE the transaction above: the migration toggles
    ;; PRAGMA foreign_keys, which is a no-op inside a transaction.
    (when (= version 9)
      (message "Upgrading Code Review database from version 9 to 10...")
      (code-review-db--migrate-v10 db)
      (message "Upgrading Code Review database from version 9 to 10...done"))
    (cl-call-next-method)))

;;; Core

(defvar code-review-db--pullreq-id nil)

;; Helper

(defun code-review-db--delete-pullreq-tree (db id)
  "Delete the pullreq row ID and its buffer/path/comment children.
FK enforcement is off on emacsql connections (nothing cascades),
so the children are deleted explicitly, child-most first."
  (emacsql db "DELETE FROM comment WHERE path IN (SELECT id FROM path WHERE buffer IN (SELECT id FROM buffer WHERE pullreq = $s1))" id)
  (emacsql db "DELETE FROM path WHERE buffer IN (SELECT id FROM buffer WHERE pullreq = $s1)" id)
  (emacsql db "DELETE FROM buffer WHERE pullreq = $s1" id)
  (emacsql db "DELETE FROM pullreq WHERE id = $s1" id))

(defun code-review-db-update (obj)
  "Update whole OBJ in datatabase."
  (closql-insert (code-review-db) obj t))

(defun code-review-db-search (owner repo number)
  "Find eieio obj of PR for OWNER, REPO, and NUMBER."
  (let ((db (code-review-db))
        (class 'code-review-db-pullreq))
    (->> (emacsql db [:select :*
                              :from 'pullreq
                              :where (and (= owner $s1)
                                          (= repo $s2)
                                          (= number $s3)
                                          (= saved 't)
                                          (is finished nil))]
                  owner
                  repo
                  number)
         (mapcar
          (lambda (row) (closql--remake-instance class db row)))
         (-last-item))))

(defun code-review-db-all-unfinished ()
  "Get a list of all unfinished Reviews."
  (let ((class 'code-review-db-pullreq)
        (db (code-review-db)))
    (->> (emacsql db
                  [:select :*
                           :from 'pullreq
                           :where (and (= saved 't)
                                       (is finished nil))])
         (mapcar
          (lambda (row) (closql--remake-instance class db row))))))

;;;###autoload
(defun code-review-db-cleanup (&optional purge-unsaved-p)
  "Clean up the review database.
Runs a pending schema migration first (the daemon's live
singleton connection may predate the migration code), then
dedupes rows: for every (OWNER REPO NUMBER) only the NEWEST row
(rowid is insertion order), with its buffer/path/comment
children, is kept.  Finished rows are purged; with a prefix
argument PURGE-UNSAVED-P every saved=nil row except the current
review's is purged too (nothing reads them back: unfinished
unsaved rows are pure render cache, the forge is the record).
VACUUMs the file at the end."
  (interactive "P")
  (let ((db (code-review-db))
        (deleted 0))
    ;; a pending migration: the singleton's live connection may
    ;; have been opened by pre-migration code
    (when (/= (closql--db-get-version db)
              (oref-default 'code-review-db-database version))
      (emacsql-close db)
      (oset-default code-review-db-database singleton eieio--unbound)
      (setq db (code-review-db)))
    (let* ((rows (emacsql db [:select [id owner repo number] :from pullreq
                                      :order-by [(asc rowid)]]))
           (newest (make-hash-table :test 'equal))
           (current code-review-db--pullreq-id))
      ;; dedupe per PR, keeping the newest row and the current
      ;; review's row (the current PR may not be the newest one
      ;; for its key in a pre-dedupe database)
      (dolist (row rows)
        (puthash (list (nth 1 row) (nth 2 row) (nth 3 row)) (nth 0 row) newest))
      (dolist (row rows)
        (let ((id (nth 0 row))
              (key (list (nth 1 row) (nth 2 row) (nth 3 row))))
          (unless (or (equal id (gethash key newest))
                      (equal id current))
            (code-review-db--delete-pullreq-tree db id)
            (setq deleted (1+ deleted)))))
      ;; purge finished rows; with the prefix also every saved=nil
      ;; row except the current review's
      (let ((rows (if purge-unsaved-p
                      (emacsql db [:select [id] :from pullreq
                                          :where (or (= finished 't)
                                                     (is saved nil))])
                    (emacsql db [:select [id] :from pullreq
                                        :where (= finished 't)]))))
        (dolist (row rows)
          (unless (equal (car row) current)
            (code-review-db--delete-pullreq-tree db (car row))
            (setq deleted (1+ deleted)))))
      (emacsql db "VACUUM")
      (message "code-review db cleanup: deleted %d rows, %d remain"
               deleted
               (length (emacsql db [:select [id] :from pullreq]))))))

;;; Domain

;; Simplified getters

(defun code-review-db-get-pullreq ()
  "Get pullreq obj from ID."
  (closql-get (code-review-db) code-review-db--pullreq-id 'code-review-db-pullreq))

(defun code-review-db-get-buffer ()
  "Get buffer obj from BUFFER-ID."
  (closql-get (code-review-db) code-review-db--pullreq-id 'code-review-db-buffer))

(defun code-review-db-local-pr-p ()
  "Return non-nil when the current review is a local diff (not a forge PR)."
  (and code-review-db--pullreq-id
       (ignore-errors
         (equal (oref (code-review-db-get-pullreq) state)
                "LOCAL"))))

(defun code-review-db-get-comment (id)
  "Get comment obj from ID."
  (closql-get (code-review-db) id 'code-review-db-comment))

;; ...

(defun code-review-db--pullreq-create (obj)
  "Create a pullreq db object from OBJ.
One row per (OWNER REPO NUMBER) exists at any time: rows for the
same PR left over from earlier opens (and their
buffer/path/comment children) are deleted first, so opening a PR
always starts from fresh path bookkeeping."
  (let* ((db (code-review-db))
         (pr-id (uuidgen-4)))
    (dolist (row (emacsql db [:select [id] :from pullreq
                                    :where (and (= owner $s1)
                                                (= repo $s2)
                                                (= number $s3))]
                          (oref obj owner)
                          (oref obj repo)
                          (oref obj number)))
      (code-review-db--delete-pullreq-tree db (car row)))
    (oset obj id pr-id)
    (closql-insert db obj t)
    (setq code-review-db--pullreq-id pr-id)))

(defun code-review-db--pullreq-sha-update (sha-value)
  "Update pullreq obj of ID with value SHA-VALUE."
  (let ((pr (code-review-db-get-pullreq)))
    (oset pr sha sha-value)
    (closql-insert (code-review-db) pr t)))

(defun code-review-db--pullreq-raw-infos-update (infos)
  "Save INFOS to the PULLREQ entity."
  (let ((pullreq (code-review-db-get-pullreq)))
    (let-alist infos
      (oset pullreq raw-infos infos)
      (oset pullreq title .title)
      (oset pullreq state .state)
      (oset pullreq base-ref-name .baseRefName)
      (oset pullreq head-ref-name .headRefName)
      (oset pullreq description .bodyHTML)
      (oset pullreq sha .headRefOid)
      (oset pullreq raw-comments .reviews.nodes)
      (oset pullreq assignees .assignees.nodes)
      (oset pullreq milestones `((title . ,.milestone.title)
                                 (perc . ,.milestone.progressPercentage)
                                 (number . nil)))
      (closql-insert (code-review-db) pullreq t))))

(defun code-review-db--pullreq-raw-diff-update (raw-diff)
  "Save RAW-DIFF to the PULLREQ entity."
  (let ((pullreq (code-review-db-get-pullreq)))
    (oset pullreq raw-diff raw-diff)
    (closql-insert (code-review-db) pullreq t)))

(defun code-review-db--pullreq-raw-infos ()
  "Get raw infos alist from ID."
  (oref (code-review-db-get-pullreq) raw-infos))

(defun code-review-db--pullreq-raw-comments ()
  "Get raw comments alist from ID."
  (oref (code-review-db-get-pullreq) raw-comments))

(defun code-review-db--pullreq-raw-diff ()
  "Get raw diff alist from ID."
  (oref (code-review-db-get-pullreq) raw-diff))

(defun code-review-db--pullreq-title ()
  "Get title of pullreq."
  (oref (code-review-db-get-pullreq) title))

(defun code-review-db--pullreq-state ()
  "Get state of pullreq."
  (oref (code-review-db-get-pullreq) state))

(defun code-review-db--pullreq-labels ()
  "Get labels of pullreq."
  (oref (code-review-db-get-pullreq) labels))

(defun code-review-db--pullreq-assignees ()
  "Get assignees of pullreq."
  (oref (code-review-db-get-pullreq) assignees))

(defun code-review-db--pullreq-milestones ()
  "Get milestones of pullreq."
  (oref (code-review-db-get-pullreq) milestones))

(defun code-review-db--pullreq-raw-comments-update (comment)
  "Add COMMENT to the pullreq ID."
  (let* ((pr (code-review-db-get-pullreq))
         (raw-comments (oref pr raw-comments)))
    (oset pr raw-comments (append raw-comments (list comment)))
    (code-review-db-update pr)))

(defun code-review-db--pullreq-feedback ()
  "Get feedback for the current pr."
  (let ((pr (code-review-db-get-pullreq)))
    (oref pr feedback)))

(defun code-review-db--pullreq-feedback-update (feedback)
  "Save most recent FEEDBACK."
  (let ((pr (code-review-db-get-pullreq)))
    (oset pr feedback feedback)
    (closql-insert (code-review-db) pr t)))

;;;

(defun code-review-db--curr-path-update (curr-path)
  "Update pullreq (ID) with CURR-PATH."
  (let* ((buf (code-review-db-get-buffer))
         (new-path-id (uuidgen-4))
         (db (code-review-db)))
    (if (not buf)
        (let* ((pr (code-review-db-get-pullreq))
               (pr-id (oref pr id))
               (buf (code-review-db-buffer :id pr-id :pullreq pr-id))
               (path (code-review-db-path :id new-path-id
                                          :buffer pr-id
                                          :name curr-path
                                          :at-pos-p t)))
          (closql-with-transaction db
            (closql-insert db buf t)
            (closql-insert db path t)))
      (let* ((paths (oref buf paths))
             (curr-path-re-enabled? nil))
        (closql-with-transaction db
          ;;; disable all previous ones and enable curr-path is already exists
          (-map
           (lambda (path)
             (if (string-equal (oref path name) curr-path)
                 (progn
                   (oset path at-pos-p t)
                   (closql-insert db path t)
                   (setq curr-path-re-enabled? t))
               (progn
                 (oset path at-pos-p nil)
                 (closql-insert db path t))))
           paths)
          (when (not curr-path-re-enabled?)
            ;; save new one
            (closql-insert db (code-review-db-path
                               :id new-path-id
                               :buffer (oref buf id)
                               :name curr-path
                               :at-pos-p t)
                           t)))))))

(defun code-review-db--curr-path-head-pos-update (curr-path hunk-head-pos)
  "Update pullreq (ID) on CURR-PATH using HUNK-HEAD-POS."
  (let* ((buf (code-review-db-get-buffer))
         (paths (oref buf paths)))
    (dolist (p paths)
      (when (string-equal (oref p name) curr-path)
        (oset p head-pos hunk-head-pos)
        (closql-insert (code-review-db) p t)))))


;;; Accessor Functions

(defun code-review-db--curr-path ()
  "Get the latest activated path for the current pullreq obj."
  (let* ((buf (code-review-db-get-buffer)))
    (->> (oref buf paths)
         (-filter (lambda (p) (oref p at-pos-p)))
         (-first-item))))

(defun code-review-db--curr-comment ()
  "Get the latest activated path comment for the current pullreq obj."
  (let* ((path (code-review-db--curr-path)))
    (code-review-db-get-comment (oref path id))))

;; comments

(defun code-review-db-delete-raw-comment (identifier)
  "Remove comment identified by IDENTIFIER from raw comments list.
IDENTIFIER can be our local 'internal-id' (string UUID) or the provider
'databaseId' (number or string)."
  (let* ((pr (code-review-db-get-pullreq))
         (id-str (cond ((numberp identifier) (number-to-string identifier))
                       ((stringp identifier) identifier)
                       (t (format "%s" identifier))))
         (new-comments
          (-filter
           (lambda (c)
             (let* ((nodes (a-get-in c (list 'comments 'nodes)))
                    (res (-filter
                          (lambda (node)
                            (let* ((iid (a-get node 'internal-id))
                                   (dbid (a-get node 'databaseId))
                                   (dbid-str (when dbid (number-to-string dbid)))
                                   (fullid (a-get node 'fullDatabaseId)))
                              (not (or (and iid (string-equal iid id-str))
                                       (and dbid-str (string-equal dbid-str id-str))
                                       (and fullid (string-equal fullid id-str))))))
                          nodes)))
               res))
           (oref pr raw-comments))))
    (oset pr raw-comments new-comments)
    (closql-insert (code-review-db) pr t)))

(provide 'code-review-db)
;;; code-review-db.el ends here
