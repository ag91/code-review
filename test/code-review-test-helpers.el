;;; code-review-test-helpers.el --- Shared helpers for code-review ERT tests -*- lexical-binding: t; -*-
;;
;; Every test that touches the database must run against a fresh,
;; isolated sqlite file.  The database is an `eieio-singleton' (see
;; `closql-db'), so "isolating" means three things:
;;
;;   1. close any live connection,
;;   2. forget the singleton instance,
;;   3. point `code-review-db-database-file' at a new file
;;      and reset `code-review-db--pullreq-id'.
;;
;; Without this, tests would silently share (or worse: write to) the
;; user's real database.

(require 'eieio)
(require 'cl-lib)
(require 'closql)
(require 'emacsql)
(require 'uuidgen)
(require 'a)
(require 'magit-section)
(require 'ert)
(require 'code-review-db)

(defun code-review-test--fresh-db-file ()
  "Return a unique temporary sqlite file name."
  (format "/tmp/code-review-test-db-%s.sqlite" (uuidgen-4)))

(defun code-review-test--reset-db (file)
  "Point the singleton database at FILE, closing any live connection first.
FILE nil means: just drop the singleton."
  (let ((db (ignore-errors (oref-default 'code-review-db-database singleton))))
    (when (and (eieio-object-p db)
               (slot-boundp db 'connection)
               (emacsql-live-p (oref db connection)))
      (emacsql-close db))
    (oset-default code-review-db-database singleton eieio--unbound)
    (setf code-review-db-database-file file
          code-review-db--pullreq-id nil)))

(defmacro code-review-test--with-db (&rest body)
  "Run BODY with a fresh, isolated test database.
Cleans up (connection, singleton, sqlite file) when done."
  (declare (indent 0))
  (let ((file (make-symbol "file")))
    `(let ((,file (code-review-test--fresh-db-file)))
       (code-review-test--reset-db ,file)
       (unwind-protect
           (progn ,@body)
         (code-review-test--reset-db nil)
         (ignore-errors
           (delete-file ,file)
           (delete-file (concat ,file "-shm"))
           (delete-file (concat ,file "-wal")))))))

(defun code-review-test--sections-match (insert-fn expected &optional buffer-empty?)
  "Run INSERT-FN in a temp buffer, match sections against EXPECTED.
EXPECTED is a list of alists \((type . TYPE) (value . VALUE)) for each
section created, in order.  When BUFFER-EMPTY? is non-nil the buffer
must stay empty."
  (with-temp-buffer
    (funcall insert-fn)
    (let ((count 0))
      (magit-wash-sequence
       (lambda ()
         (when-let (section (magit-current-section))
           (with-slots (type value) section
             (let ((rule (nth count expected)))
               (should (eq (a-get rule 'type) type))
               (should (equal (a-get rule 'value) value)))
             (setq count (1+ count))))
         (magit-section-forward-sibling)))
      (if buffer-empty?
          (should (string-empty-p (buffer-string)))
        (should (string-match-p "[[:word:]]" (buffer-string)))))))

(provide 'code-review-test-helpers)
;;; code-review-test-helpers.el ends here
