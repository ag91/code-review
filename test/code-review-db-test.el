;;; code-review-db-test.el --- ERT tests for the code-review database -*- lexical-binding: t; -*-

(require 'ert)
(require 'eieio)
(require 'dash)
(require 'a)
(require 'code-review-db)
(require 'code-review-github)
(require 'code-review-local)
(require 'code-review-test-helpers)

(defun code-review-db-test--sample-pr-obj ()
  "Return a fresh sample pullreq object.
Must be rebuilt per test: closql writes through `oset', so a shared
object would accumulate ids and a stale database pointer."
  (code-review-github-repo
   :owner "owner"
   :repo "repo"
   :number "num"))

(ert-deftest code-review-db-test/create-pullreq ()
  "We should be able to create a pullreq db obj from a pr-obj."
  (code-review-test--with-db
    (code-review-db--pullreq-create (code-review-db-test--sample-pr-obj))
    (let ((pr (code-review-db-get-pullreq)))
      (should (oref pr id))
      (should (equal (oref pr repo) "repo"))
      (should (equal (oref pr owner) "owner"))
      (should (equal (oref pr number) "num")))))

(ert-deftest code-review-db-test/get-back-original-fields ()
  "From the pullreq db obj we can get back the original fields."
  (code-review-test--with-db
    (code-review-db--pullreq-create (code-review-db-test--sample-pr-obj))
    (let ((pr (code-review-db-get-pullreq)))
      (should (equal (oref pr owner) "owner"))
      (should (equal (oref pr repo) "repo"))
      (should (equal (oref pr number) "num")))))

(ert-deftest code-review-db-test/pullreq-sha-update ()
  "Update the sha value of a pullreq."
  (code-review-test--with-db
    (code-review-db--pullreq-create (code-review-db-test--sample-pr-obj))
    (code-review-db--pullreq-sha-update "SHA")
    (should (equal (oref (code-review-db-get-pullreq) sha)
                   "SHA"))))

(ert-deftest code-review-db-test/curr-path-update-creates-buffer-with-paths ()
  "Updating the current path should create a buffer with paths."
  (code-review-test--with-db
    (code-review-db--pullreq-create (code-review-db-test--sample-pr-obj))
    (code-review-db--curr-path-update "github.el")
    (let* ((paths (oref (code-review-db-get-buffer) paths))
           (path (-first-item paths)))
      (should (equal (oref path name) "github.el"))
      (should (null (oref path head-pos)))
      (should (oref path at-pos-p)))))

(ert-deftest code-review-db-test/curr-path-update-disables-previous-at-pos-p ()
  "Updating current path should disable `at-pos-p' of previous paths."
  (code-review-test--with-db
    (code-review-db--pullreq-create (code-review-db-test--sample-pr-obj))
    (code-review-db--curr-path-update "github.el")
    (code-review-db--curr-path-update "gitlab.el")
    (let* ((paths (oref (code-review-db-get-buffer) paths)))
      (dolist (path paths)
        (cond
         ((string-equal (oref path name) "github.el")
          (should (null (oref path at-pos-p))))
         ((string-equal (oref path name) "gitlab.el")
          (should (oref path at-pos-p))))))))

(ert-deftest code-review-db-test/curr-path-head-pos-update ()
  "Update path head-pos value."
  (code-review-test--with-db
    (code-review-db--pullreq-create (code-review-db-test--sample-pr-obj))
    (code-review-db--curr-path-update "github.el")
    (code-review-db--curr-path-head-pos-update "github.el" 42)
    (let* ((paths (oref (code-review-db-get-buffer) paths)))
      (dolist (p paths)
        (when (string-equal (oref p name) "github.el")
          (should (equal (oref p head-pos) 42)))))))

;;;; Schema/column order (phase 21)

(ert-deftest code-review-db-test/schema-column-order-matches-class-slots ()
  "THE assertion that would have caught the v9 url shift on day one.
closql serializes slots POSITIONALLY (a bare INSERT INTO ...
VALUES with no column list), so every table's column order must
equal its class's slot order.  The `closql-database' slot maps
onto the class tag column; `:closql-class' child slots (e.g.
pullreq.buffer, buffer.paths, path.comments) merely occupy their
column's position, sometimes under a different name."
  (code-review-test--with-db
    (let ((db (code-review-db)))
      (dolist (pair '((pullreq . code-review-github-repo)
                      (pullreq . code-review-local-diff)
                      (buffer . code-review-db-buffer)
                      (path . code-review-db-path)
                      (comment . code-review-db-comment)))
        (pcase-let ((`(,table . ,class) pair))
          (let* ((slot-names
                  (mapcar (lambda (d)
                            (let ((name (cl--slot-descriptor-name d)))
                              (string-replace "-" "_" (symbol-name name))))
                          (eieio-class-slots (eieio--class-object class))))
                 (columns (mapcar #'symbol-name
                                  (closql--table-columns db table))))
            (should (equal (length slot-names) (length columns)))
            (should (equal (car columns) "class"))
            (dotimes (i (length slot-names))
              (let ((slot (nth i slot-names))
                    (column (nth i columns)))
                (should
                 (or (equal slot column)
                     ;; the db slot maps onto the class tag column
                     (equal slot "closql_database")
                     ;; a child slot: only the position matters
                     (closql--slot-properties
                      (eieio--class-object class)
                      (intern (string-replace "_" "-" slot)))))))))))))

(ert-deftest code-review-db-test/named-column-roundtrip ()
  "Inserting a pullreq and reading it through NAMED-column SQL
must land every value in its own column: before v10 every named
predicate past `number' read the neighbor's column (the v9 url
slot shift)."
  (code-review-test--with-db
    (code-review-db--pullreq-create
     (code-review-github-repo :owner "owner" :repo "repo" :number "42"
                              :url "https://example.test/repo/pull/42"))
    (let ((pr (code-review-db-get-pullreq)))
      (oset pr description "DESC-MARK")
      (oset pr title "TITLE-MARK")
      (oset pr sha "SHA-MARK")
      (oset pr state "OPEN")
      (code-review-db-update pr))
    (let ((row (car (emacsql (code-review-db)
                             [:select [url description title sha state]
                              :from pullreq]))))
      (should (equal row '("https://example.test/repo/pull/42"
                          "DESC-MARK" "TITLE-MARK" "SHA-MARK" "OPEN"))))))

(ert-deftest code-review-db-test/local-cleanup-deletes-local-rows ()
  "The phase 12 local-row cleanup predicate (WHERE state =
LOCAL) must match rows again: between phase 7 and phase 21 the
state column carried feedback values while the LOCAL marker sat
in the replies column, so the cleanup silently deleted nothing."
  (code-review-test--with-db
    (code-review-db--pullreq-create (code-review-db-test--sample-pr-obj))
    (let ((pr (code-review-db-get-pullreq)))
      (oset pr state "LOCAL")
      (code-review-db-update pr))
    (should (equal (oref (code-review-db-get-pullreq) state) "LOCAL"))
    (code-review-local--cleanup-rows)
    (should (null (emacsql (code-review-db) [:select [id] :from pullreq])))))

;;;; One row per PR (phase 21)

(ert-deftest code-review-db-test/pullreq-create-dedupes-rows-per-pr ()
  "Creating a pullreq deletes rows for the same (OWNER REPO
NUMBER) left over from earlier opens, together with their
buffer/path/comment children: one row per PR at any time, fresh
path bookkeeping per open."
  (code-review-test--with-db
    (code-review-db--pullreq-create (code-review-db-test--sample-pr-obj))
    (let ((first-id code-review-db--pullreq-id))
      (code-review-db--curr-path-update "a.el")
      (should (= 1 (length (emacsql (code-review-db)
                                    [:select [id] :from buffer]))))
      (code-review-db--pullreq-create (code-review-db-test--sample-pr-obj))
      (should (not (equal code-review-db--pullreq-id first-id)))
      (should (= 1 (length (emacsql (code-review-db)
                                    [:select [id] :from pullreq]))))
      ;; the old row's children died with it
      (should (null (emacsql (code-review-db)
                             [:select [id] :from buffer :where (= pullreq $s1)]
                             first-id)))
      ;; a different PR coexists
      (code-review-db--pullreq-create
       (code-review-github-repo :owner "owner" :repo "repo" :number "43"))
      (should (= 2 (length (emacsql (code-review-db)
                                    [:select [id] :from pullreq])))))))

;;;; GC (phase 21)

(ert-deftest code-review-db-test/cleanup-dedupes-purges-finished-and-vacuums ()
  "`code-review-db-cleanup' dedupes rows per PR (keeping the
newest), purges finished rows, and — with a prefix — every
saved=nil row except the current review's.  Children are
deleted with their parents."
  (code-review-test--with-db
    (let ((db (code-review-db)))
      (code-review-db--pullreq-create (code-review-db-test--sample-pr-obj))
      (let ((newest-id code-review-db--pullreq-id))
        ;; a legacy dup PAIR for another PR (raw inserts bypass the
        ;; create-dedupe; legacy databases are full of these)
        (emacsql db "INSERT INTO pullreq (class, id, owner, repo, number, buffer) VALUES ('github-repo', '\"legacy-a\"', '\"owner\"', '\"repo-legacy\"', '\"11\"', 'eieio-unbound')")
        (emacsql db "INSERT INTO buffer (class, id, pullreq) VALUES ('buffer', '\"legacy-a\"', '\"legacy-a\"')")
        (emacsql db "INSERT INTO pullreq (class, id, owner, repo, number, buffer) VALUES ('github-repo', '\"legacy-b\"', '\"owner\"', '\"repo-legacy\"', '\"11\"', 'eieio-unbound')")
        ;; a finished row and a saved unfinished row for other PRs
        (emacsql db "INSERT INTO pullreq (class, id, owner, repo, number, finished, buffer) VALUES ('github-repo', '\"fin-1\"', '\"owner\"', '\"repo\"', '\"9\"', 't', 'eieio-unbound')")
        (emacsql db "INSERT INTO pullreq (class, id, owner, repo, number, saved, buffer) VALUES ('github-repo', '\"sav-1\"', '\"owner\"', '\"repo2\"', '\"5\"', 't', 'eieio-unbound')")
        (code-review-db-cleanup)
        (let ((rows (emacsql db [:select [id] :from pullreq])))
          ;; the current row, the newest dup (legacy-b) and the saved
          ;; row survive; the older dup and the finished row die
          (should (member (list newest-id) rows))
          (should (member (list "legacy-b") rows))
          (should (member (list "sav-1") rows))
          (should (= 3 (length rows))))
        ;; the older dup's children died with it
        (should (null (emacsql db [:select [id] :from buffer :where (= id $s1)]
                              "legacy-a")))
        ;; prefix purge: every saved=nil row except the current PR
        (code-review-db-cleanup t)
        (let ((rows (emacsql db [:select [id] :from pullreq])))
          (should (member (list newest-id) rows))
          (should (member (list "sav-1") rows))
          (should (= 2 (length rows))))))))

;;;; v10 migration (phase 21)

(defun code-review-db-test--build-v9-db (file shape)
  "Create FILE as a version-9 db with the pre-v10 pullreq table SHAPE.
SHAPE `migrated' is what the running v7-v8-v9 migrations produced
(the 31-column pre-phase-7 table with the url column ALTERed on
at the END, like the user's real database); `fresh-schema' is
what v9-era code created from scratch (url after linked_issues).
Seeds a pre-phase-7 row (values in the correct columns), a
phase-7+ row in the SHIFTED layout (the class's positional write:
the url slot's value lands on the description column, the state
slot's on replies, the buffer slot's on callback), and
buffer/path children on the shifted row.  String values carry
the embedded quotes closql stores them with (prin1 escaping:
strings quoted, symbols bare); emacsql reads them back through
the same convention."
  (code-review-test--reset-db file)
  (let ((db (code-review-db)))
    (emacsql db "DROP TABLE pullreq")
    (if (eq shape 'migrated)
        (progn
          (emacsql db "CREATE TABLE pullreq (class TEXT NOT NULL, id TEXT NOT NULL PRIMARY KEY, base_ref_name, head_ref_name, finished, finished_at, saved, saved_at, raw_infos, raw_diff, raw_comments, owner, repo, number, description, title, host, sha, feedback, state, replies, review, labels, merge, milestones, projects, reviewers, assignees, linked_issues, buffer DEFAULT eieio_unbound, callback)")
          (emacsql db "ALTER TABLE pullreq ADD COLUMN url DEFAULT NULL"))
      (emacsql db "CREATE TABLE pullreq (class TEXT NOT NULL, id TEXT NOT NULL PRIMARY KEY, base_ref_name, head_ref_name, finished, finished_at, saved, saved_at, raw_infos, raw_diff, raw_comments, owner, repo, number, description, title, host, sha, feedback, state, replies, review, labels, merge, milestones, projects, reviewers, assignees, linked_issues, url, buffer DEFAULT eieio_unbound, callback)"))
    ;; a pre-phase-7 row: values in the CORRECT named columns
    (emacsql db "INSERT INTO pullreq (class, id, owner, repo, number, description, title, sha, state, buffer) VALUES ('github-repo', '\"old-id\"', '\"o\"', '\"r\"', '\"3\"', '\"OLD-DESC\"', '\"OLD-TITLE\"', '\"OLD-SHA\"', '\"OPEN\"', 'eieio-unbound')")
    ;; a phase-7+ row: the class's POSITIONAL write into all 32
    ;; columns (value k = slot k, so the url value lands on
    ;; description, the state value on replies, ...)
    (emacsql db "INSERT INTO pullreq VALUES ('github-repo', '\"new-id\"', NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, '\"o\"', '\"r\"', '\"7\"', '\"https://example.test/r/pull/7\"', '\"NEW-DESC\"', '\"NEW-TITLE\"', '\"NEW-HOST\"', '\"NEW-SHA\"', '\"NEW-FEEDBACK\"', '\"OPEN\"', NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 'eieio-unbound', NULL)")
    ;; a LIKE-only row: a phase-7+ positional write whose only set
    ;; slot past number was url (the url value sits in the
    ;; description column, replies is NULL), so the classifier
    ;; reaches it ONLY through the description LIKE branch
    (emacsql db "INSERT INTO pullreq (class, id, owner, repo, number, description) VALUES ('github-repo', '\"like-id\"', '\"o\"', '\"r\"', '\"11\"', '\"https://example.test/r/pull/11\"')")
    ;; children on the shifted row
    (emacsql db "INSERT INTO buffer (class, id, pullreq, raw_text) VALUES ('buffer', '\"new-id\"', '\"new-id\"', '\"RAW\"')")
    (emacsql db "INSERT INTO path (class, id, name, buffer, at_pos_p) VALUES ('path', '\"path-1\"', '\"f.el\"', '\"new-id\"', 1)")
    (emacsql db [:pragma (= user-version 9)])
    ;; leave closing to `code-review-test--reset-db': closing here
    ;; would leave the singleton with a nil-but-bound connection,
    ;; which its live-p check would trip over
    nil)
  (code-review-test--reset-db file))

(defun code-review-db-test--v10-migration-assertions (db)
  "Assert the state of DB after a v10 migration of a seeded v9 db."
  (should (= 10 (closql--db-get-version db)))
  ;; canonical column order, no callback column
  (should (equal (closql--table-columns db 'pullreq)
                 code-review-db--v10-canonical-columns))
  ;; the shifted row: every value in its own named column now
  (let ((row (car (emacsql db [:select [url description title host sha
                                          feedback state replies buffer]
                                   :from pullreq :where (= id $s1)]
                           "new-id"))))
    (should (equal (nth 0 row) "https://example.test/r/pull/7"))
    (should (equal (nth 1 row) "NEW-DESC"))
    (should (equal (nth 2 row) "NEW-TITLE"))
    (should (equal (nth 3 row) "NEW-HOST"))
    (should (equal (nth 4 row) "NEW-SHA"))
    (should (equal (nth 5 row) "NEW-FEEDBACK"))
    (should (equal (nth 6 row) "OPEN"))
    (should (null (nth 7 row)))
    ;; the buffer marker: closql stores it as the plain text
    ;; `eieio-unbound' (`closql--intern-unbound' on write) and
    ;; post-processes every decoded row with
    ;; `closql--extern-unbound', which returns the LIVE marker
    ;; (`eieio--unbound' on Emacs 31; always the value of
    ;; `eieio-unbound') -- never the quoted literal symbol.
    (should (eq (nth 8 row) eieio-unbound)))
  ;; the pre-phase-7 row is untouched
  (let ((row (car (emacsql db [:select [description title sha state url]
                                   :from pullreq :where (= id $s1)]
                           "old-id"))))
    (should (equal row '("OLD-DESC" "OLD-TITLE" "OLD-SHA" "OPEN" nil))))
  ;; the LIKE-only row: rotated through the description LIKE
  ;; branch alone (url value from the description column into
  ;; the url column, NULLs stay NULL)
  (let ((row (car (emacsql db [:select [url description title state]
                                   :from pullreq :where (= id $s1)]
                           "like-id"))))
    (should (equal (nth 0 row) "https://example.test/r/pull/11"))
    (should (null (nth 1 row))))
  ;; children survive the table rebuild
  (should (= 1 (length (emacsql db [:select [id] :from buffer
                                          :where (= pullreq $s1)]
                                "new-id"))))
  (should (= 1 (length (emacsql db [:select [id] :from path
                                          :where (= buffer $s1)]
                                "new-id")))))

(ert-deftest code-review-db-test/migrate-v10-unshifts-migrated-shape ()
  "The v10 migration unshifts the values of the MIGRATED v9 shape
(the user's real database: url ALTERed on after callback) into
the canonical column order."
  (let ((file (code-review-test--fresh-db-file)))
    (unwind-protect
        (progn
          (code-review-db-test--build-v9-db file 'migrated)
          (code-review-db-test--v10-migration-assertions (code-review-db)))
      (code-review-test--reset-db nil)
      (ignore-errors
        (delete-file file)
        (delete-file (concat file "-shm"))
        (delete-file (concat file "-wal"))))))

(ert-deftest code-review-db-test/migrate-v10-unshifts-fresh-schema-shape ()
  "The v10 migration unshifts the values of the FRESH-SCHEMA v9
shape (url after linked_issues, what v9-era code created from
scratch) into the canonical column order."
  (let ((file (code-review-test--fresh-db-file)))
    (unwind-protect
        (progn
          (code-review-db-test--build-v9-db file 'fresh-schema)
          (code-review-db-test--v10-migration-assertions (code-review-db)))
      (code-review-test--reset-db nil)
      (ignore-errors
        (delete-file file)
        (delete-file (concat file "-shm"))
        (delete-file (concat file "-wal"))))))

(ert-deftest code-review-db-test/migrate-v10-from-v8-runs-the-chain ()
  "A v8 database migrates through the v9 ALTER (url appended at
the end) into v10's canonical layout, values intact."
  (let ((file (code-review-test--fresh-db-file)))
    (unwind-protect
        (progn
          (code-review-test--reset-db file)
          (let ((db (code-review-db)))
            (emacsql db "DROP TABLE pullreq")
            (emacsql db "CREATE TABLE pullreq (class TEXT NOT NULL, id TEXT NOT NULL PRIMARY KEY, base_ref_name, head_ref_name, finished, finished_at, saved, saved_at, raw_infos, raw_diff, raw_comments, owner, repo, number, description, title, host, sha, feedback, state, replies, review, labels, merge, milestones, projects, reviewers, assignees, linked_issues, buffer DEFAULT eieio_unbound, callback)")
            (emacsql db "INSERT INTO pullreq (class, id, owner, repo, number, description, title, sha, state, buffer) VALUES ('github-repo', '\"old-id\"', '\"o\"', '\"r\"', '\"3\"', '\"OLD-DESC\"', '\"OLD-TITLE\"', '\"OLD-SHA\"', '\"OPEN\"', 'eieio-unbound')")
            (emacsql db [:pragma (= user-version 8)]))
          (code-review-test--reset-db file)
          (let ((db (code-review-db)))
            (should (= 10 (closql--db-get-version db)))
            (should (equal (closql--table-columns db 'pullreq)
                           code-review-db--v10-canonical-columns))
            (let ((row (car (emacsql db [:select [description title sha state url]
                                         :from pullreq :where (= id $s1)]
                                     "old-id"))))
              (should (equal row '("OLD-DESC" "OLD-TITLE" "OLD-SHA" "OPEN" nil)))))
          (code-review-test--reset-db nil)
          (ignore-errors
            (delete-file file)
            (delete-file (concat file "-shm"))
            (delete-file (concat file "-wal")))))))

;;; code-review-db-test.el ends here
