;;; code-review-search-test.el --- PR search query/parse tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'code-review-search)

;;; req-2026-11-09-003: the query builder is the reviewer's recipe
;; covers: code-review-search.el
;; covers-fn: code-review-search--query
;; enforcement: test
(ert-deftest code-review-search/req-2026-11-09-003-query-builder ()
  "WHEN building a search THE SYSTEM SHALL emit the exact qualifier recipe."
  (let ((as-of (encode-time 0 0 0 9 11 2026 nil nil 0)))
    (should (equal (code-review-search--query "WriterInternal" "amartin" 5 as-of)
                   "org:WriterInternal is:pr author:amartin -review:approved state:open created:>=2026-11-04"))
    ;; a different window shifts the date
    (should (equal (code-review-search--query "acme" "bob" 1 as-of)
                   "org:acme is:pr author:bob -review:approved state:open created:>=2026-11-08"))
    ;; zero days pins the same day
    (should (equal (code-review-search--query "acme" "bob" 0 as-of)
                   "org:acme is:pr author:bob -review:approved state:open created:>=2026-11-09"))))

;;; req-2026-11-09-004: REST search items parse into queue rows
;; covers: code-review-search.el
;; covers-fn: code-review-search--parse-item
;; enforcement: test
(ert-deftest code-review-search/req-2026-11-09-004-parse-item ()
  "WHEN a search response item arrives THE SYSTEM SHALL parse the row fields."
  (let* ((item '((html_url . "https://github.com/acme/api/pull/1137")
                 (title . "Fix billing retry")
                 (number . 1137)
                 (draft)
                 (created_at . "2026-11-08T10:00:00Z")
                 (user (login . "amartin"))
                 (repository_url . "https://api.github.com/repos/acme/api")))
         (row (code-review-search--parse-item item)))
    (should (equal (plist-get row :key)
                   "https://github.com/acme/api/pull/1137"))
    (should (equal (plist-get row :url)
                   "https://github.com/acme/api/pull/1137"))
    (should (equal (plist-get row :title) "Fix billing retry"))
    (should (equal (plist-get row :author) "amartin"))
    (should (equal (plist-get row :number) 1137))
    (should (equal (plist-get row :repo) "acme/api"))
    (should (plist-get row :created-ts))
    (should-not (plist-get row :draft?)))
  ;; unusable items drop out (no repository_url)
  (should (null (code-review-search--parse-item
                 '((html_url . "https://github.com/x/y/pull/1"))))))

(ert-deftest code-review-search/parse-item-draft-flag ()
  "A draft PR parses its flag truthily."
  (should (plist-get
           (code-review-search--parse-item
            '((html_url . "https://github.com/acme/api/pull/2")
              (draft . t)
              (title . "wip")
              (repository_url . "https://api.github.com/repos/acme/api")))
           :draft?)))

;;; req-2026-11-09-005: the queue is a proper tabulated-list
;; covers: code-review-search.el
;; covers-fn: code-review-search--row code-review-search--sort-age
;; enforcement: test
(ert-deftest code-review-search/req-2026-11-09-005-queue-rows-are-two-element-lists ()
  "WHEN rows render THE SYSTEM SHALL build proper (ID VECTOR) entries.
The default column sorter reads (cadr ENTRY): a bare (ID . VECTOR)
cons crashes the sorter far from the row builder (phase 25's trap)."
  (let* ((entry `(:key "https://github.com/acme/api/pull/7"
                       :url "https://github.com/acme/api/pull/7"
                       :title "Fix" :author "bob" :number 7
                       :repo "acme/api"
                       :created-ts ,(encode-time 0 0 0 8 11 2026)
                       :draft? nil))
         (row (code-review-search--row entry)))
    ;; a PROPER two-element list, not a (ID . VECTOR) cons: the
    ;; default column sorter reads (cadr ENTRY) (phase 25's trap)
    (should (listp row))
    (should (listp (cdr row)))
    (should (= (length row) 2))
    (should (stringp (car row)))
    (should (vectorp (cadr row)))
    (should (= (length (cadr row)) 5))))

(ert-deftest code-review-search/age-renders-in-days ()
  "The age column renders whole days."
  (should (equal (code-review-search--age
                  (time-subtract (current-time) (days-to-time 3)))
                 "3d"))
  (should (equal (code-review-search--age nil) "")))

(ert-deftest code-review-search/sort-age-youngest-first ()
  "The age sorter puts the newest PR first."
  (setq code-review-search--table
        (let ((table (make-hash-table :test #'equal)))
          (puthash "new" `(:created-ts ,(encode-time 0 0 0 9 11 2026)) table)
          (puthash "old" `(:created-ts ,(encode-time 0 0 0 1 11 2026)) table)
          table))
  (unwind-protect
      (should (funcall 'code-review-search--sort-age
                       '("new") '("old")))
    (setq code-review-search--table nil)))

;;; req-2026-11-09-006: the org comes from the project clones' remotes
;; covers: code-review-search.el
;; covers-fn: code-review-search--url-owner
;; enforcement: test
(ert-deftest code-review-search/req-2026-11-09-006-url-owner-extraction ()
  "WHEN reading a clone's remote URL THE SYSTEM SHALL extract the org owner."
  (should (equal (code-review-search--url-owner
                  "https://github.com/WriterInternal/api.git")
                 "WriterInternal"))
  (should (equal (code-review-search--url-owner
                  "https://github.com/WriterInternal/api")
                 "WriterInternal"))
  (should (equal (code-review-search--url-owner
                  "git@github.com:WriterInternal/api.git")
                 "WriterInternal"))
  (should (equal (code-review-search--url-owner
                  "ssh://git@github.com/WriterInternal/api")
                 "WriterInternal"))
  (should (null (code-review-search--url-owner nil)))
  ;; a non-repo remote shape yields nothing sane rather than a crash
  (should (null (code-review-search--url-owner "git@github.com:only"))))

;;; req-2026-11-09-007: authors derive from the project clones' commits
;; covers: code-review-search.el
;; covers-fn: code-review-search--email->author
;; covers-fn: code-review-search--rank-authors
;; enforcement: test
(ert-deftest code-review-search/req-2026-11-09-007-email-to-author ()
  "WHEN ranking committer emails THE SYSTEM SHALL resolve GitHub
noreply emails to logins and keep plain emails searchable as-is."
  (should (equal (code-review-search--email->author
                  "424242+amartin@users.noreply.github.com")
                 "amartin"))
  (should (equal (code-review-search--email->author
                  "amartin@users.noreply.github.com")
                 "amartin"))
  (should (equal (code-review-search--email->author
                  "amartin@writer.com")
                 "amartin@writer.com"))
  ;; bot committers are skipped: machine churn is not reviewed
  (should (null (code-review-search--email->author
                 "49699333+dependabot[bot]@users.noreply.github.com")))
  (should (null (code-review-search--email->author
                 "github-actions[bot]@users.noreply.github.com")))
  (should (null (code-review-search--email->author nil))))

(ert-deftest code-review-search/req-2026-11-09-007-rank-authors ()
  "WHEN deriving the author list THE SYSTEM SHALL order by commit
frequency (most commits first) and cap at
`code-review-search-max-authors'.  Plain commit emails stay
searchable as-is; only noreply emails resolve to logins."
  (let ((code-review-search-max-authors 3))
    (should (equal (code-review-search--rank-authors
                    (append
                     (make-list 4 "bjones@writer.com")
                     (make-list 3 "424242+amartin@users.noreply.github.com")
                     (make-list 2 "carol@writer.com")
                     '("ignored@users.noreply.github.com")))
                   '("bjones@writer.com" "amartin" "carol@writer.com")))))

(ert-deftest code-review-search/req-2026-11-09-007-rank-authors-cap ()
  "WHEN there are more committers than the cap THE SYSTEM SHALL
keep only the most frequent."
  (let ((code-review-search-max-authors 2))
    (should (equal (code-review-search--rank-authors
                    (append
                     (make-list 3 "a@x.com")
                     (make-list 2 "b@x.com")
                     '("c@x.com" "d@x.com")))
                   '("a@x.com" "b@x.com")))))

;;; req-2026-11-09-008: the author pool delivers pooled rows
;; covers: code-review-search.el
;; covers-fn: code-review-search--fetch-authors
;; enforcement: test
(ert-deftest code-review-search/req-2026-11-09-008-fetch-authors-pool ()
  "WHEN pooling the author searches THE SYSTEM SHALL deliver the
deduped rows to the callback after the deferred queue drains.
(Regression: a mis-nested splice left `authors' outside the
`mapcar' call — balanced but wrong, crashing every live run.)"
  (let ((code-review-search--last 'pending)
        (code-review-search--stub--fetch
         (symbol-function 'code-review-search--fetch)))
    ;; synchronous stub: the callback fires immediately, like the
    ;; real ghub callback under the deferred queue
    (fset 'code-review-search--fetch
          (lambda (_author callback)
            (funcall callback
                     (list (list :key _author :url _author :title "t"
                                 :author _author :number 1 :repo "o/r"
                                 :created-ts nil :draft? nil))))
          )
    (unwind-protect
        (progn
          (code-review-search--fetch-authors
           '("a" "b" "a")
           (lambda (rows) (setq code-review-search--last rows)))
          (let ((deadline (time-add (current-time) (seconds-to-time 5))))
            (while (and (eq code-review-search--last 'pending)
                        (time-less-p (current-time) deadline))
              (sit-for 0.05)))
          (should (equal (mapcar (lambda (r) (plist-get r :author))
                                 code-review-search--last)
                         '("a" "b"))))
      (fset 'code-review-search--fetch code-review-search--stub--fetch))))

;;; req-2026-11-09-009: the review backlog (review-requested:@me)
;; covers: code-review-search.el
;; covers-fn: code-review-search--reconcile
;; covers-fn: code-review-search--sort-age
;; covers-fn: code-review-search--row
;; enforcement: test
(ert-deftest code-review-search/req-2026-11-09-009-reconcile-merges-flag ()
  "WHEN the same PR comes from an author search AND the backlog
search THE SYSTEM SHALL keep one row with the requested flag."
  (let* ((plain '(:key "u1" :url "u1" :title "t" :author "a"
                       :number 1 :repo "o/r" :created-ts 100))
         (requested (plist-put (copy-sequence plain) :requested? t))
         (merged (code-review-search--reconcile (list plain requested))))
    (should (= (length merged) 1))
    (should (plist-get (car merged) :requested?)))
  ;; distinct keys survive untouched, no flag
  (should (= (length (code-review-search--reconcile
                      (list '(:key "u1") '(:key "u2"))))
             2)))

(ert-deftest code-review-search/req-2026-11-09-009-sort-requested-first ()
  "WHEN sorting the queue THE SYSTEM SHALL put requested PRs first,
youngest first inside each group."
  (let* ((old-plain '(:key "old" :created-ts 100))
         (new-plain '(:key "new" :created-ts 900))
         (old-req '(:key "old-req" :created-ts 50 :requested? t))
         (rows (list old-plain new-plain old-req))
         (table (make-hash-table :test #'equal)))
    (dolist (r rows table) (puthash (plist-get r :key) r table))
    (let ((code-review-search--table table))
      (should (equal (mapcar #'car
                             (sort (mapcar #'code-review-search--row rows)
                                   #'code-review-search--sort-age))
                     '("old-req" "new" "old"))))))

(ert-deftest code-review-search/req-2026-11-09-009-row-highlights-requested ()
  "WHEN a row is requested THE SYSTEM SHALL show the flag token and
face the title with `code-review-search-requested'."
  (let* ((entry '(:key "u1" :title "the title" :author "a" :number 1
                       :repo "o/r" :created-ts nil :requested? t))
         (row (code-review-search--row entry))
         (vec (cadr row))
         (plain (code-review-search--row
                 '(:key "u2" :title "plain" :author "a" :number 2
                   :repo "o/r" :created-ts nil)))
         (plain-vec (cadr plain)))
    (should (equal (aref vec 4) "requested"))
    (should (get-text-property 0 'face (aref vec 1)))
    (should (equal (aref plain-vec 4) ""))
    (should (null (get-text-property 0 'face (aref plain-vec 1))))))

(provide 'code-review-search-test)
;;; code-review-search-test.el ends here
