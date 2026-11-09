;;; code-review-people-test.el --- People cache tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'code-review-people)

;;; req-2026-11-09-001: the people cache versioned round-trip
;; covers: code-review-people.el
;; covers-fn: code-review-people--load code-review-people--save
;; enforcement: test
(ert-deftest code-review-people/req-2026-11-09-001-cache-round-trip-and-version ()
  "WHEN the cache saves THE SYSTEM SHALL read it back only at a supported version."
  (let ((file (make-temp-file "cr-people-test")))
    (unwind-protect
        (progn
          ;; absent file: nothing
          (should (null (code-review-people--load file)))
          ;; a fresh save reads back
          (code-review-people--save
           `(:version ,code-review-people--version :org "acme"
                      :people (("alice" . "Alice A") ("bob" . nil))
                      :fetched-at ,(float-time))
           file)
          (let ((state (code-review-people--load file)))
            (should state)
            (should (equal (plist-get state :org) "acme"))
            (should (equal (plist-get state :people)
                           '(("alice" . "Alice A") ("bob" . nil)))))
          ;; an OLDER version is rejected (old caches re-fetch once)
          (code-review-people--save
           `(:version ,(1- code-review-people--version) :org "acme"
                      :people (("alice" . "Alice A")) :fetched-at ,(float-time))
           file)
          (should (null (code-review-people--load file)))
          ;; garbage is rejected, not signaled
          (with-temp-file file (insert "not a plist"))
          (should (null (code-review-people--load file))))
      (delete-file file))))

(ert-deftest code-review-people/req-2026-11-09-001-freshness-respects-ttl ()
  "WHEN the cache is older than the TTL THE SYSTEM SHALL consider it stale."
  (let ((now (float-time)))
    (should (code-review-people--fresh-p
             `(:people (("a" . "A")) :fetched-at ,(- now 86400))))
    (should (null (code-review-people--fresh-p
                   `(:people (("a" . "A"))
                     :fetched-at ,(- now (* 8 86400))))))
    ;; no people yet: never fresh
    (should (null (code-review-people--fresh-p
                   `(:people nil :fetched-at ,now))))))

;;; req-2026-11-09-002: searchable by display NAME, not just the slug
;; covers: code-review-people.el
;; covers-fn: code-review-people-match code-review-people-candidates
;; enforcement: test
(ert-deftest code-review-people/req-2026-11-09-002-name-matching ()
  "WHEN the reviewer searches by name or login THE SYSTEM SHALL match case-insensitively."
  (let ((people '(("amartin" . "Ana Martin")
                  ("bob" . nil)
                  ("jdoe" . "Jane Doe"))))
    (should (equal (code-review-people-match people "ana")
                   '(("amartin" . "Ana Martin"))))
    ;; case-insensitive both ways
    (should (equal (code-review-people-match people "ANA MARTIN")
                   '(("amartin" . "Ana Martin"))))
    ;; login fallback when there is no name
    (should (equal (code-review-people-match people "BOB")
                   '(("bob" . nil))))
    (should (equal (code-review-people-match people "jdoe")
                   '(("jdoe" . "Jane Doe"))))
    ;; no match: empty
    (should (null (code-review-people-match people "zzz")))))

(ert-deftest code-review-people/req-2026-11-09-002-candidates-carry-login ()
  "WHEN a candidate is completed THE SYSTEM SHALL yield the login."
  (let ((cands (code-review-people-candidates
                '(("amartin" . "Ana Martin") ("bob" . nil)))))
    (should (equal (cdr (assoc "Ana Martin (amartin)" cands)) "amartin"))
    (should (equal (cdr (assoc "bob" cands)) "bob"))
    (should (equal (code-review-people-login-of
                    "Ana Martin (amartin)" cands)
                   "amartin"))
    ;; a typed-not-completed pick IS the login
    (should (equal (code-review-people-login-of "someone" cands)
                   "someone"))))

(ert-deftest code-review-people/page-parse-unwraps-data ()
  "WHEN the GraphQL callback runs THE SYSTEM SHALL unwrap ghub
5.1's SPLICED shape — the data contents follow the `data' symbol
as BARE ELEMENTS, so the old (assq 'data ...) reader reads nil
forever.  Fixture is the real captured response (ghub 20261021)."
  (let* ((nodes '(((login . "alice") (name . "Alice A"))
                  ((login . "bob") (name . nil))))
         (response `(data (organization
                           (membersWithRole
                            (pageInfo (endCursor . "c2") (hasNextPage . t))
                            (nodes ,@nodes))))))
    (pcase-let* ((`(,got ,cursor)
                  (code-review-people--parse-response response)))
      (should (equal got nodes))
      (should (equal cursor "c2"))
      ;; the merge folds them keyed by login
      (let ((people (code-review-people--merge-page nil got)))
        (should (equal (cdr (assoc "alice" people)) "Alice A"))
        (should (assoc "bob" people))))))

(ert-deftest code-review-people/page-parse-junk-yields-nil ()
  "WHEN the response is garbage THE SYSTEM SHALL yield (nil nil)."
  (should (equal (code-review-people--parse-response nil) '(nil nil)))
  (should (equal (code-review-people--parse-response '(data)) '(nil nil))))

(provide 'code-review-people-test)
;;; code-review-people-test.el ends here
