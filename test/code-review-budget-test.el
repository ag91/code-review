;;; code-review-budget-test.el --- ERT tests for code-review-budget -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'a)
(require 'code-review-db)
(require 'code-review-github)
(require 'code-review-budget)
(require 'code-review-section-header)
(require 'code-review-test-helpers)

(defconst code-review-budget-test--diff
  (concat "diff --git a/app.el b/app.el\n"
          "index 111..222 100644\n"
          "--- a/app.el\n"
          "+++ b/app.el\n"
          "@@ -1,3 +1,4 @@\n"
          " context\n"
          "-old\n"
          "+new\n"
          "+extra\n"))

(defun code-review-budget-test--entry (key risk cost tier &optional mandatory)
  "A minimal queue ENTRY plist for the pure allocation tests."
  (list :key key
        :assess (list :risk risk :cost cost :tier tier
                      :reasons (list "stub") :mandatory mandatory)))

;;; req-2026-10-29-001: cost-estimation-deterministic
;; covers: code-review-budget.el
;; covers-fn: code-review-budget--shape, code-review-budget--cost
;; enforcement: test
(ert-deftest code-review-budget/req-2026-10-29-001-cost-from-hunks-and-lines ()
  "WHEN a reviewer sizes a change for the budget THE SYSTEM SHALL
estimate its cost deterministically from the diff's hunk count and
added lines — the same diff always costs the same minutes."
  (let ((code-review-review-cost-base 2.0)
        (code-review-review-cost-per-hunk 1.0)
        (code-review-review-cost-per-100-lines 1.0))
    ;; 1 hunk, +2 lines (the header's ---/+++ lines are metadata):
    ;; raw cost 2 + 1 + 0.02 = 3.02, reported to ONE decimal = 3.0
    (let* ((shape (code-review-budget--shape
                   code-review-budget-test--diff))
           (cost (code-review-budget--cost shape)))
      (should (= (plist-get shape :files) 1))
      (should (= (plist-get shape :hunks) 1))
      (should (= (plist-get shape :added) 2))
      (should (= (plist-get shape :deleted) 1))
      (should (< (abs (- cost 3.0)) 0.001)))))

;;; req-2026-10-29-002: safe-classification-honest-defaults
;; covers: code-review-budget.el
;; covers-fn: code-review-budget--assess
;; enforcement: test
(ert-deftest code-review-budget/req-2026-10-29-002-safe-only-from-known-gates ()
  "WHEN a PR's gates are known and green (traceability without
strong findings, all changed files COLD, no delicate hunk, no
incident path, cost within the per-PR slot) THE SYSTEM SHALL
classify it SAFE — and WHEN ANY signal is unknown or red THE
SYSTEM SHALL classify it ADMIT: unknown risk is never waved
through."
  (let ((code-review-review-bounce-minutes 45)
        (code-review-review-per-pr-minutes 15)
        (code-review-analysis-delicacy-threshold 0.5)
        (heat '((:path "a.el" :score 0.1 :bucket "COLD"))))
    ;; green and known -> safe
    (should (eq (plist-get
                 (code-review-budget--assess
                  code-review-budget-test--diff
                  (list :trace nil :heat-entries heat
                        :delicate-analyzed t :delicate-hunks nil
                        :incident-hits 0))
                 :tier)
                'safe))
    ;; heat unknown -> admit (honest default)
    (should (eq (plist-get
                 (code-review-budget--assess
                  code-review-budget-test--diff
                  (list :trace nil :heat-entries nil
                        :delicate-analyzed t :delicate-hunks nil
                        :incident-hits 0))
                 :tier)
                'admit))
    ;; delicacy never analyzed -> admit
    (should (eq (plist-get
                 (code-review-budget--assess
                  code-review-budget-test--diff
                  (list :trace nil :heat-entries heat
                        :delicate-analyzed nil :delicate-hunks nil
                        :incident-hits 0))
                 :tier)
                'admit))
    ;; a strong trace finding (spec change unbound) -> admit
    (should (eq (plist-get
                 (code-review-budget--assess
                  code-review-budget-test--diff
                  (list :trace (list (list :kind 'strong :id "x"
                                           :path "a.md"))
                        :heat-entries heat
                        :delicate-analyzed t :delicate-hunks nil
                        :incident-hits 0))
                 :tier)
                'admit))
    ;; a HOT file -> admit
    (should (eq (plist-get
                 (code-review-budget--assess
                  code-review-budget-test--diff
                  (list :trace nil
                        :heat-entries '((:path "a.el" :score 9.9
                                               :bucket "HOT"))
                        :delicate-analyzed t :delicate-hunks nil
                        :incident-hits 0))
                 :tier)
                'admit))))

(ert-deftest code-review-budget/bounce-over-threshold ()
  "A PR estimated above the bounce threshold is tier `bounce'."
  (let* ((code-review-review-bounce-minutes 45)
         (code-review-review-per-pr-minutes 15)
         (code-review-analysis-delicacy-threshold 0.5)
         ;; at 10min/100-lines: 1000 added lines -> way over 45
         (big (concat "diff --git a/x.py b/x.py\n"
                      "index 1..2 100644\n"
                      "--- a/x.py\n"
                      "+++ b/x.py\n"
                      "@@ -1,1 +1,1001 @@\n"
                      (string-join
                       (cl-loop for i to 1000 collect (format "+l%d" i))
                       "\n")
                      "\n"))
         (code-review-review-cost-per-100-lines 10.0)
         (a (code-review-budget--assess
             big (list :trace nil :heat-entries nil
                       :delicate-analyzed nil :delicate-hunks nil
                       :incident-hits nil))))
    (should (> (plist-get a :cost)
               code-review-review-bounce-minutes))
    (should (eq (plist-get a :tier) 'bounce))))

(ert-deftest code-review-budget/incidents-force-mandatory-admit ()
  "Incident-touched PRs are `admit' with :mandatory t, even when
every other signal would make them safe (phase 22: never
sampleable)."
  (let ((code-review-review-bounce-minutes 45)
        (code-review-review-per-pr-minutes 15)
        (code-review-analysis-delicacy-threshold 0.5)
        (heat '((:path "a.el" :score 0.1 :bucket "COLD"))))
    (let ((a (code-review-budget--assess
              code-review-budget-test--diff
              (list :trace nil :heat-entries heat
                    :delicate-analyzed t :delicate-hunks nil
                    :incident-hits 1))))
      (should (eq (plist-get a :tier) 'admit))
      (should (plist-get a :mandatory)))))

;;; req-2026-10-29-003: admission-fits-the-remaining-balance
;; covers: code-review-budget.el
;; covers-fn: code-review-budget--rank, code-review-budget--allocate
;; enforcement: test
(ert-deftest code-review-budget/req-2026-10-29-003-rank-and-cap-admission ()
  "WHEN the reviewer sits down with a fixed remaining balance THE
SYSTEM SHALL offer the risk-ranked items that fit it, deferring
the rest — and WHEN a higher-risk PR arrives mid-cycle THE
SYSTEM SHALL offer it first at the next sit-down (jumping the
queue costs a re-rank, never a clawback)."
  (let ((a (code-review-budget-test--entry "a" 1.0 10.0 'admit))
        (b (code-review-budget-test--entry "b" 2.0 10.0 'admit))
        (c (code-review-budget-test--entry "c" 3.0 10.0 'admit)))
    ;; ranked risk-first
    (should (equal (mapcar (lambda (e) (plist-get e :key))
                           (code-review-budget--rank (list a b c)))
                   '("c" "b" "a")))
    ;; the balance rations: 20 of 30 -> the third is deferred
    (let ((allocs (code-review-budget--allocate
                   (code-review-budget--rank (list a b c))
                   20.0 (lambda (_) nil))))
      (should (equal (mapcar (lambda (e) (plist-get e :key))
                             (plist-get allocs :admit))
                     '("c" "b")))
      (should (equal (mapcar (lambda (e) (plist-get e :key))
                             (plist-get allocs :deferred))
                     '("a"))))
    ;; mid-cycle arrival at risk 4.0 jumps the queue for the same
    ;; remaining balance
    (let* ((d (code-review-budget-test--entry "d" 4.0 10.0 'admit))
           (allocs (code-review-budget--allocate
                    (code-review-budget--rank (list a b c d))
                    20.0 (lambda (_) nil))))
      (should (equal (mapcar (lambda (e) (plist-get e :key))
                             (plist-get allocs :admit))
                     '("d" "c")))
      (should (equal (mapcar (lambda (e) (plist-get e :key))
                             (plist-get allocs :deferred))
                     '("b" "a"))))))

(ert-deftest code-review-budget/mandatory-admitted-beyond-balance ()
  "Mandatory (incident) entries are admitted first, even beyond
the remaining balance; the rest of the queue defers around them."
  (let* ((m (code-review-budget-test--entry "m" 9.0 30.0 'admit t))
         (a (code-review-budget-test--entry "a" 1.0 10.0 'admit))
         (allocs (code-review-budget--allocate
                  (code-review-budget--rank (list a m))
                  15.0 (lambda (_) nil))))
    ;; m is admitted unconditionally and over-runs the balance;
    ;; nothing is left for a, which defers
    (should (equal (mapcar (lambda (e) (plist-get e :key))
                           (plist-get allocs :admit))
                   '("m")))
    (should (equal (mapcar (lambda (e) (plist-get e :key))
                           (plist-get allocs :deferred))
                   '("a")))))

(ert-deftest code-review-budget/audit-draw-from-safe ()
  "The random audit slice draws SAFE entries for human minutes,
bounded by the remaining balance; the undrawn SAFE entries stay
SAFE (they never wait for a human)."
  (let* ((s1 (code-review-budget-test--entry "s1" 1.0 5.0 'safe))
         (s2 (code-review-budget-test--entry "s2" 2.0 5.0 'safe))
         (yes (lambda (_) t))
         (no (lambda (_) nil)))
    ;; audit-p yes, ranked s2 first (risk 2.0): 5 min fits one slot
    (let ((allocs (code-review-budget--allocate
                   (code-review-budget--rank (list s1 s2))
                   5.0 yes)))
      (should (equal (mapcar (lambda (e) (plist-get e :key))
                             (plist-get allocs :audit))
                     '("s2")))
      (should (equal (mapcar (lambda (e) (plist-get e :key))
                             (plist-get allocs :safe))
                     '("s1"))))
    ;; audit-p no: everything stays safe
    (let ((allocs (code-review-budget--allocate
                   (code-review-budget--rank (list s1 s2))
                   5.0 no)))
      (should (null (plist-get allocs :audit)))
      (should (= (length (plist-get allocs :safe)) 2)))))

(ert-deftest code-review-budget/bounced-never-consume-budget ()
  "Bounced entries never enter the admission slices."
  (let* ((x (code-review-budget-test--entry "x" 9.0 90.0 'bounce))
         (a (code-review-budget-test--entry "a" 1.0 10.0 'admit))
         (allocs (code-review-budget--allocate
                  (list x a) 15.0 (lambda (_) nil))))
    (should (equal (mapcar (lambda (e) (plist-get e :key))
                           (plist-get allocs :bounce))
                   '("x")))
    (should (equal (mapcar (lambda (e) (plist-get e :key))
                           (plist-get allocs :admit))
                   '("a")))))

(ert-deftest code-review-budget/approve-template-substitution ()
  "The approve-on-gates audit-trail body states what it is: the
cost is filled in and the wording says no human deep review; the
bounce body asks for a split and names the threshold."
  (let* ((entry (code-review-budget-test--entry "o/r#7" 1.0 3.5 'safe))
         (body (code-review-budget--substitute
                code-review-review-approve-template entry)))
    (should (string-match-p "Approved on gates" body))
    (should (string-match-p "no human deep review" body))
    (should (string-match-p "3\\.5 minutes" body)))
  (let* ((entry (code-review-budget-test--entry "o/r#7" 1.0 50.0 'bounce))
         (body (code-review-budget--substitute
                code-review-review-bounce-template entry
                (list (cons ?b "45")))))
    (should (string-match-p "please split" body))
    (should (string-match-p "50\\.0 minutes" body))
    (should (string-match-p "threshold: 45" body))))

;;; db-backed tests

(defun code-review-budget-test--make-pr (owner repo number diff title)
  "Persist an unfinished, saved PR row with the given RAW-DIFF."
  (let ((pr (code-review-github-repo
             :owner owner :repo repo :number number)))
    (code-review-db--pullreq-create pr)
    (oset pr raw-diff diff)
    (oset pr title title)
    (oset pr url (format "https://github.com/%s/%s/pull/%s"
                         owner repo number))
    (oset pr saved t)
    (oset pr saved-at (current-time-string))
    (code-review-db-update pr)
    pr))

;;; req-2026-10-29-004: queue-view-two-lists
;; covers: code-review-budget.el
;; covers-fn: code-review-budget-queue, code-review-budget--collect
;; enforcement: test
(ert-deftest code-review-budget/req-2026-10-29-004-queue-renders-ranked-open-prs ()
  "WHEN the reviewer opens the budget queue THE SYSTEM SHALL list
every unfinished open PR ranked by estimated risk, with its cost,
tier and signal evidence, and a header reporting the remaining
balance and the backlog ratio."
  (let ((code-review-budget-state-file (make-temp-file "cr-budget-"))
        (code-review-repo-worktree nil)
        (code-review-budget-focus-enabled nil)
        (code-review-history-enabled nil))
    (code-review-test--with-db
      (code-review-budget-test--make-pr "o" "r" "1"
                                        code-review-budget-test--diff
                                        "Small change")
      (code-review-budget-test--make-pr "o" "r" "2"
                                        code-review-budget-test--diff
                                        "Another change")
      (unwind-protect
          (progn
            (code-review-budget-queue)
            (with-current-buffer "*Code Review Budget*"
              (goto-char (point-min))
              (let ((text (buffer-string)))
                (should (string-match-p "o/r#1" text))
                (should (string-match-p "o/r#2" text))
                (should (string-match-p "ADMIT" text)))
              ;; the header reports balance and backlog honestly
              ;; (the header line is not part of buffer-string)
              (should (string-match-p "min left today"
                                      header-line-format))
              (should (string-match-p "backlog:" header-line-format)))
            ;; decided rows drop out of the collect
            (code-review-budget--state-record :approved "o/r#1")
            (should (equal (mapcar (lambda (e) (plist-get e :key))
                                   (code-review-budget--collect))
                           '("o/r#2"))))
        (and (get-buffer "*Code Review Budget*")
             (kill-buffer "*Code Review Budget*"))
        (ignore-errors (delete-file code-review-budget-state-file))))))

(ert-deftest code-review-budget/spent-today-from-finished-rows-and-log ()
  "Spent minutes are the sunk cost of today's finished reviews
plus the manually logged minutes; the remaining balance never
goes negative."
  (let ((code-review-budget-state-file (make-temp-file "cr-budget-"))
        (code-review-repo-worktree nil)
        (code-review-history-enabled nil)
        (code-review-review-budget-minutes 90))
    (code-review-test--with-db
      (code-review-budget-test--make-pr "o" "r" "1"
                                        code-review-budget-test--diff
                                        "Done today")
      (let ((pr (code-review-db-get-pullreq)))
        (oset pr finished t)
        (oset pr finished-at (current-time-string))
        (code-review-db-update pr))
      ;; ~3 minutes of finished cost, plus 10 logged manually
      (let ((spent (code-review-budget--finished-today-cost)))
        (should (> spent 0.0))
        (should (< spent 10.0))
        (code-review-budget-log-minutes 10)
        (should (< (- (+ spent 10.0)
                      (code-review-budget--spent-today))
                   0.001))
        (should (< (- (code-review-budget--remaining)
                      (- 90.0 (+ spent 10.0)))
                   0.001))
        ;; the balance clamps at zero
        (let ((code-review-review-budget-minutes 5))
          (should (= (code-review-budget--remaining) 0)))))
    (ignore-errors (delete-file code-review-budget-state-file))))

;;; The in-buffer estimate line (phase 25b): under the Title, local
;;; and remote PRs alike

;;; req-2026-10-30-003: estimate-line-in-review-buffer
;; covers: code-review-section-header.el
;; covers-fn: code-review-section-insert-budget
;; enforcement: test
(ert-deftest code-review-budget/req-2026-10-30-003-estimate-line-under-title ()
  "WHEN a review buffer renders a PR with a stored diff THE SYSTEM
SHALL show the review-budget estimate under the Title — the
deterministic cost, the budget tier, and the day's remaining
balance (local and remote rows alike) — and nothing when there is
no diff."
  (let ((code-review-history-enabled nil)
        (code-review-review-budget-minutes 90)
        (code-review-budget-focus-enabled nil)
        (code-review-budget-state-file (make-temp-file "cr-est-"))
        (code-review-repo-worktree nil))
    (code-review-test--with-db
      ;; no diff yet: no line
      (let ((pr (code-review-github-repo :owner "o" :repo "r"
                                         :number "1")))
        (code-review-db--pullreq-create pr)
        (oset pr saved t))
      (code-review-test--sections-match
       (lambda () (code-review-section-insert-budget))
       nil t)
      ;; with a diff: the estimate line (1 hunk, +2 -> ~2.8 min)
      (code-review-budget-test--make-pr "o" "r" "1"
                                        code-review-budget-test--diff
                                        "Small change")
      (code-review-test--sections-match
       (lambda () (code-review-section-insert-budget))
       `(((type . code-review-budget-section)
          (value . (:cost 2.8 :tier admit :balance 90.0)))))
      ;; the rendered TEXT names the parts
      (with-temp-buffer
        (code-review-section-insert-budget)
        (let ((text (buffer-string)))
          (should (string-match-p "Review budget:" text))
          (should (string-match-p "~2\\.8 min" text))
          (should (string-match-p "tier: ADMIT" text))
          (should (string-match-p "balance today: 90\\.0 min" text))))
      ;; budget off: the estimate stays, the balance says so
      (let ((code-review-review-budget-minutes nil))
        (with-temp-buffer
          (code-review-section-insert-budget)
          (should (string-match-p "budget off" (buffer-string))))))))

;;; code-review-budget-test.el ends here
