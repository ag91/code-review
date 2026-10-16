;;; code-review-viewed-test.el --- ERT tests for the phase 13 read tracking -*- lexical-binding: t; -*-

(require 'ert)
(require 'a)
(require 'magit-section)
(require 'code-review-section)
(require 'code-review-viewed)
(require 'code-review-test-helpers)

(defun code-review-viewed-test--diff ()
  "Return a two-hunk diff with canonical paths."
  (concat "diff --git app.el app.el\n"
          "index 111..222 100644\n--- app.el\n+++ app.el\n"
          "@@ -1,2 +1,2 @@\n context\n-old\n+new\n"
          "@@ -5,2 +5,3 @@\n context\n+added\n"))

(defmacro code-review-viewed-test--with-stubs (bindings &rest body)
  "Run BODY with function BINDINGS, restoring them even after a failure."
  (declare (indent 1))
  (let ((saved (make-symbol "saved")))
    `(let ((,saved (mapcar (lambda (name) (cons name (symbol-function name)))
                          ',(mapcar #'car bindings))))
       (unwind-protect
           (progn
             ,@(mapcar (lambda (binding)
                         `(fset ',(car binding) ,(cadr binding)))
                       bindings)
             ,@body)
         (dolist (entry ,saved) (fset (car entry) (cdr entry)))))))

(defun code-review-viewed-test--sample-pr-obj ()
  "A fresh sample pullreq object (a shared one accumulates ids)."
  (code-review-github-repo
   :owner "owner"
   :repo "repo"
   :number "num"))

(ert-deftest code-review-viewed-test/target-at-file ()
  "At a file section the target is the path stripped of the diff
prefixes, no hunk key."
  (with-temp-buffer
    (magit-section-mode)
    (let ((inhibit-read-only t))
      (magit-insert-section (magit-file-section "b/server/src/App.scala")
        (insert "modified   server/src/App.scala\n"))
      (goto-char (point-min))
      (should (equal (code-review-viewed--target-at (magit-current-section))
                     (list :path "server/src/App.scala" :hunk-key nil))))))

(ert-deftest code-review-viewed-test/target-at-hunk ()
  "At a hunk section the target carries the hunk's diff-header key
and its file path."
  (with-temp-buffer
    (magit-section-mode)
    (let ((inhibit-read-only t))
      (magit-insert-section (magit-file-section "b/app.el")
        (insert "modified   app.el\n")
        (magit-insert-section
            (magit-hunk-section
             '((value . (("-1,2 +1,3" . ((1 2) (1 3)))))
               (path . "app.el")
               (ranges . "-1,2 +1,3")
               (head-pos . 10)))
          (insert "@@ -1,2 +1,3 @@\n context\n")))
      (goto-char (point-min))
      (search-forward "@@")
      (beginning-of-line)
      (should (equal (code-review-viewed--target-at (magit-current-section))
                     (list :path "app.el" :hunk-key "-1,2 +1,3"))))))

(ert-deftest code-review-viewed-test/target-at-other-sections-nil ()
  "Non-diff sections (no string/plist value) yield no target."
  (with-temp-buffer
    (magit-section-mode)
    (let ((inhibit-read-only t))
      (magit-insert-section (code-review-title-section)
        (insert "Title\n"))
      (goto-char (point-min))
      (should-not (code-review-viewed--target-at (magit-current-section))))))

(ert-deftest code-review-viewed-test/toggle-command ()
  "v marks the file at point viewed and re-renders; C-u unmarks.
The forge sync transport is stubbed; the test counts its calls."
  (code-review-test--with-db
    (code-review-db--pullreq-create (code-review-viewed-test--sample-pr-obj))
    (code-review-db--pullreq-raw-diff-update (code-review-viewed-test--diff))
    (with-temp-buffer
      (magit-section-mode)
      (let ((inhibit-read-only t))
        (magit-insert-section (magit-file-section "b/app.el")
          (insert "modified   app.el\n")))
      (goto-char (point-min))
      (let ((renders 0)
            (syncs nil)
            (old-build (symbol-function 'code-review--build-buffer))
            (old-sync (symbol-function 'code-review-viewed--sync-file)))
        (fset 'code-review--build-buffer
              (lambda (&rest _) (setq renders (1+ renders))))
        (fset 'code-review-viewed--sync-file
              (lambda (path mark? _before) (push (cons path mark?) syncs)))
        (unwind-protect
            (progn
              (code-review-viewed-toggle)
              (should (= renders 1))
              (should (code-review-db--reviewed-file-p "app.el"))
              (should (equal syncs '(("app.el" . t))))
              ;; marking twice is a no-op (already viewed)
              (code-review-viewed-toggle)
              (should (= renders 1))
              (code-review-viewed-toggle '(4))
              (should (= renders 2))
              (should-not (code-review-db--reviewed-file-p "app.el")))
          (fset 'code-review--build-buffer old-build)
          (fset 'code-review-viewed--sync-file old-sync))))))

;;; req-2026-10-16-001:
;; covers: code-review-db.el, code-review-db-reviewed.el, code-review-local.el
;; covers-fn: code-review-db--pullreq-create, code-review-db--reviewed-insert
;; enforcement: test
(ert-deftest code-review-viewed-test/req-2026-10-16-001-persist-reopen ()
  "WHEN a reviewer reopens a change THE SYSTEM SHALL retain its review progress."
  (code-review-test--with-db
    (code-review-db--pullreq-create (code-review-viewed-test--sample-pr-obj))
    (code-review-db--pullreq-raw-diff-update (code-review-viewed-test--diff))
    (code-review-db--reviewed-insert "app.el")
    (code-review-db--reviewed-insert "app.el")
    (code-review-db--reviewed-insert "app.el" "-1,2 +1,2")
    (let ((old-id code-review-db--pullreq-id)
          (file code-review-db-database-file))
      ;; Simulate a restart before opening the same review.
      (code-review-test--reset-db file)
      (code-review-db--pullreq-create (code-review-viewed-test--sample-pr-obj))
      (should-not (equal old-id code-review-db--pullreq-id))
      (should (equal (code-review-db--pullreq-raw-diff)
                     (code-review-viewed-test--diff)))
      (should (code-review-db--reviewed-file-p "app.el"))
      (should (= 2 (length (code-review-db--reviewed-rows))))
      (should-not (code-review-db--reviewed-rows old-id))
      ;; A different review never inherits these marks.
      (code-review-db--pullreq-create
       (code-review-github-repo :owner "owner" :repo "repo" :number "other"))
      (should-not (code-review-db--reviewed-rows)))))

(ert-deftest code-review-viewed-test/local-reopen-keeps-matching-root-and-range ()
  "The local review keeps marks only for the same repository and diff range."
  (code-review-test--with-db
    (let ((new-local
           (lambda (root)
             (let ((pr (code-review-local-diff
                        :owner "local" :repo "repo" :number 0 :url nil)))
               (oset pr state "LOCAL")
               (oset pr host root)
               (oset pr base-ref-name "HEAD")
               pr))))
      (code-review-db--pullreq-create (funcall new-local "/tmp/a/repo/"))
      (code-review-db--pullreq-raw-diff-update (code-review-viewed-test--diff))
      (code-review-db--reviewed-insert "app.el")
      (code-review-local--cleanup-rows "/tmp/a/repo/" "HEAD")
      (code-review-db--pullreq-create (funcall new-local "/tmp/a/repo/"))
      (should (code-review-db--reviewed-file-p "app.el"))
      ;; Equal basenames do not make two roots the same local review.
      (code-review-local--cleanup-rows "/tmp/b/repo/" "HEAD")
      (code-review-db--pullreq-create (funcall new-local "/tmp/b/repo/"))
      (should-not (code-review-db--reviewed-rows)))))

(ert-deftest code-review-viewed-test/unmark-hunk-splits-file-mark ()
  "Unmarking one hunk of a whole-file mark preserves the other hunks."
  (code-review-test--with-db
    (code-review-db--pullreq-create (code-review-viewed-test--sample-pr-obj))
    (code-review-db--pullreq-raw-diff-update (code-review-viewed-test--diff))
    (code-review-db--reviewed-insert "app.el")
    (let ((info (gethash "app.el" (code-review-viewed--index))))
      (code-review-viewed--set-state "app.el" "-1,2 +1,2" nil info))
    (let ((info (gethash "app.el" (code-review-viewed--index))))
      (should-not (code-review-viewed--file-p info))
      (should (equal (plist-get info :hunks) '("-5,2 +5,3"))))
    (code-review-viewed--set-state "app.el" nil nil nil)
    (should-not (code-review-db--reviewed-rows))))

(ert-deftest code-review-viewed-test/unmark-rollup-file-clears-hunks ()
  "A full-file unmark clears a completed rollup even without a file row."
  (code-review-test--with-db
    (code-review-db--pullreq-create (code-review-viewed-test--sample-pr-obj))
    (code-review-db--pullreq-raw-diff-update (code-review-viewed-test--diff))
    (dolist (key '("-1,2 +1,2" "-5,2 +5,3"))
      (code-review-db--reviewed-insert "app.el" key))
    (with-temp-buffer
      (magit-section-mode)
      (let ((inhibit-read-only t))
        (magit-insert-section (magit-file-section "app.el")
          (insert "modified app.el\n")))
      (goto-char (point-min))
      (let (sync)
        (code-review-viewed-test--with-stubs
            ((code-review--build-buffer #'ignore)
             (code-review-viewed--sync-file
              (lambda (_path viewed? _before) (setq sync (list viewed?)))))
          (code-review-viewed-toggle '(4))
          (should (equal sync '(nil)))
          (should-not (code-review-db--reviewed-rows)))))))

(ert-deftest code-review-viewed-test/github-seed-known-states-only ()
  "GitHub wins over completed marks; missing fields and partial progress survive."
  (code-review-test--with-db
    (code-review-db--pullreq-create (code-review-viewed-test--sample-pr-obj))
    (code-review-db--pullreq-raw-diff-update (code-review-viewed-test--diff))
    (code-review-db--reviewed-insert "app.el" "-1,2 +1,2")
    (code-review-db--reviewed-insert "unknown.el")
    (code-review-db--pullreq-raw-infos-update
     '((files (nodes ((path . "app.el") (viewerViewedState . "UNVIEWED"))
                     ((path . "unknown.el"))))))
    (code-review-viewed--seed-github)
    (should (= 1 (length (code-review-db--reviewed-hunk-keys))))
    (should (code-review-db--reviewed-file-p "unknown.el"))
    ;; A failed mutation must not leave an authoritative local rollup.
    (code-review-db--reviewed-insert "app.el" "-5,2 +5,3")
    (code-review-viewed--seed-github)
    (should-not (code-review-db--reviewed-hunk-keys))
    (code-review-db--pullreq-raw-infos-update
     '((files (nodes ((path . "app.el") (viewerViewedState . "VIEWED"))))))
    (code-review-viewed--seed-github)
    (should (code-review-db--reviewed-file-p "app.el"))
    (code-review-db--reviewed-insert "app.el" "-1,2 +1,2")
    (code-review-db--pullreq-raw-infos-update
     '((files (nodes ((path . "app.el") (viewerViewedState . "DISMISSED"))))))
    (code-review-viewed--seed-github)
    (should-not (code-review-viewed--path-rows "app.el"))))

;;; req-2026-10-16-002:
;; covers: code-review-viewed.el, code-review-github-query.el
;; covers-fn: code-review-viewed--sync-file, code-review-toggle-file-viewed
;; enforcement: test
(ert-deftest code-review-viewed-test/req-2026-10-16-002-sync-rollback-isolated ()
  "WHEN review progress cannot sync THE SYSTEM SHALL restore it without changing another review."
  (code-review-test--with-db
    (code-review-db--pullreq-create (code-review-viewed-test--sample-pr-obj))
    (code-review-db--pullreq-raw-diff-update (code-review-viewed-test--diff))
    (code-review-db--reviewed-insert "app.el" "-1,2 +1,2")
    (let ((first-id code-review-db--pullreq-id)
          (before (code-review-viewed--path-rows "app.el"))
          failure)
      (code-review-db--reviewed-insert "app.el" "-5,2 +5,3")
      (code-review-viewed-test--with-stubs
          ((code-review-toggle-file-viewed
            (lambda (_pr _path _state _success errorback)
              (setq failure errorback)))
           (code-review-utils--log #'ignore))
        (code-review-viewed--sync-file "app.el" t before)
        (code-review-db--pullreq-create
         (code-review-github-repo :owner "other" :repo "repo" :number "2"))
        (code-review-db--reviewed-insert "app.el")
        (let ((second-id code-review-db--pullreq-id))
          (funcall failure 'network-error)
          (should (equal code-review-db--pullreq-id second-id))
          (should (code-review-db--reviewed-file-p "app.el")))
        (let ((code-review-db--pullreq-id first-id))
          (should (equal before (code-review-viewed--path-rows "app.el"))))))))

(ert-deftest code-review-viewed-test/overlapping-syncs-are-serialized ()
  "Unconfirmed progress cannot become the rollback baseline of another mutation."
  (code-review-test--with-db
    (code-review-db--pullreq-create (code-review-viewed-test--sample-pr-obj))
    (code-review-db--reviewed-insert "app.el")
    (let (failure)
      (code-review-viewed-test--with-stubs
          ((code-review-toggle-file-viewed
            (lambda (_pr _path _state _success errorback)
              (setq failure errorback)))
           (code-review-utils--log #'ignore))
        (code-review-viewed--sync-file "app.el" t nil)
        (should-error (code-review-viewed--sync-file "app.el" nil nil)
                      :type 'user-error)
        (funcall failure 'network-error)
        (should-not (code-review-db--reviewed-file-p "app.el"))
        (should-not (gethash (list code-review-db--pullreq-id "app.el")
                             code-review-viewed--pending))))))

(ert-deftest code-review-viewed-test/command-waits-for-pending-sync ()
  "An unmark while a mark is in flight cannot race its rollback."
  (code-review-test--with-db
    (code-review-db--pullreq-create (code-review-viewed-test--sample-pr-obj))
    (code-review-db--pullreq-raw-diff-update (code-review-viewed-test--diff))
    (with-temp-buffer
      (magit-section-mode)
      (let ((inhibit-read-only t))
        (magit-insert-section (magit-file-section "app.el")
          (insert "modified app.el\n")))
      (goto-char (point-min))
      (let (failure)
        (code-review-viewed-test--with-stubs
            ((code-review--build-buffer #'ignore)
             (code-review-toggle-file-viewed
              (lambda (_pr _path _state _success errorback)
                (setq failure errorback)))
             (code-review-utils--log #'ignore))
          (code-review-viewed-toggle)
          (should-error (code-review-viewed-toggle '(4)) :type 'user-error)
          (should (code-review-db--reviewed-file-p "app.el"))
          (funcall failure 'offline)
          (should-not (code-review-db--reviewed-rows)))))))

(ert-deftest code-review-viewed-test/synchronous-sync-error-restores-state ()
  "Transport setup errors also roll back optimistic marks."
  (code-review-test--with-db
    (code-review-db--pullreq-create (code-review-viewed-test--sample-pr-obj))
    (code-review-db--reviewed-insert "app.el")
    (code-review-viewed-test--with-stubs
        ((code-review-toggle-file-viewed (lambda (&rest _) (error "offline")))
         (code-review-utils--log #'ignore))
      (code-review-viewed--sync-file "app.el" t nil)
      (should-not (code-review-db--reviewed-file-p "app.el")))))

(ert-deftest code-review-viewed-test/expire-removed-and-shifted-hunks ()
  "A refresh drops changed-file marks, removed paths, and changed headers."
  (code-review-test--with-db
    (code-review-db--pullreq-create (code-review-viewed-test--sample-pr-obj))
    (code-review-db--pullreq-raw-diff-update (code-review-viewed-test--diff))
    (code-review-db--reviewed-insert "app.el")
    (code-review-db--reviewed-insert "app.el" "-1,2 +1,2")
    (code-review-db--reviewed-insert "app.el" "-99,2 +99,3")
    (code-review-db--reviewed-insert "gone.el")
    (code-review-viewed--expire '("app.el"))
    (should-not (code-review-db--reviewed-file-p "app.el"))
    (should-not (code-review-db--reviewed-file-p "gone.el"))
    (should (equal (code-review-db--reviewed-hunk-keys)
                   '(("app.el" "-1,2 +1,2"))))))

(ert-deftest code-review-viewed-test/index-ignores-stale-rows-and-empty-hunks ()
  "Removed paths never inflate progress, and binary files need an explicit mark."
  (code-review-test--with-db
    (code-review-db--pullreq-create (code-review-viewed-test--sample-pr-obj))
    (code-review-db--pullreq-raw-diff-update
     (concat (code-review-viewed-test--diff)
             "diff --git image.png image.png\n"
             "index 111..222 100644\nBinary files differ\n"))
    (code-review-db--reviewed-insert "gone.el")
    (code-review-db--reviewed-insert "image.png" "-1 +1")
    (let* ((code-review-section--viewed (code-review-viewed--index))
           (info (gethash "image.png" code-review-section--viewed)))
      (should (= 2 (hash-table-count code-review-section--viewed)))
      (should-not (code-review-viewed--file-p info))
      (should (= 0 (code-review-viewed--count-files))))))

(ert-deftest code-review-viewed-test/target-keeps-canonical-prefix-directory ()
  "Canonical paths under directories literally named a/ or b/ are not truncated."
  (dolist (path '("a/app.el" "b/app.el"))
    (with-temp-buffer
      (magit-section-mode)
      (let ((inhibit-read-only t))
        (magit-insert-section (magit-file-section path)
          (insert (propertize "modified file\n" 'code-review-path path))))
      (goto-char (point-min))
      (should (equal path (plist-get
                          (code-review-viewed--target-at
                           (magit-current-section))
                          :path))))))

(ert-deftest code-review-viewed-test/hunk-rollup-completes-file ()
  "Marking the last unviewed hunk of a file rolls the file up:
the index reports :all-hunks and the file counts as viewed."
  (code-review-test--with-db
    (code-review-db--pullreq-create (code-review-viewed-test--sample-pr-obj))
    (let ((diff "diff --git a/app.el b/app.el\nindex 111..222 100644\n--- a/app.el\n+++ b/app.el\n@@ -1,2 +1,2 @@\n context\n-old\n+new\n@@ -5,2 +5,3 @@\n context\n+added\n"))
      (code-review-db--pullreq-raw-diff-update diff)
      (code-review-db--reviewed-insert "app.el" "-1,2 +1,2")
      (let ((index (code-review-viewed--index)))
        (should-not (plist-get (gethash "app.el" index) :all-hunks)))
      (code-review-db--reviewed-insert "app.el" "-5,2 +5,3")
      (let ((index (code-review-viewed--index)))
        (should (plist-get (gethash "app.el" index) :all-hunks)))
      (let ((code-review-section--viewed (code-review-viewed--index)))
        (should (= 1 (code-review-viewed--count-files)))))))

;;; code-review-viewed-test.el ends here
