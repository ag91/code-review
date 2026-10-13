;;; code-review-utils-test.el --- ERT tests for utility functions -*- lexical-binding: t; -*-

(require 'ert)
(require 'a)
(require 'code-review-utils)
(require 'code-review)

(defconst code-review-utils-test--sample-grouped-comments
  (a-alist
   "github.el:42" (list '((position . 42) (path . "github.el") (author . "test-1"))
                        '((position . 42) (path . "github.el") (author . "test-2")))))

(defconst code-review-utils-test--sample-comment-written-lines
  (a-alist
   "github.el" 0))

(defconst code-review-utils-test--sample-comment-lines
  (list "This need to be changed"
        "Improve this code please"))

(defconst code-review-utils-test--sample-suggestion-comment
  "Suggested change\n        \n          \n    \n\n        \n      \n    \n    \n      \n          \n            \n               :extra-deps {thheller/shadow-cljs {:mvn/version \"2.15.12\"}\n          \n          \n            \n               :extra-deps {thheller/shadow-cljs {:mvn/version \"2.15.14\"}")

(ert-deftest code-review-utils-test/comments-grouped-by-path-and-position ()
  (should (equal (code-review-utils--comment-key "github.el" 42)
                 "github.el:42")))

(ert-deftest code-review-utils-test/comment-get-helper ()
  (let ((comments (code-review-utils--comment-get
                   code-review-utils-test--sample-grouped-comments
                   "github.el:42")))
    (should (equal (length comments) 2))
    (should (member '((position . 42) (path . "github.el") (author . "test-2"))
                    comments))))

(ert-deftest code-review-utils-test/comment-clean-msg-removes-placeholder ()
  (let ((placeholder-msg ";;; This is a placeholder in a buffer")
        (full-msg ";;; This is a placeholder in a buffer\nThis is my real comment"))
    (should (equal (code-review-utils--comment-clean-msg full-msg placeholder-msg)
                   "This is my real comment")))
  (let ((placeholder-msg ";;; This is a placeholder in a buffer")
        (full-msg ";;; This is a placeholder in a bufferThis is my real comment"))
    (should (equal (code-review-utils--comment-clean-msg full-msg placeholder-msg)
                   "This is my real comment"))))

(ert-deftest code-review-utils-test/comment-update-written-count ()
  (should (equal (code-review-utils--comment-update-written-count
                  code-review-utils-test--sample-comment-written-lines
                  "github.el"
                  (length code-review-utils-test--sample-comment-lines))
                 (a-alist "github.el" 2)))
  (should (equal (code-review-utils--comment-update-written-count
                  (a-alist "github.el" 5)
                  "github.el"
                  (length code-review-utils-test--sample-comment-lines))
                 (a-alist "github.el" 7))))

(ert-deftest code-review-utils-test/clean-suggestion ()
  (should (equal (code-review-utils--clean-suggestion
                  code-review-utils-test--sample-suggestion-comment)
                 `("Suggested change"
                   "-   :extra-deps {thheller/shadow-cljs {:mvn/version \"2.15.12\"}"
                   "+   :extra-deps {thheller/shadow-cljs {:mvn/version \"2.15.14\"}"))))

(ert-deftest code-review-utils-test/missing-outdated-comments ()
  (should (equal (code-review-utils--missing-outdated-commments?
                  "github.el"
                  (list "github.el:30" "github.el:20" "gitlab.el:12")
                  `(("github.el:30" . (list 1 2 3))
                    ("github.el:20" . (list 1 2 3))
                    ("github.el:50" . (list 1 2 3))))
                 (list "github.el:50"))))

(ert-deftest code-review-utils-test/pr-from-url-github ()
  (should (equal (code-review-utils-pr-from-url
                  "https://github.com/eval-all-software/tempo/pull/98")
                 (a-alist
                  'num "98"
                  'repo "tempo"
                  'owner "eval-all-software"
                  'forge 'github
                  'url "https://github.com/eval-all-software/tempo/pull/98"))))

(ert-deftest code-review-utils-test/pr-from-url-gitlab ()
  (should (equal (code-review-utils-pr-from-url
                  "https://gitlab.com/code-review-experiment/default/-/merge_requests/1")
                 (a-alist
                  'num "1"
                  'repo "default"
                  'owner "code-review-experiment"
                  'forge 'gitlab
                  'url "https://gitlab.com/code-review-experiment/default/-/merge_requests/1"))))

(ert-deftest code-review-utils-test/pr-from-url-gitlab-subgroup ()
  (should (equal (code-review-utils-pr-from-url
                  "https://gitlab.com/owner/group/subgroup/project/-/merge_requests/1")
                 (a-alist
                  'num "1"
                  'repo "group%2Fsubgroup%2Fproject"
                  'owner "owner"
                  'forge 'gitlab
                  'url "https://gitlab.com/owner/group/subgroup/project/-/merge_requests/1"))))

(ert-deftest code-review-utils-test/pr-from-url-gitlab-nested-subgroups ()
  (should (equal (code-review-utils-pr-from-url
                  "https://gitlab.com/owner/group/subgroup1/subgroup2/subgroup3/project/-/merge_requests/1")
                 (a-alist
                  'num "1"
                  'repo "group%2Fsubgroup1%2Fsubgroup2%2Fsubgroup3%2Fproject"
                  'owner "owner"
                  'forge 'gitlab
                  'url "https://gitlab.com/owner/group/subgroup1/subgroup2/subgroup3/project/-/merge_requests/1"))))

(ert-deftest code-review-utils-test/pr-from-url-strips-text-properties ()
  ;; Slack (lui) buttons hand `browse-url' URLs carrying buffer
  ;; text properties (`lui-raw-text' with the whole message, plus
  ;; keymaps); `match-string' preserves them, and emacsql encodes
  ;; scalars with `prin1', so a propertized slot would write its
  ;; entire property payload into the db — a giant blob that
  ;; then fails to read back ("EmacSQL had an unhandled
  ;; condition" on the embedded unreadable objects).  The parse
  ;; must return property-free strings.
  (let* ((url (propertize
               "https://github.com/WriterInternal/writer-data-platform/pull/780"
               'lui-raw-text "Zeshan Anwar: two backfill PRs for your eyes"
               'face 'slack-message-output-text))
         (pr (code-review-utils-pr-from-url url)))
    (should (equal pr
                   (a-alist
                    'num "780"
                    'repo "writer-data-platform"
                    'owner "WriterInternal"
                    'forge 'github
                    'url "https://github.com/WriterInternal/writer-data-platform/pull/780")))
    (dolist (cell pr)
      (should-not (and (stringp (cdr cell))
                       (text-properties-at 0 (cdr cell)))))))

;;; PR URL input (code-review--read-pr-url)

(ert-deftest code-review-utils-test/read-pr-url-kill-ring ()
  "A valid PR URL in the kill ring is passed through unchanged."
  (let ((kill-ring
         (list "https://github.com/WriterInternal/be.service.billing/pull/1050")))
    (should (equal (code-review--read-pr-url)
                   "https://github.com/WriterInternal/be.service.billing/pull/1050"))))

(ert-deftest code-review-utils-test/read-pr-url-valid-manual-entry ()
  "A typed valid PR URL is validated and returned unchanged."
  (let ((kill-ring (list "not a url"))
        (old-read (symbol-function 'read-string))
        (old-gui (symbol-function 'gui-get-selection)))
    (unwind-protect
        (progn
          (fset 'read-string
                (lambda (_prompt &rest _)
                  "https://github.com/WriterInternal/be.service.billing/pull/1050"))
          (fset 'gui-get-selection (lambda (&rest _) nil))
          (should (equal (code-review--read-pr-url)
                         "https://github.com/WriterInternal/be.service.billing/pull/1050")))
      (fset 'read-string old-read)
      (fset 'gui-get-selection old-gui))))

(ert-deftest code-review-utils-test/read-pr-url-invalid-manual-entry ()
  "A typed non-URL aborts with user-error, never a garbage string.
The old interactive spec validated a typo'd variable (void
`urk'), caught the void-variable, and returned the message
string \"Not a valid url\" as the URL — the command then failed
far away with \"Could not identify the PR ... Not a valid url\"."
  (let ((kill-ring (list "not a url"))
        (old-read (symbol-function 'read-string))
        (old-gui (symbol-function 'gui-get-selection)))
    (unwind-protect
        (progn
          (fset 'read-string (lambda (_prompt &rest _) "also not a url"))
          (fset 'gui-get-selection (lambda (&rest _) nil))
          (should-error (code-review--read-pr-url)
                        :type 'user-error))
      (fset 'read-string old-read)
      (fset 'gui-get-selection old-gui))))

;;; code-review-utils-test.el ends here
