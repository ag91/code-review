;;; code-review-comment-test.el --- ERT tests for comment grouping -*- lexical-binding: t; -*-

(require 'ert)
(require 'a)
(require 'code-review-comment)
(require 'code-review-utils)

(defconst code-review-comment-test--sample-raw-comments
  `(;; comment 1
    ((author (login . "wandersoncferreira"))
     (bodyHTML . "<p>This PR looks great</p>")
     (state . "COMMENTED")
     (createdAt . "2021-11-08T00:24:09Z")
     (updatedAt . "2021-11-08T00:24:09Z")
     (comments
      (nodes ((createdAt . "2021-11-08T00:24:09Z")
              (updatedAt . "2021-11-08T00:24:09Z")
              (bodyHTML . "<p>Why keep everything in Emacs?</p>")
              (body . "Why keep everything in Emacs?")
              (originalPosition . 3)
              (diffHunk . "@@ -5,3 +5,5 @@ All I can save about my current computer setup:
 - [archlinux](https://archlinux.org)
 - [macos](https://www.apple.com/macbook-pro-13/)
 - [emacs](https://www.gnu.org/software/emacs/)")
              (position . 3)
              (outdated)
              (path . "README.md")
              (databaseId . 735203147)))))

    ;; comment 2
    ((author (login . "another_user"))
     (bodyHTML . "<p>This can be improved a lot!</p>")
     (state . "REQUEST_CHANGES")
     (createdAt . "2021-11-08T00:24:09Z")
     (updatedAt . "2021-11-08T00:24:09Z")
     (comments
      (nodes ((createdAt . "2021-11-08T00:24:09Z")
              (updatedAt . "2021-11-08T00:24:09Z")
              (bodyHTML . "")
              (body . "")
              (originalPosition . 3)
              (diffHunk . "@@ -5,3 +5,5 @@ All I can save about my current computer setup:
 - [archlinux](https://archlinux.org)
 - [macos](https://www.apple.com/macbook-pro-13/)
 - [emacs](https://www.gnu.org/software/emacs/)")
              (position . 3)
              (outdated)
              (path . "README.md")
              (databaseId . 735203148)))))))

(defconst code-review-comment-test--sample-grouped-raw-comments
  (a-alist "README.md:3"
           (list
            (code-review-code-comment-section
             :createdAt "2021-11-08T00:24:09Z"
             :updatedAt "2021-11-08T00:24:09Z"
             :state "COMMENTED"
             :author "wandersoncferreira"
             :msg "<p>Why keep everything in Emacs?</p>"
             :body "Why keep everything in Emacs?"
             :position 3
             :reactions nil
             :path "README.md"
             :diffHunk "@@ -5,3 +5,5 @@ All I can save about my current computer setup:
 - [archlinux](https://archlinux.org)
 - [macos](https://www.apple.com/macbook-pro-13/)
 - [emacs](https://www.gnu.org/software/emacs/)"
             :internalId nil
             :id 735203147)

            ;; comment 2
            (code-review-code-comment-section
             :createdAt "2021-11-08T00:24:09Z"
             :updatedAt "2021-11-08T00:24:09Z"
             :state "REQUEST_CHANGES"
             :author "another_user"
             :msg ""
             :body ""
             :position 3
             :reactions nil
             :path "README.md"
             :diffHunk "@@ -5,3 +5,5 @@ All I can save about my current computer setup:
 - [archlinux](https://archlinux.org)
 - [macos](https://www.apple.com/macbook-pro-13/)
 - [emacs](https://www.gnu.org/software/emacs/)"
             :internalId nil
             :id 735203148))))

(ert-deftest code-review-comment-test/grouping-key-is-path-and-position ()
  "Should use PATH + `position' or `originalPosition' fields as key."
  (let ((group (code-review-utils-make-group
                code-review-comment-test--sample-raw-comments)))
    (should (equal (a-keys group)
                   `("README.md:3")))))

(ert-deftest code-review-comment-test/grouping-keeps-all-comments ()
  "Should keep all the comments under the key value."
  (let ((group (code-review-utils-make-group
                code-review-comment-test--sample-raw-comments)))
    (should (equal (length (alist-get "README.md:3" group nil nil 'equal))
                   2))))

(ert-deftest code-review-comment-test/grouping-flattens-structure ()
  "Should flat the structure, add state and login to the comment level."
  (let ((group (code-review-utils-make-group
                code-review-comment-test--sample-raw-comments))
        (expected code-review-comment-test--sample-grouped-raw-comments))
    ;; order-insensitive comparison of the two comment objects
    (should (= (length group) (length expected)))
    (dolist (entry expected)
      (let ((got (alist-get (car entry) group nil nil 'equal)))
        (should got)
        (should (= (length got) (length (cdr entry))))
        (dolist (obj (cdr entry))
          (should (member obj got)))))))

(ert-deftest code-review-comment-test/grouping-carries-raw-body ()
  "The raw (markdown) body must reach the comment sections.
Phase 6: `e' (edit submitted comment) prefills the comment buffer
with the body as written by its author, not the rendered HTML."
  (let* ((group (code-review-utils-make-group
                 code-review-comment-test--sample-raw-comments))
         (comments (alist-get "README.md:3" group nil nil 'equal)))
    (should (equal (mapcar (lambda (c) (oref c body)) comments)
                   '("Why keep everything in Emacs?" "")))))

;;; Phase 6: editing submitted comments

(ert-deftest code-review-comment-test/edit-target-conversation-kinds ()
  "Conversation comments map to the provider endpoint by typename."
  (should (equal (code-review-comment--edit-target
                  (code-review-comment-section
                   :author "a" :msg "m" :typename "IssueComment"))
                 "issue-comment"))
  (should (equal (code-review-comment--edit-target
                  (code-review-comment-section
                   :author "a" :msg "m" :typename "PullRequestReview"))
                 "review-summary"))
  (should (null (code-review-comment--edit-target
                 (code-review-comment-section
                  :author "a" :msg "m" :typename "SomethingElse")))))

(ert-deftest code-review-comment-test/edit-target-diff-comment-kinds ()
  "Diff-anchored comments are review comments; local ones are `local'."
  (should (equal (code-review-comment--edit-target
                  (code-review-code-comment-section
                   :state "s" :author "a" :msg "m" :path "p"))
                 "review-comment"))
  (should (equal (code-review-comment--edit-target
                  (code-review-outdated-comment-section
                   :state "s" :author "a" :msg "m" :path "p"))
                 "review-comment"))
  (should (eq (code-review-comment--edit-target
               (code-review-local-comment-section
                :state "s" :author "a" :msg "m" :path "p" :line-type "ADDED"))
              'local))
  (should (eq (code-review-comment--edit-target
               (code-review-reply-comment-section
                :state "s" :author "a" :msg "m" :path "p"))
              'local)))

(ert-deftest code-review-comment-test/edit-target-non-comment-is-nil ()
  "Anything that is not a comment has no edit target."
  (should (null (code-review-comment--edit-target nil)))
  (should (null (code-review-comment--edit-target (list 'a 'b)))))

(ert-deftest code-review-comment-test/edit-remote-header-is-stripped ()
  "The helper header must be stripped from the new comment body."
  (should (equal (code-review-utils--comment-clean-msg
                  (format "%s\n\nEdited body"
                          code-review-comment-edit-remote-msg)
                  code-review-comment-edit-remote-msg)
                 "Edited body")))

(ert-deftest code-review-comment-test/edit-remote-no-comment-at-point ()
  "The edit command user-errors outside of a comment section."
  (with-temp-buffer
    (should-error (code-review-edit-remote-comment-at-point)
                  :type 'user-error)))

;;; code-review-comment-test.el ends here
