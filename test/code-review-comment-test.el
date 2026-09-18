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

;;; code-review-comment-test.el ends here
