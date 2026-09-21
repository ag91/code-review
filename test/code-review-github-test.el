;;; code-review-github-test.el --- ERT tests for GitHub API plumbing -*- lexical-binding: t; -*-

;; Phase 6 (edit submitted comments / PR lifecycle): the endpoint
;; helper is pure, so it is tested offline.  The actual ghub calls
;; are exercised live only.

(require 'ert)
(require 'code-review-github)

(ert-deftest code-review-github-test/update-comment-endpoint-review-comment ()
  "Diff comments go to the review comment PATCH endpoint."
  (should (equal (code-review-github--update-comment-endpoint
                  "review-comment" "octocat" "repo" 5 123)
                 (cons "/repos/octocat/repo/pulls/comments/123" #'ghub-patch))))

(ert-deftest code-review-github-test/update-comment-endpoint-issue-comment ()
  "Conversation comments go to the issue comment PATCH endpoint."
  (should (equal (code-review-github--update-comment-endpoint
                  "issue-comment" "octocat" "repo" 5 456)
                 (cons "/repos/octocat/repo/issues/comments/456" #'ghub-patch))))

(ert-deftest code-review-github-test/update-comment-endpoint-review-summary ()
  "Submitted review summaries go to the PUT reviews endpoint."
  (should (equal (code-review-github--update-comment-endpoint
                  "review-summary" "octocat" "repo" 5 789)
                 (cons "/repos/octocat/repo/pulls/5/reviews/789" #'ghub-put))))

(ert-deftest code-review-github-test/graphql-queries-fetch-raw-body ()
  "Both GraphQL queries must fetch `body' (raw markdown) on review
nodes, review comment nodes and top-level comment nodes: editing a
submitted comment needs the body as written, not the rendered
bodyHTML."
  (dolist (query (list code-review-github-graphql-fallback
                       code-review-github-graphql-complete))
    ;; review nodes carry bodyHTML and body
    (should (string-match-p "bodyHTML\n\\s-*body\n\\s-*state" query))
    ;; review comment nodes carry bodyHTML and body
    (should (string-match-p "bodyHTML\n\\s-*body\n\\s-*originalPosition" query))
    ;; top-level comment nodes carry databaseId, bodyHTML and body
    (should (string-match-p "databaseId\n\\s-*bodyHTML\n\\s-*body\n\\s-*createdAt" query))))

;;; code-review-github-test.el ends here
