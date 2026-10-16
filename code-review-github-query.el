;;; code-review-github-query.el --- GitHub GraphQL queries -*- lexical-binding: t; -*-

;; This file is part of code-review, distributed under the GNU GPL v3 or later.

;;; Commentary:
;; Query documents, paginated read helpers, and the viewed-file mutation.
;; Loaded by code-review-github after its class and configuration.

;;; Code:

(require 'ghub)
(require 'a)
(require 'code-review-db)
(require 'code-review-interfaces)

(defvar code-review-auth-login-marker)
(defvar code-review-github-graphql-host)
(declare-function code-review-github-errback "code-review-github")

(defun code-review-github--review-query (complete?)
  "Build the review query, including optional fields when COMPLETE?.
Keep the legacy repository/owner/number format placeholders for callers."
  (format
   "query {
  repository(name: \"%%s\", owner: \"%%s\") {
    pullRequest(number:%%s) {
      id
      author { login url }
      headRefOid
      baseRefName
      headRefName
      isDraft
      databaseId
      %s
      number
      createdAt
      updatedAt
      latestOpinionatedReviews(first: 100) {
        nodes {
          author { login %s }
          createdAt
          state
        }
      }
      reviewRequests(first: 100) {
        nodes {
          asCodeOwner
          requestedReviewer {
            __typename
            ... on User { login name %s }
          }
        }
      }
      files(first: 100) {
        totalCount
        pageInfo { endCursor hasNextPage }
        nodes {
          path
          additions
          deletions
          viewerViewedState
        }
      }
      milestone { title progressPercentage }
      labels(first: 10) { nodes { name color } }
      assignees(first: 15) { nodes { name login %s } }
      suggestedReviewers { reviewer { name login } }
      commits(first: 100) {
        totalCount
        nodes {
          commit {
            abbreviatedOid
            committedDate
            message
            statusCheckRollup {
              state
              contexts(first: 50) {
                nodes {
                  ... on StatusContext {
                    createdAt
                    context
                    state
                    targetUrl
                    description
                  }
                  ... on CheckRun {
                    startedAt
                    completedAt
                    name
                    text
                    title
                    summary
                    status
                    conclusion
                    detailsUrl
                    checkSuite {
                      app { id name description slug logoUrl }
                      %s
                    }
                  }
                }
              }
            }
          }
        }
      }
      title
      state
      bodyHTML
      bodyText
      reactions(first: 50) { nodes { id content } }
      comments(first: 50) {
        nodes {
          typename: __typename
          reactions(first: 50) { nodes { id content } }
          author { login }
          databaseId
          bodyHTML
          body
          createdAt
          updatedAt
        }
      }
      reviews(first: 50) {
        nodes {
          typename: __typename
          author { login }
          bodyHTML
          body
          state
          createdAt
          databaseId
          updatedAt
          comments(first: 50) {
            nodes {
              createdAt
              updatedAt
              bodyHTML
              body
              originalPosition
              diffHunk
              position
              line
              startLine
              outdated
              path
              databaseId
              %s
              reactions(first: 50) { nodes { id content } }
            }
          }
        }
      }
      reviewThreads(first: 100) {
        nodes {
          id
          isResolved
          isOutdated
          path
          diffSide
          line
          startDiffSide
          startLine
          comments(first: 100) {
            nodes { databaseId fullDatabaseId }
          }
        }
      }
    }
  }
}"
   (if complete? "fullDatabaseId" "")
   (if complete? "url" "")
   (if complete? "url" "")
   (if complete? "url" "")
   (if complete? "workflowRun { workflow { name } }" "")
   (if complete? "" "fullDatabaseId")))

(defvar code-review-github-graphql-fallback
  (code-review-github--review-query nil)
  "Review query without optional GitHub fields.")

(defvar code-review-github-graphql-complete
  (code-review-github--review-query t)
  "Review query including optional GitHub fields.")

(defconst code-review-github--files-query
  "query($id:ID!, $cursor:String!) {
  node(id:$id) {
    ... on PullRequest {
      files(first:100, after:$cursor) {
        totalCount
        pageInfo { endCursor hasNextPage }
        nodes { path additions deletions viewerViewedState }
      }
    }
  }
}"
  "Continuation query for a PR's files and viewed state.")

(defun code-review-github--graphql-data (res)
  "Return the data contents of a `ghub-query' response RES.
Synchronous ghub 5.1 responses are a single (data . CONTENTS) pair."
  (if (and (listp res) (eq (car-safe res) 'data)) (cdr res) res))

(defun code-review-github--path (data path)
  "Resolve the symbol PATH in DATA, or return nil."
  (while (and data path)
    (setq data (cdr (assq (pop path) data))))
  data)

(defun code-review-github--continue-files (res callback &optional seen)
  "Complete RES's files asynchronously, then invoke CALLBACK.
SEEN bounds pagination and stops a repeated cursor.  A failed page
keeps the fetched subset; reconciliation never clears unknown paths."
  (let* ((data (code-review-github--graphql-data res))
         (pr (a-get-in data '(repository pullRequest)))
         (files (a-get pr 'files))
         (page (a-get files 'pageInfo))
         (cursor (a-get page 'endCursor)))
    (if (and (eq (a-get page 'hasNextPage) t)
             cursor (not (member cursor seen)) (< (length seen) 30))
        (let ((failed
               (lambda (&rest _)
                 (message "code-review: could not fetch all file viewed states")
                 (funcall callback res))))
          (condition-case nil
              (ghub-query
               code-review-github--files-query
               `((id . ,(a-get pr 'id)) (cursor . ,cursor))
               :auth code-review-auth-login-marker
               :host code-review-github-graphql-host
               :callback
               (lambda (answer &rest _)
                 (let* ((next (a-get-in
                               (code-review-github--graphql-data answer)
                               '(node files)))
                        (next-page (a-get next 'pageInfo)))
                   (if (not next-page)
                       (funcall failed)
                     (code-review-github--continue-files
                      (cons 'data
                            (a-assoc-in
                             data '(repository pullRequest files)
                             (a-assoc next 'nodes
                                      (append (a-get files 'nodes)
                                              (a-get next 'nodes)))))
                      callback (cons cursor seen)))))
               :errorback failed)
            (error (funcall failed))))
      (funcall callback res))))

(cl-defmethod code-review-pullreq-infos
  ((github code-review-github-repo) fallback? callback)
  "Fetch GITHUB's review infos and all file states, then call CALLBACK.
FALLBACK? selects the query without optional fields."
  (let* ((number (oref github number))
         (query (format (if fallback?
                            code-review-github-graphql-fallback
                          code-review-github-graphql-complete)
                        (oref github repo) (oref github owner)
                        (if (numberp number) number
                          (string-to-number number)))))
    (ghub-query query nil
                :auth code-review-auth-login-marker
                :host code-review-github-graphql-host
                :callback (lambda (res &rest _)
                            (code-review-github--continue-files res callback))
                :errorback #'code-review-github-errback)))

(defun code-review-github--fetch-connection (_github query variables path)
  "Fetch all nodes of the paginated QUERY at PATH.
VARIABLES supplies the first page's inputs.  These completion
queries are synchronous because callers need their return value."
  (let ((has-next-page t)
        cursor res)
    (while has-next-page
      (let* ((page (code-review-github--graphql-data
                    (ghub-query query
                                (append variables
                                        (and cursor `((cursor . ,cursor))))
                                :auth code-review-auth-login-marker
                                :host code-review-github-graphql-host
                                :synchronous t)))
             (connection (code-review-github--path page path))
             (page-info (cdr (assq 'pageInfo connection))))
        (setq res (append res (cdr (assq 'nodes connection)))
              cursor (cdr (assq 'endCursor page-info))
              has-next-page (cdr (assq 'hasNextPage page-info)))))
    res))

(cl-defmethod code-review-get-assignable-users ((github code-review-github-repo))
  "Return GITHUB's assignable users, cached in its raw infos."
  (let ((infos (oref github raw-infos)))
    (or (a-get infos 'assignable-users)
        (let ((res (code-review-github--fetch-connection
                    github
                    "query($repo_owner:String!, $repo_name:String!, $cursor:String) {
  repository(owner:$repo_owner, name:$repo_name) {
    assignableUsers(first:100, after:$cursor) {
      pageInfo { endCursor hasNextPage }
      nodes { id login name }
    }
  }
}"
                    `((repo_owner . ,(oref github owner))
                      (repo_name . ,(oref github repo)))
                    '(repository assignableUsers))))
          (oset github raw-infos (a-assoc infos 'assignable-users res))
          (code-review-db-update github)
          res))))

(cl-defmethod code-review-get-mentionable-users ((github code-review-github-repo))
  "Return GITHUB's organization members, or its assignable users.
Cache the result in raw infos as mentionable-users."
  (let ((infos (oref github raw-infos)))
    (or (a-get infos 'mentionable-users)
        (let ((res (or (code-review-github--fetch-connection
                        github
                        "query($org:String!, $cursor:String) {
  organization(login:$org) {
    membersWithRole(first:100, after:$cursor) {
      pageInfo { endCursor hasNextPage }
      nodes { id login name }
    }
  }
}"
                        `((org . ,(oref github owner)))
                        '(organization membersWithRole))
                       (code-review-get-assignable-users github))))
          (oset github raw-infos
                (a-assoc (oref github raw-infos) 'mentionable-users res))
          (code-review-db-update github)
          res))))

(cl-defmethod code-review-toggle-file-viewed
  ((github code-review-github-repo) path viewed? callback errorback)
  "Mirror PATH's VIEWED? state to GitHub and invoke the appropriate callback."
  (let ((pr-id (a-get (oref github raw-infos) 'id)))
    (unless pr-id (user-error "Refresh the review to fetch its GitHub node id"))
    (ghub-query
     (format "mutation($input:%s!) { %s(input:$input) { __typename } }"
             (if viewed? "MarkFileAsViewedInput" "UnmarkFileAsViewedInput")
             (if viewed? "markFileAsViewed" "unmarkFileAsViewed"))
     `((input . ((path . ,path) (pullRequestId . ,pr-id))))
     :auth code-review-auth-login-marker
     :host code-review-github-graphql-host
     :callback callback :errorback errorback)))

(provide 'code-review-github-query)
;;; code-review-github-query.el ends here
