;;; code-review-interfaces.el --- Main APIs you need to provide to add a new forge -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Wanderson Ferreira
;;
;; Author: Wanderson Ferreira <https://github.com/wandersoncferreira>
;; Maintainer: Wanderson Ferreira <wand@hey.com>
;; Version: 0.0.7
;; Homepage: https://github.com/wandersoncferreira/code-review
;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;;  Description
;;
;;; Code:

(cl-defgeneric code-review-pullreq-infos (obj fallback? callback)
  "Return infos Pull Request from a OBJ, use FALLBACK? to minimal query, run CALLBACK.")

(cl-defgeneric code-review-infos-deferred (obj fallback?)
  "Run OBJ and set if minimal query should be run using FALLBACK?.")

(cl-defgeneric code-review-pullreq-diff (obj callback)
  "Return diff for OBJ running CALLBACK with results.")

(cl-defgeneric code-review-diff-deferred (obj)
  "Run OBJ with deferred.")

(cl-defgeneric code-review-commit-diff (obj callback)
  "Return diff for OBJ running CALLBACK with results.")

(cl-defgeneric code-review-commit-comments (obj callback)
  "Return commit comments for OBJ running CALLBACK with results.")

(cl-defgeneric code-review-commit-comments-deferred (obj)
  "Run OBJ with deferred.")

(cl-defgeneric code-review-commit-diff-deferred (obj)
  "Run OBJ with deferred.")

(cl-defgeneric code-review-send-review (obj callback)
  "Send review stored in OBJ and call CALLBACK afterward.")

(cl-defgeneric code-review-send-comments (obj callback)
  "Send comments stored in OBJ and call CALLBACK afterward.")

(cl-defgeneric code-review-send-replies (obj callback)
  "Send review comment replies stored in OBJ and call CALLBACK afterward.")

(cl-defgeneric code-review-get-labels (obj)
  "Sync call to get a list of labels from OBJ.")

(cl-defgeneric code-review-send-labels (obj callback)
  "Sync call to set a list of labels for an OBJ and call CALLBACK afterward..")

(cl-defgeneric code-review-get-assignees (obj)
  "Sync call to get a list of assignees from OBJ.")

(cl-defgeneric code-review-send-assignee (obj callback)
  "Set an assignee for an OBJ and call CALLBACK afterward..")

(cl-defgeneric code-review-get-milestones (obj)
  "Sync call to get a list of milestones from OBJ.")

(cl-defgeneric code-review-send-milestone (obj callback)
  "Set a milestone for an OBJ and call CALLBACK afterward.")

(cl-defgeneric code-review-send-title (obj callback)
  "Set a pullrequest title for an OBJ and call CALLBACK afterward.")

(cl-defgeneric code-review-close (obj callback)
  "Close a PR for an OBJ and call CALLBACK afterward.")

(cl-defgeneric code-review-send-description (obj callback)
  "Set a pullrequest description for an OBJ and call CALLBACK afterward.")

(cl-defgeneric code-review-merge (obj strategy)
  "Merge a PR for an OBJ using a given STRATEGY.")

(cl-defgeneric code-review-send-reaction (obj context-name comment-id reaction)
  "Set a REACTION to a COMMENT-ID in OBJ given a CONTEXT-NAME.")

(cl-defgeneric code-review-delete-reaction (obj context-name comment-id reaction-id)
  "Delete a REACTION to a COMMENT-ID in OBJ given a CONTEXT-NAME.")

(cl-defgeneric code-review-get-assignable-users (obj)
  "Get users that can review a PR for OBJ.")

(cl-defgeneric code-review-request-review (obj user-ids callback)
  "Request for OBJ a list of USER-IDS to review a PR and call CALLBACK afterward.")

(cl-defgeneric code-review-new-issue (obj body title callback)
  "Create new issue in OBJ with BODY and TITLE and call CALLBACK.")

(cl-defgeneric code-review-new-issue-comment (obj comment-msg callback)
  "Create a new comment issue for OBJ sending the COMMENT-MSG and call CALLBACK.")

(cl-defgeneric code-review-new-code-comment (obj local-comment callback)
  "Create a new diff comment for OBJ given a LOCAL-COMMENT and call CALLBACK.")

;; Deleting comments
(cl-defgeneric code-review-delete-code-comment (obj comment-id callback)
  "Delete a diff comment identified by COMMENT-ID from OBJ and call CALLBACK.
COMMENT-ID is the provider's identifier (e.g., GitHub review comment id).")

(cl-defmethod code-review-delete-code-comment ((obj t) comment-id callback)
  "Fallback when provider deletion is not implemented."
  (ignore obj comment-id callback)
  (user-error "Deleting code comments is not implemented for this provider"))

;; Resolving/unresolving review threads
(cl-defgeneric code-review-toggle-resolved (obj thread-id resolve? callback)
  "Toggle RESOLVE? on review thread THREAD-ID in OBJ.
RESOLVE? non-nil means resolve the thread, nil means unresolve it.
Call CALLBACK when the provider API call completes.")

(cl-defmethod code-review-toggle-resolved ((obj t) thread-id resolve? callback)
  "Fallback when provider thread resolution is not implemented."
  (ignore obj thread-id resolve? callback)
  (user-error "Toggling resolved threads is not implemented for this provider"))

;; Editing submitted comments/reviews
(cl-defgeneric code-review-update-comment (obj kind comment-id body callback)
  "Update the BODY of an already-submitted comment in OBJ.
KIND is a provider-agnostic token selecting what COMMENT-ID
refers to: \"issue-comment\" (a conversation comment),
\"review-summary\" (a submitted review body) or
\"review-comment\" (a diff-anchored review comment, including
thread replies and outdated ones).  Call CALLBACK when the
provider API call completes.")

(cl-defmethod code-review-update-comment (obj kind comment-id body callback)
  "Fallback when provider comment editing is not implemented."
  (ignore obj kind comment-id body callback)
  (user-error "Editing submitted comments is not implemented for this provider"))

;; PR lifecycle
(cl-defgeneric code-review-reopen (obj callback)
  "Reopen a closed pull request in OBJ and call CALLBACK afterward.")

(cl-defmethod code-review-reopen (obj callback)
  "Fallback when provider PR reopening is not implemented."
  (ignore obj callback)
  (user-error "Reopening PRs is not implemented for this provider"))

(cl-defgeneric code-review-toggle-draft (obj make-draft? callback)
  "Convert OBJ's pull request to a draft when MAKE-DRAFT? is non-nil,
mark it ready for review otherwise.  Call CALLBACK when the
provider API call completes.")

(cl-defmethod code-review-toggle-draft (obj make-draft? callback)
  "Fallback when provider draft toggling is not implemented."
  (ignore obj make-draft? callback)
  (user-error "Toggling draft status is not implemented for this provider"))

(provide 'code-review-interfaces)
;;; code-review-interfaces.el ends here
