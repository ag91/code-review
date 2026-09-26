;;; code-review-section-header.el --- the PR header section inserters -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026 Andrea <andrea-dev@hotmail.com>
;;
;; Author: Wanderson Ferreira <https://github.com/wandersoncferreira>
;; Maintainer: Andrea <andrea-dev@hotmail.com>
;; Keywords: git, tools, vc
;; Homepage: https://github.com/wandersoncferreira/code-review
;;
;; Split from code-review-section.el (phase 15c, see Improvements.org):
;; the code is mostly Wanderson Ferreira's original
;; code-review-section.el, moved verbatim by the split; the fork and
;; this file are maintained by Andrea.

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; the PR header section inserters (part of the code-review section rendering, split from
;; code-review-section.el in phase 15c).

;;; Code:

(require 'code-review-section-shared)
(require 'magit-section)
(require 'magit-diff)
(require 'cl-lib)
(require 'a)
(require 'code-review-faces)
(require 'code-review-db)
(require 'code-review-utils)




(defclass code-review-url-section (magit-section)
  ((keymap :initform 'code-review-url-section-map)
   (url :initarg :url)))
(defvar code-review-url-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'browse-url)
    (define-key map [mouse-2] 'browse-url)
    (define-key map [follow-link] 'browse-url)
    map)
  "Keymaps for header url section.")
(defclass code-review-author-section (magit-section)
  ((keymap :initform 'code-review-author-section-map)
   (login :initarg :login)
   (url :initarg :url)))
(defvar code-review-author-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'code-review-utils--visit-author-at-point)
    (define-key map [mouse-2] 'code-review-utils--visit-author-at-point)
    (define-key map [follow-link] 'code-review-utils--visit-author-at-point)
    map)
  "Keymaps for header author section.")
(defun code-review-section-insert-url ()
  "Insert the author of the PR in the buffer."
  (with-slots (url) (code-review-db-get-pullreq)
    (when url
      (let ((obj (code-review-url-section
                  :url url)))
        (magit-insert-section (code-review-author-section obj)
          (insert (format "%-17s" "Url: "))
          (insert (propertize (format "%s" url)
                              'face 'code-review-url-header-face
                              'mouse-face 'code-review-hover-face
                              'help-echo "Visit PR url"
                              'keymap 'code-review-url-section-map))
          (insert ?\n))))))
(defun code-review-section-insert-author ()
  "Insert the author of the PR in the buffer."
  (let-alist (code-review-db--pullreq-raw-infos)
    (when .author.login
      (let ((obj (code-review-author-section
                  :login .author.login
                  :url .author.url)))
        (magit-insert-section (code-review-author-section obj)
          (insert (format "%-17s" "Author: "))
          (insert (propertize (format "@%s" .author.login)
                              'face 'code-review-author-header-face
                              'mouse-face 'code-review-hover-face
                              'help-echo "Visit author's page"
                              'keymap 'code-review-author-section-map))
          (insert ?\n))))))
(defclass code-review-title-section (magit-section)
  ((keymap  :initform 'code-review-title-section-map)
   (title  :initform nil
           :type (or null string))))
(defvar code-review-title-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'code-review-set-title)
    map)
  "Keymaps for code-comment sections.")
(defun code-review-section-insert-header-title ()
  "Insert the title header line."
  (let ((pr (code-review-db-get-pullreq)))
    (setq header-line-format
          (propertize
           (format "#%s: %s" (oref pr number) (oref pr title))
           'font-lock-face
           'magit-section-heading))))
(defun code-review-section-insert-title ()
  "Insert the title of the header buffer."
  (when-let (title (code-review-db--pullreq-title))
    (magit-insert-section (code-review-title-section title)
      (insert (format "%-17s" "Title: ") title)
      (insert ?\n))))
(defclass code-review-state-section (magit-section)
  ((state  :initform nil
           :type (or null string))))
(defun code-review-section-insert-state ()
  "Insert the state of the header buffer."
  (when-let (state (code-review-db--pullreq-state))
    (let ((value (if state state "none")))
      (magit-insert-section (code-review-state-section value)
        (insert (format "%-17s" "State: ") value)
        (insert ?\n)))))
(defclass code-review-ref-section (magit-section)
  ((base   :initarg :base
           :type (or null string))
   (head   :initarg :head
           :type (or null string))))
(defun code-review-section-insert-ref ()
  "Insert the state of the header buffer."
  (let* ((pr (code-review-db-get-pullreq))
         (obj (code-review-ref-section
               :base (oref pr base-ref-name)
               :head (oref pr head-ref-name))))
    (magit-insert-section (code-review-ref-section obj)
      (insert (format "%-17s" "Refs: "))
      (insert (oref pr base-ref-name))
      (insert (propertize " ... " 'font-lock-face 'magit-dimmed))
      (insert (oref pr head-ref-name))
      (insert ?\n))))
(defclass code-review-milestone-section (magit-section)
  ((keymap :initform 'code-review-milestone-section-map)
   (title  :initarg :title)
   (perc   :initarg :perc)
   (number :initarg :number
           :type number)))
(defvar code-review-milestone-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'code-review-set-milestone)
    map)
  "Keymaps for milestone section.")
(defun code-review-section-insert-milestone ()
  "Insert the milestone of the header buffer."
  (let ((milestones (code-review-db--pullreq-milestones)))
    (let-alist milestones
      (let* ((title (when (not (string-empty-p .title)) .title))
             (obj (code-review-milestone-section :title title :perc .perc)))
        (magit-insert-section (code-review-milestone-section obj)
          (insert (format "%-17s" "Milestone: "))
          (insert (propertize (code-review-pretty-milestone obj) 'font-lock-face 'magit-dimmed))
          (insert ?\n))))))
(defclass code-review-labels-section (magit-section)
  ((keymap :initform 'code-review-labels-section-map)
   (labels :initarg :labels)))
(defvar code-review-labels-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'code-review-set-label)
    map)
  "Keymaps for code-comment sections.")
(defun code-review-section-insert-labels ()
  "Insert the labels of the header buffer."
  (let* ((infos (code-review-db--pullreq-raw-infos))
         (labels (code-review--distinct-labels
                  (append (code-review-db--pullreq-labels)
                          (a-get-in infos (list 'labels 'nodes)))))
         (obj (code-review-labels-section :labels labels)))
    (magit-insert-section (code-review-labels-section obj)
      (insert (format "%-17s" "Labels: "))
      (if labels
          (dolist (label labels)
            (insert (a-get label 'name))
            (let* ((raw-color (a-get label 'color))
                   (color (if (string-prefix-p "#" raw-color)
                              raw-color
                            (concat "#" raw-color)))
                   (background (code-review-utils--sanitize-color color))
                   (foreground (code-review-utils--contrast-color color))
                   (o (make-overlay (- (point) (length (a-get label 'name))) (point))))
              (overlay-put o 'priority 2)
              (overlay-put o 'evaporate t)
              (overlay-put o 'font-lock-face
                           `((:background ,background)
                             (:foreground ,foreground)
                             forge-topic-label)))
            (insert " "))
        (insert (propertize "None yet" 'font-lock-face 'magit-dimmed)))
      (insert ?\n))))
(defclass code-review-assignees-section (magit-section)
  ((keymap :initform 'code-review-assignees-section-map)
   (assignees :initarg :assignees)))
(defvar code-review-assignees-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'code-review-set-assignee)
    (define-key map [mouse-2] 'code-review-set-assignee)
    (define-key map [follow-link] 'code-review-set-assignee)
    map)
  "Keymaps for code-comment sections.")
(defclass code-review-assignee-section (magit-section)
  ((keymap :initform 'code-review-assignee-section-map)
   (name :initarg :name)
   (url :initarg :url)))
(defvar code-review-assignee-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'code-review-assignee-visit-at-remote)
    (define-key map [mouse-2] 'code-review-assignee-visit-at-remote)
    (define-key map [follow-link] 'code-review-assignee-visit-at-remote)
    map)
  "Keymaps for assignee section")
(defun code-review-section--insert-assignee-action (label)
  "Insert the LABEL action link (the set-assignee actions)."
  (insert (propertize label
                      'font-lock-face 'code-review-dimmed
                      'mouse-face 'code-review-hover-face
                      'help-echo "Set new assignee"
                      'keymap 'code-review-assignees-section-map)))
(defun code-review-section--insert-assignee-line (assignee)
  "Insert one ASSIGNEE (a name/url alist) as its own section."
  (let-alist assignee
    (let ((assignee-obj (code-review-assignee-section
                         :name .name
                         :url .url)))
      (magit-insert-section (code-review-assignee-section assignee-obj)
        (insert (propertize .name
                            'face 'code-review-author-header-face
                            'mouse-face 'code-review-hover-face
                            'help-echo "Visit author's page"
                            'keymap 'code-review-assignee-section-map))))))
(defun code-review-section-insert-assignee ()
  "Insert the assignee of the header buffer."
  (let* ((infos (code-review-db--pullreq-assignees))
         (assignee-names (-map
                          (lambda (a)
                            (let ((name
                                   (if (a-get a 'name)
                                       (format "%s (@%s)"
                                               (a-get a 'name)
                                               (a-get a 'login))
                                     (format "@%s" (a-get a 'login)))))
                              `((name . ,name)
                                (url . ,(a-get a 'url)))))
                          infos)))
    (magit-insert-section (code-review-assignees-section)
      (insert (format "%-17s" "Assignees: "))
      (if (not assignee-names)
          (code-review-section--insert-assignee-action
           "No one — Assign yourself")
        (progn
          (code-review-section--insert-assignee-action "Set new assignee")
          (insert ?\n)
          (dolist (assignee assignee-names)
            (code-review-section--insert-assignee-line assignee))
          (insert ?\n)))
      (insert ?\n))))
(defclass code-review-project-section (magit-section)
  ((name :initarg :name)))
(defun code-review-section-insert-project ()
  "Insert the project of the header buffer."
  (let-alist (code-review-db--pullreq-raw-infos)
    (let* ((project-names (-map
                           (lambda (p)
                             (a-get-in p (list 'project 'name)))
                           .projectCards.nodes))
           (projects (if project-names
                         (string-join project-names ", ")
                       (propertize "None yet" 'font-lock-face 'magit-dimmed))))
      (magit-insert-section (code-review-project-section projects)
        (insert (format "%-17s" "Projects: ") projects)
        (insert ?\n)))))
(defclass code-review-is-draft-section (magit-section)
  ((draft? :initform nil
           :type (or null string))))
(defun code-review-section-insert-is-draft ()
  "Insert the isDraft value of the header buffer."
  (let-alist (code-review-db--pullreq-raw-infos)
    (let* ((draft? (if .isDraft "true" "false")))
      (magit-insert-section (code-review-is-draft-section draft?)
        (insert (format "%-17s" "Draft: ") draft?)
        (insert ?\n)))))
(defclass code-review-suggested-reviewers-section (magit-section)
  ((keymap :initform 'code-review-suggested-reviewers-section-map)))
(defvar code-review-suggested-reviewers-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'code-review-request-review-at-point)
    (define-key map [mouse-2] 'code-review-request-review-at-point)
    (define-key map [follow-link] 'code-review-request-review-at-point)
    map)
  "Keymaps for suggested reviewers section.")
(defun code-review-section--requested-reviewer-logins (reviewers-group)
  "All reviewer logins in REVIEWERS-GROUP (the status -> users hash)."
  (let (res)
    (maphash (lambda (_status users)
               (setq res (append res
                                 (-map (lambda (it) (a-get it 'login))
                                       users))))
             reviewers-group)
    res))
(defun code-review-section--suggested-reviewer-logins (infos)
  "Suggested reviewer logins from INFOS, minus the ones already
requested or with an opinionated review."
  (let-alist infos
    (let ((requested (code-review-section--requested-reviewer-logins
                      (code-review-utils--fmt-reviewers infos))))
      (->> .suggestedReviewers
           (-map (lambda (r)
                   (a-get-in r (list 'reviewer 'login))))
           (-filter (lambda (login)
                      (and (not (equal login nil))
                           (not (-contains-p requested login)))))))))
(defun code-review-section--insert-suggested-reviewer (login)
  "Insert one suggested reviewer LOGIN with its request-review link."
  (insert ?\n)
  (insert (propertize "Request Review"
                      'face 'code-review-request-review-face
                      'mouse-face 'code-review-hover-face
                      'help-echo "Request review from reviewe"
                      'keymap 'code-review-suggested-reviewers-section-map))
  (insert " - ")
  (insert (propertize (concat "@" login) 'face 'code-review-author-face)))
(defun code-review-section-insert-suggested-reviewers ()
  "Insert the suggested reviewers."
  (let* ((infos (code-review-db--pullreq-raw-infos))
         (reviewers (code-review-section--suggested-reviewer-logins infos))
         (suggested-reviewers (if (not reviewers)
                                  (propertize "No suggestions" 'font-lock-face 'magit-dimmed)
                                reviewers)))
    (magit-insert-section (code-review-suggested-reviewers-section suggested-reviewers)
      (insert "Suggested-Reviewers:")
      (if (not reviewers)
          (insert " " suggested-reviewers)
        (dolist (sr suggested-reviewers)
          (code-review-section--insert-suggested-reviewer sr)))
      (insert ?\n))))
(defclass code-review-reviewers-section (magit-section)
  (()))
(defclass code-review-reviewer-section (magit-section)
  ((keymap :initform 'code-review-reviewer-section-map)
   (login :initarg :login)
   (url :initarg :url)))
(defvar code-review-reviewer-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-2] 'code-review-reviewer-visit-at-remote)
    (define-key map [follow-link] 'code-review-reviewer-visit-at-remote)
    map)
  "Keymaps for reviewer section.")
(defun code-review-section-insert-reviewers ()
  "Insert the reviewers section."
  (let* ((infos (code-review-db--pullreq-raw-infos))
         (groups (code-review-utils--fmt-reviewers infos)))
    (magit-insert-section (code-review-reviewers-section)
      (insert "Reviewers:\n")
      (maphash (lambda (status users-objs)
                 (dolist (user-o users-objs)
                   (let-alist user-o
                     (let ((obj (code-review-reviewer-section
                                 :login .login
                                 :url .url)))
                       (magit-insert-section (code-review-reviewer-section obj)
                         (insert (code-review--propertize-keyword status))
                         (insert " - ")
                         (insert (propertize (concat "@" .login)
                                             'face 'code-review-author-face
                                             'mouse-face 'code-review-hover-face
                                             'help-echo "Visit user profile"
                                             'keymap 'code-review-reviewer-section-map))
                         (when .code-owner?
                           (insert " as CODE OWNER"))
                         (when .at
                           (insert " " (propertize (code-review-utils--format-timestamp .at) 'face 'code-review-timestamp-face))))
                       (insert ?\n)))))
               groups))))

;; headers hook definition

(defun code-review-section-insert-headers ()
  "Insert all the headers."
  (magit-insert-headers 'code-review-headers-hook))

;; commits

(defclass code-review-commits-header-section (magit-section)
  (()))
(defclass code-review-commit-section (magit-section)
  ((keymap :initform 'code-review-commit-section-map)
   (sha    :initarg :sha)
   (msg    :initarg :msg)))
(defvar code-review-commit-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'code-review-commit-at-point)
    map)
  "Keymaps for commit section.")
(defclass code-review-commit-check-detail-section (magit-section)
  ((keymap :initform 'code-review-commit-check-detail-section-map)
   (details :initarg :details)
   (check   :initarg :check)))
(defclass code-review-commit-checks-section (magit-section)
  ()
  "Groups the CI check details of one commit behind one heading.")
(defvar code-review-commit-check-detail-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'code-review-commit-goto-check-at-remote)
    (define-key map [mouse-2] 'code-review-commit-goto-check-at-remote)
    (define-key map [follow-link] 'code-review-commit-goto-check-at-remote)
    map)
  "Keymaps for commit check section.")
(defun code-review-section--insert-commit-check-detail (check)
  "Insert one CI CHECK line: check name, outcome detail, details link."
  (let-alist check
    (let ((obj (code-review-commit-check-detail-section
                :check check :details (or .detailsUrl .targetUrl)))
          (success? (string-equal .conclusion "SUCCESS"))
          (check-name
           (if-let ((check-suite-name
                     (or .checkSuite.workflowRun.workflow.name
                         .checkSuite.app.name)))
               (format "%s / %s" check-suite-name .name)
             ;; for StatusContext actions
             .context)))
      (magit-insert-section (code-review-commit-check-detail-section obj)
        (insert (propertize (format "%-7s %s" "" check-name)
                            'font-lock-face 'code-review-checker-name-face))
        (insert " - ")
        (if success?
            (when .startedAt
              (insert (propertize
                       (format "%s  "
                               (format "Successful in %s."
                                       (code-review-utils--elapsed-time
                                        .completedAt .startedAt)))
                       'font-lock-face 'magit-dimmed)))
          (insert (propertize (format "%s  " (or .summary .description))
                              'font-lock-face 'magit-dimmed)))
        (insert (propertize (if success?
                                ":white_check_mark: Details"
                              ":x: Details")
                            'font-lock-face 'code-review-checker-detail-face
                            'mouse-face 'code-review-hover-face
                            'help-echo "Visit the page for details"
                            'keymap 'code-review-commit-check-detail-section-map))
        (insert "\n")))))
(defun code-review-section--insert-commit-checks (contexts)
  "Insert the CI Checks section for the commit rollup CONTEXTS."
  (code-review-section--hide-if-hidden
   (magit-insert-section (code-review-commit-checks-section nil t)
     (insert (propertize
              (format "  CI Checks (%s)" (length contexts))
              'font-lock-face 'code-review-checker-name-face))
     (magit-insert-heading)
     (dolist (check contexts)
       (code-review-section--insert-commit-check-detail check)))))
(defun code-review-section--insert-commit-with-checks (obj state contexts)
  "Insert the commit OBJ heading with its CI checks subtree.
STATE is the rollup state, CONTEXTS the checks list."
  (insert (format "%s%s %s "
                  (propertize (format "%-6s " (oref obj sha))
                              'font-lock-face 'magit-hash)
                  (car (split-string (oref obj msg) "\n"))
                  (if (string-equal state "SUCCESS")
                      ":white_check_mark:"
                    ":x:")))
  (insert (propertize "Expand for Details:"
                      'font-lock-face 'code-review-checker-detail-face))
  (magit-insert-heading)
  (when (> (length (split-string (oref obj msg) "\n")) 1)
    (insert (oref obj msg))
    (insert "\n"))
  (code-review-section--insert-commit-checks contexts))
(defun code-review-section--insert-plain-commit (obj)
  "Insert the commit OBJ (no CI checks): sha and message only."
  (insert (propertize (format "%-6s " (oref obj sha))
                      'font-lock-face 'magit-hash))
  (insert (oref obj msg))
  (insert ?\n))
(defun code-review-section--insert-commit (pr c obj)
  "Insert one commit section: the raw alist C rendered as OBJ.
On github repos whose commit carries a statusCheckRollup the
section starts collapsed and carries the CI checks subtree."
  (let-alist c
    (let ((contexts (and (code-review-github-repo-p pr)
                         .commit.statusCheckRollup.contexts.nodes)))
      (code-review-section--hide-if-hidden
       (magit-insert-section (code-review-commit-section obj)
                             ;; collapsed by default when expandable
                             contexts
                             (if contexts
                                 (code-review-section--insert-commit-with-checks
                                  obj .commit.statusCheckRollup.state contexts)
                               (code-review-section--insert-plain-commit obj)))))))
(defun code-review-section-insert-commits ()
  "Insert commits from PULL-REQUEST."
  (let ((pr (code-review-db-get-pullreq)))
    (let-alist (oref pr raw-infos)
      (code-review-section--hide-if-hidden
       (magit-insert-section (code-review-commits-header-section
                             nil code-review-fold-header-sections)
        (insert (propertize "Commits:" 'font-lock-face 'magit-section-heading))
        (magit-insert-heading)
        (dolist (c .commits.nodes)
          (let-alist c
            (let* ((sha (a-get-in c (list 'commit 'abbreviatedOid)))
                   (msg (a-get-in c (list 'commit 'message)))
                   (obj (code-review-commit-section :sha sha :msg msg)))
              (code-review-section--insert-commit pr c obj))))
        (insert ?\n))))))

;; description

(defclass code-review-description-section (magit-section)
  ((keymap :initform 'code-review-description-section-map)
   (id     :initarg :id)
   (msg    :initarg :msg)
   (reactions :initarg :reactions)))
(defvar code-review-description-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-r") 'code-review-description-reaction-at-point)
    map)
  "Keymaps for description section.")
(defun code-review-section-insert-pr-description ()
  "Insert PULL-REQUEST description."
  (when-let (infos (code-review-db--pullreq-raw-infos))
    (let-alist infos
      (let* ((is-html? (when .bodyHTML t))
             (is-empty? (and (string-empty-p .bodyHTML)
                             (string-empty-p .bodyText)))
             (description-cleaned (if is-empty?
                                      "No description provided."
                                    (or .bodyHTML .bodyText)))
             (reaction-objs (-map
                             (lambda (r)
                               (code-review-reaction-section
                                :id (a-get r 'id)
                                :content (a-get r 'content)))
                             .reactions.nodes))
             (obj (code-review-description-section :msg description-cleaned
                                                   :id .databaseId
                                                   :reactions reaction-objs)))
        (code-review-section--hide-if-hidden
         (magit-insert-section (code-review-description-section obj
                                                                code-review-fold-header-sections)
          (insert (propertize "Description" 'font-lock-face 'magit-section-heading))
          (magit-insert-heading)
          (insert ?\n)
          (magit-insert-section (code-review-description-section obj)
            (if is-empty?
                (insert (propertize description-cleaned 'font-lock-face 'magit-dimmed))
              (if is-html?
                  (code-review--insert-html description-cleaned (* 2 code-review-section-indent-width))
                (insert description-cleaned)))
            (insert ?\n)
            (when .reactions.nodes
              (code-review-comment-insert-reactions
               reaction-objs
               "pr-description"
               .databaseId))
            (insert ?\n))))))))

;; feedback

(defclass code-review-feedback-section (magit-section)
  ((keymap :initform 'code-review-feedback-section-map)
   (msg    :initarg :msg)))
(defvar code-review-feedback-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'code-review-set-feedback)
    (define-key map (kbd "C-c C-k") 'code-review-delete-feedback)
    map)
  "Keymaps for feedback section.")
(defun code-review-section-insert-feedback-heading ()
  "Insert feedback heading.
Local diff reviews are read-only: no feedback section."
  (when (not (code-review-db-local-pr-p))
    (let* ((feedback (code-review-db--pullreq-feedback))
           (obj (code-review-feedback-section :msg feedback)))
      (code-review-section--hide-if-hidden
       (magit-insert-section (code-review-feedback-section obj
                                                            code-review-fold-header-sections)
         (insert (propertize "Your Review Feedback" 'font-lock-face 'magit-section-heading))
         (magit-insert-heading)
        (magit-insert-section (code-review-feedback-section obj)
          (if feedback
              (insert feedback)
            (insert (propertize "Leave a comment here." 'font-lock-face 'magit-dimmed))))
        (insert ?\n)
        (insert ?\n))))))
(cl-defmethod code-review-pretty-milestone ((obj code-review-milestone-section))
  "Get the pretty version of milestone for a given OBJ."
  (cond
   ((and (oref obj title) (oref obj perc))
    (format "%s (%0.2f%%)"
            (oref obj title)
            (oref obj perc)))
   ((oref obj title)
    (oref obj title))
   (t
    "No milestone")))
(provide 'code-review-section-header)
;;; code-review-section-header ends here
