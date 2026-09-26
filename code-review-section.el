;;; code-review-section.el --- UI -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Wanderson Ferreira
;;
;; Author: Wanderson Ferreira <https://github.com/wandersoncferreira>
;; Maintainer: Wanderson Ferreira <wand@hey.com>
;; Version: 0.0.7
;; Homepage: https://github.com/wandersoncferreira/code-review
;;
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
;;  Code to build the UI.
;;
;;; Code:

(require 'emojify)
(require 'deferred)
(require 'magit-section)
(require 'magit-diff)
(require 'cl-lib)
(require 'shr)

(require 'code-review-faces)
(require 'code-review-db)
(require 'code-review-utils)
(require 'code-review-repo)
(require 'code-review-diff)
(require 'code-review-analysis)
(require 'code-review-history)
(require 'code-review-hunkhighlight)
(require 'code-review-reactions)

(declare-function code-review--diff--classify-diff "code-review-diff")
(declare-function code-review--maybe-reorder-diff "code-review-diff")
(declare-function code-review-browse--reveal "code-review-browse")
(declare-function code-review-browse--strip-diff-prefix "code-review-browse")

(require 'code-review-interfaces)
(require 'code-review-github)
(require 'code-review-gitlab)
(require 'code-review-bitbucket)

(defcustom code-review-section-indent-width 1
  "Indent width for nested sections."
  :type 'integer
  :group 'code-review)

(defcustom code-review-section-image-scaling 0.8
  "Image scaling number used to resize images in buffer."
  :type 'float
  :group 'code-review)

(defcustom code-review-fill-column 80
  "Column number to wrap comments."
  :group 'code-review
  :type 'integer)



(defvar-local code-review-focus-mode nil
  "When non-nil, files auto-flagged as noise are hidden.
See `code-review-diff-noise-rules' and
`code-review-toggle-focus-mode'.")
(put 'code-review-focus-mode 'permanent-local t)

(defvar-local code-review-section--file-classifications nil
  "Hash table path -> plist (:tag :collapse :hide) for the diff.
Recomputed by `code-review--diff--classify-diff' on every render.
For internal usage only.")

(defcustom code-review-fold-header-sections t
  "When non-nil, low-signal top sections start folded.
\"Commits\" (with their CI checks), \"Description\", \"Your
Review Feedback\" and \"Conversation\" are collapsed to their
headings, so the diff is the first thing you see when the buffer
opens.  Press TAB on any of them to expand."
  :type 'boolean
  :group 'code-review)

(defcustom code-review-collapse-bot-comments t
  "When non-nil, comments authored by bots start folded.
AI-bot review comments (and bot-to-bot chatter in particular)
are noise for the human reviewer, so each is collapsed to its
one-line \"@author - date\" heading.  Threads where a human
replied stay fully expanded, and TAB unfolds any of them."
  :type 'boolean
  :group 'code-review)

(defcustom code-review-bot-author-regexp
  "\\`\\(github-actions\\|devin\\|cursor\\|copilot\\|gemini\\|codeium\\|coderabbit\\|codspeed\\|greptile\\|codecov\\|claassistant\\|renovate\\|dependabot\\|imgbot\\|semantic-release\\|all-contributors\\)\\|\\[bot\\]\\'"
  "Regexp matching comment authors considered bots.
Matched case-insensitively against the login, e.g.
\"devin-ai-integration\" or \"cursor[bot]\".  Extend this list
when a new automated reviewer shows up in your PRs.
See `code-review-collapse-bot-comments'."
  :type 'regexp
  :group 'code-review)

(defcustom code-review-comment-fringe-markers t
  "When non-nil, diff lines carrying a review thread get a fringe marker.
The marker (violet chevron in the left fringe) flags exactly which
diff lines have a conversation attached, including threads folded
away by `code-review-collapse-bot-comments' or collapsed hunks.
Mouse-1 on the marked line jumps to the thread.
Re-laid automatically on every render; turn off if the fringe
noise bothers you."
  :type 'boolean
  :group 'code-review)

(defcustom code-review-buffer-name "*Code Review*"
  "Fallback name of the code review main buffer.
Buffers are normally named after the reviewed PR
\(see `code-review-pr-buffer-name') so that several reviews can
stay open at once."
  :group 'code-review
  :type 'string)

(defvar-local code-review-review-buffer-pr-id nil
  "Database id of the pull request displayed in this review buffer.")
(put 'code-review-review-buffer-pr-id 'permanent-local t)

(defvar-local code-review-comment-review-buffer nil
  "Review buffer a comment buffer was opened for.")
(put 'code-review-comment-review-buffer 'permanent-local t)

(defun code-review-local--buffer-name-hint (args)
  "Compact buffer-name hint for a LOCAL review's diff ARGS.
Empty for the classic args (HEAD, --cached) so existing local
review buffer names stay unchanged; \"@ REV\" for a commit review
(7-char short form when REV is a full sha), \"@ A..B\" for a range
review (each full-sha side shortened, the first-parent \"^\"
dropped: \"OLD^..NEW\" shows as \"OLD..NEW\"), \"@ ARGS\"
otherwise."
  (cond
   ((or (not args)
        (member args '("HEAD" "--cached")))
    "")
   ;; REV^..REV / REV^!: the commit-review args, same REV both sides
   ((string-match "\\`\\(.+\\)\\^\\.\\.\\1\\'" args)
    (format " @ %s"
            (substring args (match-beginning 1)
                       (min (match-end 1) (+ (match-beginning 1) 7)))))
   ((string-match "\\`\\(.+\\)\\^!\\'" args)
    (format " @ %s"
            (substring args (match-beginning 1)
                       (min (match-end 1) (+ (match-beginning 1) 7)))))
   ;; a range: OLD^..NEW (log-region review) or any other A..B.
   ;; Bind both sides BEFORE the side-shrinking matches: the global
   ;; match data is clobbered by any nested string-match.
   ((string-match "\\`\\(.+\\)\\.\\.\\(.+\\)\\'" args)
    (let* ((a (match-string 1 args))
           (b (match-string 2 args))
           (shrink (lambda (side)
                     (if (string-match
                          "\\`\\([0-9a-f]\\{7\\}\\)[0-9a-f]*\\'" side)
                         (match-string 1 side)
                       side))))
      (when (string-suffix-p "^" a)
        (setq a (substring a 0 -1)))
      (format " @ %s..%s" (funcall shrink a) (funcall shrink b))))
   (t (format " @ %s" args))))

(defun code-review-pr-buffer-name (&optional pr)
  "Return the review buffer name for PR (defaults to the DB current pullreq).
Falls back to `code-review-buffer-name' when the PR cannot be
determined."
  (let ((pr (or pr (ignore-errors (code-review-db-get-pullreq)))))
    (if (and pr
             (slot-boundp pr 'owner)
             (slot-boundp pr 'repo)
             (slot-boundp pr 'number))
        (if (equal (oref pr state) "LOCAL")
            (format "*Code Review: local: %s%s*"
                    (replace-regexp-in-string "%2F" "/" (oref pr repo))
                    (code-review-local--buffer-name-hint
                     (and (slot-boundp pr 'base-ref-name)
                          (oref pr base-ref-name))))
          (format "*Code Review: %s/%s#%s*"
                  (oref pr owner)
                  (replace-regexp-in-string "%2F" "/" (oref pr repo))
                  (oref pr number)))
      code-review-buffer-name)))

(defun code-review-review-buffer ()
  "Return the review buffer for the PR being acted on, or nil.
Prefer the current buffer when it already is a review buffer, then
the review buffer recorded in a comment buffer, then the buffer
named after the DB's current pullreq."
  (cond ((and (bound-and-true-p code-review-review-buffer-pr-id)
              (buffer-live-p (current-buffer)))
         (current-buffer))
        ((and (bound-and-true-p code-review-comment-review-buffer)
              (buffer-live-p code-review-comment-review-buffer))
         code-review-comment-review-buffer)
        (t (get-buffer (code-review-pr-buffer-name)))))

(defun code-review--sync-db-pullreq ()
  "Make the DB current pullreq match the PR shown in this buffer."
  (when (and code-review-review-buffer-pr-id
             (not (equal code-review-db--pullreq-id
                         code-review-review-buffer-pr-id)))
    (setq code-review-db--pullreq-id code-review-review-buffer-pr-id)))

(defcustom code-review-commit-buffer-name "*Code Review Commit*"
  "Name of the code review commit buffer."
  :group 'code-review
  :type 'string)

(defcustom code-review-new-buffer-window-strategy
  #'switch-to-buffer-other-window
  "Function used after create a new Code Review buffer."
  :group 'code-review
  :type 'function)

(defun code-review--propertize-keyword (str)
  "Add property face to STR."
  (propertize str 'face
              (cond
               ((member str '("MERGED" "SUCCESS" "COMPLETED" "APPROVED" "REJECTED"))
                'code-review-success-state-face)
               ((member str '("FAILURE" "TIMED_OUT" "ERROR" "CHANGES_REQUESTED" "CLOSED" "CONFLICTING"))
                'code-review-error-state-face)
               ((member str '("RESOLVED" "OUTDATED"))
                'code-review-info-state-face)
               ((member str '("PENDING"))
                'code-review-pending-state-face)
               (t
                'code-review-state-face))))

;; fix unbound symbols
(defvar magit-root-section)
(defvar code-review-comment-commit-buffer?)
(defvar code-review-comment-cursor-pos)


(declare-function code-review-promote-comment-to-new-issue "code-review")
(declare-function code-review-utils--visit-binary-file-at-remote "code-review-utils")
(declare-function code-review-utils--visit-binary-file-at-point "code-review-utils")
(declare-function code-review-toggle-resolved "code-review-interfaces"
  (obj thread-id resolve? callback))

(defvar code-review-section-full-refresh? nil
  "Indicate if we want to perform a complete restart.
For internal usage only.")

(defvar code-review-section-grouped-comments nil
  "Hold grouped comments to avoid computation on every hunk line.
For internal usage only.")

(defvar code-review-section-hold-written-comment-ids nil
  "List to hold written comments ids.
For internal usage only.")

(defvar code-review-section-hold-written-comment-count nil
  "List of number of lines of comments written in the buffer.
For internal usage only.")

(defvar code-review-section--display-all-comments t
  "Variable to define if we should display all comments or not.
For internal usage only.")

(defvar code-review-section--display-top-level-comments t
  "Variable to define if we should display top level comments or not.
For internal usage only.")

(defvar code-review-section--display-diff-comments t
  "Variable to define if we should display diff comments or not.
For internal usage only.")

;; utility functions

;; Phase 11b: code-review OWNS the diff wash.  Up to this point the
;; render went through magit-diff internals (`magit-diff-wash-diff',
;; `magit-diff-insert-file-section', `magit-diff-wash-hunk') via
;; :override advices, whose private contracts changed repeatedly
;; across magit 4.x (paint helpers renamed/removed, `:washer' became
;; a lazy-body inserter, return values silently drive the wash loop).
;; The wash primitives below read plain unified-diff text, whose
;; format git has kept stable for decades.  Only magit-SECTION is
;; used (the `magit-insert-section' macro, section classes,
;; visibility), which is the stable part of magit.

(defun code-review-wash--delete-line ()
  "Delete the current line, including its newline."
  (delete-region (line-beginning-position)
                 (min (point-max) (1+ (line-end-position)))))

(defun code-review-wash--paint-hunk (section)
  "Paint the body of hunk SECTION with the diff faces.
Our own base pass: the first character of each line selects the
face (`magit-diff-added', `magit-diff-removed' or
`magit-diff-context'), which guarantees red/green hunks on every
magit version.  When the stable `magit-section-paint' generic is
available, call it on top for the extras (whitespace and
refinement details)."
  (save-excursion
    (goto-char (oref section start))
    (forward-line)                       ; skip the hunk heading
    (let ((end (oref section end)))
      (while (< (point) end)
        (put-text-property (point) (line-end-position)
                           'font-lock-face
                           (cond ((eq (char-after) ?+) 'magit-diff-added)
                                 ((eq (char-after) ?-) 'magit-diff-removed)
                                 (t 'magit-diff-context)))
        (forward-line))))
  (when (fboundp 'magit-section-paint)
    (save-excursion
      (goto-char (oref section start))
      (magit-section-paint section nil))))

(defun code-review--html-written-loc (body &optional indent)
  "Compute how many lines the HTML BODY will have in the buffer.
INDENT is an optional."
  (let ((shr-indentation (* (or indent 0) (shr-string-pixel-width "-")))
        (image-scaling-factor code-review-section-image-scaling)
        (shr-width code-review-fill-column)
        start
        dom)
    (with-temp-buffer
      (insert body)
      (setq dom (libxml-parse-html-region (point-min) (point-max))))
    (-> (with-temp-buffer
          (setq start (point))
          (insert " ")
          (narrow-to-region start (1+ start))
          (goto-char start)
          (shr-insert-document dom)
          ;; delete the inserted " "
          (delete-char 1)
          (buffer-substring-no-properties (point-min) (point-max)))
        (split-string "\n")
        (length)
        (- 1))))

(defun code-review--dom-string (dom)
  "Return string of DOM."
  (mapconcat (lambda (sub)
               (if (stringp sub)
                   sub
                 (code-review--dom-string sub)))
             (dom-children dom) ""))

(defun code-review--shr-tag-div (dom)
  "Rendering div tag as DOM in shr, with special handle for suggested-changes."
  (if (not (string-match-p ".*suggested-changes.*" (or (dom-attr dom 'class) "")))
      (shr-tag-div dom)
    (let ((tbody (dom-by-tag dom 'tbody)))
      (let ((shr-current-font 'code-review-info-state-face))
        (shr-insert "* Suggested change:")
        (insert "\n"))
      (dolist (tr (dom-non-text-children tbody))
        (dolist (td (dom-non-text-children tr))
          (let ((classes (split-string (or (dom-attr td 'class) ""))))
            (cond
             ((member "blob-num" classes) t)
             ((member "blob-code-deletion" classes)
              (let ((shr-current-font 'diff-indicator-removed))
                (shr-insert "-"))
              (insert (propertize (concat (code-review--dom-string td)
                                          "\n")
                                  'face 'diff-removed)))
             ((member "blob-code-addition" classes)
              (let ((shr-current-font 'diff-indicator-added))
                (shr-insert "+"))
              (insert (propertize (concat (code-review--dom-string td)
                                          "\n")
                                  'face 'diff-added)))
             (t
              (shr-generic td)))))))))

(defun code-review--insert-html (body &optional indent)
  "Insert html content BODY.
INDENT is an optional number, if provided,
INDENT count of spaces are added at the start of every line."
  (let ((shr-indentation (* (or indent 0) (shr-string-pixel-width "-")))
        (image-scaling-factor code-review-section-image-scaling)
        (shr-external-rendering-functions '((div . code-review--shr-tag-div)))
        (shr-width code-review-fill-column)
        (start (point))
        end
        dom)
    (with-temp-buffer
      (insert body)
      (setq dom (libxml-parse-html-region (point-min) (point-max))))
    ;; narrow the buffer and insert dom. otherwise there would be an extra new line at start
    (save-restriction
      (insert " ")
      (narrow-to-region start (1+ start))
      (goto-char start)
      (shr-insert-document dom)
      ;; delete the inserted " "
      (delete-char 1)
      (setq end (point)))
    (when (> shr-indentation 0)
      ;; shr-indentation does not work for images and code block
      ;; let's fix it: prepend space for any lines that does not starts with a space
      ;; (but we still need to use shr-indentation because otherwise the line will be too long)
      (save-excursion
        (goto-char start)
        (while (< (point) end)
          (unless (or (looking-at-p "\n")
                      (eq 'space (car-safe (get-text-property (point) 'display))))
            (beginning-of-line)
            (insert (propertize " " 'display `(space :width (,shr-indentation)))))
          (forward-line))))))











(defun code-review--diff--file-has-comments-p (path)
  "Non-nil when PATH carries review comments."
  (let ((res nil)
        (groups code-review-section-grouped-comments))
    (while (and groups (not res))
      (let ((objs (cdr (pop groups))))
        (while (and objs (not res))
          (let ((c (pop objs)))
            (when (and (slot-boundp c 'path)
                       (string-equal (oref c path) path))
              (setq res t))))))
    res))




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
          (insert (propertize "No one — Assign yourself"
                              'font-lock-face 'code-review-dimmed
                              'mouse-face 'code-review-hover-face
                              'help-echo "Set new assignee"
                              'keymap 'code-review-assignees-section-map))
        (progn
          (insert (propertize "Set new assignee"
                              'font-lock-face 'code-review-dimmed
                              'mouse-face 'code-review-hover-face
                              'help-echo "Set new assignee"
                              'keymap 'code-review-assignees-section-map))
          (insert ?\n)
          (dolist (assignee assignee-names)
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

(defun code-review-section-insert-suggested-reviewers ()
  "Insert the suggested reviewers."
  (let ((infos (code-review-db--pullreq-raw-infos)))
    (let-alist infos
      (let* ((reviewers-group (code-review-utils--fmt-reviewers infos))
             (reviewers (->> .suggestedReviewers
                             (-map
                              (lambda (r)
                                (a-get-in r (list 'reviewer 'login))))
                             (-filter
                              (lambda (r)
                                (let* ((res nil))
                                  (maphash
                                   (lambda (_status users)
                                     (setq res (append res
                                                       (-map
                                                        (lambda (it)
                                                          (a-get it 'login))
                                                        users))))
                                   reviewers-group)
                                  (and (not (equal r nil))
                                       (not (-contains-p res r))))))))
             (suggested-reviewers (if (not reviewers)
                                      (propertize "No suggestions" 'font-lock-face 'magit-dimmed)
                                    reviewers)))
        (magit-insert-section (code-review-suggested-reviewers-section suggested-reviewers)
          (insert "Suggested-Reviewers:")
          (if (not reviewers)
              (insert " " suggested-reviewers)
            (dolist (sr suggested-reviewers)
              (insert ?\n)
              (insert (propertize "Request Review"
                                  'face 'code-review-request-review-face
                                  'mouse-face 'code-review-hover-face
                                  'help-echo "Request review from reviewe"
                                  'keymap 'code-review-suggested-reviewers-section-map))
              (insert " - ")
              (insert (propertize (concat "@" sr) 'face 'code-review-author-face))))
          (insert ?\n))))))

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
              (code-review-section--hide-if-hidden
               (magit-insert-section (code-review-commit-section obj)
                                     (and (code-review-github-repo-p pr)
                                          .commit.statusCheckRollup.contexts.nodes)
                                    (if (and (code-review-github-repo-p pr) .commit.statusCheckRollup.contexts.nodes)
                                        (progn
                                          (insert (format "%s%s %s "
                                                          (propertize (format "%-6s " (oref obj sha)) 'font-lock-face 'magit-hash)
                                                          (car (split-string (oref obj msg) "\n"))
                                                          (if (string-equal .commit.statusCheckRollup.state "SUCCESS")
                                                              ":white_check_mark:"
                                                            ":x:")))
                                          (insert
                                           (propertize "Expand for Details:" 'font-lock-face 'code-review-checker-detail-face))
                                          (magit-insert-heading)
                                          (when (> (length (split-string (oref obj msg) "\n")) 1)
                                            (insert (oref obj msg))
                                            (insert "\n"))
                                          (code-review-section--hide-if-hidden
                                           (magit-insert-section (code-review-commit-checks-section nil t)
                                             (insert
                                              (propertize
                                               (format "  CI Checks (%s)"
                                                       (length .commit.statusCheckRollup.contexts.nodes))
                                               'font-lock-face 'code-review-checker-name-face))
                                             (magit-insert-heading)
                                          (dolist (check .commit.statusCheckRollup.contexts.nodes)
                                            (let-alist check
                                              (let ((obj (code-review-commit-check-detail-section :check check :details (or .detailsUrl .targetUrl))))
                                                (magit-insert-section (code-review-commit-check-detail-section obj)
                                                  (if (string-equal .conclusion "SUCCESS")
                                                      (progn
                                                        (insert (propertize
                                                                 (format
                                                                  "%-7s %s" ""
                                                                  (if-let ((check-suite-name (or .checkSuite.workflowRun.workflow.name .checkSuite.app.name)))
                                                                      (format "%s / %s" check-suite-name .name)
                                                                    ;; for StatusContext actions
                                                                    .context))
                                                                 'font-lock-face 'code-review-checker-name-face))
                                                        (insert " - ")
                                                        (when .startedAt
                                                          (insert (propertize (format "%s  " (format "Successful in %s."
                                                                                                     (code-review-utils--elapsed-time .completedAt .startedAt)))
                                                                              'font-lock-face 'magit-dimmed)))
                                                        (insert (propertize ":white_check_mark: Details"
                                                                            'font-lock-face 'code-review-checker-detail-face
                                                                            'mouse-face 'code-review-hover-face
                                                                            'help-echo "Visit the page for details"
                                                                            'keymap 'code-review-commit-check-detail-section-map)))
                                                    (progn
                                                      (insert (propertize (format
                                                                           "%-7s %s" ""
                                                                           (if-let ((check-suite-name (or .checkSuite.workflowRun.workflow.name .checkSuite.app.name)))
                                                                               (format "%s / %s" check-suite-name .name)
                                                                             ;; for StatusContext actions
                                                                             .context))
                                                                          'font-lock-face 'code-review-checker-name-face))
                                                      (insert " - ")
                                                      (insert (propertize (format "%s  " (or .summary .description))
                                                                          'font-lock-face 'magit-dimmed))
                                                      (insert (propertize ":x: Details"
                                                                          'font-lock-face 'code-review-checker-detail-face
                                                                          'mouse-face 'code-review-hover-face
                                                                          'help-echo "Visit the page for details"
                                                                          'keymap 'code-review-commit-check-detail-section-map))))))
                                              (insert "\n"))))))
                                      (progn
                                        (insert (propertize (format "%-6s " (oref obj sha)) 'font-lock-face 'magit-hash))
                                        (insert (oref obj msg))
                                        (insert ?\n))))))))
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

(defclass code-review-analysis-section (magit-section)
  (()))

(defun code-review-section--insert-analysis-jump (worktree path line)
  "Insert a button jumping to PATH at LINE in WORKTREE."
  (insert-button (format "%s:%s" path line)
                 'face 'code-review-url-header-face
                 'follow-link t
                 'help-echo "Visit this location in the worktree"
                 'action (lambda (&rest _)
                           (find-file-other-window
                            (expand-file-name path worktree))
                           (goto-char (point-min))
                           (forward-line (1- line)))))

(defun code-review-section-insert-analysis ()
  "Insert the heuristic Analysis section (phase 5, 15).
Duplicate and dead code findings, and (phase 15) the Delicate
hunks jump list (risk: blast radius, line age and ownership,
complexity delta, dead definitions).  Toggle it with TAB like any
other section.  Nothing is inserted when the analysis found
nothing (or is disabled, or there is no worktree)."
  (let ((res (code-review-analysis-run)))
    (when res
      (let ((similar (plist-get res :similar))
            (dead (plist-get res :dead))
            (dangling (plist-get res :dangling))
            (worktree code-review-repo-worktree))
        (magit-insert-section (code-review-analysis-section)
          (insert (propertize "Analysis" 'font-lock-face 'magit-section-heading))
          (insert (propertize " (heuristic)" 'font-lock-face 'magit-dimmed))
          (magit-insert-heading)
          (pcase-dolist (`(,diff-path ,total ,repo-path ,covered ,lo ,hi)
                         similar)
            (insert "  ")
            (insert (propertize "similar: " 'font-lock-face 'magit-dimmed))
            (insert (format "%s: %s/%s added lines also in "
                            diff-path covered total))
            (insert-button (format "%s:%s-%s" repo-path lo hi)
                           'face 'code-review-url-header-face
                           'follow-link t
                           'help-echo "Visit the similar code in the worktree"
                           'action (lambda (&rest _)
                                     (find-file-other-window
                                      (expand-file-name repo-path worktree))
                                     (goto-char (point-min))
                                     (forward-line (1- lo))))
            (insert ?\n))
          (pcase-dolist (`(,name ,path ,line) dead)
            (insert "  ")
            (insert (propertize "possibly dead: " 'font-lock-face 'magit-dimmed))
            (insert (format "%s (added in %s:%s; no references found in the "
                            name path line))
            (insert "worktree)\n"))
          (pcase-dolist (`(,name ,path ,refs) dangling)
            (insert "  ")
            (insert (propertize "dangling: " 'font-lock-face 'magit-dimmed))
            (insert (format "%s (deleted in %s; still used at "
                            name path))
            (let ((first t))
              (pcase-dolist (`(,rpath ,rline ,_rtext)
                             (cl-subseq refs 0 (min (length refs) 3)))
                (unless first (insert ", "))
                (setq first nil)
                (code-review-section--insert-analysis-jump
                 worktree rpath rline)))
            (insert ")\n"))
          ;; phase 15: delicate hunks (top-K, hottest first).  The
          ;; buttons jump inside the review buffer: the section is
          ;; inserted there, so the button action runs there.
          (let ((entries (cl-remove-if
                          (lambda (e)
                            (< (plist-get e :score)
                               code-review-analysis-delicacy-threshold))
                          (or (plist-get res :hunks) nil))))
            (setq entries (cl-subseq
                           entries 0 (min (length entries)
                                          code-review-analysis-delicacy-top-k)))
            (dolist (e entries)
              (let* ((path (plist-get e :path))
                     (ranges (plist-get e :ranges))
                     (reasons (string-join
                               (code-review-analysis--hunk-reasons e)
                               "; ")))
                (insert "  ")
                (insert (propertize "delicate: "
                                    'font-lock-face 'magit-dimmed))
                (insert-button (format "%s %s" path ranges)
                               'face 'code-review-url-header-face
                               'follow-link t
                               'help-echo "Jump to this hunk in the review"
                               'action (lambda (&rest _)
                                         (code-review-section--goto-hunk-section
                                          path ranges)))
                (insert (format "  %.2f (%s)\n"
                                (plist-get e :score) reasons)))))
          (insert ?\n))))))

(defclass code-review-review-order-section (magit-section)
  (()))

(defun code-review-section--goto-file-section (path)
  "Move point to the review buffer's file section for PATH.
Reveal its ancestors and recenter; no-op when the diff carries no
such file."
  (let ((target nil))
    (magit-map-sections
     (lambda (sec)
       (when (and (null target)
                  (magit-file-section-p sec)
                  (slot-boundp sec 'value)
                  (stringp (oref sec value))
                  (equal (code-review-browse--strip-diff-prefix
                          (substring-no-properties (oref sec value)))
                         path))
         (setq target sec))))
    (when target
      (code-review-browse--reveal target)
      (magit-section-goto target)
      (recenter))))

(defun code-review-section--find-hunk-section (path ranges)
  "The hunk section for PATH RANGES (nil when the diff has none).
RANGES is the raw ranges text of the @@ header (the hunk key, see
`code-review-analysis--split-hunks')."
  (let ((target nil))
    (magit-map-sections
     (lambda (sec)
       (when (and (null target)
                  (eq (eieio-object-class sec) 'magit-hunk-section)
                  (slot-boundp sec 'value)
                  (let ((v (oref sec value)))
                    (and (equal (cdr (assq 'path v)) path)
                         (equal (cdr (assq 'ranges v)) ranges))))
         (setq target sec))))
    target))

(defun code-review-section--goto-hunk-section (path ranges)
  "Move point to the review buffer's hunk section for PATH RANGES.
RANGES is the raw ranges text of the @@ header (the hunk key, see
`code-review-analysis--split-hunks').  Reveals the hunk's
ancestors (a collapsed file does not hide its delicate hunks);
no-op when the buffer carries no such hunk."
  (let ((target (code-review-section--find-hunk-section path ranges)))
    (when target
      (code-review-browse--reveal target)
      (magit-section-goto target)
      ;; `recenter' acts on the SELECTED window: only recenter when
      ;; that window actually displays this buffer (batch/async
      ;; callers run elsewhere — a bare `get-buffer-window' guard
      ;; still errors, the phase 15 daemon verification caught this)
      (when (eq (window-buffer (selected-window)) (current-buffer))
        (recenter)))))

(defun code-review-section-insert-review-order ()
  "Insert the phase 14 Review order section (heuristic).
The PR's changed files ranked by repository heat: churn
percentile x complexity x knowledge factors, hottest first,
each with a one-line reason and a button jumping to the file
section in this buffer.  Nothing is inserted while the heat data
is unavailable (disabled, code-compass missing, or the history
harvest still running: the sentinel re-renders when it lands)."
  (when code-review-history--order
    (magit-insert-section (code-review-review-order-section)
      (insert (propertize "Review order"
                          'font-lock-face 'magit-section-heading))
      (insert (propertize " (heuristic)" 'font-lock-face 'magit-dimmed))
      (magit-insert-heading)
      (let ((n 0))
        (dolist (e code-review-history--order)
          (setq n (1+ n))
          (let ((path (plist-get e :path))
                (reason (plist-get e :reason)))
            (insert "  ")
            (insert (format "%2d. " n))
            (insert (propertize (format "[%s]" (plist-get e :bucket))
                                'font-lock-face 'magit-dimmed))
            (insert " ")
            (insert-button path
                           'face 'code-review-url-header-face
                           'follow-link t
                           'help-echo "Jump to this file in the review"
                           'action (lambda (&rest _)
                                     (code-review-section--goto-file-section
                                      path)))
            (unless (string-empty-p reason)
              (insert (propertize (concat "  " reason)
                                  'font-lock-face 'magit-dimmed)))
            (insert ?\n)))))))

;;; general comments - top level comments

(defclass code-review-comment-header-section (magit-section)
  (()))

(defclass code-review-comment-section (magit-section)
  ((keymap :initform 'code-review-comment-section-map)
   (author :initarg :author
           :type string)
   (msg    :initarg :msg
           :type string)
   (body   :initarg :body
           :initform nil
           :type (or null string)
           :documentation "Raw (markdown) body as written by the author.
Used when editing a submitted comment.")
   (id     :initarg :id)
   (reactions :initarg :reactions)
   (typename :initarg :typename)
   (face   :initform 'magit-log-author)))

(defvar code-review-comment-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "e") 'code-review-edit-remote-comment-at-point)
    (define-key map (kbd "C-c C-r") 'code-review-conversation-reaction-at-point)
    (define-key map (kbd "C-c C-i") 'code-review-promote-comment-at-point-to-new-issue)
    map)
  "Keymaps for comment section.")

(cl-defmethod code-review--insert-conversation-section ((github code-review-github-repo))
  "Function to insert conversation section for GITHUB PRs."
  (let-alist (oref github raw-infos)
    (let ((list-of-comments (->> .reviews.nodes
                                 (-filter
                                  (lambda (n)
                                    (not (string-empty-p (a-get n 'bodyHTML)))))
                                 (append .comments.nodes)
                                 (--sort
                                  (< (time-to-seconds (date-to-time (a-get it 'createdAt)))
                                     (time-to-seconds (date-to-time (a-get other 'createdAt))))))))
      (when (not list-of-comments)
        (insert (propertize "No conversation found." 'font-lock-face 'magit-dimmed))
        (insert ?\n))

      (dolist (c list-of-comments)
        (let* ((reactions (a-get-in c (list 'reactions 'nodes)))
               (reaction-objs (when reactions
                                (-map
                                 (lambda (r)
                                   (code-review-reaction-section
                                    :id (a-get r 'id)
                                    :content (a-get r 'content)))
                                 reactions)))
               (obj (code-review-comment-section
                     :author (a-get-in c (list 'author 'login))
                     :msg (a-get c 'bodyHTML)
                     :body (a-get c 'body)
                     :id (a-get c 'databaseId)
                     :typename (a-get c 'typename)
                     :reactions reaction-objs)))
          (magit-insert-section (code-review-comment-section obj)
            (insert (concat
                     (propertize (format "@%s" (oref obj author)) 'font-lock-face (oref obj face))
                     " - "
                     (propertize (code-review-utils--format-timestamp (a-get c 'createdAt)) 'face 'code-review-timestamp-face)))
            (magit-insert-heading)
            (insert ?\n)
            (code-review-insert-comment-lines obj)
            (when reactions
              (code-review-comment-insert-reactions
               reaction-objs
               "comment"
               (a-get c 'databaseId)))
            (insert ?\n))))
      (insert ?\n))))

(cl-defmethod code-review--insert-conversation-section ((gitlab code-review-gitlab-repo))
  "Function to insert conversation section for GITLAB PRs."
  (let-alist (oref gitlab raw-infos)
    (let ((thread-groups (-group-by
                          (lambda (it)
                            (a-get-in it (list 'discussion 'id)))
                          .comments.nodes)))
      (dolist (g (a-keys thread-groups))
        (magit-insert-section (code-review-comment-thread-section)
          (insert (propertize "New Thread" 'font-lock-face 'code-review-thread-face))
          (magit-insert-heading)
          (let ((thread-comments (alist-get g thread-groups nil nil 'equal)))
            (dolist (c thread-comments)
              (let* ((obj (code-review-comment-section
                           :author (a-get-in c (list 'author 'login))
                           :msg (a-get c 'bodyHTML)
                           :id (a-get c 'databaseId))))
                (magit-insert-section (code-review-comment-section obj)
                  (insert (concat
                           (propertize (format "@%s" (oref obj author)) 'font-lock-face (oref obj face))
                           " - "
                           (propertize (code-review-utils--format-timestamp (a-get c 'createdAt)) 'face 'code-review-timestamp-face)))
                  (magit-insert-heading)
                  (code-review-insert-comment-lines obj)
                  (insert ?\n))))))))))

(cl-defmethod code-review--insert-conversation-section ((bitbucket code-review-bitbucket-repo))
  (let-alist (oref bitbucket raw-infos)
    (let ((list-of-comments (--sort
                             (< (time-to-seconds (date-to-time (a-get it 'createdAt)))
                                (time-to-seconds (date-to-time (a-get other 'createdAt))))
                             .comments.nodes)))
      (dolist (c list-of-comments)
        (let* ((obj (code-review-comment-section
                     :author (a-get-in c (list 'author 'login))
                     :msg (a-get c 'bodyHTML)
                     :id (a-get c 'databaseId)
                     :typename (a-get c 'typename))))
          (magit-insert-section (code-review-comment-section obj)
            (insert (concat
                     (propertize (format "@%s" (oref obj author)) 'font-lock-face (oref obj face))
                     " - "
                     (propertize (code-review-utils--format-timestamp (a-get c 'createdAt)) 'face 'code-review-timestamp-face)))
            (magit-insert-heading)
            (insert ?\n)
            (code-review-insert-comment-lines obj)
            (insert ?\n)))))))

(defun code-review-section-insert-top-level-comments ()
  "Insert general comments for the PULL-REQUEST in the buffer."
  (when-let (pr (and (not (code-review-db-local-pr-p))
                     (code-review-db-get-pullreq)))
    (code-review-section--hide-if-hidden
     (magit-insert-section (code-review-comment-header-section
                           nil code-review-fold-header-sections)
       (insert (propertize "Conversation" 'font-lock-face 'magit-section-heading))
       (magit-insert-heading)
       (when code-review-section--display-top-level-comments
         (code-review--insert-conversation-section pr))))))

;; files report

(defclass code-review-files-report-section (magit-section)
  (()))

;; -

(defclass code-review-base-comment-section (magit-section)
  ((state      :initarg :state
               :type string)
   (author     :initarg :author
               :type string)
   (msg        :initarg :msg
               :type string)
   (body       :initarg :body
               :initform nil
               :type (or null string)
               :documentation "Raw (markdown) body as written by the author.
nil for local comments rendered before the data was available; the
raw body is what `e' (edit submitted comment) pre-fills the
comment buffer with.")
   (position   :initarg :position
               :initform nil
               :type (or null number))
   (side         :initarg :side
                 :documentation "GitHub side for comment: LEFT or RIGHT")
   (line         :initarg :line
                 :initform nil
                 :documentation "GitHub line number for side")
   (start-side   :initarg :start-side
                 :initform nil
                 :documentation "GitHub start_side for multi-line comments")
   (start-line   :initarg :start-line
                 :initform nil
                 :documentation "GitHub start_line for multi-line comments")
   (thread-id   :initarg :thread-id
                :initform nil
                :documentation "Forge review thread id (e.g. GraphQL node id) when known.")
   (resolved?   :initarg :resolved?
                :initform nil
                :type boolean
                :documentation "Whether the review thread is resolved.")
   (reactions  :initarg :reactions
               :type (or null
                         (satisfies
                          (lambda (it)
                            (-all-p #'code-review-reaction-section-p it)))))
   (path       :initarg :path
               :type string)
   (diffHunk   :initarg :diffHunk
               :type (or null string))
   (id         :initarg :id
               :documentation "ID that identifies the comment in the Forge.")
   (internalId :initarg :internalId)
   (amount-loc :initform nil)
   (outdated?  :initform nil
               :type boolean)
   (reply?     :initform nil
               :type boolean)
   (local?     :initform nil
               :type boolean)
   (createdAt  :initarg :createdAt)
   (updatedAt  :initarg :updatedAt)))

;; Comment classes are dual purpose: the same class instantiates the
;; prebuilt data objects AND the inserted magit sections (the section
;; wraps the data object in its `value' slot).  `magit-section-ident'
;; therefore calls this generic twice with the same class: first on
;; the section, then (via magit's fallback) on the data object.
;; Without these methods both calls return nil, every comment
;; section gets the SAME ident, and magit's visibility cache plus
;; old-tree matching treat all comments as one section: hiding one
;; bot comment then re-hides every comment on the next render, human
;; ones included.  The dispatch below covers both roles.
(cl-defmethod magit-section-ident-value ((obj code-review-base-comment-section))
  "Identify a comment by its forge id, falling back to author/msg.
When OBJ is the inserted section (its `value' wraps the data
object), delegate to that data object."
  (if (and (slot-boundp obj 'value)
           (eieio-object-p (oref obj value)))
      (magit-section-ident-value (oref obj value))
    (or (and (slot-boundp obj 'id) (oref obj id))
        (and (slot-boundp obj 'author) (oref obj author))
        (and (slot-boundp obj 'msg) (oref obj msg)))))

(cl-defmethod magit-section-ident-value ((obj code-review-comment-section))
  "Identify a conversation comment by id, falling back to author.
When OBJ is the inserted section (its `value' wraps the data
object), delegate to that data object."
  (if (and (slot-boundp obj 'value)
           (eieio-object-p (oref obj value)))
      (magit-section-ident-value (oref obj value))
    (or (and (slot-boundp obj 'id) (oref obj id))
        (and (slot-boundp obj 'author) (oref obj author))
        (and (slot-boundp obj 'msg) (oref obj msg)))))



(defclass code-review-code-comment-section (code-review-base-comment-section)
  ((keymap     :initform 'code-review-code-comment-section-map)
   (diffHunk   :initarg :diffHunk)
   (id         :initarg :id
               :documentation "ID that identifies the comment in the Forge.")
   (amount-loc :initform nil)
   (outdated?  :initform nil
               :type boolean)
   (reply?     :initform nil
               :type boolean)
   (local?     :initform nil
               :type boolean)))

(defclass code-review-local-comment-section (code-review-base-comment-section)
  ((keymap       :initform 'code-review-local-comment-section-map)
   (local?       :initform t)
   (reply?       :initform nil)
   (edit?        :initform nil)
   (send?        :initarg :send?)
   (outdated?    :initform nil)
   (heading-face :initform 'code-review-recent-comment-heading)
   (body-face    :initform nil)
   (diffHunk     :initform nil)
   (line-type    :initarg :line-type)
   (render-heading           :initform "Comment by YOU: " :allocation :class)
   (render-extra-newline?    :initform nil                :allocation :class)))

(defclass code-review-reply-comment-section (code-review-base-comment-section)
  ((keymap       :initform 'code-review-reply-comment-section-map)
   (reply?       :initform t)
   (local?       :initform t)
   (edit?        :initform nil)
   (outdated?    :initform nil)
   (heading-face :initform 'code-review-recent-comment-heading)
   (body-face    :initform nil)
   (render-heading           :initform "Reply by YOU: " :allocation :class)
   (render-extra-newline?    :initform t               :allocation :class)))

(defclass code-review-outdated-comment-section (code-review-base-comment-section)
  ((keymap       :initform 'code-review-outdated-comment-section-map)
   (local?       :initform t)
   (outdated?    :initform t)))

(defclass code-review-check-section (magit-section)
  ((details :initarg :details)))

(defclass code-review-binary-file-section (magit-section)
  ((keymap :initform 'code-review-binary-file-section-map)))

(defvar code-review-code-comment-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'code-review-comment-add-or-edit)
    (define-key map (kbd "e") 'code-review-edit-remote-comment-at-point)
    (define-key map (kbd "C-c C-r") 'code-review-code-comment-reaction-at-point)
    (define-key map (kbd "C-c C-n") 'code-review-promote-comment-at-point-to-new-issue)
    (define-key map (kbd "K") 'code-review-section-delete-comment-remote)
    map)
  "Keymaps for code-comment sections.")

(defvar code-review-local-comment-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'code-review-comment-add-or-edit)
    (define-key map (kbd "C-c C-k") 'code-review-section-delete-comment)
    (define-key map (kbd "K") 'code-review-section-delete-comment-remote)
    map)
  "Keymaps for local-comment sections.")

(defvar code-review-reply-comment-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'code-review-comment-add-or-edit)
    (define-key map (kbd "C-c C-k") 'code-review-section-delete-comment)
    (define-key map (kbd "K") 'code-review-section-delete-comment-remote)
    map)
  "Keymaps for reply-comment sections.")

(defvar code-review-outdated-comment-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'code-review-comment-add-or-edit)
    (define-key map (kbd "e") 'code-review-edit-remote-comment-at-point)
    (define-key map (kbd "K") 'code-review-section-delete-comment-remote)
    map)
  "Keymaps for outdated-comment sections.")

(defvar code-review-binary-file-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'code-review-utils--visit-binary-file-at-point)
    (define-key map [mouse-2] 'code-review-utils--visit-binary-file-at-point)
    (define-key map [follow-link] 'code-review-utils--visit-binary-file-at-point)
    (define-key map [mouse-3] 'code-review-utils--visit-binary-file-at-remote)
    (define-key map (kbd "C-c C-v") 'code-review-utils--visit-binary-file-at-remote)
    map)
  "Keymaps for binary files sections.")




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






(cl-defmethod code-review-insert-comment-lines ((obj code-review-comment-section))
  "Insert the comment lines given in the OBJ with colored background."
  (let* ((start (point))
         (infos (code-review-db--pullreq-raw-infos))
         (pr-author (and infos (a-get-in infos '(author login))))
         (author (oref obj author))
         (my-login (code-review-utils--git-get-user))
         (face (cond
                ((and my-login author (string= my-login author)) 'code-review-comment-self-bg)
                ((and pr-author author (string= pr-author author)) 'code-review-comment-author-bg)
                (t 'code-review-comment-other-bg))))
    (code-review--insert-html (oref obj msg) (* 3 code-review-section-indent-width))
    (let ((ov (make-overlay start (point))))
      (overlay-put ov 'face face)
      (overlay-put ov 'priority 100))))








(cl-defgeneric code-review-comment-insert-lines (obj)
  "Insert comment lines in the code section based on section type denoted by OBJ.")

(cl-defmethod code-review-comment-insert-lines ((obj code-review-local-comment-section))
  "Insert local comment lines present in the OBJ."
  (code-review-comment--insert-you-lines obj 'code-review-local-comment-section))

(cl-defmethod code-review-comment-insert-lines ((obj code-review-reply-comment-section))
  "Insert reply comment lines present in the OBJ."
  (code-review-comment--insert-you-lines obj 'code-review-reply-comment-section))

(defun code-review-comment--insert-you-lines (obj type)
  "Insert the local or reply comment OBJ as a section of TYPE.
The heading text and the trailing blank line come from the class
slots `render-heading' and `render-extra-newline?'."
  (magit-insert-section ((eval type) obj)
    (let ((heading (oref obj render-heading)))
      (add-face-text-property 0 (length heading) (oref obj heading-face) t heading)
      (magit-insert-heading heading))
    (magit-insert-section ((eval type) obj)
      (let ((start (point)))
        (dolist (l (code-review-utils--split-comment
                    (code-review-utils--wrap-text
                     (oref obj msg)
                     code-review-fill-column)))
          (insert l)
          (insert ?\n))
        (when (oref obj render-extra-newline?)
          (insert ?\n))
        (let ((ov (make-overlay start (point))))
          (overlay-put ov 'face 'code-review-comment-self-bg)
          (overlay-put ov 'priority 1))))))


(cl-defmethod code-review-comment-insert-lines (obj)
  "Default insert comment lines in the OBJ."
  (magit-insert-section (code-review-code-comment-section obj)
    (let* ((infos (code-review-db--pullreq-raw-infos))
           (pr-author (and infos (a-get-in infos '(author login))))
           (author (oref obj author))
           (my-login (code-review-utils--git-get-user))
           (bgface (cond
                    ((and my-login author (string= my-login author)) 'code-review-comment-self-bg)
                    ((and pr-author author (string= pr-author author)) 'code-review-comment-author-bg)
                    (t 'code-review-comment-other-bg)))
           (heading (concat
                     (propertize "Reviewed by " 'face 'magit-section-heading)
                     (propertize (concat "@" (oref obj author)) 'face 'code-review-author-face)
                     " - "
                     (code-review--propertize-keyword (oref obj state))
                     " - "
                     (propertize (code-review-utils--format-timestamp (oref obj createdAt)) 'face 'code-review-timestamp-face)
                     (when (and (slot-boundp obj 'resolved?) (oref obj resolved?))
                       (concat " - " (code-review--propertize-keyword "RESOLVED"))))))
      (add-face-text-property 0 (length heading) 'code-review-recent-comment-heading t heading)
      (magit-insert-heading heading)
      (save-excursion
        (forward-line 0)
        (let ((bol (point))
              (eol (line-end-position)))
          (let ((ov (make-overlay bol eol)))
            (overlay-put ov 'face bgface)
            (overlay-put ov 'priority 1))))
      (magit-insert-section (code-review-code-comment-section obj)
        (let* ((start (point))
               (infos (code-review-db--pullreq-raw-infos))
               (pr-author (and infos (a-get-in infos '(author login))))
               (author (oref obj author))
               (my-login (code-review-utils--git-get-user))
               (face (cond
                      ((and my-login author (string= my-login author)) 'code-review-comment-self-bg)
                      ((and pr-author author (string= pr-author author)) 'code-review-comment-author-bg)
                      (t 'code-review-comment-other-bg))))
          (code-review--insert-html
           (oref obj msg)
           (* 3 code-review-section-indent-width))
          (when-let (reactions-obj (oref obj reactions))
            (code-review-comment-insert-reactions
             reactions-obj
             "code-comment"
             (oref obj id)))
          (let ((ov (make-overlay start (point))))
            (overlay-put ov 'face face)
            (overlay-put ov 'priority 100)))))))

(defun code-review-section--hide-if-hidden (section)
  "Fold the body of SECTION when it was created hidden.
Magit computes the initial visibility of a section when it is
created, but the actual folding only happens while a buffer is
refreshed via `magit-refresh-buffer'.  Code Review renders its
buffers directly, so sections that should start collapsed
(outdated comments, commit CI details) must be folded here
explicitly."
  (when (and (oref section hidden)
             (not (eq section magit-root-section)))
    (magit-section-hide section)))

(defun code-review-section-insert-outdated-comment (comments amount-loc)
  "Insert outdated COMMENTS in the buffer of PULLREQ-ID considering AMOUNT-LOC.
Safeguards against non-outdated/local comments accidentally passed in."
  ;; Only consider true outdated diff comments that have a diff hunk.
  (let* ((outdated-comments
          (-filter (lambda (el)
                     (and (ignore-errors (oref el outdated?))
                          (oref el outdated?)
                          (ignore-errors (slot-exists-p el 'diffHunk))
                          (oref el diffHunk)))
                   comments)))
    (when outdated-comments
      ;;; hunk groups are necessary because we usually have multiple reviews about
      ;;; the same original position across different commits snapshots.
      ;;; as github UI we will add those hunks and its comments
      (let* ((hunk-groups (-group-by (lambda (el) (oref el diffHunk)) outdated-comments))
             (hunks (a-keys hunk-groups))
             (amount-loc-internal amount-loc))
        (dolist (hunk hunks)
          (when (not hunk)
            (code-review-utils--log
             "code-review-section-insert-outdated-comment"
             (format "Every outdated comment must have a hunk! Error found for %S"
                     (prin1-to-string hunk)))
            (message "Hunk empty found. A empty string will be used instead. Report this bug please."))
          (let* ((safe-hunk (or hunk ""))
                 (diff-hunk-lines (split-string safe-hunk "\n"))
                 (amount-new-loc (+ 1 (length diff-hunk-lines)))
                 (first-hunk-commit (-first-item (alist-get safe-hunk hunk-groups nil nil 'equal)))
                 (metadata1 `((comment . ,first-hunk-commit)
                              (amount-loc ., (+ amount-loc-internal amount-new-loc)))))

            (setq amount-loc-internal (+ amount-loc-internal amount-new-loc))

            (setq code-review-section-hold-written-comment-count
                  (code-review-utils--comment-update-written-count
                   code-review-section-hold-written-comment-count
                   (oref first-hunk-commit path)
                   amount-new-loc))

            (code-review-section--hide-if-hidden
             (magit-insert-section (code-review-outdated-hunk-section metadata1 t)
                                  (let ((heading (format "Reviewed - [OUTDATED]")))
                                    (add-face-text-property 0 (length heading)
                                                            'code-review-outdated-comment-heading
                                                            t heading)
                                    (magit-insert-heading heading)
                                    (magit-insert-section ()
                                      (save-excursion
                                        (insert safe-hunk))
                                      (code-review-wash-hunk)
                                      (insert ?\n)

                                      (dolist (c (alist-get safe-hunk hunk-groups nil nil 'equal))
                                        (let* ((written-loc (code-review--html-written-loc
                                                             (oref c msg)
                                                             (* 3 code-review-section-indent-width)))
                                               (amount-new-loc-outdated-partial (+ 1 written-loc))
                                               (amount-new-loc-outdated (if (oref c reactions)
                                                                            (+ 2 amount-new-loc-outdated-partial)
                                                                          amount-new-loc-outdated-partial)))

                                          (setq amount-loc-internal (+ amount-loc-internal amount-new-loc-outdated))

                                          (setq code-review-section-hold-written-comment-count
                                                (code-review-utils--comment-update-written-count
                                                 code-review-section-hold-written-comment-count
                                                 (oref first-hunk-commit path)
                                                 amount-new-loc-outdated))
                                          (oset c amount-loc amount-loc-internal)
                                          (magit-insert-section (code-review-outdated-comment-section c)
                                            (let* ((infos (code-review-db--pullreq-raw-infos))
                                                   (pr-author (and infos (a-get-in infos '(author login))))
                                                   (author (oref c author))
                                                   (my-login (code-review-utils--git-get-user))
                                                   (bgface (cond
                                                            ((and my-login author (string= my-login author)) 'code-review-comment-self-bg)
                                                            ((and pr-author author (string= pr-author author)) 'code-review-comment-author-bg)
                                                            (t 'code-review-comment-other-bg)))
                                                   (heading (format "Reviewed by %s[%s]:"
                                                                    (oref c author)
                                                                    (oref c state))))
                                              (magit-insert-heading heading)
                                              (save-excursion
                                                (forward-line 0)
                                                (let ((bol (point))
                                                      (eol (line-end-position)))
                                                  (let ((ov (make-overlay bol eol)))
                                                    (overlay-put ov 'face bgface)
                                                    (overlay-put ov 'priority 100)))))
                                            (magit-insert-section (code-review-outdated-comment-section c)
                                              (let* ((start (point))
                                                     (infos (code-review-db--pullreq-raw-infos))
                                                     (pr-author (and infos (a-get-in infos '(author login))))
                                                     (author (oref c author))
                                                     (my-login (code-review-utils--git-get-user))
                                                     (face (cond
                                                            ((and my-login author (string= my-login author)) 'code-review-comment-self-bg)
                                                            ((and pr-author author (string= pr-author author)) 'code-review-comment-author-bg)
                                                            (t 'code-review-comment-other-bg))))
                                                (code-review--insert-html
                                                 (oref c msg)
                                                 (* 3 code-review-section-indent-width))
                                                (when-let (reactions-obj (oref c reactions))
                                                  (code-review-comment-insert-reactions
                                                   reactions-obj
                                                   "outdated-comment"
                                                   (oref c id)))
                                                (let ((ov (make-overlay start (point))))
                                                  (overlay-put ov 'face face)
                                                  (overlay-put ov 'priority 100))))
                                            (insert ?\n))))))))))))))

(defun code-review-section-insert-outdated-comment-missing (path-name missing-paths grouped-comments)
  "Write missing outdated comments in the end of the current path.
We need PATH-NAME, MISSING-PATHS, and GROUPED-COMMENTS to make this work."
  (dolist (path-pos missing-paths)
    (let* ((comment-written-pos
            (or (alist-get path-name code-review-section-hold-written-comment-count nil nil 'equal)
                0))
           (comments (code-review-utils--comment-get grouped-comments path-pos))
           (first (car comments)))
      (when comments
        (if (and (ignore-errors (oref first outdated?))
                 (oref first outdated?))
            (code-review-section-insert-outdated-comment comments comment-written-pos)
          (code-review-section-insert-comment comments comment-written-pos)))
      (push path-pos code-review-section-hold-written-comment-ids))))

(defun code-review-section-insert-comment (comments amount-loc)
  "Insert COMMENTS to PULLREQ-ID keep the AMOUNT-LOC of comments written.
A quite good assumption: every comment in an outdated hunk will be outdated."
  (if (and (oref (-first-item comments) outdated?)
           code-review-section--display-diff-comments)
      (code-review-section-insert-outdated-comment
       comments
       amount-loc)
    (let ((new-amount-loc amount-loc))
      (forward-line)
      (dolist (c comments)
        (when (or (and (code-review-local-comment-section-p c)
                       code-review-section--display-diff-comments)
                  (and (code-review-reply-comment-section-p c)
                       code-review-section--display-diff-comments)
                  (and (code-review-code-comment-section-p c)
                       code-review-section--display-diff-comments))
          (let* ((written-loc (code-review--html-written-loc
                               (oref c msg)
                               (* 3 code-review-section-indent-width)))
                 (amount-loc-incr-partial (+ 1 written-loc))
                 (amount-loc-incr (if (oref c reactions)
                                      (+ 2 amount-loc-incr-partial)
                                    amount-loc-incr-partial)))
            (setq new-amount-loc (+ new-amount-loc amount-loc-incr))
            (oset c amount-loc new-amount-loc)

            (setq code-review-section-hold-written-comment-count
                  (code-review-utils--comment-update-written-count
                   code-review-section-hold-written-comment-count
                   (oref c path)
                   amount-loc-incr))
            (code-review-comment-insert-lines c)))))))

(defun code-review-section--focus-summary ()
  "One-line summary of the files hidden by focus mode, or nil."
  (when code-review-section--file-classifications
    (let ((tags nil)
          (total 0))
      (maphash (lambda (_path info)
                 (when (plist-get info :hide)
                   (setq total (1+ total))
                   (let ((tag (or (plist-get info :tag) "noise")))
                     (let ((cell (assoc tag tags)))
                       (if cell
                           (setcdr cell (1+ (cdr cell)))
                         (push (cons tag 1) tags))))))
               code-review-section--file-classifications)
      (when (> total 0)
        (format "  --  focus: %d noise file%s hidden (%s)"
                total
                (if (= total 1) "" "s")
                (mapconcat (lambda (x)
                             (format "%d %s" (cdr x) (car x)))
                           tags
                           ", "))))))

(defun code-review-section-insert-files-changed ()
  (let ((files (a-get (code-review-db--pullreq-raw-infos) 'files)))
    (let-alist files
      (insert (propertize
               (concat "Files changed"
                       (when files
                         (format " (%s files; %s additions, %s deletions)"
                                 (length .nodes)
                                 (apply #'+ (mapcar (lambda (x) (alist-get 'additions x)) .nodes))
                                 (apply #'+ (mapcar (lambda (x) (alist-get 'deletions x)) .nodes))))
                       (when code-review-focus-mode
                         (or (code-review-section--focus-summary)
                             "  --  focus: no noise files to hide")))
               'font-lock-face
               'magit-section-heading)))
    (magit-insert-heading)))

(defun code-review-wash-diff ()
  "Wash one `diff --git' block of a raw unified diff at point.
Owned copy of magit-diff's file-block washer (phase 11b): it
parses the diff header (status, rename, binary) from plain diff
text and delegates to `code-review-wash-insert-file-section'.
The only inputs are the text at point and the `magit-wash-sequence'
loop contract: returns non-nil while a block was washed, nil when
nothing matches (which ends the loop)."
  (when (looking-at
         ;; The file names on this line may be ambiguous due to
         ;; whitespace; that is fine, the subsequent `---'/`+++'
         ;; headers are authoritative.  The backreference group
         ;; is optional (as in magit's own washer): renames put
         ;; two different paths on this line, so the group never
         ;; matches there and the file name comes from
         ;; `rename to'/`+++' instead.  Without the optional
         ;; wrapper the whole pattern never matches any standard
         ;; `a/X b/X' line, the wash stops at the first block and
         ;; the rest of the diff lands as raw, uncolored text.
         "^diff --\\(?:\\(?1:git\\) \\(?:\\(?2:.+?\\) \\2\\)?\\|\\(?3:cc\\|combined\\) \\(?4:.+\\)\\)")
    (let ((status (cond ((equal (match-string 1) "git") "modified")
                        ((match-string 3)              "resolved")
                        (t                            "unmerged")))
          (orig nil)
          (file (or (match-string 2) (match-string 4)))
          (header (list (buffer-substring-no-properties
                         (line-beginning-position) (1+ (line-end-position)))))
          (modes nil)
          (rename nil)
          (binary nil))
      (code-review-wash--delete-line)
      (while (not (or (eobp) (looking-at "^@@\\|^diff --\\|^Submodule")))
        (cond
          ((looking-at "old mode \\(?:[^\n]+\\)\nnew mode \\(?:[^\n]+\\)\n")
           (setq modes (match-string 0)))
          ((looking-at "deleted file .+\n")
           (setq status "deleted"))
          ((looking-at "new file .+\n")
           (setq status "new file"))
          ((looking-at "rename from \\(.+\\)\nrename to \\(.+\\)\n")
           (setq rename (match-string 0))
           (setq orig (match-string 1))
           (setq file (match-string 2))
           (setq status "renamed"))
          ((looking-at "copy from \\(.+\\)\ncopy to \\(.+\\)\n")
           (setq orig (match-string 1))
           (setq file (match-string 2))
           (setq status "copied"))
          ((looking-at "similarity index .+\n"))
          ((looking-at "dissimilarity index .+\n"))
          ((looking-at "index .+\n"))
          ((looking-at "--- \\(.+?\\)\t?\n")
           (unless (equal (match-string 1) "/dev/null")
             (setq orig (match-string 1))))
          ((looking-at "\\+\\+\\+ \\(.+?\\)\t?\n")
           (unless (equal (match-string 1) "/dev/null")
             (setq file (match-string 1))))
          ((looking-at "Binary files .+ and .+ differ\n")
           (setq binary t))
          ((looking-at "Binary files differ\n")
           (setq binary t))
          ;; TODO Use all combined diff extended headers.
          ((looking-at "mode .+\n"))
          (t (error "code-review-wash-diff: unknown extended header: %S"
                    (buffer-substring (point) (line-end-position)))))
        ;; `old mode' and `rename' headers are shown as special
        ;; hunks, not part of the section header text.
        (unless (or (string-prefix-p "old mode" (match-string 0))
                    (string-prefix-p "rename" (match-string 0)))
          (push (match-string 0) header))
        (delete-region (point) (match-end 0)))
      (when orig
        (setq orig (code-review-wash--decode-git-path orig)))
      (setq file (code-review-wash--decode-git-path file))
      (setq header (string-join (nreverse header)))
      (code-review-wash-insert-file-section
       file orig status modes rename header binary nil))))

(defun code-review-wash--decode-git-path (path)
  "Decode git-quoted PATH (\"\\226...\" style) to its raw form.
Delegates to magit's utility when available; identity otherwise."
  (if (fboundp 'magit-decode-git-path)
      (magit-decode-git-path path)
    path))

(defun code-review-wash-insert-file-section
    (file orig status modes rename header binary long-status)
  "Insert the file section for FILE and wash its hunks.
ORIG is the original file name (renames), STATUS the change type,
MODES a mode-change header, RENAME a rename header, HEADER the
raw extended headers, BINARY non-nil for binary files and
LONG-STATUS an alternative status text.  This is an owned copy of
magit-diff's file inserter (phase 11b); ours adds the
code-review specifics: file classification (tags, collapse),
comment bookkeeping and the trailing `missing comments' pass.
Returns the section: `magit-wash-sequence' keeps washing while
this is non-nil."

  ;;; --- beg -- code-review specific code.
  ;;; I need to set a reference point for the first hunk header
  ;;; so the positioning of comments is done correctly.
  ;;; Also apply file classification (tags, collapse) from
  ;;; `code-review--diff--classify-diff'.
  (let* ((raw-path-name (substring-no-properties file))
         (clean-path (if (string-prefix-p "b/" raw-path-name)
                         (replace-regexp-in-string "^b\\/" "" raw-path-name)
                       raw-path-name))
         (info (and code-review-section--file-classifications
                    (gethash clean-path
                             code-review-section--file-classifications)))
         (tag (plist-get info :tag))
         (collapse (and (plist-get info :collapse)
                        ;; never auto-collapse files that carry
                        ;; review comments: they'd hide discussion
                        (not (code-review--diff--file-has-comments-p
                              clean-path)))))
    (code-review-db--curr-path-update clean-path)
    ;;; --- end -- code-review specific code.
    (insert ?\n)
    ;; the HIDE flag above only sets the section's `hidden' slot;
    ;; code-review renders directly (no `magit-refresh-buffer'
    ;; pass), so collapse the body here as well.  Return the
    ;; section afterwards: `magit-wash-sequence' keeps washing
    ;; files only while this function returns non-nil, so the
    ;; collapse wrapper must not be the last form (a `when'
    ;; around a visible section evaluates to nil, which stopped
    ;; the wash after the first file and left the rest of the
    ;; diff as uncolored raw text).
    (let ((section (magit-insert-section section
      (file file (or (equal status "deleted")
                    (derived-mode-p 'magit-status-mode)
                    collapse))
      (insert (propertize (format "%-10s %s" status
                                  (if (or (not orig) (equal orig file))
                                      file
                                    (format "%s -> %s" orig file)))
                          'font-lock-face 'magit-diff-file-heading))
      (when tag
        (insert (propertize (format "  [%s]" tag)
                            'font-lock-face 'code-review-diff-tag-face)))
      (when long-status
        (insert (format " (%s)" long-status)))
      (magit-insert-heading)
    (unless (equal orig file)
      (oset section source orig))
    (oset section header header)
    (when modes
      (magit-insert-section (hunk '(chmod))
        (insert modes)
        (magit-insert-heading)))
    (when rename
      (magit-insert-section (hunk '(rename))
        (insert rename)
        (magit-insert-heading)))
    (when (string-match-p "Binary files.*" header)
      (magit-insert-section (code-review-binary-file-section file)
        (insert (propertize "Visit file"
                            'face 'code-review-request-review-face
                            'mouse-face 'code-review-hover-face
                            'help-echo "Visit the file in Dired buffer"
                            'keymap 'code-review-binary-file-section-map))
        (magit-insert-heading)))
    (magit-wash-sequence #'code-review-wash-hunk)
    ;; After washing all hunks for this file, insert any remaining
    ;; comments (e.g., local or outdated ones keyed by side/line)
    ;; that weren’t anchored to a concrete diff position.
    (let* ((raw-path-name (substring-no-properties file))
           (clean-path (if (string-prefix-p "b/" raw-path-name)
                           (replace-regexp-in-string "^b/" "" raw-path-name)
                         raw-path-name))
           (missing-paths (code-review-utils--missing-outdated-commments?
                           clean-path
                           code-review-section-hold-written-comment-ids
                           code-review-section-grouped-comments)))
      (when (and missing-paths code-review-section--display-all-comments)
        (code-review-section-insert-outdated-comment-missing
         clean-path missing-paths code-review-section-grouped-comments))))))
      (code-review-section--hide-if-hidden section)
      section)))

(defun code-review-wash-hunk ()
  "Wash the hunk at point, inserting PR comment sections inline.
Owned copy of magit-diff's hunk washer (phase 11b), interleaving
the grouped comments (`code-review-section-grouped-comments',
keyed by path and diff position or by side/line) into the hunk
body as it is washed.  Returns t when a hunk was washed, nil
otherwise, per the `magit-wash-sequence' contract."
  (when (looking-at "^@\\{2,\\} \\(.+?\\) @\\{2,\\}\\(?: \\(.*\\)\\)?")
    ;; Bind the match data BEFORE any database access below: the db
    ;; writes clobber the global match data (emacsql compiles
    ;; statements with string-matches on a cold cache, leaving
    ;; string-relative positions), which made the `match-string'
    ;; reads below return garbage or even signal args-out-of-range
    ;; on the first hunk of a fresh Emacs.  Bind first, use after.
    (let* ((heading    (match-string 0))
           (raw-ranges (match-string 1))
           (about      (match-string 2))
    ;;; --- beg -- code-review specific code.
    ;;; I need to set a reference point for the first hunk header
    ;;; so the positioning of comments is done correctly.
           (path (code-review-db--curr-path))
           (path-name (oref path name))
           (head-pos (oref path head-pos)))
      (when (not head-pos)
        (let ((adjusted-pos (+ (code-review--line-number-at-pos) 1)))
          (code-review-db--curr-path-head-pos-update path-name adjusted-pos)
          (setq head-pos adjusted-pos)
          (setq path-name path-name)))

      (when (not head-pos)
        (code-review-utils--log
         "code-review-wash-hunk"
         (format "Every diff is associated with a PATH (the file). Head pos nil for %S"
                 (prin1-to-string path)))
        (message "ERROR: Head position for path %s was not found.
Please Report this Bug" path-name))
    ;;; --- end -- code-review specific code.

      (let* ((ranges   (mapcar (lambda (str)
                                 (let ((range
                                        (mapcar #'string-to-number
                                                (split-string (substring str 1) ","))))
                                   ;; A single line is +1 rather than +1,1.
                                   (if (length= range 1)
                                       (nconc range (list 1))
                                     range)))
                               (split-string raw-ranges)))
             (combined (= (length ranges) 3))
             (value    (cons about ranges)))
        (magit-delete-line)
        ;; Paint the hunk (diff colors) as soon as it is washed:
        ;; `magit-insert-section' returns the section object, and by
        ;; then its `end' marker is set, which the painter needs.
        (let ((hunk-section
               (magit-insert-section
            ( hunk
              `((value . ,value) ;; TODO not sure if this has to diverge as well
                (path . ,path-name)
                ;; the hunk KEY: byte-identical to the delicacy
                ;; entry's :ranges (phase 15 badge/jump list; the
                ;; phase 13 read-tracking key too)
                (ranges . ,raw-ranges)
                (head-pos . ,head-pos))
              nil
              :combined combined
              :from-range (if combined (butlast ranges) (car ranges))
              :to-range (car (last ranges))
              :about about)
          (insert (propertize heading 'font-lock-face 'magit-diff-hunk-heading))
          ;; phase 15: the delicacy badge (cache hit: the Analysis
          ;; section runs before the diff wash in the sections hook)
          (let ((badge (code-review-analysis--hunk-badge-for
                        path-name raw-ranges)))
            (when badge
              (insert (propertize badge
                                  'font-lock-face
                                  'code-review-delicate-hunk-face))))
          (insert ?\n)
          (magit-insert-heading)
          ;; Keep track of old/new line numbers from the hunk header so we
          ;; can anchor local comments keyed by SIDE/LINE inline.
          (let* ((from-range (car ranges))
                 (to-range (car (last ranges)))
                 (old-line-current (and from-range (car from-range)))
                 (new-line-current (and to-range (car to-range))))
            (while (not (or (eobp) (looking-at "^[^-+\s\\]")))
              ;; --- code-review specific code: add code comments
              (let* ((line-text (buffer-substring-no-properties (line-beginning-position)
                                                                (line-end-position)))
                     (ch (if (> (length line-text) 0) (substring line-text 0 1) ""))
                     ;; Position-keyed comments (original API)
                     (diff-pos (+ 1 (- (code-review--line-number-at-pos)
                                       (or head-pos 0)
                                       (or (alist-get path-name code-review-section-hold-written-comment-count nil nil 'equal) 0))))
                     (path-pos (code-review-utils--comment-key path-name diff-pos))
                     (written-pos? (-contains-p code-review-section-hold-written-comment-ids path-pos))
                     (grouped-pos (code-review-utils--comment-get
                                   code-review-section-grouped-comments
                                   path-pos))
                     ;; Side/line-keyed comments (locals / GraphQL line
                     ;; API).  Context lines can anchor comments on
                     ;; either side, so try both keys.
                     (line-keys
                      (cond
                       ((string= ch " ")
                        (list (code-review-utils--comment-key-from-line path-name "RIGHT" new-line-current)
                              (code-review-utils--comment-key-from-line path-name "LEFT" old-line-current)))
                       ((string= ch "+")
                        (list (code-review-utils--comment-key-from-line path-name "RIGHT" new-line-current)))
                       ((string= ch "-")
                        (list (code-review-utils--comment-key-from-line path-name "LEFT" old-line-current)))
                       (t nil)))
                     (did-insert nil))
                ;; Insert position-keyed comments
                (when (and (not written-pos?) grouped-pos code-review-section--display-all-comments)
                  (push path-pos code-review-section-hold-written-comment-ids)
                  (let ((comment-written-pos (or (alist-get path-name code-review-section-hold-written-comment-count nil nil 'equal) 0)))
                    (code-review-section-insert-comment grouped-pos comment-written-pos))
                  (setq did-insert t))
                ;; Insert side/line-keyed comments
                (dolist (pos-line line-keys)
                  (let ((grouped-line (code-review-utils--comment-get
                                       code-review-section-grouped-comments pos-line))
                        (written-line? (-contains-p code-review-section-hold-written-comment-ids pos-line)))
                    (when (and grouped-line (not written-line?) code-review-section--display-all-comments)
                      (push pos-line code-review-section-hold-written-comment-ids)
                      (let ((comment-written-pos (or (alist-get path-name code-review-section-hold-written-comment-count nil nil 'equal) 0)))
                        (code-review-section-insert-comment grouped-line comment-written-pos))
                      (setq did-insert t))))
                ;; Advance line only when nothing was inserted (insertion moves point)
                (unless did-insert
                  (forward-line))
                ;; Update counters for old/new line numbers following this patch line
                (cond
                 ((string= ch " ")
                  (when old-line-current (setq old-line-current (1+ old-line-current)))
                  (when new-line-current (setq new-line-current (1+ new-line-current))))
                 ((string= ch "+")
                  (when new-line-current (setq new-line-current (1+ new-line-current))))
                 ((string= ch "-")
                  (when old-line-current (setq old-line-current (1+ old-line-current))))))))

          ;; Remaining comments for this file are handled once per file
          ;; in code-review-wash-insert-file-section.

        ;;; --- end -- code-review specific code.
          )))
          (code-review-wash--paint-hunk hunk-section)
          (code-review-hunkhighlight-hunk hunk-section path-name))))
    t))

;;; * build buffer

(defclass code-review--root-section (magit-section)
  ((body :initform nil)))

(defun code-review--trigger-hooks (buff-name &optional commit-focus? msg)
  "Trigger magit section hooks and draw BUFF-NAME.
Run code review commit buffer hook when COMMIT-FOCUS? is non-nil.
If you want to display a minibuffer MSG in the end."
  (progn
    (setq code-review-section-grouped-comments
          (code-review-utils-make-group
           (code-review-db--pullreq-raw-comments))
          code-review-section-hold-written-comment-count nil
          code-review-section-hold-written-comment-ids nil)

    (with-current-buffer (get-buffer-create buff-name)
      ;; local repository context: worktree checked out at PR head.
      ;; For a local diff review the repository itself is the worktree.
      (if (code-review-db-local-pr-p)
          (when (and code-review-repo-enable
                     (not code-review-repo-worktree))
            (setq code-review-repo-worktree
                  (or (let ((root (oref (code-review-db-get-pullreq) host)))
                        (and root (file-name-as-directory root)))
                       (magit-toplevel))))
        (when (and code-review-repo-enable
                   (or (not code-review-repo-worktree)
                       code-review-section-full-refresh?))
          (condition-case err
              (code-review-repo-setup (code-review-db-get-pullreq))
            (error (message "code-review: repo setup failed: %s"
                            (error-message-string err))))))
      (when code-review-repo-worktree
        (setq default-directory
              (file-name-as-directory code-review-repo-worktree)))
      (let* ((window (get-buffer-window buff-name))
             (ws (window-start window))
             (inhibit-read-only t)
             ;; before the render replaces it: t when this buffer
             ;; has never been rendered before
             (fresh-render? (not magit-root-section)))
        (save-excursion
          ;; phase 14 heat: compute (or load) before the wash; a
          ;; cold cache kicks the async harvest and stays nil here
          (code-review-history-prepare
           (code-review-db--pullreq-raw-diff)
           code-review-repo-worktree)
          (setq code-review-section--file-classifications
                (code-review--diff--classify-diff
                 (code-review-db--pullreq-raw-diff)))
          (erase-buffer)
          (insert (code-review--maybe-reorder-diff
                   (code-review-db--pullreq-raw-diff)
                   code-review-section--file-classifications))
          (insert ?\n))
        (magit-insert-section section (code-review--root-section)
                              (magit-insert-section (code-review)
                                (magit-run-section-hook (if commit-focus?
                                                            'code-review-sections-commit-hook
                                                          'code-review-sections-hook)))
                              (magit-insert-section (code-review-files-report-section)
                                (code-review-section-insert-files-changed)
                                (magit-insert-section (code-review-files-chnged)
                                  (save-restriction
                                    (narrow-to-region (point) (point-max))
                                    (run-hooks 'magit-diff-wash-diffs-hook)
                                    (magit-wash-sequence
                                     #'code-review-wash-diff)))))
        ;; fold bot-authored comment threads (AI chatter).  Only on
        ;; a fresh render: on re-render magit inherits each
        ;; section's previous visibility, and re-folding here would
        ;; clobber sections the user deliberately expanded.
        (when (and code-review-collapse-bot-comments
                   fresh-render?)
          (code-review--collapse-bot-comments magit-root-section))
        ;; fringe markers on diff lines carrying a thread: needed on
        ;; EVERY render (erasing the buffer kills the overlays), and
        ;; cheap enough (one section-tree walk) to always be worth it.
        (when code-review-comment-fringe-markers
          (code-review--mark-comment-lines))
        (if window
            (progn
              (pop-to-buffer buff-name)
              (set-window-start window ws))
          (progn
            (funcall code-review-new-buffer-window-strategy buff-name)
            (goto-char (point-min))))
        (code-review-mode)
        ;; per-PR review buffers: remember which PR this buffer
        ;; shows, and make commands issued here act on that PR.
        ;; Done after `code-review-mode', which kills local
        ;; variables and hooks.
        (setq code-review-review-buffer-pr-id code-review-db--pullreq-id)
        (add-hook 'post-command-hook #'code-review--sync-db-pullreq nil t)
        ;; sticky file name while reading deep inside a hunk
        (setq header-line-format nil)
        (add-hook 'post-command-hook #'code-review--update-header-line
                  nil t)
        (add-hook 'window-scroll-functions #'code-review--update-header-line
                  nil t)
        (when commit-focus?
          (code-review-commit-minor-mode 1))
        (code-review-section-insert-header-title)
        (when code-review-comment-cursor-pos
          (goto-char code-review-comment-cursor-pos))
        (when msg
          (message nil)
          (message msg))
        ;; Run post hook after everything is rendered and mode is active
        (run-hooks 'code-review-post-hook)))))

(cl-defmethod code-review--auth-token-set? ((_github code-review-github-repo) res)
  "Check if the RES has a message for auth token not set for GITHUB."
  (string-prefix-p "Required Github token" (-first-item (a-get res 'error))))

(cl-defmethod code-review--auth-token-set? ((_gitlab code-review-gitlab-repo) res)
  "Check if the RES has a message for auth token not set for GITLAB."
  (string-prefix-p "Required Gitlab token" (-first-item (a-get res 'error))))

(cl-defmethod code-review--auth-token-set? ((_bitbucket code-review-bitbucket-repo) res)
  "Check if the RES has a message for auth token not set for BITBUCKET."
  (string-prefix-p "Required Bitbucket token" (-first-item (a-get res 'error))))

(cl-defmethod code-review--auth-token-set? (obj res)
  "Default catch all unknown values passed to this function as OBJ and RES."
  (code-review-utils--log
   "code-review--auth-token-set?"
   (string-join (list
                 (prin1-to-string obj)
                 (prin1-to-string res))
                " <->"))
  (error "Unknown backend obj created.  Look at `code-review-log-file' and report the bug upstream"))

(cl-defmethod code-review--internal-build ((obj code-review-github-repo) progress res &optional buff-name msg)
  "Helper function to build process for GITHUB based on the fetched RES informing PROGRESS."
  ;; This runs in a timer: the db's current pullreq may have been
  ;; switched to another buffer's PR meanwhile, so re-assert it.
  (setq code-review-db--pullreq-id (oref obj id))
  (let* ((errors-complete-query (alist-get 'errors (-second-item res)))
         (raw-infos-complete (a-get-in (cdr (-second-item res)) (list 'repository 'pullRequest)))
         (raw-infos-fallback (a-get-in (cdr (-third-item res)) (list 'repository 'pullRequest)))
         (raw-infos
          (if (not raw-infos-complete)
              raw-infos-fallback
            raw-infos-complete)))

    (when errors-complete-query
      (code-review-utils--log "code-review--internal-build"
                              (format "Data returned by GraphQL API: \n %s" (prin1-to-string res)))
      (message "GraphQL Github data contains errors. See `code-review-log-file' for details."))

    ;; verify must have value!
    (let-alist raw-infos
      (when (not .headRefOid)
        (code-review-utils--log "code-review--internal-build"
                                "Commit SHA not returned by GraphQL Github API. See `code-review-log-file' for details")
        (code-review-utils--log "code-review--internal-build"
                                (format "Data returned by GraphQL API: \n %s" (prin1-to-string res)))
        (error "Missing required data")))

    ;; 1. save raw diff data
    (progress-reporter-update progress 3)
    (code-review-db--pullreq-raw-diff-update
     (code-review-utils--clean-diff-prefixes
      (a-get (-first-item res) 'message)))

    ;; 1.1 save raw info data e.g. data from GraphQL API
    (progress-reporter-update progress 4)
    (code-review-db--pullreq-raw-infos-update
     (code-review-github-fix-infos raw-infos))

    ;; 1.2 trigger renders
    (progress-reporter-update progress 5)
    (code-review--trigger-hooks buff-name nil msg)
    (progress-reporter-done progress)))

(cl-defmethod code-review--internal-build ((obj code-review-gitlab-repo) progress res &optional buff-name msg)
  "Helper function to build process for GITLAB based on the fetched RES informing PROGRESS."
  ;; This runs in a timer: the db's current pullreq may have been
  ;; switched to another buffer's PR meanwhile, so re-assert it.
  (setq code-review-db--pullreq-id (oref obj id))

  (when-let (err (a-get (-second-item res) 'errors))
    (code-review-utils--log
     "code-review--internal-build"
     (format "Data returned by GraphQL API: \n%s" (prin1-to-string err))))

  ;; 1. save raw diff data
  (progress-reporter-update progress 3)
  (code-review-db--pullreq-raw-diff-update
   (code-review-gitlab-fix-diff
    (a-get (-first-item res) 'changes)))

  ;; 1.1. compute position line numbers to diff line numbers
  (progress-reporter-update progress 4)
  (code-review-gitlab-pos-line-number->diff-line-number
   (a-get (-first-item res) 'changes))

  ;; 1.2. save raw info data e.g. data from GraphQL API
  (progress-reporter-update progress 5)
  (code-review-db--pullreq-raw-infos-update
   (code-review-gitlab-fix-infos
    (code-review-github-fix-infos
     (a-get-in (-second-item res) (list 'data 'repository 'pullRequest)))))

  ;; 1.3. trigger renders
  (progress-reporter-update progress 6)
  (code-review--trigger-hooks buff-name nil msg)
  (progress-reporter-done progress))

(cl-defmethod code-review--internal-build ((obj code-review-bitbucket-repo) progress res &optional buff-name msg)
  "Helper function to build process for BITBUCKET based on the fetched RES informing PROGRESS."
  ;; This runs in a timer: the db's current pullreq may have been
  ;; switched to another buffer's PR meanwhile, so re-assert it.
  (setq code-review-db--pullreq-id (oref obj id))
  (prin1 (format "RESULT:%s\n" (-second-item res)))
  (let* ((raw-infos (let-alist (-second-item res)
                      `((title . ,.title)
                        (author . ((login . ,.author.nickname)
                                   (url . ,(format "https://%s/%s"
                                                   code-review-bitbucket-base-url
                                                   .author.nickname))))
                        (number . ,.id)
                        (state . ,.state)
                        (bodyHTML . ,.rendered.description.html)
                        (headRef (target (oid . ,.source.commit.hash)))
                        (baseRefName . ,.destination.branch.name)
                        (headRefName . ,.source.branch.name)
                        (comments (nodes . ,.comments.nodes))
                        (commits . ,.commits)
                        (reviews (nodes . ,.reviews.nodes))))))

    ;; 1. save raw diff data
    (progress-reporter-update progress 3)
    (code-review-db--pullreq-raw-diff-update
     (code-review-utils--clean-diff-prefixes
      (a-get (-first-item res) 'message)))

    ;; 1.1. compute position line numbers to diff line numbers
    (progress-reporter-update progress 4)
    (code-review-bitbucket-pos-line-number->diff-line-number
     (a-get (-first-item res) 'message))

    ;; 1.2 save raw info data e.g. data from GraphQL API
    (progress-reporter-update progress 4)
    (code-review-db--pullreq-raw-infos-update
     (code-review-bitbucket--fix-diff-comments raw-infos))

    ;; 1.3 trigger renders
    (progress-reporter-update progress 5)
    (code-review--trigger-hooks buff-name nil msg)
    (progress-reporter-done progress)))

(defcustom code-review-log-raw-request-responses nil
  "Log the Raw request responses from your VC provider."
  :type 'string
  :group 'code-review)

(defun code-review--build-buffer (&optional buf-name commit-focus? msg)
  "Build BUF-NAME set COMMIT-FOCUS? mode to use commit list of hooks.
If you want to provide a MSG for the end of the process.
BUF-NAME defaults to the per-PR buffer name."
  (let ((buff-name (or buf-name (code-review-pr-buffer-name))))
    (if (not code-review-section-full-refresh?)
        (code-review--trigger-hooks buff-name commit-focus? msg)
      (let ((obj (code-review-db-get-pullreq))
            (progress (make-progress-reporter "Fetch diff PR..." 1 6)))
        (progress-reporter-update progress 1)
        (deferred:$
         (deferred:parallel
          (lambda () (code-review-diff-deferred obj))
          (lambda () (code-review-infos-deferred obj))
          (lambda () (code-review-infos-deferred obj t)))
         (deferred:nextc it
                         (lambda (x)
                           (when code-review-log-raw-request-responses
                             (code-review-utils--log
                              "code-review--build-buffer: [DIFF]"
                              (prin1-to-string (-first-item x)))
                             (code-review-utils--log
                              "code-review--build-buffer: [INFOS_main]"
                              (prin1-to-string (-second-item x)))
                             (code-review-utils--log
                              "code-review--build-buffer: [INFOS_fallback]"
                              (prin1-to-string (-third-item x))))

                           (progress-reporter-update progress 2)
                           (if (code-review--auth-token-set? obj x)
                               (progn
                                 (progress-reporter-done progress)
                                 (message "Required %s token. Look at the README for how to setup your Personal Access Token"
                                          (cond
                                           ((code-review-github-repo-p obj)
                                            "Github")
                                           ((code-review-gitlab-repo-p obj)
                                            "Gitlab")
                                           (t "Unknown"))))
                             (code-review--internal-build obj progress x buff-name msg))))
         (deferred:error it
                         (lambda (err)
                           (code-review-utils--log
                            "code-review--build-buffer"
                            (prin1-to-string err))
                           (if (and (sequencep err)
                                    (stringp (-second-item err))
                                    (string-prefix-p "BUG: Unknown extended header:" (-second-item err)))
                               (message "Your PR might have diffs too large. Currently not supported.")
                             (message "Got an error from your VC provider. Check `code-review-log-file'.")))))))))

;;; * commit buffer
;;; Rebuild commit buffer focusing on a single commit's diff.
(defun code-review-section--build-commit-buffer (buff-name)
  "Build commit buffer review given by BUFF-NAME.
Fetches the commit diff using the backend and renders the buffer
with commit-focused hooks and keybindings."
  (let* ((obj (code-review-db-get-pullreq))
         (progress (make-progress-reporter "Fetch commit diff..." 1 3)))
    (progress-reporter-update progress 1)
    (deferred:$
     (code-review-commit-diff-deferred obj)
     (deferred:nextc it
                     (lambda (res)
                       ;; Save raw diff data into DB and render using commit hooks
                       (progress-reporter-update progress 2)
                       (code-review-db--pullreq-raw-diff-update
                        (code-review-utils--clean-diff-prefixes
                         (a-get res 'message)))
                       (progress-reporter-update progress 3)
                       (code-review--trigger-hooks buff-name t)
                       (progress-reporter-done progress)))
     (deferred:error it
                     (lambda (err)
                       (code-review-utils--log
                        "code-review-section--build-commit-buffer"
                        (prin1-to-string err))
                       (message "Got an error while fetching commit diff. See `code-review-log-file'."))))))






;;; Patch line helpers (skip review UI lines inside hunks)

(defun code-review--section-in-review-subsection-p (&optional section)
  "Return non-nil when SECTION (or current section) is inside a review UI subsection.
Detects comment/reply/local/reactions/outdated comment sections which render
non-patch lines inside hunks."
  (let ((cur (or section (magit-current-section)))
        found)
    (while (and cur (not found))
      (setq found (or (and (fboundp 'code-review-code-comment-section-p)
                           (code-review-code-comment-section-p cur))
                      (and (fboundp 'code-review-reply-comment-section-p)
                           (code-review-reply-comment-section-p cur))
                      (and (fboundp 'code-review-local-comment-section-p)
                           (code-review-local-comment-section-p cur))
                      (and (fboundp 'code-review-reactions-section-p)
                           (code-review-reactions-section-p cur))
                      (and (fboundp 'code-review-outdated-comment-section-p)
                           (code-review-outdated-comment-section-p cur))))
      (setq cur (and (slot-boundp cur 'parent) (oref cur parent))))
    found))

(defun code-review--patch-line-p ()
  "Return non-nil if point is on a patch line inside a Magit hunk.
Skips lines that belong to code-review comment UI subsections."
  (and (magit-hunk-section-p (magit-current-section))
       (not (code-review--section-in-review-subsection-p))
       (let ((c (char-after (line-beginning-position))))
         (or (eq c ?\s) (eq c ?+) (eq c ?-)))))

(defun code-review--hunk-content-start (hunk)
  "Return buffer position of first content line for HUNK (line after header)."
  (save-excursion
    (goto-char (marker-position (oref hunk start)))
    (forward-line 1)
    (line-beginning-position)))

(defun code-review--nearest-patch-line-in-hunk (hunk &optional pos)
  "Find nearest patch line position (BOL) within HUNK from POS (default point).
Prefers previous patch line; if none, uses next patch line; returns nil if none."
  (let ((here (or pos (point)))
        prev next)
    (save-excursion
      ;; Search backward within hunk bounds
      (goto-char here)
      (while (and (>= (point) (marker-position (oref hunk start)))
                  (not prev))
        (when (code-review--patch-line-p)
          (setq prev (line-beginning-position)))
        (forward-line -1))
      ;; Search forward within hunk bounds
      (goto-char here)
      (while (and (< (point) (marker-position (oref hunk end)))
                  (not next))
        (when (code-review--patch-line-p)
          (setq next (line-beginning-position)))
        (forward-line 1)))
    (or prev next)))

(defun code-review--count-patch-advances (hunk start-pos end-pos)
  "Count old/new advances across patch lines from START-POS up to END-POS in HUNK.
Returns cons (OLD . NEW) where OLD counts lines with ' ' or '-', and NEW counts
lines with ' ' or '+'. END-POS is exclusive. Review UI lines are ignored."
  (let ((old 0) (new 0))
    (save-excursion
      (goto-char start-pos)
      (while (< (point) end-pos)
        (when (code-review--patch-line-p)
          (pcase (char-after (line-beginning-position))
            (?\s (setq old (1+ old) new (1+ new)))
            (?+  (setq new (1+ new)))
            (?-  (setq old (1+ old)))))
        (forward-line 1)))
    (cons old new)))








(declare-function difftastic-git-diff-range "difftastic"
                  (&optional rev-or-range args files))











(defun code-review--section-author (section)
  "Return the author string of SECTION, or nil.
Comment sections are created by passing a prebuilt object to
`magit-insert-section', which stores it in the `value' slot, so
the author may live either in the section itself or in its
value object.  Sections of classes with no `author' slot (most
of them) safely return nil instead of signaling
`invalid-slot-name'."
  (or (and (slot-exists-p section 'author)
           (slot-boundp section 'author)
           (let ((author (oref section author)))
             (and (stringp author) author)))
      (let ((val (and (slot-boundp section 'value)
                      (oref section value))))
        (and (eieio-object-p val)
             (slot-exists-p val 'author)
             (slot-boundp val 'author)
             (let ((author (oref val author)))
               (and (stringp author) author))))))

(defun code-review--bot-comment-p (section)
  "Non-nil when SECTION is a comment authored by a bot."
  (let ((author (code-review--section-author section)))
    (and author
         (string-match-p code-review-bot-author-regexp
                         (downcase author))
         t)))

(defun code-review--section-tree-any (section pred)
  "Non-nil when PRED holds for SECTION or one of its descendants."
  (or (funcall pred section)
      (let ((found nil))
        (dolist (c (oref section children))
          (unless found
            (setq found (code-review--section-tree-any c pred))))
        found)))

(defun code-review--collapse-bot-comments (section)
  "Fold bot-authored comment sections under SECTION.
In plain terms: reviews of AI-generated PRs are full of bots
commenting on each other.  Those comments are collapsed to their
one-line \"@author - date\" heading, so what's left reads like a
human conversation.  Threads where a human replied stay expanded,
and TAB unfolds any of them."
  (if (and (code-review--bot-comment-p section)
           (not (code-review--section-tree-any
                 section
                 (lambda (s)
                   (let ((author (code-review--section-author s)))
                     (and author
                          (not (code-review--bot-comment-p s))))))))
      (magit-section-hide section)
    (dolist (c (oref section children))
      (code-review--collapse-bot-comments c))))


;;; Phase 9: fringe markers linking diff lines to their threads

(defvar code-review--fringe-bitmap-defined nil
  "Non-nil after the `code-review-comment-marker' bitmap was registered.")

(defun code-review--ensure-fringe-bitmap ()
  "Register the thread marker fringe bitmap once (no-op on text frames)."
  (when (and (display-graphic-p)
             (fboundp 'define-fringe-bitmap)
             (not code-review--fringe-bitmap-defined))
    (define-fringe-bitmap 'code-review-comment-marker
      (vector 0 128 192 224 224 192 128 0))
    (setq code-review--fringe-bitmap-defined t)))

(defvar code-review-fringe-marker-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] 'code-review-fringe-jump-to-thread)
    map)
  "Keymap for thread-marker lines.
A sparse keymap: only mouse-1 is bound (jump to the thread); any
other key falls through to the buffer's own maps untouched.")

(defun code-review--comment-anchor-position (section)
  "Buffer position (BOL) of the diff line SECTION is anchored to.
The wash inserts each thread right AFTER its anchor line, so scan
backwards from the section start, inside the hunk, until a patch
line is found.  nil when SECTION does not sit inside a hunk."
  (when (and (slot-boundp section 'parent)
             (magit-hunk-section-p (oref section parent)))
    (let ((bound (oref (oref section parent) start)))
      (save-excursion
        (goto-char (oref section start))
        (catch 'found
          (while (> (point) bound)
            (forward-line -1)
            (when (code-review--patch-line-p)
              (throw 'found (line-beginning-position))))
          nil)))))

(defun code-review--mark-comment-line (section)
  "Lay the clickable fringe marker on SECTION's anchor diff line.
Idempotent: any previous marker on that line is removed first."
  (let ((bol (code-review--comment-anchor-position section)))
    (when bol
      (remove-overlays bol (1+ bol) 'cr-comment-marker t)
      (let ((ov (make-overlay bol (min (1+ bol) (point-max)))))
        (overlay-put ov 'cr-comment-marker t)
        (overlay-put ov 'evaporate t)
        (overlay-put ov 'cr-thread-start (copy-marker (oref section start)))
        (overlay-put ov 'before-string
                     (propertize
                      " " 'display
                      (list 'left-fringe 'code-review-comment-marker
                            'code-review-fringe-comment-face)
                      'keymap code-review-fringe-marker-keymap
                      'help-echo "Review thread on this line (mouse-1: jump)"))))))

(defun code-review--mark-comment-lines ()
  "Fringe-mark every review thread anchored inside a hunk.
Sweeps stale marker overlays first (a re-render erases the buffer,
but the walk below is cheap enough to stay idempotent anyway).
Runs from `code-review--trigger-hooks' after every render."
  (dolist (ov (overlays-in (point-min) (point-max)))
    (when (overlay-get ov 'cr-comment-marker)
      (delete-overlay ov)))
  (code-review--ensure-fringe-bitmap)
  (magit-map-sections
   (lambda (section)
     (when (cl-typep section 'code-review-base-comment-section)
       (code-review--mark-comment-line section)))))

(provide 'code-review-section)
;;; code-review-section.el ends here
