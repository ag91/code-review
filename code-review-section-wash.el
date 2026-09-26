;;; code-review-section-wash.el --- the owned diff wash (phase 11b) and diff report -*- lexical-binding: t; -*-
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

;; the owned diff wash (phase 11b) and diff report (part of the code-review section rendering, split from
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
(require 'code-review-analysis)
(require 'code-review-hunkhighlight)
(require 'code-review-history)

(declare-function code-review-section-insert-comment "code-review-section-comment")
(declare-function code-review-comment-insert-reactions "code-review-reactions")



;; files report

(defclass code-review-files-report-section (magit-section)
  (()))
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
(defun code-review-wash-hunk--comment-line-keys (path-name ch old-line new-line)
  "The side/line comment grouping keys for one washed hunk line.
PATH-NAME is the file path, CH the line's +/-/space sigil, and
OLD-LINE / NEW-LINE the line numbers the line carries on each
side (nil on a side the line does not exist in)."
  (cond
   ((string= ch " ")
    (list (code-review-utils--comment-key-from-line path-name "RIGHT" new-line)
          (code-review-utils--comment-key-from-line path-name "LEFT" old-line)))
   ((string= ch "+")
    (list (code-review-utils--comment-key-from-line path-name "RIGHT" new-line)))
   ((string= ch "-")
    (list (code-review-utils--comment-key-from-line path-name "LEFT" old-line)))
   (t nil)))
(defun code-review-wash-hunk--insert-line-comments (keys path-name)
  "Insert the grouped comments anchored at the comment group KEYS
of the current hunk line (PATH-NAME), marking each consumed key
written.  Returns t when any comment was inserted (insertion
moves point, so the caller must then not advance a line)."
  (let ((did-insert nil))
    (dolist (key keys)
      (let ((grouped (code-review-utils--comment-get
                      code-review-section-grouped-comments key))
            (written? (-contains-p code-review-section-hold-written-comment-ids
                                    key)))
        (when (and grouped (not written?)
                    code-review-section--display-all-comments)
          (push key code-review-section-hold-written-comment-ids)
          (let ((comment-written-pos
                 (or (alist-get path-name
                                code-review-section-hold-written-comment-count
                                nil nil 'equal)
                     0)))
            (code-review-section-insert-comment grouped comment-written-pos))
          (setq did-insert t))))
    did-insert))
(defun code-review-wash-hunk--advance-counters (ch old-line new-line)
  "The (OLD . NEW) line counters after washing the line with sigil CH.
OLD-LINE / NEW-LINE are the counters before this line (nil on a
side the hunk does not cover)."
  (cond
   ((string= ch " ")
    (cons (and old-line (1+ old-line))
          (and new-line (1+ new-line))))
   ((string= ch "+")
    (cons old-line (and new-line (1+ new-line))))
   ((string= ch "-")
    (cons (and old-line (1+ old-line)) new-line))
   (t (cons old-line new-line))))
(defun code-review-wash-hunk--ranges (raw-ranges)
  "Parse RAW-RANGES (the @@ header ranges text) into number pairs.
A single line is +1 rather than +1,1."
  (mapcar (lambda (str)
            (let ((range
                   (mapcar #'string-to-number
                           (split-string (substring str 1) ","))))
              (if (length= range 1)
                  (nconc range (list 1))
                range)))
          (split-string raw-ranges)))
(defun code-review-wash-hunk--ensure-head-pos (path path-name)
  "The head-pos for PATH (its hunk-header reference line), fixing
it on the db (PATH-NAME) when missing so comment positions anchor."
  (or (oref path head-pos)
      (let ((adjusted-pos (+ (code-review--line-number-at-pos) 1)))
        (code-review-db--curr-path-head-pos-update path-name adjusted-pos)
        adjusted-pos)))
(defun code-review-wash-hunk--insert-heading (heading badge)
  "Insert the hunk HEADING line and, when non-nil, the delicacy
BADGE (phase 15), then close the heading (`magit-insert-heading')."
  (insert (propertize heading 'font-lock-face 'magit-diff-hunk-heading))
  (when badge
    (insert (propertize badge
                        'font-lock-face
                        'code-review-delicate-hunk-face)))
  (insert ?\n)
  (magit-insert-heading))
(defun code-review-wash-hunk--interleave-comments (path-name head-pos
                                                  old-line new-line)
  "Wash the hunk body lines from point, interleaving the grouped
comments (`code-review-section-grouped-comments', keyed by path
and diff position or by side/line) inline as the lines are
consumed.  OLD-LINE / NEW-LINE are the starting line counters from
the hunk header; each line advances them per its +/-/space
sigil.  The wash stops at the next file header or buffer end."
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
           ;; Side/line-keyed comments (locals / GraphQL
           ;; line API).  Context lines can anchor
           ;; comments on either side, so try both keys.
           (line-keys (code-review-wash-hunk--comment-line-keys
                       path-name ch old-line new-line))
           (did-insert
            (code-review-wash-hunk--insert-line-comments
             (cons path-pos line-keys) path-name)))
      ;; Advance line only when nothing was inserted
      ;; (insertion moves point)
      (unless did-insert
        (forward-line))
      ;; Update counters for old/new line numbers following
      ;; this patch line
      (pcase-let ((`(,old . ,new)
                   (code-review-wash-hunk--advance-counters
                    ch old-line new-line)))
        (setq old-line old
              new-line new)))))
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
           (head-pos (code-review-wash-hunk--ensure-head-pos path path-name)))
      (when (not head-pos)
        (code-review-utils--log
         "code-review-wash-hunk"
         (format "Every diff is associated with a PATH (the file). Head pos nil for %S"
                 (prin1-to-string path)))
        (message "ERROR: Head position for path %s was not found.
Please Report this Bug" path-name))
    ;;; --- end -- code-review specific code.

      (let* ((ranges   (code-review-wash-hunk--ranges raw-ranges))
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
          ;; phase 15: the delicacy badge (cache hit: the Analysis
          ;; section runs before the diff wash in the sections hook)
          (code-review-wash-hunk--insert-heading
           heading
           (code-review-analysis--hunk-badge-for path-name raw-ranges))
          ;; Keep track of old/new line numbers from the hunk header so we
          ;; can anchor local comments keyed by SIDE/LINE inline.
          (code-review-wash-hunk--interleave-comments
           path-name head-pos
           (and (car ranges) (car (car ranges)))
           (and (car (last ranges)) (car (car (last ranges)))))

          ;; Remaining comments for this file are handled once per file
          ;; in code-review-wash-insert-file-section.
          )))
          (code-review-wash--paint-hunk hunk-section)
          (code-review-hunkhighlight-hunk hunk-section path-name))))
    t))
(provide 'code-review-section-wash)
;;; code-review-section-wash ends here
