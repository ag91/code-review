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
(require 'code-review-diff)
(require 'code-review-history)
(require 'code-review-testimpact)
(require 'code-review-registry)
(require 'code-review-criteria)

(declare-function code-review-section-insert-comment "code-review-section-comment")
(declare-function code-review-comment-insert-reactions "code-review-reactions")



;; files report

(defclass code-review-files-report-section (magit-section)
  (()))
;; phase 13 (render-time value owned by code-review-section.el):
;; the new-since-last-review alist, NIL forward-declaration — a
;; valued defvar here would pre-bind it and shadow the owner
(defvar code-review-section--new-since-last-review)
;; phase 13 slice 2 (same deal): the explicit read-tracking index,
;; owned by code-review-section.el, populated in --insert-pr-text
(defvar code-review-section--viewed)
(declare-function code-review-viewed--count-files "code-review-viewed")

(defun code-review-section--new-since-last-review-suffix ()
  "One-line report for the Files changed heading.
Exported for the tests."
  (let ((news code-review-section--new-since-last-review))
    (when news
      (let ((files (a-get news 'files))
            (commits (a-get news 'commits)))
        (if (and (null files) (null commits))
            "  --  no new changes since your last review"
          (format "  --  %d file%s changed since your last review (%d new commit%s)"
                  (length files)
                  (if (= (length files) 1) "" "s")
                  (length commits)
                  (if (= (length commits) 1) "" "s")))))))
;; specs report (phase 24: the criteria files of the diff render
;; ABOVE "Files changed" - the reviewed-once artifact gets the
;; top of the buffer; the code diff is supporting evidence)

(defclass code-review-specs-report-section (magit-section)
  ())
(defclass code-review-specs-chnged-section (magit-section)
  ())

(defun code-review-section--spec-path-p (path)
  "Non-nil when PATH is a criteria file (the phase 24 SPEC class).
One predicate for every consumer: classification (diff.el, through
`code-review-criteria-file-regexp'), the diff split
(`code-review-section--specs-first') and the stage-1 wash
(`code-review-wash-spec-diff') - they can never disagree."
  (and code-review-criteria-file-regexp
       (string-match-p code-review-criteria-file-regexp path)))

(defun code-review-section-insert-specs-changed ()
  "Insert the phase 24 Specs heading (the criteria files of this
diff, first-class review artifacts ABOVE the code)."
  (insert (propertize "Specs (criteria)"
                      'font-lock-face 'magit-section-heading))
  (insert (propertize
           "  - requirements first: the code below is evidence"
           'font-lock-face 'magit-dimmed))
  (magit-insert-heading))

(defun code-review-wash-spec-diff ()
  "Wash ONE SPEC-classified diff block, or stop the specs stage.
The `magit-wash-sequence' loop contract: returns non-nil while a
spec block was washed (then EARS-fontified); nil at the first
NON-spec `diff --git' block, which ENDS the specs stage - the
files-report wash (`code-review-wash-diff') picks the rest up."
  (when (looking-at "^diff --\\(git\\|cc\\|combined\\) ")
    (let* ((header (buffer-substring-no-properties
                    (line-beginning-position) (line-end-position)))
           (path (code-review--diff--extract-b-path header)))
      (when (and path (code-review-section--spec-path-p path))
        (let ((section (code-review-wash-diff)))
          (code-review-wash--fontify-ears section)
          section)))))

(defun code-review-wash--fontify-ears (section)
  "Fontify the EARS keywords of the washed SPEC SECTION.
OVERLAYS, not text properties: they override the hunk paint's
text-property faces (the phase 10 lesson).  `search-forward'
does NOT set match data, so the match range is derived from
POINT, never `match-beginning'/`match-end' (the documented
trap).  Bounded: at most 200 overlays per keyword, idempotent
per render (erase-buffer kills overlays)."
  (when section
    (let ((start (oref section start))
          (end (oref section end)))
      (save-excursion
        (dolist (kw '("WHEN" "WHILE" "WHERE" "SHALL" "MUST" "MAY"))
          (goto-char start)
          (let ((n 0))
            (while (and (< (point) end)
                        (search-forward kw end t)
                        (< n 200))
              (let* ((beg (- (point) (length kw)))
                     (ov (make-overlay beg (point))))
                (overlay-put ov 'face 'font-lock-keyword-face)
                (overlay-put ov 'evaporate t)
                (overlay-put ov 'cr-criteria-ears t))
              (setq n (1+ n)))))))))

(defun code-review-section--specs-first (diff-text)
  "Reorder DIFF-TEXT so criteria (SPEC) blocks come first.
Pure string surgery (phase 24): the stage-1 wash
(`code-review-section--wash-specs') stops at the first NON-spec
block, so the specs section needs the criteria files at the HEAD
of the inserted diff text.  Identity when no criteria file is in
the diff (no join, the text is returned untouched).  Blocks keep
their trailing newline (a block missing it glues onto the next
header and breaks the wash downstream - the documented
reorder-engine trap, same join idiom as
`code-review--maybe-reorder-diff')."
  (if (or (not code-review-criteria-file-regexp)
          (not (string-match-p "^diff --" diff-text)))
      diff-text
    (let* ((first-pos (string-match "^diff --" diff-text 0))
           (prefix (if (and first-pos (> first-pos 0))
                       (substring diff-text 0 first-pos)
                     ""))
           (blocks (code-review--diff--split-by-files
                    (if first-pos
                        (substring diff-text first-pos)
                      diff-text)))
           (specs nil)
           (rest nil))
      (dolist (blk blocks)
        (if (code-review-section--spec-path-p (car blk))
            (push blk specs)
          (push blk rest)))
      (if (null specs)
          diff-text
        (concat prefix
                (mapconcat
                 (lambda (blk)
                   (let ((text (cdr blk)))
                     (if (string-suffix-p "\n" text) text (concat text "\n"))))
                 (append (nreverse specs) (nreverse rest))
                 ""))))))

(defun code-review-section--wash-specs ()
  "Wash the SPEC-classified blocks at the head of the diff text.
Stage 1 of the phase 24 criteria surface: the criteria files of
the diff (moved to the head by `code-review-section--specs-first'
in `code-review-section--insert-pr-text') wash into their own
`code-review-specs-report-section' ABOVE 'Files changed'.  Runs
inside the buffer, at the head of the inserted diff text.  Only
inserts when the head block IS a criteria file: with no criteria
in the diff nothing changes (no empty section, no heading)."
  (save-restriction
    (narrow-to-region (point) (point-max))
    (when (and (looking-at "^diff --\\(git\\|cc\\|combined\\) ")
               (let* ((header (buffer-substring-no-properties
                               (line-beginning-position)
                               (line-end-position)))
                      (path (code-review--diff--extract-b-path header)))
                 (and path (code-review-section--spec-path-p path))))
      (magit-insert-section (code-review-specs-report-section)
        (code-review-section-insert-specs-changed)
        (magit-insert-section (code-review-specs-chnged-section)
          (magit-wash-sequence #'code-review-wash-spec-diff))))))

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
                       ;; phase 13 slice 2: the read-tracking counter
                       ;; (explicit marks + the all-hunks rollup)
                       (when (and (hash-table-p code-review-section--viewed)
                                  (hash-table-p
                                   code-review-section--file-classifications))
                         (format code-review-section--viewed-counter-format
                                 (code-review-viewed--count-files)
                                 (hash-table-count
                                  code-review-section--file-classifications)))
                       ;; phase 13: what a returning reviewer needs
                       (code-review-section--new-since-last-review-suffix)
                       (when code-review-focus-mode
                         (or (code-review-section--focus-summary)
                             "  --  focus: no noise files to hide"))
                       (when (and code-review-hide-viewed-files
                                  (hash-table-p code-review-section--viewed))
                         "  --  viewed files hidden"))
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
    (let* ((status (cond ((equal (match-string 1) "git") "modified")
                        ((match-string 3)              "resolved")
                        (t                            "unmerged")))
          (orig nil)
          (file (or (match-string 2) (match-string 4)))
          (header (list (buffer-substring-no-properties
                         (line-beginning-position) (1+ (line-end-position)))))
          (modes nil)
          (rename nil)
          (binary nil)
          ;; Canonical provenance comes from the diff header, not
          ;; from read-tracking state or a path's leading directory.
          (canonical-path
           (or (code-review--diff--extract-b-path
                (string-trim-right (car header)))
               file)))
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
      (setq file (code-review-wash--decode-git-path (or file canonical-path)))
      (setq canonical-path (code-review-wash--decode-git-path canonical-path))
      (setq header (string-join (nreverse header)))
      (code-review-wash-insert-file-section
       file orig status modes rename header binary nil canonical-path))))
(defun code-review-wash--decode-git-path (path)
  "Decode git-quoted PATH (\"\\226...\" style) to its raw form.
Delegates to magit's utility when available; identity otherwise."
  (if (fboundp 'magit-decode-git-path)
      (magit-decode-git-path path)
    path))
(defun code-review-wash-insert-file-section
    (file orig status modes rename header binary long-status
          &optional canonical-path)
  "Insert the file section for FILE and wash its hunks.
ORIG is the original file name (renames), STATUS the change type,
MODES a mode-change header, RENAME a rename header, HEADER the
raw extended headers, BINARY non-nil for binary files,
LONG-STATUS an alternative status text, and CANONICAL-PATH the
repository-relative path from the diff header.  This is an owned copy of
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
         (clean-path (or canonical-path
                         (code-review-diff--strip-path raw-path-name)))
         (info (and code-review-section--file-classifications
                    (gethash clean-path
                             code-review-section--file-classifications)))
         (tag (plist-get info :tag))
         (collapse (code-review-section--file-collapse-p clean-path info)))
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
                          'font-lock-face 'magit-diff-file-heading
                          'code-review-path clean-path))
      (when tag
        (insert (propertize (format "  [%s]" tag)
                            'font-lock-face 'code-review-diff-tag-face)))
      (when (plist-get info :new)
        (insert (propertize "  [NEW]"
                            'font-lock-face 'code-review-diff-tag-face)))
      ;; phase 17: hard tag on files with CI-gaming findings (skip
      ;; markers, gated/removed CI steps, lowered thresholds)
      (when (code-review-testimpact--file-tag-for clean-path)
        (insert (propertize "  [CI-GAME]"
                            'font-lock-face 'font-lock-warning-face)))
      ;; phase 22: incident count on the file heading (permanent
      ;; review heat from the incident registry)
      (when-let ((itag (code-review-registry--file-tag clean-path)))
        (insert (propertize (format "  %s" itag)
                            'font-lock-face
                            'code-review-delicate-hunk-face)))
      ;; phase 13 slice 2: the reviewer marked this file viewed
      (when (plist-get info :viewed)
        (insert (propertize code-review-section--viewed-tag
                            'font-lock-face 'code-review-dimmed)))
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
(defun code-review-wash-hunk--insert-heading (heading badge notest &optional viewed?)
  "Insert the hunk HEADING line, the phase 17 NO-TEST tag NOTEST
and, when non-nil, the delicacy BADGE (phase 15), then close the
heading (`magit-insert-heading').  VIEWED? (phase 13 slice 2)
dims the heading instead of the regular hunk heading face: the
reviewer marked this hunk read."
  (insert (propertize heading
                      'font-lock-face
                      (if viewed? 'code-review-dimmed
                        'magit-diff-hunk-heading)))
  (when notest
    (insert (propertize (format "  [%s]" notest)
                        'font-lock-face 'font-lock-warning-face)))
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
          ;; phase 15: the delicacy badge; phase 17: the NO-TEST tag
          ;; (cache hits: the Analysis section runs before the diff
          ;; wash in the sections hook); phase 13 slice 2: a hunk
          ;; the reviewer marked read dims its heading
          (code-review-wash-hunk--insert-heading
           heading
           (code-review-analysis--hunk-badge-for path-name raw-ranges)
           (code-review-testimpact--hunk-tag-for path-name raw-ranges)
           (let ((vinfo (and (hash-table-p code-review-section--viewed)
                             (gethash path-name
                                      code-review-section--viewed))))
             (and vinfo
                  (or (member raw-ranges (plist-get vinfo :hunks))
                      (plist-get vinfo :file)))))
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
