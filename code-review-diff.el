;;; code-review-diff.el --- Diff classification and file ordering -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026 Andrea <andrea-dev@hotmail.com>
;;
;; This file is part of code-review.
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either under version 3, or
;; (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; Commentary:
;;
;;  The phase-3 classification engine: pure functions that tag,
;;  collapse, hide and order the files of a raw unified diff.
;;  Comment anchors refer to the API diff, so this only classifies;
;;  it never rewrites the diff content being reviewed.
;;
;;; Code:

(require 'dash)

(defvar code-review-history-heat-tags)       ; code-review-history.el
(defvar code-review-history-focus-hide-cold) ; code-review-history.el
(defvar code-review-history-order-diff-by-heat) ; code-review-history.el
(defvar code-review-history--order)          ; code-review-history.el
(declare-function code-review-history--tag-for "code-review-history")
(declare-function code-review-history--score-for "code-review-history")
(require 'a)
(require 'code-review-db)
(require 'code-review-repo)


(defcustom code-review-diff-file-order-rules nil
  "Rules to order and classify files in the diff.
Each element is either a regexp string or a property list:

  (:match REGEXP :tag TAG :collapse BOOL :hide BOOL)

- A string is a pure ordering rule: files matching the first
  regexp come first, then the second, and so on.
- `:match' is the regexp (required for plist rules), tested
  against the file path without the leading \"a/\" or \"b/\".
- `:tag' shows a [TAG] label on the file heading.
- `:collapse' starts the file section collapsed.  Files carrying
  review comments are never auto-collapsed.
- `:hide' removes the file while focus mode is on (see
  `code-review-toggle-focus-mode').  The \"Files changed\" heading
  always reports what is hidden, so nothing is lost.

Files matching no user rule come next, and files matching the
built-in `code-review-diff-noise-rules' sink to the bottom.

Example:
  (setq code-review-diff-file-order-rules
        \\='(\"^src/\"                           ;; source first
          (:match \"^\\\\(test/\\\\|tests/\\\\)\" :tag \"TEST\") ;; tests last
          \"^\\\\(config/\\\\|\\\\.github/\\\\|\\\\.gitlab-ci\\\\.yml$\\\\)\")) ;; config"
  :group 'code-review
  :type
  '(repeat
    (choice (string :tag "Regexp (order only)")
            (set (cons (const :match) (regexp :tag "Regexp"))
                 (cons (const :tag) (string :tag "Tag"))
                 (cons (const :collapse) (boolean :tag "Collapse by default"))
                 (cons (const :hide) (boolean :tag "Hide in focus mode"))))))
(defcustom code-review-diff-noise-rules
  '((:match "\\`\\(package-lock\\.json\\|yarn\\.lock\\|pnpm-lock\\.yaml\\|Cargo\\.lock\\|flake\\.lock\\|poetry\\.lock\\|Gemfile\\.lock\\|composer\\.lock\\|mix\\.lock\\|Pipfile\\.lock\\|packages\\.lock\\.json\\)\\'"
     :tag "GEN" :collapse t)
    (:match "\\.lock\\'"
     :tag "GEN" :collapse t)
    (:match "\\`\\(dist\\|build\\|out\\)/\\|\\.min\\.[cm]?js\\'\\|\\.bundle\\.[cm]?js\\'\\|\\.map\\'"
     :tag "GEN" :collapse t)
    (:match "\\`\\(CHANGELOG\\|CHANGES\\|NEWS\\|HISTORY\\|AUTHORS\\|CREDITS\\)\\(\\.[^/]*\\)?\\'\\|/\\(doc\\|docs\\|documentation\\)/\\|\\.\\(md\\|markdown\\|rst\\)\\'\\|\\(^\\|/\\)README\\(\\.[^/]*\\)?\\'"
     :tag "DOC" :collapse t))
  "Built-in rules classifying low-signal files as noise.
These come enabled by default so you don't have to maintain
anything: lockfiles, generated/minified output and changelog/docs
are tagged (e.g. [GEN], [DOC]) and collapsed, and focus mode
(see `code-review-toggle-focus-mode') hides them entirely.
Set this variable to nil to disable all built-in classification.
The same plist format of `code-review-diff-file-order-rules'
applies, so you can add your own entries or place
`code-review-diff-file-order-rules' entries after these to
override them."
  :group 'code-review
  :type
  '(repeat
    (set (cons (const :match) (regexp :tag "Regexp"))
         (cons (const :tag) (string :tag "Tag"))
         (cons (const :collapse) (boolean :tag "Collapse by default"))
         (cons (const :hide) (boolean :tag "Hide in focus mode")))))

;;; Classification functions

(defun code-review--diff--extract-b-path (header-line)
  "Extract the new-file path from a diff HEADER-LINE.
Handles both the usual \"diff --git a/old b/new\" form and the
no-prefix \"diff --git old new\" form some forges return."
  (cond
   ((string-match "^diff --git a/\\(.+?\\) b/\\(.+\\)$" header-line)
    (match-string 2 header-line))
   ((string-match "^diff --git \\(.+?\\) \\(.+\\)$" header-line)
    (match-string 2 header-line))))

(defun code-review--diff--split-by-files (diff-text)
  "Split DIFF-TEXT into a list of (path . block) per file."
  (let ((pos 0)
        (len (length diff-text))
        blocks)
    (while (and (< pos len)
                (string-match "^diff --git .+$" diff-text pos))
      (let* ((start (match-beginning 0))
             ;; advance to next header or end
             (next (if (string-match "^diff --git .+$" diff-text (match-end 0))
                       (match-beginning 0)
                     len))
             (block (substring diff-text start next))
             (first-line-end (string-match "\n" block))
             (header (if first-line-end
                         (substring block 0 first-line-end)
                       block))
             (path (or (code-review--diff--extract-b-path header) "")))
        (push (cons path block) blocks)
        (setq pos next)))
    (nreverse blocks)))

(defun code-review--diff--rule-regexp (rule)
  "Return the match regexp of RULE, or nil when it has none.
A string rule matches as itself."
  (cond ((stringp rule) rule)
        ((plist-get rule :match))))

(defun code-review--diff--classify-path (path)
  "Return plist (:tag ... :collapse ... :hide ...) for PATH.
Merge `code-review-diff-file-order-rules' and
`code-review-diff-noise-rules': the first rule that matches and
sets a property wins for that property."
  (let ((info nil))
    (dolist (rule (append code-review-diff-file-order-rules
                          code-review-diff-noise-rules))
      (when-let* ((regexp (code-review--diff--rule-regexp rule))
                  ((string-match-p regexp path)))
        (dolist (key '(:tag :collapse :hide))
          (unless (plist-member info key)
            (when-let* ((val (and (not (stringp rule))
                                  (plist-get rule key))))
              (setq info (plist-put info key val)))))))
    info))

(defun code-review--diff--ws-normalize (line)
  "Collapse whitespace in LINE for whitespace-only comparisons."
  (string-join (split-string line "\\s-+" t) " "))

(defun code-review--diff--hunk-ws-only-p (minus plus)
  "Non-nil when MINUS/PLUS line lists differ only in whitespace."
  (and (= (length minus) (length plus))
       (let ((a (mapcar #'code-review--diff--ws-normalize minus))
             (b (mapcar #'code-review--diff--ws-normalize plus)))
         (while (and a b (string-equal (car a) (car b)))
           (setq a (cdr a) b (cdr b)))
         (null a))))

(defun code-review--diff--block-ws-only-p (block)
  "Non-nil when every hunk of BLOCK only changes whitespace.
Blocks without hunks (pure renames, binary, mode-only) return nil."
  (let ((lines (split-string block "\n"))
        (saw-hunk nil)
        (in-hunk nil)
        (minus nil)
        (plus nil))
    (catch 'done
      (dolist (line lines)
        (cond
         ((string-prefix-p "@@" line)
          (when in-hunk
            (unless (code-review--diff--hunk-ws-only-p (nreverse minus)
                                                        (nreverse plus))
              (throw 'done nil)))
          (setq in-hunk t
                saw-hunk t
                minus nil
                plus nil))
         (t
          (when in-hunk
            (cond
             ((string-prefix-p "-" line) (push (substring line 1) minus))
             ((string-prefix-p "+" line) (push (substring line 1) plus)))))))
      (when in-hunk
        (unless (code-review--diff--hunk-ws-only-p (nreverse minus)
                                                   (nreverse plus))
          (throw 'done nil)))
      saw-hunk)))

(defun code-review--diff--block-pure-rename-p (block)
  "Non-nil when BLOCK is a pure rename with no content hunks."
  (and (string-match-p "^rename from " block)
       (not (string-match-p "^@@" block))))

(defun code-review--diff--block-has-content-p (block)
  "Non-nil when BLOCK has at least one added or removed line."
  (or (string-match-p "\n\\+[^+]" block)
      (string-match-p "\n-[^-]" block)))

(defun code-review--diff--classify-diff (diff-text)
  "Classify every file of DIFF-TEXT.
Return a hash table path -> plist (:tag :collapse :hide),
combining:
- rule-based classification from
  `code-review-diff-file-order-rules' and
  `code-review-diff-noise-rules';
- pure renames ([MOVED]);
- whitespace-only files ([WS-ONLY]), detected textually and,
  when a local worktree is available, with `git diff -w';
- phase 14 heat buckets ([HOT]/[WARM]/[COLD]) for the files the
  checks above left untagged (noise classification wins).

Comment anchors refer to the API diff, so classification only
tags, collapses and hides: it never replaces the diff itself."
  (let ((table (make-hash-table :test #'equal))
        (substantive
         (ignore-errors
           (let ((pr (code-review-db-get-pullreq)))
             (and (slot-boundp pr 'number)
                  (code-review-repo-substantive-files
                   (format "%s" (oref pr number))))))))
    (dolist (blk (code-review--diff--split-by-files diff-text))
      (let* ((path (car blk))
             (block (cdr blk))
             (info (code-review--diff--classify-path path))
             (tag (plist-get info :tag)))
        (cond
         ((code-review--diff--block-pure-rename-p block)
          (setq tag "MOVED"
                info (plist-put info :collapse t)))
         ((code-review--diff--block-ws-only-p block)
          (setq tag "WS-ONLY"
                info (plist-put info :collapse t)))
         ((and substantive
               (code-review--diff--block-has-content-p block)
               (not (member path substantive)))
          (setq tag "WS-ONLY"
                info (plist-put info :collapse t))))
        (when tag
          (setq info (plist-put info :tag tag)))
        ;; phase 14 heat tags: only files the noise checks left
        ;; untagged (noise classification wins over heat)
        (when (and (null tag)
                   code-review-history-heat-tags)
          (when-let ((heat (code-review-history--tag-for path)))
            (setq info (plist-put info :tag heat)
                  tag heat)))
        ;; focus mode hides auto-flagged noise, unless a rule
        ;; explicitly opted out with :hide nil
        (when (and (or (member tag '("GEN" "DOC" "WS-ONLY"))
                       (and code-review-history-focus-hide-cold
                            (equal tag "COLD")))
                   (not (plist-member info :hide)))
          (setq info (plist-put info :hide t)))
        (puthash path info table)))
    table))

(defun code-review--diff--file-order-index (path)
  "Return the ordering index for PATH.
User rules come first (in their order); files matching no user
rule come next; files matching only `code-review-diff-noise-rules'
sink to the very bottom, keeping noise out of the way."
  (let ((user code-review-diff-file-order-rules)
        (noise code-review-diff-noise-rules)
        (idx 0)
        (found nil))
    (while (and user (not found))
      (let ((rule (pop user)))
        (if (when-let* ((regexp (code-review--diff--rule-regexp rule)))
              (string-match-p regexp path))
            (setq found idx)
          (setq idx (1+ idx)))))
    (or found
        ;; not matched by user rules: check noise rules
        (let ((j 0)
              (noise-found nil))
          (while (and noise (not noise-found))
            (let ((rule (pop noise)))
              (when (when-let* ((regexp (code-review--diff--rule-regexp rule)))
                      (string-match-p regexp path))
                (setq noise-found (+ idx 1 j))))
            (setq j (1+ j)))
          (or noise-found idx)))))

(defun code-review--maybe-reorder-diff (diff-text &optional classifications)
  "Reorder DIFF-TEXT per the file rules, and honor focus mode.
CLASSIFICATIONS is the hash table produced by
`code-review--diff--classify-diff'.  When focus mode is on, files
whose classification has a non-nil :hide are omitted from the
buffer; they remain counted in the \"Files changed\" heading."
  (if (and (not code-review-diff-file-order-rules)
           (not code-review-diff-noise-rules)
           (not code-review-focus-mode)
           (not (and code-review-history-order-diff-by-heat
                     code-review-history--order)))
      diff-text
    (let* ((first-pos (string-match "^diff --git .+$" diff-text 0))
           (prefix (if (and first-pos (> first-pos 0))
                       (substring diff-text 0 first-pos)
                     ""))
           (blocks (code-review--diff--split-by-files
                    (if first-pos (substring diff-text first-pos) diff-text)))
           (sorted (sort blocks
                         (lambda (a b)
                           (let* ((ia (code-review--diff--file-order-index (car a)))
                                  (ib (code-review--diff--file-order-index (car b))))
                             (cond
                              ((/= ia ib) (< ia ib))
                              ;; phase 14: equally-ranked (unmatched)
                              ;; files read in heat order, hottest
                              ;; first, then alphabetically
                              ((and code-review-history-order-diff-by-heat
                                    (/= (code-review-history--score-for (car a))
                                        (code-review-history--score-for (car b))))
                               (> (code-review-history--score-for (car a))
                                  (code-review-history--score-for (car b))))
                              (t (string-lessp (car a) (car b))))))))
           (kept (if (and code-review-focus-mode classifications)
                     (let (acc)
                       (dolist (blk sorted)
                         (unless (plist-get (gethash (car blk)
                                                     classifications)
                                            :hide)
                           (push blk acc)))
                       (nreverse acc))
                   sorted)))
      (concat prefix
              (mapconcat
               (lambda (blk)
                 ;; a block can lose its trailing newline when the
                 ;; diff was trimmed: never glue two blocks together
                 (let ((text (cdr blk)))
                   (if (string-suffix-p "\n" text) text (concat text "\n"))))
               kept "")))))

(provide 'code-review-diff)
;;; code-review-diff.el ends here
