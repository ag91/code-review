;;; code-review-diff.el --- Diff classification and file ordering -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Wanderson Ferreira
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

(provide 'code-review-diff)
;;; code-review-diff.el ends here
