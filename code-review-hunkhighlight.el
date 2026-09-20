;;; code-review-hunkhighlight.el --- Tree-sitter semantic hunk faces -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026 Andrea <andrea-dev@hotmail.com>
;;
;; This file is part of code-review.
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;  Phase 10: semantic highlighting of diff hunks.  After the diff
;;  wash paints the base faces (+/-/context), this library makes
;;  hunks read as CODE.  The face set is deliberately MINIMAL so
;;  the reviewer's eyes focus on the important identifiers:
;;  definition names, parameters, assignment targets, UPPER_CASE
;;  constants, literal CONSTANT VALUES (strings/numbers — in test
;;  code those are the test case names and expected values), the
;;  `return' keyword — and in test files, test definition names
;;  and assertion lines.  Only ADDED lines get
;;  semantic faces (context is parse input, not signal; deleted
;;  lines are not part of the new side).
;;
;;  How it works, per hunk:
;;   1. strip the +/-/space prefixes and reconstruct the NEW side
;;      of the hunk (added + context lines);
;;   2. parse it with treesit and run `treesit-query-capture' with
;;      per-language queries (python first; see the defcustoms);
;;   3. map every captured node's range back onto the diff buffer
;;      positions and lay the semantic face as an OVERLAY over the
;;      line's diff face: overlay faces override text-property
;;      faces, so the semantic foreground displays on top of the
;;      red/green background (a face-LIST property would NOT work:
;;      earlier faces in the list win attribute conflicts, and
;;      `magit-diff-added' sets its own foreground).  Overlays also
;;      survive magit 4.x lazily replacing hunk face properties.
;;
;;  Everything degrades gracefully: no treesit, no grammar, no
;;  queries for the language, or `code-review-semantic-highlight'
;;  nil  =>  exactly the old faces, no error.
;;
;;  The core (`code-review-hunkhighlight-region') works on any
;;  buffer region containing prefixed diff lines, so it also
;;  works in plain magit diff buffers:
;;  `code-review-hunkhighlight-magit-buffer' walks the magit
;;  section tree of the current buffer and highlights every hunk
;;  (phase 10 bonus: reusable outside code-review).

;;; Code:

(require 'treesit nil t)          ; optional: Emacs 29+
(require 'magit-section)
(require 'cl-lib)

(defgroup code-review-hunkhighlight nil
  "Tree-sitter semantic highlighting of diff hunks."
  :group 'code-review)

(defcustom code-review-semantic-highlight t
  "When non-nil, highlight hunks semantically with tree-sitter.
Ignored (does nothing) when treesit is unavailable or the file's
language grammar is not installed: then hunks keep the plain
diff faces and no error is signaled."
  :type 'boolean
  :group 'code-review-hunkhighlight)

(defcustom code-review-hunkhighlight-language-map
  '(("\\.py\\'" . python)
    ("\\.scala\\'" . scala)
    ("\\.sc\\'" . scala)
    ("\\.sbt\\'" . scala)
    ("\\.el\\'" . elisp)
    ("\\.clj[scx]?\\'" . clojure))
  "Map file-name regexp to tree-sitter language symbol.
Extend this (and `code-review-hunkhighlight-queries') when adding
languages: only ship queries you have validated against the
installed grammar, a broken query silently disables that entry."
  :type '(alist :key-type regexp :value-type symbol)
  :group 'code-review-hunkhighlight)

(defcustom code-review-hunkhighlight-test-path-regexp
  "\\`test\\|/tests?/\\|_test\\.\\|\\.test\\."
  "Regexp matching file paths that are test files.
Matched against the DOWNCASED path.  In test files the
`code-review-hunkhighlight-test-queries' run on top of the
general ones."
  :type 'regexp
  :group 'code-review-hunkhighlight)

(defface code-review-test-face
  '((t :inherit font-lock-function-name-face :weight bold :slant italic))
  "Face for test definition names and assertion lines.
Used by the tree-sitter hunk highlighting in test files."
  :group 'code-review-hunkhighlight)

(defface code-review-constant-face
  '((((class color) (background light))
     :foreground "tomato" :weight bold)
    (((class color) (background dark))
     :foreground "MediumPurple1" :weight bold)
    (t :weight bold))
  "Face for literal constant values (strings, numbers) in hunks.
Theme `font-lock-constant-face's are often low-contrast over the
green/red diff backgrounds (the one in use renders as a murky
dark cyan there); this face is tuned to stay clearly readable on
top of `magit-diff-added' while staying distinct from the keyword
purple and the function-name blue."
  :group 'code-review-hunkhighlight)

(defcustom code-review-hunkhighlight-queries
  '((python
     ;; Deliberately MINIMAL (user request): definition names,
     ;; parameters, assignment targets, UPPER_CASE constants and
     ;; the `return' keyword — the reviewer's eyes should go to
     ;; the important identifiers, not to every keyword.  Also
     ;; note `--apply' only paints ADDED (+) lines.
     ("\"return\" @kw"
      . ((kw . font-lock-keyword-face)))
     ("(function_definition name: (identifier) @fn)"
      . ((fn . font-lock-function-name-face)))
     ("(class_definition name: (identifier) @cls)"
      . ((cls . font-lock-type-face)))
     ("(parameters (identifier) @param)"
      . ((param . font-lock-variable-name-face)))
     ("((assignment left: (identifier) @const)
       (#match \"\\\\`[A-Z_][A-Z0-9_]*\\\\'\" @const))"
      . ((const . code-review-constant-face)))
     ("((assignment left: (identifier) @var)
       (#match \"\\\\`[a-z_]\" @var))"
      . ((var . font-lock-variable-name-face)))
     ;; literal CONSTANT VALUES (strings, numbers) stand out in
     ;; every language: in test code they are the test case names
     ;; and expected values — the stuff the reviewer wants to see
     ("[(string) (integer) (float)] @cval"
      . ((cval . code-review-constant-face))))
    (scala
     ("\"return\" @kw"
      . ((kw . font-lock-keyword-face)))
     ("(function_definition name: (identifier) @fn)"
      . ((fn . font-lock-function-name-face)))
     ("(class_definition name: (identifier) @cls)"
      . ((cls . font-lock-type-face)))
     ("(object_definition name: (identifier) @obj)"
      . ((obj . font-lock-type-face)))
     ("(trait_definition name: (identifier) @trait)"
      . ((trait . font-lock-type-face)))
     ("(parameters (parameter name: (identifier) @param))"
      . ((param . font-lock-variable-name-face)))
     ("((val_definition pattern: (identifier) @const)
       (#match \"\\\\`[A-Z_][A-Z0-9_]*\\\\'\" @const))"
      . ((const . code-review-constant-face)))
     ("((val_definition pattern: (identifier) @var)
       (#match \"\\\\`[a-z_]\" @var))"
      . ((var . font-lock-variable-name-face)))
     ("(string) @cval"
      . ((cval . code-review-constant-face)))
     ("[(integer_literal) (floating_point_literal)] @cval"
      . ((cval . code-review-constant-face))))
    (elisp
     ;; grammar nodes (probed): defun forms are
     ;; `function_definition' with a named "defun" token child;
     ;; defvar/defconst/let are `special_form'.  defmacro/defsubst
     ;; are NOT special nodes here (defmacro: structure error,
     ;; defsubst: params leak into the name capture) — left out.
     ("(function_definition \"defun\" (symbol) @fn (list (symbol) @param))"
      . ((fn . font-lock-function-name-face)
         (param . font-lock-variable-name-face)))
     ("(special_form \"defvar\" (symbol) @var)"
      . ((var . font-lock-variable-name-face)))
     ("(special_form \"defconst\" (symbol) @const)"
      . ((const . code-review-constant-face)))
     ("(special_form \"let\" (list (list (symbol) @v)))"
      . ((v . font-lock-variable-name-face)))
     ("[(string) (integer)] @cval"
      . ((cval . code-review-constant-face))))
    (clojure
     ;; grammar nodes (probed): everything is list_lit of sym_lits,
     ;; so definitions are matched by the head symbol with #match
     ;; (#eq is NOT supported at capture time in Emacs 30.2).
     ;; defn-family name-only pattern first (defprotocol etc lack a
     ;; name-adjacent vector), then name+params for defn shapes.
     ("((list_lit . (sym_lit) @_h . (sym_lit) @name)
       (#match \"\\\\`\\\\(defn-\\\\|defn\\\\|defmacro\\\\|defonce\\\\|defmulti\\\\|defprotocol\\\\|defrecord\\\\|deftype\\\\)\\\\'\" @_h))"
      . ((name . font-lock-function-name-face)))
     ("((list_lit . (sym_lit) @_h . (sym_lit) @name . (vec_lit (sym_lit) @param))
       (#match \"\\\\`\\\\(defn-\\\\|defn\\\\|defmacro\\\\|defonce\\\\)\\\\'\" @_h))"
      . ((name . font-lock-function-name-face)
         (param . font-lock-variable-name-face)))
     ("((list_lit . (sym_lit) @_h . (sym_lit) @name)
       (#match \"\\\\`def\\\\'\" @_h))"
      . ((name . font-lock-variable-name-face)))
     ("((list_lit . (sym_lit) @_h . (vec_lit (sym_lit) @param))
       (#match \"\\\\`\\\\(fn\\\\|let\\\\|loop\\\\)\\\\'\" @_h))"
      . ((param . font-lock-variable-name-face)))
     ("[(str_lit) (num_lit)] @cval"
      . ((cval . code-review-constant-face)))))
  "Per-language treesit queries.
Each entry: (LANGUAGE (QUERY-STRING . ((CAPTURE . FACE) ...)) ...).
CAPTURE names must match the @captures in QUERY-STRING; a capture
can map to any face.  A query that fails to compile against the
installed grammar disables that entry silently (and capture-time
predicate errors disable only that query)."
  :type '(repeat (cons symbol (repeat (cons string (repeat (cons symbol face))))))
  :group 'code-review-hunkhighlight)

(defcustom code-review-hunkhighlight-test-queries
  '((python
     ("((function_definition name: (identifier) @tn) (#match \"\\\\`test\" @tn))"
      . ((tn . code-review-test-face)))
     ("(assert_statement \"assert\" @as)
       ((call function: (identifier) @af) (#match \"\\\\`assert\" @af))
       ((call function: (attribute attribute: (identifier) @am))
        (#match \"\\\\`assert\" @am))"
      . ((as . code-review-test-face)
         (af . code-review-test-face)
         (am . code-review-test-face)))))
  "Like `code-review-hunkhighlight-queries', but test files only."
  :type '(repeat (cons symbol (repeat (cons string (repeat (cons symbol face))))))
  :group 'code-review-hunkhighlight)

(defvar-local code-review-hunkhighlight--cache nil
  "Hash table \"LANG|TESTP|md5(BODY)\" -> ((LINE BEG END FACE)...).
LINE is 1-based in the reconstructed new-side text, BEG/END are
0-based columns within that line: position-independent, so the
ranges survive re-renders.  Buffer-local on purpose: it dies with
the review buffer.")

(defvar code-review-hunkhighlight--query-cache
  (make-hash-table :test #'equal)
  "Compiled-query cache: (LANG . QUERY-STRING) -> compiled query,
or `broken' when compilation failed against the grammar.")

(defun code-review-hunkhighlight--language-for (path)
  "Return the tree-sitter language symbol for file PATH, or nil."
  (cl-loop for (ext . lang) in code-review-hunkhighlight-language-map
           when (string-match-p ext path)
           return lang))

(defun code-review-hunkhighlight--compiled (lang query)
  "Return compiled QUERY for LANG, nil when it cannot compile."
  (let ((key (cons lang query)))
    (or (gethash key code-review-hunkhighlight--query-cache)
        (let ((compiled
               (condition-case nil
                   (treesit-query-compile lang query)
                 (error 'broken))))
          (puthash key compiled code-review-hunkhighlight--query-cache)
          (and (not (eq compiled 'broken)) compiled)))))

(defun code-review-hunkhighlight--reconstruct (beg end)
  "Reconstruct the NEW side of hunk body BEG..END.
Return (TEXT LINES): TEXT is the new-side text, LINES is a list
of (CONTENT-BEG . CONTENT-END) review-buffer positions per
reconstructed line, prefix excluded, in order.  Deleted (-) and
`\\ No newline' lines are not part of the new side."
  (let (rows lines)
    (save-excursion
      (goto-char beg)
      (while (< (point) end)
        (let ((ch (char-after)))
          (when (or (eq ch ?+) (eq ch ?\s))
            (push (buffer-substring-no-properties
                   (1+ (point)) (line-end-position))
                  rows)
            (push (cons (1+ (point)) (line-end-position)) lines)))
        (forward-line)))
    (list (string-join (nreverse rows) "\n") (nreverse lines))))

(defun code-review-hunkhighlight--put-face (beg end face)
  "Lay FACE on BEG..END as an overlay, over the line's diff face.
OVERLAYS, not text properties, for two reasons found the hard way:

 1. a face-LIST property merges with EARLIER faces winning
    attribute conflicts; since `magit-diff-added' sets its own
    foreground (#22aa22), appending the semantic face left the
    diff foreground winning and nothing was visible.  Overlay
    faces on the other hand OVERRIDE text-property faces, so the
    semantic foreground shows on top of the diff background.

 2. magit 4.x repaints hunk bodies by replacing their face text
    properties, which would erase semantic faces stored there.

Different semantic faces stack: each face carries its own
`cr-hh-face' marker so re-running is idempotent per face."
  (save-excursion
    (goto-char beg)
    (while (< (point) end)
      (let ((lend (min end (line-end-position))))
        (when (< (point) lend)
          (remove-overlays (point) lend 'cr-hh-face face)
          (let ((ov (make-overlay (point) lend)))
            (overlay-put ov 'cr-hh-face face)
            (overlay-put ov 'face face)
            (overlay-put ov 'evaporate t))))
      (forward-line 1))))

(defun code-review-hunkhighlight--node-to-lines (beg end line-starts line-lens)
  "Convert parse-buffer range BEG..END into ((LINE COL-B COL-E)...).
LINE is 1-based, columns 0-based; LINE-STARTS/LINE-LENS describe
the line layout of the parse buffer."
  (let ((res nil)
        (k 0)
        (n (length line-starts)))
    (while (< k n)
      (let* ((bol (nth k line-starts))
             (len (nth k line-lens))
             (eol (+ bol len)))
        (when (and (< beg eol) (< bol end))
          (let ((cb (max beg bol))
                (ce (min end eol)))
            (when (< cb ce)
              (push (list (1+ k) (- cb bol) (- ce bol)) res))))
        (when (<= end eol)
          (setq k n))
        (setq k (1+ k))))
    (nreverse res)))

(defun code-review-hunkhighlight--ranges (text lang test-p)
  "Return ((LINE BEG END FACE)...) for new-side TEXT of LANG.
LINE is 1-based, BEG/END are 0-based columns within that line.
TEST-P adds the test-file queries.  Nil when treesit is unusable
or nothing was captured."
  (when (fboundp 'treesit-parser-create)
    (let ((entries
           (append (cdr (assq lang code-review-hunkhighlight-queries))
                   (when test-p
                     (cdr (assq lang
                                 code-review-hunkhighlight-test-queries))))))
      (when entries
        (let ((line-starts (list 1))
              (line-lens nil)
              (res nil))
          (dolist (row (split-string text "\n"))
            (setq line-lens (cons (length row) line-lens)
                  line-starts
                  (cons (+ (car line-starts) (length row) 1)
                        line-starts)))
          (setq line-lens (nreverse line-lens)
                line-starts (nreverse line-starts))
          (with-temp-buffer
            (insert text)
            (let ((parser (treesit-parser-create lang)))
              (dolist (entry entries)
                (let ((compiled
                       (code-review-hunkhighlight--compiled
                        lang (car entry))))
                  (when compiled
                    ;; NB: capture-time predicate errors (e.g. a
                    ;; predicate COMPILES fine but is unsupported at
                    ;; runtime) disable only THIS entry, not the
                    ;; whole language.
                    (pcase-dolist
                        (`(,name . ,node)
                         (condition-case err
                             (treesit-query-capture parser compiled)
                           (error
                            (message "code-review-hunkhighlight: \
query %S disabled: %S" (car entry) err)
                            nil)))
                      (let ((face (cdr (assq name (cdr entry)))))
                        (when face
                          (dolist
                              (pos
                               (code-review-hunkhighlight--node-to-lines
                                (treesit-node-start node)
                                (treesit-node-end node)
                                line-starts line-lens))
                            (push (append pos (list face)) res))))))))))
          (nreverse res))))))

(defun code-review-hunkhighlight--apply (ranges lines)
  "Apply cached RANGES onto the review buffer.
LINES is the per-line (CONTENT-BEG . CONTENT-END) mapping from
`code-review-hunkhighlight--reconstruct'.  Only ADDED lines are
painted: the diff prefix char just before the content start must
be a `+'.  Context lines are parse input (they help treesit see
the structure) but they carry no changes, so they stay
diff-colored only; deleted lines never reach the new side at all."
  (dolist (r ranges)
    (let ((pair (nth (1- (nth 0 r)) lines)))
      (when (and pair (eq (char-after (1- (car pair))) ?+))
        (let* ((line-len (- (cdr pair) (car pair)))
               (beg (+ (car pair) (min (nth 1 r) line-len)))
               (end (+ (car pair) (min (nth 2 r) line-len)))
               (face (nth 3 r)))
          (when (< beg end)
            (code-review-hunkhighlight--put-face beg end face)))))))

(defun code-review-hunkhighlight-region (beg end path)
  "Apply semantic faces to hunk body BEG..END of file PATH.
BEG is the first body line (after the @@ heading), END the end of
the hunk.  Does nothing when tree-sitter, the grammar or the
language's queries are unavailable, when the file's language is
unknown, or when `code-review-semantic-highlight' is nil.
Never signals; returns non-nil when faces were applied."
  (condition-case err
      (let ((applied nil))
        (when (and code-review-semantic-highlight
                   (fboundp 'treesit-parser-create)
                   (fboundp 'treesit-language-available-p)
                   (or (require 'treesit nil t) t)
                   (let ((lang (code-review-hunkhighlight--language-for
                                path)))
                     (and lang
                          (treesit-language-available-p lang))))
          (let* ((lang (code-review-hunkhighlight--language-for path))
                 (test-p (string-match-p
                          code-review-hunkhighlight-test-path-regexp
                          (downcase path)))
                 (body (buffer-substring-no-properties beg end))
                 ;; the cache key includes the query+face MAPPINGS:
                 ;; cached ranges carry resolved faces, so a
                 ;; changed defcustom must invalidate the cache
                 ;; (learned live: repainting after a face swap
                 ;; silently reapplied the old face)
                 (key (concat (symbol-name lang) "|"
                              (if test-p "t" "nil") "|"
                              (md5 (concat
                                    body "\e"
                                    (prin1-to-string
                                     (cons
                                      (assq lang
                                            code-review-hunkhighlight-queries)
                                      (when test-p
                                        (assq lang
                                              code-review-hunkhighlight-test-queries))))))))
                 (cache (or code-review-hunkhighlight--cache
                            (setq code-review-hunkhighlight--cache
                                  (make-hash-table :test #'equal))))
                 (recon (code-review-hunkhighlight--reconstruct beg end))
                 (text (nth 0 recon))
                 (lines (nth 1 recon)))
            (let ((ranges (or (gethash key cache)
                              (puthash key
                                       (code-review-hunkhighlight--ranges
                                        text lang test-p)
                                       cache))))
              (code-review-hunkhighlight--apply ranges lines)
              (setq applied (not (null ranges))))))
        applied)
    (error
     (message "code-review-hunkhighlight: %S" err)
     nil)))

(defun code-review-hunkhighlight-hunk (section path)
  "Highlight hunk SECTION belonging to file PATH.
Thin wrapper over `code-review-hunkhighlight-region'."
  (when (and section (oref section start) (oref section end))
    (code-review-hunkhighlight-region
     (save-excursion
       (goto-char (oref section start))
       (forward-line)
       (point))
     (oref section end)
     path)))

(defun code-review-hunkhighlight--section-path (section)
  "Best-effort file path for hunk SECTION (code-review or magit)."
  (let ((value (oref section value)))
    (or (cdr (assq 'path (if (listp value) value nil)))
        (let ((parent (magit-section-parent section)))
          (when parent
            (let ((pv (oref parent value)))
              (cond ((stringp pv) pv)
                    ((and (listp pv) (stringp (car pv))) (car pv))
                    (t (magit-section-heading parent)))))))))

;;;###autoload
(defun code-review-hunkhighlight-magit-buffer ()
  "Highlight every hunk in the current magit-style diff buffer.
Works in `magit-diff' buffers and in code-review buffers alike:
walks the magit section tree and applies semantic faces on top of
the diff faces.  Re-runnable: face application merges instead of
duplicating."
  (interactive)
  (when (and code-review-semantic-highlight
             (fboundp 'treesit-parser-create)
             ;; start at the ROOT: walking from the section at point
             ;; would miss every sibling
             (bound-and-true-p magit-root-section))
    (let ((n 0))
      (cl-labels ((walk (section)
                      (dolist (child (oref section children))
                        (if (magit-section-match 'hunk child)
                            (when (code-review-hunkhighlight-hunk
                                   child
                                   (code-review-hunkhighlight--section-path
                                    child))
                              (setq n (1+ n)))
                          (walk child)))))
        (walk magit-root-section))
      (message "code-review-hunkhighlight: %d hunk(s) highlighted" n))))

(provide 'code-review-hunkhighlight)
;;; code-review-hunkhighlight.el ends here
