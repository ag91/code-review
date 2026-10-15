;;; code-review-criteria-home.el --- phase 29: definition-in-test, single local index -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026 Andrea <andrea-dev@hotmail.com>
;;
;; This file is part of code-review.
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the Free Software Foundation; either version 3,
;; or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Phase 29 — CRITERIA HOME V2.  The EARS DEFINITION lives IN THE
;; TEST; specs/ collapses to ONE LOCAL metadata index.
;;
;; The ownership split:
;;   - IN THE TEST (committed, travels with every PR): the
;;     `;;; req-<id>:' BLOCK above the primary ERT test — the
;;     EARS sentence, `covers:' (paths), `covers-fn:'
;;     (functions, display precision), `enforcement:'
;;     (test|runtime|convention), `incident:' tags.  Colleagues
;;     and agents get clearer tests with zero tooling.  In ERT
;;     the sentence may also ride the DOCSTRING (the `C-h f'
;;     bonus); the block is the canonical, portable form.
;;   - IN THE LOCAL INDEX (`specs/index.md', untracked tool
;;     state): id -> priority, verdicts, local-only definitions.
;;     The brief and `p' cycling mutate THIS, never test source.
;;
;; The seam: `code-review-criteria--criteria' appends whatever
;; `code-review-criteria--home-criteria-function' returns (set to
;; `code-review-criteria-home--criteria' at load).  Every
;; consumer (checklist, brief, grill, regression, next-id,
;; enforce-local) keeps working behind the plist shape:
;; (:id :file TEST-FILE :sentence :paths :priority :title
;;  :enforcement :incidents :fns :source 'test|'index).
;;
;; The trigger table's static rows live in the checklist render
;; (`code-review-section-criteria.el'): TEST EDITED = SPEC CHANGE
;; (a diff touching a criterion's definition file rewrites the
;; requirement — the loud row).  The path tripwire stays;
;; execution evidence stays on demand (the C-c C-p probe).
;;
;; The LIFT command (`code-review-criteria-lift-test') is the
;; work-repo operability: on a local review of agent changes,
;; lift an existing test into the ledger — draft the EARS
;; sentence (docstring seed, the phase 23 AI hook, or the
;; skeleton), write the block CO-LOCATED into the test file
;; (it travels with the PR), mint the id, add the index entry.
;; LOCAL-ONLY fallback (C-u): the definition is stored in the
;; index keyed by the test name, flagged not-co-located.
;;
;; Every scan is bounded: one `git ls-files' pass, per-file
;; byte caps, a file-count cap, a total budget, and nothing ever
;; signals into a render.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'code-review-utils)
(require 'code-review-registry)
(require 'code-review-criteria)

(defvar code-review-repo-worktree)         ; code-review-repo.el
(declare-function code-review-criteria--test-file-p
                  "code-review-criteria")
(declare-function code-review-criteria-home--first-string-literal
                  "code-review-criteria-home")

(defcustom code-review-criteria-home-index-file "specs/index.md"
  "Repo-relative LOCAL index: one stanza per req id, holding
the tool's own state (priority, local-only definitions, notes).
Untracked by design (phase 29): the DEFINITION travels with the
test; this file never leaves the machine."
  :type 'file
  :group 'code-review)

(defcustom code-review-criteria-home-max-test-bytes 262144
  "Byte cap per TEST file read by the block scanner."
  :type 'natnum
  :group 'code-review)

(defcustom code-review-criteria-home-max-test-files 200
  "Maximum test files scanned for req blocks in one repository."
  :type 'natnum
  :group 'code-review)

(defcustom code-review-criteria-home-max-total-bytes 2097152
  "Total byte budget for one block scan across all test files."
  :type 'natnum
  :group 'code-review)

;;; The req block (pure parsing)

(defconst code-review-criteria-home--keys
  '("ears" "covers" "covers-fn" "enforcement" "incident")
  "The block keys the scanner knows.  A `key: value' line with
an UNKNOWN key is a CONTINUATION of the previous key (an EARS
sentence may legitimately contain a colon).")

(defun code-review-criteria-home--parse-block (lines)
  "Parse req-block LINES into a plist, or nil (pure).
LINES: the `;;; req-<id>: title' header plus its `;; ' body
lines (no newlines).  Keys (see
`code-review-criteria-home--keys'); `covers'/`covers-fn'/`incident'
repeat, `ears' accumulates continuations joined with single
spaces.  Result: (:id :title :sentence :paths :fns :enforcement
:incidents)."
  (let ((id nil) (title nil) (key nil)
        (ears nil) (covers nil) (fns nil)
        (enforcement nil) (incidents nil))
    (dolist (line lines)
      (cond
       ((not id)                       ; the header line
        (when (string-match
               "\\`;;; req-\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}-[0-9]\\{3\\}\\):[ \t]*\\(.*\\)\\'"
               line)
          (setq id (match-string 1 line)
                title (match-string 2 line))))
       (t
        ;; the body line: strip `;; ' (or a bare `;;') — ONE
        ;; prefix, the right one first (removing `;;' first
        ;; leaves the leading space of `;; ' behind and the
        ;; anchored key regex then never matches)
        (let ((content (if (string-prefix-p ";; " line)
                           (substring line 3)
                         (string-remove-prefix ";;" line))))
          (if (and (string-match
                    "\\`\\([A-Za-z][A-Za-z0-9-]*\\):[ \t]*\\(.*\\)\\'"
                    content)
                   (member (downcase (match-string 1 content))
                           code-review-criteria-home--keys))
              (let ((k (downcase (match-string 1 content)))
                    (v (match-string 2 content)))
                (setq key k)
                (cond
                 ((string= k "ears") (push v ears))
                 ((string= k "covers")
                  (dolist (p (code-review-registry--split-values v))
                    (push p covers)))
                 ((string= k "covers-fn") (push v fns))
                 ((string= k "enforcement") (setq enforcement v))
                 ((string= k "incident") (push v incidents))))
            ;; continuation of the previous key (only the keys
            ;; whose values may span lines)
            (cond
             ((string= key "ears") (push content ears))
             ((string= key "enforcement")
              (setq enforcement
                    (string-trim (concat (or enforcement "") " " content))))))))))
    (and id
         (list :id id
               :title (or title "")
               :sentence (and ears
                             (string-trim
                              (string-join (nreverse ears) " ")))
               :paths (nreverse covers)
               :fns (nreverse fns)
               :enforcement (and enforcement (downcase enforcement))
               :incidents (nreverse incidents)))))

(defun code-review-criteria-home--deftest-docstring (text start)
  "The first ERT test after offset START in TEXT, with the
docstring sentence that carries the EARS definition, as
\(TEST-NAME . SENTENCE) or nil.  Pure, best-effort: string
literals are scanned escape-aware; the first one containing
SHALL wins.  Bounded to 64 KiB past START."
  (ignore-errors
    (let ((pos (string-match "(ert-deftest[ \t\n]+\\([^ \t\n)]+\\)"
                             text start)))
      (when pos
        (let* ((name (match-string 1 text))
               ;; bind BEFORE the next string-match: the global
               ;; match data is clobbered by any nested match
               ;; (the documented trap) — name-end is the scan
               ;; origin, whatever the sentinel search does to
               ;; the match data
               (name-end (match-end 0))
               (sentinel (min (length text) (+ start 65536)))
               (end (or (string-match "\n(ert-deftest[ \t\n]" text
                                      name-end)
                        sentinel))
               (scan name-end)
               (sentence nil))
          (while (and scan (not sentence) (< scan end))
            (let ((open (string-match "\"" text scan)))
              (if (not open)
                  (setq scan nil)
                (let* ((close (string-match
                               "\\(?:[^\"\\]\\|\\\\.\\)*\""
                               text (1+ open)))
                       ;; bind before the candidate's nested
                       ;; string-matches (`--sentence' matches
                       ;; internally — the same trap)
                       (close-end (and close (match-end 0))))
                  (if close
                      ;; close-end is PAST the closing quote: the
                      ;; candidate is the content BETWEEN the
                      ;; quotes — match-beginning 0 of the close
                      ;; match is the OPENING quote's +1 (the
                      ;; match starts there), the classic
                      ;; substring trap
                      (let ((candidate (substring text (1+ open)
                                                  (1- close-end))))
                        (when (string-search "SHALL" candidate)
                          (setq sentence
                                (code-review-criteria--sentence
                                 candidate))))
                    (setq scan nil))
                  (setq scan close-end)))))
          (and sentence (cons name sentence)))))))

(defun code-review-criteria-home--deftest-ears (text start)
  "The EARS definition in the FIRST PARAGRAPH of the docstring
of the first ert-deftest after offset START in TEXT, as
\(TEST-NAME . SENTENCE) or nil.  The CANONICAL phase 29 source:
the sentence is the test's own DESCRIPTION (a =C-h f= bonus,
readable with zero tooling), never a comment line.  Pure,
best-effort, bounded (the literal reader caps its scan)."
  (ignore-errors
    (let ((pos (string-match "(ert-deftest[ \t\n]+\\([^ \t\n)]+\\)"
                             text start)))
      (and pos
           ;; bind BEFORE the next string-match (the trap)
           (let* ((name (match-string 1 text))
                  (name-end (match-end 0))
                  (raw (code-review-criteria-home--first-string-literal
                        text name-end))
                  (first (and raw
                             (code-review-criteria--sentence raw))))
             (and first (string-search "SHALL" first)
                  (cons name first)))))))

(defun code-review-criteria-home--block-criterion (lines file off text)
  "The requirement plist for the req BLOCK LINES (in order) at
byte offset OFF in TEXT from FILE, or nil.  Pure.  The sentence
source order: the following deftest's DOCSTRING FIRST PARAGRAPH
(canonical — the sentence is the test's description), then the
block's own `ears:' key (other languages, or an explicit
override), then the legacy whole-test docstring search; a block
with none of these is nil — a lift candidate, not a criterion."
  (let* ((parsed (code-review-criteria-home--parse-block lines))
         (sentence
          (and parsed
               (or (cdr (code-review-criteria-home--deftest-ears
                         text off))
                   (plist-get parsed :sentence)
                   (cdr (code-review-criteria-home--deftest-docstring
                         text off))))))
    (and parsed sentence
         ;; `--parse-block' always carries the :sentence key:
         ;; this plist-put fills whichever source won
         (append (plist-put parsed :sentence sentence)
                 (list :file file
                       :text sentence
                       :source 'test)))))

(defun code-review-criteria-home--scan-text (text file)
  "The req blocks of TEXT (from FILE), as requirement plists
with :source `test'.  Pure.  The sentence is the following
ert-deftest's DOCSTRING first paragraph (canonical), the
block's `ears:' key, or the legacy docstring search — a block
with none is skipped (a lift candidate, not a criterion).  A
block ends at the first line that is neither `;;' nor another
`;;; req-' header; a new header FLUSHES the pending block
first."
  (let* ((lines (split-string text "\n"))
         (n (length lines))
         (res nil) (block nil) (block-off 0)
         (i 0) (off 0))
    (while (< i n)
      (let ((line (nth i lines)))
        (cond
         ((string-prefix-p ";;; req-" line)
          ;; a new header flushes the pending block first
          (when block
            (let ((req (code-review-criteria-home--block-criterion
                        (nreverse block) file block-off text)))
              (when req (push req res))))
          (setq block (list line) block-off off))
         ((and block (string-prefix-p ";;" line))
          (push line block))
         (block                          ; the block ends here
          (let ((req (code-review-criteria-home--block-criterion
                      (nreverse block) file block-off text)))
            (when req (push req res)))
          (setq block nil)))
      (setq off (+ off 1 (length line)))
      (setq i (1+ i))))
    ;; a block ending at EOF
    (when block
      (let ((req (code-review-criteria-home--block-criterion
                  (nreverse block) file block-off text)))
        (when req (push req res))))
    (nreverse res)))

(defun code-review-criteria-home--unlifted-deftests (text)
  "The ERT test names in TEXT that carry NO req id in their
name — the LIFT candidates.  Pure."
  (let ((res nil) (scan 0))
    (while (and scan (< scan (length text)))
      (let ((pos (string-match "(ert-deftest[ \t\n]+\\([^ \t\n)]+\\)"
                               text scan)))
        (if (not pos)
            (setq scan nil)
          (let ((name (match-string 1 text)))
            (unless (string-match-p
                     "req-[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}-[0-9]\\{3\\}"
                     name)
              (push name res)))
          (setq scan (match-end 0)))))
    (nreverse res)))

;;; The scan (bounded, one git pass)

(defun code-review-criteria-home--test-files (repo)
  "The test files of REPO (repo-relative), or nil.
ONE `git ls-files' pass (tracked plus untracked-not-ignored),
filtered by `code-review-criteria--test-file-p', capped by
`code-review-criteria-home-max-test-files'.  nil on any failure
(a failed scan reads as no criteria, never as a signal)."
  (ignore-errors
    (let ((out (code-review-registry--git
                repo "ls-files" "--cached" "--others"
                "--exclude-standard")))
      (when out
        (let ((files (cl-remove-if-not
                      #'code-review-criteria--test-file-p
                      (split-string out "\n" t))))
          (cl-subseq files 0 (min (length files)
                                  code-review-criteria-home-max-test-files)))))))

(defun code-review-criteria-home--scan-files (repo files)
  "Requirement plists from the req BLOCKS of REPO's FILES.
Per-file and total byte caps; oversized files are skipped and
logged.  Same-id blocks across files MERGE (first sentence and
title win, covers/fns/incidents accumulate) — the block lives in
the primary test, siblings may extend it.  Sorted by id."
  (ignore-errors
    (let ((res nil)
          (total 0))
      (dolist (file (or files ()))
        (let* ((full (expand-file-name file repo))
               (size (or (file-attribute-size
                          (file-attributes full))
                         0)))
          (cond
           ((> size code-review-criteria-home-max-test-bytes)
            (code-review-utils--log
             "code-review-criteria"
             (format "test file skipped (byte cap): %s" file)))
           ((>= total code-review-criteria-home-max-total-bytes)
            (code-review-utils--log
             "code-review-criteria"
             "block scan budget exhausted; remaining files skipped"))
           (t
            (setq total (+ total size))
            (let ((text (with-temp-buffer
                          (insert-file-contents full nil 0
                                                code-review-criteria-home-max-test-bytes)
                          (buffer-substring-no-properties
                           (point-min) (point-max)))))
              (dolist (req (code-review-criteria-home--scan-text text file))
                (let ((existing (assoc (plist-get req :id) res)))
                  (if (not existing)
                      (push (cons (plist-get req :id) req) res)
                    (let ((acc (cdr existing)))
                      (plist-put acc :paths
                                 (append (plist-get acc :paths)
                                         (plist-get req :paths) nil))
                      (plist-put acc :fns
                                 (append (plist-get acc :fns)
                                         (plist-get req :fns) nil))
                      (plist-put acc :incidents
                                 (append (plist-get acc :incidents)
                                         (plist-get req :incidents) nil)))))))))))
      (cl-sort (mapcar #'cdr res) #'string<
                :key (lambda (r) (plist-get r :id))))))

;;; The local index (specs/index.md — untracked tool state)

(defconst code-review-criteria-home--index-header
  "# criteria index (phase 29) — LOCAL tool state.\n# The DEFINITION lives in the tests (;;; req- blocks).  This file\n# holds the tool's own bookkeeping: priority, local-only\n# definitions (not-co-located lifts), notes.  Hand-edit freely;\n# unknown lines are ignored.  Generated stanzas are appended,\n# never removed.\n\n"
  "The index file header (the human contract, in place).")

(defun code-review-criteria-home--parse-index-text (text)
  "The index TEXT as ((ID . PLIST)...), PLIST keys :priority
:definition :not-colocated.  Pure: stanzas open with `## <id>',
lines are `key: value'; a plain (non-key, non-empty) line right
after a `definition:' line is its CONTINUATION; unknown keys and
prose are ignored."
  (let ((res nil) (id nil) (data nil) (last-key nil))
    (dolist (line (split-string (or text "") "\n"))
      (cond
       ((string-match
         "\\`## \\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}-[0-9]\\{3\\}\\)" line)
        (when id (push (cons id data) res))
        (setq id (match-string 1 line)
              data nil last-key nil))
       ((and id (string-match
                 "\\`\\(priority\\|definition\\|not-colocated\\):[ \t]*\\(.*\\)\\'"
                 line))
        (let ((key (match-string 1 line))
              (value (match-string 2 line)))
          (setq last-key key)
          ;; `plist-put' on a fresh (nil) plist RETURNS the new
          ;; plist: setq it back or the first key of every stanza
          ;; is silently lost
          (cond
           ((string= key "priority")
            (when (member value '("P1" "P2" "P3"))
              (setq data (plist-put data :priority value))))
           ((string= key "definition")
            (setq data (plist-put data :definition (string-trim value))))
           ((string= key "not-colocated")
            (setq data (plist-put data :not-colocated t))))))
       ((and id (string= last-key "definition")
             (plist-get data :definition)
             (not (string-empty-p (string-trim line))))
        (setq data (plist-put
                    data :definition
                    (string-trim (concat (plist-get data :definition)
                                          " " (string-trim line))))))))
    (when id (push (cons id data) res))
    (nreverse res)))

(defun code-review-criteria-home--index-set-line (key value)
  "The index line for KEY/VALUE (pure); nil when the key is
REMOVED (a nil PRIORITY removes the line, an unset key line
adds nothing)."
  (cond
   ((eq key :priority)
    (and value (member value '("P1" "P2" "P3"))
         (concat "priority: " value)))
   ((eq key :definition)
    (and value (not (string-empty-p value))
         (concat "definition: " value)))
   ((eq key :not-colocated) (and value "not-colocated: true"))
   (t nil)))

(defconst code-review-criteria-home--index-keys
  '("priority" "definition" "not-colocated")
  "The index keys `--index-rewrite' owns; a rewrite of one key
never disturbs the others.")

(defun code-review-criteria-home--index-key-line (key)
  "The `key:' line prefix the rewrite matches for KEY."
  (concat (pcase key
            (:priority "priority")
            (:definition "definition")
            (_ "not-colocated"))
          ":"))

(defun code-review-criteria-home--index-rewrite (text id key value)
  "Index TEXT with ID's stanza KEY set to VALUE (nil removes the
key's lines).  Pure: the stanza is APPENDED when missing (a
dropped criterion's notes survive — stanzas are never removed);
other stanzas and prose are left untouched.  A rewritten or
removed `definition:' drops its continuation lines with it."
  (let* ((text (or text ""))
         (lines (split-string text "\n"))
         (id-header (concat "## " id))
         (new-line (code-review-criteria-home--index-set-line key value))
         (key-name (code-review-criteria-home--index-key-line key))
         (idx (cl-position-if
               (lambda (l) (string-prefix-p id-header l)) lines)))
    (if (not idx)
        ;; append a fresh stanza (never remove anything), one
        ;; blank line above it
        (concat (if (string-empty-p (string-trim text))
                    ""
                  (concat (string-trim-right text "\n+") "\n\n"))
                (if new-line
                    (concat id-header "\n" new-line "\n")
                  (concat id-header "\n")))
      ;; rewrite inside the existing stanza: [0..idx] kept, the
      ;; stanza body rebuilt, [end..] kept
      (let* ((end (or (cl-position-if
                       (lambda (l) (string-prefix-p "## " l))
                       lines :start (1+ idx))
                      (length lines)))
             (body (cl-subseq lines (1+ idx) end))
             (has-key (cl-some
                       (lambda (l) (string-prefix-p key-name l)) body))
             (new-body nil))
        (cond
         ((not has-key)
          (setq new-body (if new-line (cons new-line body) body)))
         (t
          (let ((skip-continuation nil))
            (dolist (l body)
              (cond
               ((string-prefix-p key-name l)
                (setq skip-continuation (eq key :definition))
                (when new-line (push new-line new-body)))
               (skip-continuation
                (if (or (cl-some (lambda (k)
                                   (string-prefix-p
                                    (concat k ":") l))
                                 code-review-criteria-home--index-keys)
                         (string-prefix-p "## " l)
                         (string-empty-p (string-trim l)))
                    ;; another key line: the continuation is over
                    (progn (setq skip-continuation nil)
                           (push l new-body))
                  ;; a continuation of the replaced/removed
                  ;; definition: dropped with it
                  ))
               (t (push l new-body))))
          (setq new-body (nreverse new-body)))))
        (string-join (append (cl-subseq lines 0 (1+ idx))
                             new-body
                             (cl-subseq lines end))
                     "\n")))))

(defun code-review-criteria-home--index-ensure (text id)
  "Index TEXT with ID's stanza ensured (APPEND-only: an existing
stanza is returned UNTOUCHED — the sync contract: generated
stanzas are never removed, hand edits never clobbered).  Pure."
  (if (string-match-p (concat "^## " (regexp-quote id) "\\(\\'\\|[^0-9-]\\)")
                      (or text ""))
      text
    (concat (if (string-empty-p (string-trim (or text "")))
                ""
              (concat (string-trim-right (or text "") "\n+") "\n\n"))
            "## " id "\n")))

(defun code-review-criteria-home--index-path (repo)
  "The absolute index path of REPO, or nil without REPO."
  (and repo (expand-file-name code-review-criteria-home-index-file repo)))

(defun code-review-criteria-home--index-set (repo id key value)
  "Set ID's KEY (a plist-style keyword) to VALUE in REPO's index.
Creates the file (and the stanza) when missing.  Never signals;
returns the index path or nil."
  (ignore-errors
    (let* ((path (code-review-criteria-home--index-path repo))
           (text (and path (file-exists-p path)
                      (with-temp-buffer
                        (insert-file-contents path)
                        (buffer-substring-no-properties
                         (point-min) (point-max))))))
      (when path
        (make-directory (file-name-directory path) t)
        (with-temp-file path
          (insert (code-review-criteria-home--index-rewrite
                   (or text "") id key value)))
        path))))

(defun code-review-criteria-home--index-ensure-stanza (repo id)
  "Ensure ID has a STANZA in REPO's index (append-only: an
existing stanza is never touched — the sync contract).  Never
signals; returns the index path or nil."
  (ignore-errors
    (let* ((path (code-review-criteria-home--index-path repo))
           (text (and path (file-exists-p path)
                      (with-temp-buffer
                        (insert-file-contents path)
                        (buffer-substring-no-properties
                         (point-min) (point-max))))))
      (when path
        (make-directory (file-name-directory path) t)
        (with-temp-file path
          (insert (code-review-criteria-home--index-ensure
                   (or text "") id)))
        path))))

(defun code-review-criteria-home-set-priority (req priority)
  "Set REQ's PRIORITY in the LOCAL index (durable tool state,
never test source) and return the updated plist.  Never signals."
  (let ((repo (or (bound-and-true-p code-review-repo-worktree)
                  (code-review-registry--repo-dir))))
    (when (and repo (plist-get req :id))
      (code-review-criteria-home--index-set
       repo (plist-get req :id) :priority priority))
    (plist-put req :priority priority)))

;;; The merged provider (the seam target)

(defun code-review-criteria-home--criteria (repo)
  "The v2 requirement plists of REPO: test blocks + index
overlays + local-only index definitions — merged, id-sorted.
Never signals.  Priority comes from the INDEX (tool state); the
sentence and covers come from the TEST BLOCK; a local-only
lifted definition (a C-u lift) synthesizes a criterion whose
:file is the INDEX.  The LEGACY spec-file scan stays upstream:
`code-review-criteria--criteria' appends this provider's result
to its own and never the other way round (no recursion)."
  (ignore-errors
    (let* ((scanned (code-review-criteria-home--scan-files
                     repo (code-review-criteria-home--test-files repo)))
           (index-path (code-review-criteria-home--index-path repo))
           (index-text (and index-path (file-exists-p index-path)
                            (with-temp-buffer
                              (insert-file-contents index-path)
                              (buffer-substring-no-properties
                               (point-min) (point-max)))))
           (index (and index-text
                       (code-review-criteria-home--parse-index-text
                        index-text))))
      (dolist (entry (or index ()))
        (let* ((id (car entry))
               (data (cdr entry))
               (existing (cl-find id scanned
                                   :key (lambda (r) (plist-get r :id))
                                   :test #'string=)))
          (cond
           (existing
            (when (plist-get data :priority)
              (plist-put existing :priority (plist-get data :priority)))
            (plist-put existing :index t))
           ((plist-get data :definition)
            (push (list :id id
                        :file code-review-criteria-home-index-file
                        :sentence (plist-get data :definition)
                        :title "local (index)"
                        :paths nil
                        :text (plist-get data :definition)
                        :priority (plist-get data :priority)
                        :source 'index
                        :index t
                        :not-colocated (plist-get data :not-colocated))
                  scanned)))))
      (cl-sort (or scanned ()) #'string<
                :key (lambda (r) (plist-get r :id))))))

;; the seam (criteria.el appends whatever this returns to the
;; legacy spec-file scan; the defvar is criteria.el's)
(setq code-review-criteria--home-criteria-function
      #'code-review-criteria-home--criteria)

;;; The trigger table's static rows (pure)

(defun code-review-criteria-home--spec-change-p (req diff-paths)
  "Non-nil when this DIFF edits REQ's DEFINITION file — the
SPEC CHANGE row of the phase 29 trigger table: editing the test
block (or, legacy, the spec file) IS editing the requirement."
  (and (plist-get req :file)
       (member (plist-get req :file) (or diff-paths ()))
       t))

(defun code-review-criteria-home--lift-candidates (repo diff-paths)
  "The TEST files of the change under review that carry NO req
blocks — the lift candidates.  Bounded (the file list is the
diff's own, each read is byte-capped)."
  (ignore-errors
    (let ((res nil))
      (dolist (path (or diff-paths ()))
        (when (code-review-criteria--test-file-p path)
          (let* ((full (expand-file-name path repo))
                 (size (or (file-attribute-size (file-attributes full))
                           0)))
            (when (and (file-exists-p full)
                       (<= size code-review-criteria-home-max-test-bytes))
              (let ((text (with-temp-buffer
                            (insert-file-contents full)
                            (buffer-substring-no-properties
                             (point-min) (point-max)))))
                (unless (code-review-criteria-home--scan-text text path)
                  (push path res)))))))
      (nreverse res))))

;;; The block writer (the lift's co-located half)

(defun code-review-criteria-home--block-text (id title covers
                                        enforcement incidents)
  "The req block TEXT for a lift (pure).  Keys only: the EARS
sentence is NOT a block line — it is the test's own DESCRIPTION
(the docstring's first paragraph, written by
`--set-docstring-sentence'); the `;;; req-' header anchors the
id, covers/enforcement/incidents ride their key lines."
  (let ((lines (list (format ";;; req-%s: %s" id (or title "")))))
    (when covers
      (push (format ";; covers: %s" (string-join covers ", ")) lines))
    (when (and enforcement (not (string-empty-p enforcement)))
      (push (format ";; enforcement: %s" enforcement) lines))
    (dolist (inc (or incidents ()))
      (push (format ";; incident: %s" inc) lines))
    (string-join (nreverse lines) "\n")))

(defun code-review-criteria-home--wrap (text width)
  "TEXT split into lines of at most WIDTH columns, breaking on
the LAST space inside the window (never mid-word when a space
falls inside the window).  Pure; an empty TEXT yields no lines."
  (let ((res nil)
        (start 0)
        (n (length (or text ""))))
    (while (< start n)
      (let* ((end (min n (+ start width)))
             (brk (and (< end n)
                       (cl-position ?\s (substring text start end)
                                    :from-end t))))
        (cond
         (brk (push (substring-no-properties text start (+ start brk))
                    res)
              (setq start (+ start brk 1)))
         (t (push (substring-no-properties text start end) res)
            (setq start end)))))
    (nreverse res)))

(defun code-review-criteria-home--insert-block (file test-name block)
  "Insert BLOCK above the \(ert-deftest TEST-NAME ...) form in
FILE, replacing an existing `;;; req-\=' block directly above it
(the migration shape: a bare header grows its body lines — the
block\='s lines are the contiguous `;;\='-prefixed run above the
deftest).  Returns the new text, or nil (no deftest found);
never signals."
  (condition-case _e
      (let* ((text (with-temp-buffer
                     (insert-file-contents file)
                     (buffer-substring-no-properties
                      (point-min) (point-max))))
             (pos (string-match
                   (concat "(ert-deftest[ \t\n]+"
                           (regexp-quote test-name) "[ \t\n]")
                   text)))
        (when pos
          (let* ((nl (and (> pos 0)
                          (cl-position ?\n text :end pos :from-end t)))
                 (line-start (if nl (1+ nl) 0))
                 ;; walk the contiguous `;;' run above the deftest,
                 ;; line by line, topmost line first
                 (run-start line-start)
                 (line-end line-start))
            (while (> line-end 0)
              (let* ((pnl (cl-position ?\n text :end (1- line-end)
                                       :from-end t))
                     (bol (if pnl (1+ pnl) 0))
                     (eol (1- line-end)))
                (if (and (>= eol bol)
                         (string-prefix-p
                          ";;" (substring text bol (1+ eol))))
                    (setq run-start bol line-end bol)
                  (setq line-end 0))))
            ;; the run replaces the block only when it OPENS with
            ;; a `;;; req-\=' header (a bare `;; EARS:\='
            ;; continuation above is not a block); replace mode
            ;; drops the OLD block\='s region, insert mode keeps
            ;; everything above the deftest
            (let* ((replace-p (and (< run-start line-start)
                                   (string-prefix-p
                                    ";;; req-"
                                    (substring text run-start))))
                   (at (if replace-p run-start line-start))
                   (after (if replace-p line-start at))
                   (new (concat (substring text 0 at)
                                block "\n"
                                (substring text after))))
              (with-temp-file file
                (insert new))
              new))))
    (error nil)))


;;; The lift command (work-repo operability)

(defun code-review-criteria-home--set-docstring-sentence
    (file test-name sentence)
  "Make SENTENCE the FIRST PARAGRAPH of TEST-NAME's docstring in
FILE — the description half of the co-located lift: the
definition IS the test's own description, never a comment
line.  An existing first paragraph is REPLACED (the lift seeds
the sentence FROM it — same prose, WHEN/SHALL shape); the
remainder of the docstring survives verbatim; a docstring-less
deftest gets one (only when its arglist closed on the name
line — the usual layout; anything exotic is left alone).
Returns the new text, or nil; never signals."
  (ignore-errors
    (let* ((text (with-temp-buffer
                   (insert-file-contents file)
                   (buffer-substring-no-properties
                    (point-min) (point-max))))
           (pos (string-match
                 (concat "(ert-deftest[ \t\n]+"
                         (regexp-quote test-name) "[ \t\n]")
                 text)))
      (when pos
        (let ((new
               (if (string-match "\"" text pos)
                   ;; a docstring exists: swap its first
                   ;; paragraph for the wrapped sentence
                   (let* ((open (match-beginning 0))
                          (close (string-match
                                  "\\(?:[^\"\\]\\|\\\\.\\)*\""
                                  text (1+ open)))
                          ;; bind before the nested matches (the trap)
                          (close-end (and close (match-end 0))))
                     (and close
                          (let* ((content (substring text (1+ open)
                                                     (1- close-end)))
                                 (rest-at (string-match
                                           "[ \t]*\n[ \t]*\n" content))
                                 (wrapped
                                  (string-join
                                   (code-review-criteria-home--wrap
                                    sentence 70)
                                   "\n"))
                                 (new-content
                                  (if rest-at
                                      (concat wrapped
                                              (substring content rest-at))
                                    wrapped)))
                            (concat (substring text 0 open)
                                    "\"" new-content "\""
                                    (substring text close-end)))))
                 ;; no docstring: give it one after the arglist
                 (let ((nl (string-match "\n" text pos)))
                   (and nl
                        (string-match ")" (substring text pos nl))
                        (concat (substring text 0 (1+ nl))
                                "  \""
                                (string-join
                                 (code-review-criteria-home--wrap
                                  sentence 70)
                                 "\n")
                                "\"\n"
                                (substring text (1+ nl))))))))
          (when new
            (with-temp-file file
              (insert new))
            new))))))

(defun code-review-criteria-lift-test--candidates ()
  "The test files to offer for lifting: the review's CHANGED
test files first, else the repo's test files."
  (or (ignore-errors
        (and (derived-mode-p 'code-review-mode)
             (let* ((paths (code-review-criteria--review-paths))
                    (repo (or code-review-repo-worktree
                              (code-review-registry--repo-dir))))
               (and repo paths
                    (cl-remove-if-not
                     #'code-review-criteria--test-file-p paths)))))
      (let ((repo (code-review-registry--repo-dir)))
        (and repo (code-review-criteria-home--test-files repo)))))

;;;###autoload
(defun code-review-criteria-lift-test (&optional file local-only)
  "Lift a TEST into the criteria ledger (phase 29, work-repo
operability: the loop works where AGENTS.md cannot be
installed).  Drafts the EARS sentence from the test's own
material (docstring seed, then the phase 23 AI hook, then the
skeleton), writes the `;;; req-' block CO-LOCATED into the test
file — it travels with the PR, colleagues get clearer tests —
mints the date-numbered id, and adds the LOCAL index entry.
With LOCAL-ONLY (C-u): the definition is stored in the index
keyed by the test name, flagged not-colocated, and the test file
is NOT touched.  FILE defaults to a completing-read over the
review's changed test files (else the repo's)."
  (interactive
   (list (let ((cands (code-review-criteria-lift-test--candidates)))
           (if (not cands)
               (user-error "No test files to lift (no review at point)")
             (completing-read "Lift test file: " cands nil t)))
         current-prefix-arg))
  (let* ((repo (or (bound-and-true-p code-review-repo-worktree)
                   (code-review-registry--repo-dir))))
    (if (not (and repo file))
        (user-error "No repository here (worktree or repo root)")
      (let* ((full (expand-file-name file repo))
             (text (with-temp-buffer
                     (insert-file-contents full)
                     (buffer-substring-no-properties (point-min)
                                                     (point-max))))
             (names (code-review-criteria-home--unlifted-deftests text)))
        (if (not names)
            (progn
              (message "code-review-criteria: no unlifted tests in %s" file)
              nil)
          (let* ((name (if (cdr names)
                           (completing-read "Lift test: " names nil t)
                         (car names)))
                 (id (code-review-criteria--next-id repo))
                 (slug (file-name-sans-extension
                        (file-name-nondirectory file)))
                 (title (format "%s (%s)" name slug))
                 (covers (cl-remove-if-not
                          (lambda (p)
                            (not (code-review-criteria--test-file-p p)))
                          (or (ignore-errors
                                (code-review-criteria--review-paths))
                              ())))
                 (sentence (code-review-criteria-lift-test--sentence
                            text name repo covers)))
            (if local-only
                (progn
                  (code-review-criteria-home--index-set
                   repo id :definition sentence)
                  (code-review-criteria-home--index-set
                   repo id :not-colocated t)
                  (message "code-review-criteria: %s lifted LOCAL-ONLY \
(index) — definition keyed to %s" id name))
              (let ((block (code-review-criteria-home--block-text
                            id title covers "test" nil)))
                (if (and (code-review-criteria-home--insert-block
                          full name block)
                         (code-review-criteria-home--set-docstring-sentence
                          full name sentence))
                    (message "code-review-criteria: %s lifted CO-LOCATED \
into %s — review the block, it travels with the PR" id file)
                  (message "code-review-criteria: could not place the block \
in %s (no %s deftest found)" file name))))
            ;; the index stanza either way (it exists for `p')
            (code-review-criteria-home--index-ensure-stanza repo id)
            id))))))

(defun code-review-criteria-home--first-string-literal (text start)
  "The first string literal's CONTENT at/after offset START in
TEXT, or nil.  Pure, best-effort (escape-aware), bounded to 4
KiB past START — the LIFT SEED reader: a deftest's docstring is
already behavior prose even when it carries no SHALL (the
definition scanner is SHALL-gated by design; the seed is not)."
  (ignore-errors
    (let ((open (string-match "\"" text start)))
      (and open
           (<= open (+ start 4096))
           (let* ((close (string-match
                          "\\(?:[^\"\\]\\|\\\\.\\)*\"" text (1+ open)))
                  ;; bind before any nested match (the trap):
                  ;; close-end is PAST the closing quote
                  (close-end (and close (match-end 0))))
             (and close
                  (substring text (1+ open) (1- close-end))))))))

(defun code-review-criteria-lift-test--sentence (text name _repo covers)
  "The draft EARS sentence for lifting NAME out of TEXT (pure).
Seed order: NAME's own DOCSTRING (already behavior prose,
rewritten into WHEN/SHALL shape when it carries no SHALL); the
phase 23 AI hook (temp file in, sentence out); the skeleton."
  (let* ((pos (string-match
               (concat "(ert-deftest[ \t\n]+"
                       (regexp-quote name) "[ \t\n]")
               text))
         (seed (and pos
                    (let ((raw (code-review-criteria-home--first-string-literal
                                text pos)))
                      ;; normalize: a multiline docstring is
                      ;; behavior prose, joined into one line
                      (and raw (code-review-criteria--sentence raw))))))
    (cond
     ((and seed (string-search "SHALL" seed)) seed)
     (seed (format "WHEN %s THE SYSTEM SHALL %s" name seed))
     ((functionp code-review-criteria-generate-function)
      (let ((tmp (make-temp-file "cr-lift-" nil ".md")))
        (unwind-protect
            (progn
              (with-temp-file tmp
                (insert code-review-criteria--template))
              (funcall code-review-criteria-generate-function
                       tmp covers)
              (or (code-review-criteria--sentence
                   (with-temp-buffer
                     (insert-file-contents tmp)
                     (buffer-substring-no-properties
                      (point-min) (point-max))))
                  "WHEN <precondition> THE SYSTEM SHALL <postcondition>"))
          (delete-file tmp))))
     (t "WHEN <precondition> THE SYSTEM SHALL <postcondition>"))))

;;; The index sync (generated skeleton, never curated)

;;;###autoload
(defun code-review-criteria-home-index-sync (&optional repo)
  "Ensure every scanned req id of REPO has an index stanza.
The generated-skeleton half of the index: stanzas are APPENDED
\(never removed — a dropped criterion's notes survive), and
EXISTING stanzas are never modified (a `p' cycle or a hand edit
survives a sync — the append-only rewrite, never the priority
clobber).  REPO defaults to the review worktree or the
repository of `default-directory'."
  (interactive)
  (let* ((root (or repo (code-review-registry--repo-dir)))
         (ids (and root
                   (mapcar (lambda (r) (plist-get r :id))
                           (code-review-criteria-home--criteria root)))))
    (dolist (id (or ids ()))
      (code-review-criteria-home--index-ensure-stanza root id))
    (message "code-review-criteria: index synced (%d stanza%s ensured)"
             (length (or ids ()))
             (if (= (length (or ids ())) 1) "" "s"))))

(provide 'code-review-criteria-home)
;;; code-review-criteria-home.el ends here
