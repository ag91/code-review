;;; code-review-dossier.el --- On-demand hunk change context (dossier) -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026

;; Author: Andrea
;; Keywords: git, tools, vc

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Phase 16 (see Improvements.org): the ranking phases say WHERE the
;; delicate spots are; the dossier says WHY, one keypress away.
;; `C-c C-h' on a diff hunk inserts a collapsible "Context
;; (dossier)" section right after the hunk:
;;
;;  - HISTORY of exactly those lines: one bounded `git log
;;    --no-patch -L' at the base rev (pre-PR commits only, capped),
;;    with authors, dates and subjects.
;;  - Blame authors of the old (context+deleted) lines and the most
;;    recent touch ("last touched 3m ago by mrossi").
;;  - All call sites of the definitions the hunk touches, as jump
;;    buttons into the worktree, split into code and tests.
;;  - The file's heat facts (phase 14) and the hunk's phase 15 risk
;;    reasons.
;;
;; On demand only — zero render cost — and cached per (PR id, diff
;; md5, path, hunk ranges).  On promisor/partial clones the
;; blob-lazy-fetching git calls are governed by defcustoms and any
;; skip is reported, never silent.
;;
;; OPTIONAL AI garnish, OFF by default: with
;; `code-review-dossier-llm-summarizer' set, `C-u C-c C-h' pipes the
;; DOSSIER (not the PR) to that function and renders its output
;; marked "(machine summary)".  Every claim must stay traceable to a
;; dossier entry; the buffer is fully useful with the feature off.

;;; Code:

(require 'magit-section)
(require 'cl-lib)
(require 'code-review-utils)
(require 'code-review-diff)
(require 'code-review-analysis)
(require 'code-review-history)

(declare-function code-review-section--insert-analysis-jump
                  "code-review-section-analysis")
(declare-function code-review-section--find-hunk-section
                  "code-review-section-analysis")
(declare-function code-review-db--pullreq-raw-diff "code-review-db")
(declare-function code-review-db-get-pullreq "code-review-db")

;;; Configuration

(defcustom code-review-dossier-enabled t
  "When non-nil, `C-c C-h' inserts the on-demand hunk dossier.
The dossier never runs during a render; this only gates the
interactive command."
  :type 'boolean
  :group 'code-review-dossier)

(defcustom code-review-dossier-history-limit 5
  "How many commits of line history the dossier shows.
`git log -L' is capped at this many commits per hunk."
  :type 'natnum
  :group 'code-review-dossier)

(defcustom code-review-dossier-max-defs 5
  "How many touched definitions the dossier follows call sites for."
  :type 'natnum
  :group 'code-review-dossier)

(defcustom code-review-dossier-max-callsites 10
  "How many call sites per definition the dossier shows."
  :type 'natnum
  :group 'code-review-dossier)

(defcustom code-review-dossier-test-path-regexp
  "\\`test\\|/tests?/\\|/specs?/\\|_test\\.\\|\\.test\\.\\|_spec\\.\\|\\.spec\\."
  "Regexp matching file paths that are test files.
Matched against the DOWNCASED path (mirrors
`code-review-hunkhighlight-test-path-regexp').  Used to split a
definition's call sites into code and tests."
  :type 'regexp
  :group 'code-review-dossier)

(defcustom code-review-dossier-partial-clone-history nil
  "When nil, skip the `git log -L' line history on promisor clones.
On a partial (promisor) clone `git log -L' lazy-fetches a blob per
historical version of the ONE file — bounded by
`code-review-dossier-history-limit' and by one file, unlike the
phase 15 blame — so the skip is REPORTED in the dossier (never
silent).  Set non-nil to run it anyway (a message is shown once
per repository the first time)."
  :type 'boolean
  :group 'code-review-dossier)

(defcustom code-review-dossier-llm-summarizer nil
  "Function turning dossier text into a short summary, or nil.
When set, `C-u C-c C-h' calls it with the rendered dossier text
and inserts the return value marked \"(machine summary)\".  OFF by
default (the deterministic facts are the point; see
Improvements.org phase 16 for the restraint argument)."
  :type '(choice (const :tag "Off" nil) function)
  :group 'code-review-dossier)

;;; Pure helpers

(defun code-review-dossier--parse-log-L (out)
  "Parse `git log -L' OUT (\\x1f-separated sha/author/date/subject).
One entry per commit, newest first; malformed lines are skipped.
Pure."
  (let ((res nil))
    (dolist (line (split-string out "\n" t))
      (let ((capped (code-review-analysis--cap-line line)))
        ;; bind the match data BEFORE anything else can match
        (when (string-match
               "\\`\\(.+?\\)\x1f\\(.+?\\)\x1f\\(.+?\\)\x1f\\(.+\\)\\'"
               capped)
          (let ((sha (match-string-no-properties 1 capped))
                (author (match-string-no-properties 2 capped))
                (date (match-string-no-properties 3 capped))
                (subject (match-string-no-properties 4 capped)))
            (push (list sha author date subject) res)))))
    (nreverse res)))

(defun code-review-dossier--old-range (hunk)
  "The old-side (context+deleted) line span of HUNK, (MIN . MAX).
nil when the hunk only adds brand new lines.  HUNK is a plist from
`code-review-analysis--split-hunks'.  Pure."
  (let ((lines (mapcar #'car (plist-get hunk :old))))
    (when lines
      (cons (apply #'min lines) (apply #'max lines)))))

(defun code-review-dossier--hunk-for (diff path ranges)
  "The split-hunks plist for PATH+RANGES in raw DIFF text.
RANGES is the raw @@ ranges text (the hunk key).  nil on a miss.
Pure."
  (let* ((blocks (code-review--diff--split-by-files diff))
         (block (cdr (assoc path blocks))))
    (cl-loop for hu in (and block
                            (code-review-analysis--split-hunks block))
              thereis (when (equal (plist-get hu :ranges) ranges)
                        hu))))

(defun code-review-dossier--defs (path hunk)
  "Definitions touched by HUNK of PATH: ((NAME . LINE)...).
Added-side definitions first, then deleted-side, deduped by name,
capped at `code-review-dossier-max-defs'.  Pure."
  (let* ((added (plist-get hunk :added))
         (deleted (plist-get hunk :deleted))
         (all (append (code-review-analysis--definitions-in path added)
                      (code-review-analysis--definitions-in path deleted)))
         (res nil))
    (dolist (pair all)
      (unless (assoc (car pair) res)
        (push pair res)))
    (setq res (nreverse res))
    (cl-subseq res 0 (min (length res) code-review-dossier-max-defs))))

(defun code-review-dossier--classify-callsites (hits)
  "Split call-site HITS ((PATH LINE TEXT)...) into tests and code.
`code-review-dossier-test-path-regexp' (downcased path match)
decides.  Return (:code HITS :tests HITS).  Pure."
  (let ((code nil)
        (tests nil))
    (dolist (hit hits)
      (let ((path (nth 0 hit)))
        (if (and code-review-dossier-test-path-regexp
                 (string-match-p code-review-dossier-test-path-regexp
                                 (downcase path)))
            (push hit tests)
          (push hit code))))
    (list :code (nreverse code) :tests (nreverse tests))))

(defun code-review-dossier--cache-key (pr-id diff path ranges)
  "Cache key: (PR id, diff md5, path, hunk ranges).  Pure."
  (concat pr-id "|" (md5 diff) "|" path "|" ranges))

;;; Git engine

(defun code-review-dossier--log-L (worktree rev path lo hi)
  "Pre-REV commits that touched PATH lines LO..HI, newest first.
One bounded `git log --no-patch -L' call in WORKTREE, capped at
`code-review-dossier-history-limit'; the rev limit keeps the PR's
own commits out of the lore.  nil when there is no history (or
the rev does not exist: git errors to a discarded stderr and the
output stays empty)."
  (when (and (stringp rev) (natnump lo) (>= hi lo) (>= lo 1))
    (let ((buf (generate-new-buffer " *code-review-dossier*")))
      (unwind-protect
          (with-current-buffer buf
            (setq default-directory worktree)
            ;; NB: the --format value must NOT pass through elisp
            ;; `format' (it would interpret git's %h as a directive)
            (apply #'call-process "git" nil (list (current-buffer) nil) nil
                   (list "log" "--no-patch"
                         "--format=%h%x1f%an%x1f%ad%x1f%s"
                         "--date=short"
                         (format "-n%d" code-review-dossier-history-limit)
                         (format "-L%d,%d:%s" lo hi path)
                         rev))
            (code-review-dossier--parse-log-L
             (buffer-substring-no-properties (point-min) (point-max))))
        (kill-buffer buf)))))

(defun code-review-dossier--compute (worktree diff old-rev path ranges)
  "The engine dossier for the hunk PATH+RANGES of DIFF in WORKTREE.
OLD-REV is the diff's old-side rev (the pre-PR lore limit).  Return
a plist (:path :ranges :history :history-skipped :last-touch
:authors :defs :calls), nil when the diff has no such hunk.
:history is ((SHA AUTHOR DATE SUBJECT)...) from `git log -L';
:authors ((AUTHOR . COUNT)...) and :last-touch (AUTHOR . AGE-DAYS)
from one bounded blame of the old lines; :calls maps each touched
definition to its worktree call sites.  On a partial (promisor)
clone the blob-lazy-fetching calls are skipped per their
defcustoms and the skip is REPORTED via :history-skipped.  This
runs ON DEMAND only (C-c C-h), never during a render."
  (let* ((hunk (code-review-dossier--hunk-for diff path ranges))
         (old-range (and hunk (code-review-dossier--old-range hunk)))
         (partial-p (and hunk (code-review-analysis--partial-clone-p
                               worktree)))
         (history-skip
          (cond
           ((not hunk) 'no-hunk)
           ((not old-rev) "no base rev")
           ((not old-range) "new lines")
           ((and partial-p (not code-review-dossier-partial-clone-history))
            "partial clone")))
         (history
          (when (and old-rev old-range (not history-skip))
            (when (and partial-p
                       (not (gethash (expand-file-name worktree)
                                     code-review-dossier--messaged)))
              (puthash (expand-file-name worktree) t
                       code-review-dossier--messaged)
              (message "code-review: partial clone: \`git log -L' may \
lazy-fetch blobs of %s (bounded to one file; \
code-review-dossier-partial-clone-history to skip)" path))
            (code-review-dossier--log-L
             worktree old-rev path (car old-range) (cdr old-range))))
         (blame
          (when (and hunk old-rev old-range
                     (or code-review-analysis-blame-partial-clones
                         (not partial-p)))
            (code-review-analysis--blame
             worktree old-rev path (list old-range))))
         (old-lines (and hunk (mapcar #'car (plist-get hunk :old))))
         (authors
          (when blame
            (let ((counts nil))
              (dolist (ln old-lines)
                (let ((hit (gethash ln blame)))
                  (when hit
                    (let ((cell (assoc (car hit) counts)))
                      (if cell
                          (setcdr cell (1+ (cdr cell)))
                        (push (cons (car hit) 1) counts))))))
              (sort counts
                    (lambda (a b)
                      (or (> (cdr a) (cdr b))
                          (and (= (cdr a) (cdr b))
                               (string< (car a) (car b)))))))))
         (last-touch
          (when blame
            (let ((best nil))
              (dolist (ln old-lines)
                (let ((hit (gethash ln blame)))
                  (when (or (not best) (and hit (> (cdr hit) (cdr best))))
                    (setq best hit))))
              (when best
                (cons (car best)
                      (max 0.0 (/ (- (float-time) (cdr best)) 86400.0)))))))
         (defs (and hunk (code-review-dossier--defs path hunk)))
         (calls
          (when defs
            (mapcar (lambda (def)
                      (let* ((name (car def))
                             (hits (code-review-analysis--references
                                    worktree name))
                             (hits (cl-subseq
                                    hits 0
                                    (min (length hits)
                                         code-review-dossier-max-callsites))))
                        (cons name hits)))
                    defs))))
    (when hunk
      (list :path path
            :ranges ranges
            :history history
            :history-skipped (and (stringp history-skip) history-skip)
            :last-touch last-touch
            :authors authors
            :defs defs
            :calls calls))))

;;; Cache

(defvar code-review-dossier--cache (make-hash-table :test #'equal)
  "Dossier results keyed by (PR id, diff md5, path, hunk ranges).")

(defvar code-review-dossier--messaged (make-hash-table :test #'equal)
  "Repositories already messaged about partial-clone log -L.")

(defun code-review-dossier--risk (path ranges)
  "The phase 15 risk reasons for hunk PATH+RANGES, or nil.
Reads the analysis CACHE (`code-review-analysis-run' is a cache
hit at on-demand time: the Analysis section runs before the diff
wash in `code-review-sections-hook')."
  (let* ((res (code-review-analysis-run))
         (entry (cl-loop for e in (and res (plist-get res :hunks))
                         thereis (when (and (equal (plist-get e :path) path)
                                            (equal (plist-get e :ranges)
                                                   ranges))
                                   e)))
         (reasons (and entry
                       (code-review-analysis--hunk-reasons entry))))
    (when reasons
      (string-join reasons "; "))))

(defun code-review-dossier--get (path ranges)
  "The dossier for hunk PATH+RANGES of the current review.
Derives its inputs from the db (raw diff, PR — hence the base
rev), enriches the engine result with the file's phase 14 heat
(from the render's history order) and the hunk's phase 15 risk,
and caches per (PR id, diff md5, path, hunk ranges).  nil when
there is no worktree, no diff, or the compute failed (a failing
dossier is logged and never takes the command down)."
  (let* ((worktree code-review-repo-worktree)
         (diff (and worktree (code-review-db--pullreq-raw-diff)))
         (pr (and diff (code-review-db-get-pullreq))))
    (when (and worktree diff pr)
      (let ((key (code-review-dossier--cache-key (oref pr id) diff
                                                  path ranges)))
        (or (gethash key code-review-dossier--cache)
            (let* ((old-rev (code-review-analysis--old-rev
                             (oref pr base-ref-name)))
                   (res (condition-case err
                            (code-review-dossier--compute
                             worktree diff old-rev path ranges)
                          (error
                           (code-review-utils--log
                            "code-review-dossier"
                            (format "dossier failed, skipping (%s): %S"
                                    key err))
                           nil))))
              (when res
                (plist-put res :heat
                           (cl-find path code-review-history--order
                                    :key (lambda (e)
                                           (plist-get e :path))
                                    :test #'equal))
                (plist-put res :risk (code-review-dossier--risk
                                      path ranges))
                (puthash key res code-review-dossier--cache)
                res)))))))

(defun code-review-dossier-reset ()
  "Forget all cached dossiers."
  (interactive)
  (clrhash code-review-dossier--cache))

;;; Machine summary (optional, off by default)

(defun code-review-dossier--text (dossier)
  "Plain-text render of DOSSIER (no faces, no buttons) for the
optional LLM summarizer: every fact it may phrase must come from
this text.  Pure."
  (string-join
   (delq nil
         (list (format "file: %s" (plist-get dossier :path))
               (format "hunk: %s" (plist-get dossier :ranges))
               (let ((touch (plist-get dossier :last-touch)))
                 (when touch
                   (format "last touched %s ago by %s"
                           (code-review-analysis--age-string
                            (cdr touch))
                           (car touch))))
               (let ((authors (plist-get dossier :authors)))
                 (when authors
                   (format "authors of these lines: %s"
                           (string-join
                            (mapcar (lambda (a)
                                      (format "%s (%d)"
                                              (car a) (cdr a)))
                                    authors)
                            ", "))))
               (let ((history (plist-get dossier :history)))
                 (when history
                   (format "history (newest first): %s"
                           (string-join
                            (mapcar (lambda (e)
                                      (format "%s %s %s: %s"
                                              (nth 0 e) (nth 2 e)
                                              (nth 1 e) (nth 3 e)))
                                    history)
                            "; "))))
               (plist-get dossier :risk)
               (let ((heat (plist-get dossier :heat)))
                 (when heat
                   (format "file heat: %s — %s"
                           (plist-get heat :bucket)
                           (plist-get heat :reason))))
               (let ((defs (plist-get dossier :defs))
                     (calls (plist-get dossier :calls)))
                 (when defs
                   (format "touched defs: %s"
                           (string-join
                            (mapcar
                             (lambda (def)
                               (let* ((name (car def))
                                      (hits (cdr (assoc name calls))))
                                 (format "%s <- %s" name
                                         (if hits
                                             (string-join
                                              (mapcar (lambda (h)
                                                        (format "%s:%d"
                                                                (nth 0 h)
                                                                (nth 1 h)))
                                                      hits)
                                              ", ")
                                           "no call sites"))))
                             defs)
                            "; ")))))) "\n"))

(defun code-review-dossier--machine-summary (text)
  "The LLM summary of TEXT, or nil (off by default).
Calls `code-review-dossier-llm-summarizer' when set; a failing
summarizer is logged and reported, never raised."
  (when code-review-dossier-llm-summarizer
    (condition-case err
        (funcall code-review-dossier-llm-summarizer text)
      (error
       (code-review-utils--log
        "code-review-dossier"
        (format "llm summarizer failed: %S" err))
       (message "code-review: LLM summarizer failed (see \
*code-review-log*); no machine summary")
       nil))))

;;; Rendering

(defclass code-review-dossier-section (magit-section)
  ((path :initarg :path)
   (ranges :initarg :ranges)))

(defun code-review-dossier--hunk-section-at-point ()
  "The hunk section at or above point, or nil.
Point inside a dossier section (where the command leaves it)
resolves to the hunk the dossier belongs to, so a second press
(e.g. C-u for the machine summary) keeps working."
  (let ((sec (magit-current-section)))
    (while (and sec
                (not (memq (eieio-object-class sec)
                           '(magit-hunk-section
                             code-review-dossier-section))))
      (setq sec (and (slot-boundp sec 'parent) (oref sec parent))))
    (if (and sec (eq (eieio-object-class sec) 'code-review-dossier-section))
        (code-review-section--find-hunk-section
         (cdr (assq 'path (oref sec value)))
         (cdr (assq 'ranges (oref sec value))))
      sec)))

(defun code-review-dossier--find (hunk-sec)
  "The dossier section for HUNK-SEC already in the buffer, or nil."
  (let ((res nil)
        (ranges (cdr (assq 'ranges (oref hunk-sec value)))))
    (dolist (c (oref (oref hunk-sec parent) children))
      (when (and (null res)
                 (eq (eieio-object-class c) 'code-review-dossier-section)
                 (equal (cdr (assq 'ranges (oref c value))) ranges))
        (setq res c)))
    res))

(defun code-review-dossier--remove (sec)
  "Remove dossier section SEC from the buffer and its parent."
  (let ((parent (oref sec parent))
        (inhibit-read-only t))
    (delete-region (oref sec start) (oref sec end))
    (oset parent children (delq sec (oref parent children)))))

(defun code-review-dossier--insert-jumps (hits)
  "Insert comma-separated jump buttons for call-site HITS.
`code-review-dossier-max-callsites' caps them (the compute already
capped; the render double-checks)."
  (let ((first t))
    (dolist (hit (cl-subseq hits 0
                            (min (length hits)
                                 code-review-dossier-max-callsites)))
      (unless first (insert ", "))
      (setq first nil)
      (code-review-section--insert-analysis-jump
       code-review-repo-worktree (nth 0 hit) (nth 1 hit)))))

(defun code-review-dossier--render-ages (dossier)
  "The last-touch and blame-authors line of DOSSIER."
  (let ((touch (plist-get dossier :last-touch))
        (authors (plist-get dossier :authors)))
    (when (or touch authors)
      (insert "  ")
      (when touch
        (insert (format "last touched %s ago by %s"
                        (code-review-analysis--age-string (cdr touch))
                        (car touch))))
      (when (and touch authors) (insert " — "))
      (when authors
        (insert "authors of these lines: ")
        (insert (string-join
                 (mapcar (lambda (a) (format "%s (%d)" (car a) (cdr a)))
                         authors)
                 ", ")))
      (insert ?\n))))

(defun code-review-dossier--render-history (dossier)
  "The line-history block of DOSSIER."
  (let ((history (plist-get dossier :history))
        (skipped (plist-get dossier :history-skipped)))
    (cond
     (history
      (insert "  ")
      (insert (propertize "history of these lines (newest first):"
                          'font-lock-face 'magit-dimmed))
      (insert ?\n)
      (dolist (e history)
        (insert "    ")
        (insert (propertize (format "%-8s" (nth 0 e))
                            'font-lock-face 'magit-dimmed))
        (insert (propertize (format "%-11s" (nth 2 e))
                            'font-lock-face 'magit-dimmed))
        (insert (format "%s — %s\n" (nth 1 e) (nth 3 e)))))
     (skipped
      (insert "  ")
      (insert (propertize (format "history: %s" skipped)
                          'font-lock-face 'magit-dimmed))
      (insert ?\n)))))

(defun code-review-dossier--render-context-lines (dossier)
  "The risk (phase 15) and heat (phase 14) lines of DOSSIER."
  (let ((risk (plist-get dossier :risk))
        (heat (plist-get dossier :heat)))
    (when risk
      (insert "  ")
      (insert (propertize "risk: " 'font-lock-face 'magit-dimmed))
      (insert (format "%s\n" risk)))
    (when heat
      (insert "  ")
      (insert (propertize "heat: " 'font-lock-face 'magit-dimmed))
      (insert (format "%s — %s\n"
                      (plist-get heat :bucket)
                      (plist-get heat :reason))))))

(defun code-review-dossier--render-calls (dossier)
  "The touched-defs and call-site lines of DOSSIER."
  (let ((defs (plist-get dossier :defs))
        (calls (plist-get dossier :calls)))
    (when defs
      (insert "  ")
      (insert (propertize "touched defs: "
                          'font-lock-face 'magit-dimmed))
      (insert (format "%s\n" (string-join (mapcar #'car defs) ", ")))
      (dolist (def defs)
        (let* ((name (car def))
               (res (code-review-dossier--classify-callsites
                     (cdr (assoc name calls))))
               (code (plist-get res :code))
               (tests (plist-get res :tests)))
          (when code
            (insert (format "  %s called from: " name))
            (code-review-dossier--insert-jumps code)
            (insert ?\n))
          (when tests
            (insert (format "  tests referencing %s: " name))
            (code-review-dossier--insert-jumps tests)
            (insert ?\n))
          (when (and (not code) (not tests))
            (insert "  ")
            (insert (propertize (format "%s: no call sites found" name)
                                'font-lock-face 'magit-dimmed))
            (insert ?\n)))))))

(defun code-review-dossier--render-summary (dossier)
  "The marked machine summary of DOSSIER, when there is one."
  (let ((summary (plist-get dossier :summary)))
    (when summary
      (insert "  ")
      (insert (propertize "(machine summary)"
                          'font-lock-face 'magit-dimmed))
      (insert ?\n)
      (insert "  ")
      (insert (propertize summary 'font-lock-face 'italic))
      (insert ?\n))))

(defun code-review-dossier--insert (hunk-sec dossier)
  "Insert the collapsible Context section for HUNK-SEC after the
hunk, rendering DOSSIER (a `code-review-dossier--get' result).
An existing dossier for the same hunk is REPLACED.  Return the
section."
  (let ((existing (code-review-dossier--find hunk-sec)))
    (when existing
      (code-review-dossier--remove existing)))
  (let ((parent (oref hunk-sec parent)))
    (goto-char (oref hunk-sec end))
    (let* ((inhibit-read-only t)
           (magit-insert-section--parent parent)
           (sec (magit-insert-section
                    (code-review-dossier-section
                     `((path . ,(plist-get dossier :path))
                       (ranges . ,(plist-get dossier :ranges)))
                     nil
                     :path (plist-get dossier :path)
                     :ranges (plist-get dossier :ranges))
                  (insert (propertize "Context"
                                      'font-lock-face 'magit-section-heading))
                  (insert (propertize " (dossier)"
                                      'font-lock-face 'magit-dimmed))
                  (magit-insert-heading)
                  (code-review-dossier--render-ages dossier)
                  (code-review-dossier--render-history dossier)
                  (code-review-dossier--render-context-lines dossier)
                  (code-review-dossier--render-calls dossier)
                  (code-review-dossier--render-summary dossier)
                  (insert ?\n))))
      ;; `magit-insert-section--finish' nconc'd the section at the
      ;; END of the parent's children; move it right after the hunk
      ;; so navigation order matches the buffer order
      (let* ((kids (delq sec (oref parent children)))
             (pos (cl-position hunk-sec kids)))
        (oset parent children
              (if pos
                  (append (cl-subseq kids 0 (1+ pos))
                          (list sec)
                          (nthcdr (1+ pos) kids))
                (append kids (list sec)))))
      (goto-char (oref sec start))
      sec)))

;;; The command

(defun code-review-dossier-hunk (&optional arg)
  "Insert the change-context dossier for the hunk at point.
\\[code-review-dossier-hunk] inserts a collapsible \"Context
(dossier)\" section right after the hunk: the history of exactly
those lines (`git log -L' at the base rev, pre-PR commits only),
their blame authors and last touch, all call sites of the touched
definitions (jump buttons into the worktree, tests split out),
the file's heat and the hunk's risk reasons.  Cached per (PR,
diff, hunk); a re-render removes dossiers (they are on demand),
but the cache makes re-summoning instant.

With \\[universal-argument] ARG and
`code-review-dossier-llm-summarizer' set, append its summary of
the DOSSIER marked \"(machine summary)\" (off by default; see
Improvements.org phase 16 for the restraint argument)."
  (interactive "P")
  (unless code-review-dossier-enabled
    (user-error "Dossier disabled (code-review-dossier-enabled)"))
  (unless code-review-repo-worktree
    (user-error "No worktree for this review; the dossier needs the repository"))
  (let ((hunk-sec (code-review-dossier--hunk-section-at-point)))
    (unless hunk-sec
      (user-error "Point is not in a diff hunk"))
    (let* ((path (cdr (assq 'path (oref hunk-sec value))))
           (ranges (cdr (assq 'ranges (oref hunk-sec value))))
           (dossier (code-review-dossier--get path ranges)))
      (unless dossier
        (user-error "No context found for this hunk"))
      (when (equal arg '(4))
        (let ((summary (code-review-dossier--machine-summary
                        (code-review-dossier--text dossier))))
          (if summary
              (plist-put dossier :summary summary)
            (unless code-review-dossier-llm-summarizer
              (message "code-review: no LLM summarizer configured \
\(code-review-dossier-llm-summarizer); showing the deterministic \
dossier only")))))
      (code-review-dossier--insert hunk-sec dossier))))

(provide 'code-review-dossier)
;;; code-review-dossier.el ends here
