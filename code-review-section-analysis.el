;;; code-review-section-analysis.el --- the analysis and review-order sections -*- lexical-binding: t; -*-
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

;; the analysis and review-order sections (part of the code-review section rendering, split from
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
(require 'code-review-testimpact)
(require 'code-review-coupling)
(require 'code-review-criteria)

(declare-function code-review-browse--reveal "code-review-browse")
(declare-function code-review-diff--strip-path "code-review-diff")



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
(defun code-review-section--insert-analysis-similar (worktree similar)
  "Insert the SIMILAR findings (each: diff-path total repo-path
covered lo hi), with a jump button into WORKTREE."
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
    (insert ?\n)))
(defun code-review-section--insert-analysis-dead (dead)
  "Insert the DEAD findings (each: name path line)."
  (pcase-dolist (`(,name ,path ,line) dead)
    (insert "  ")
    (insert (propertize "possibly dead: " 'font-lock-face 'magit-dimmed))
    (insert (format "%s (added in %s:%s; no references found in the "
                    name path line))
    (insert "worktree)\n")))
(defun code-review-section--insert-analysis-dangling (worktree dangling)
  "Insert the DANGLING findings (each: name path refs), the first
three refs as jump buttons into WORKTREE."
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
    (insert ")\n")))
(defun code-review-section--insert-analysis-delicate (res)
  "Insert the delicate-hunk jump list from RES (phase 15): the
top-K entries at or above `code-review-analysis-delicacy-threshold',
hottest first, with in-buffer jump buttons."
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
                        (plist-get e :score) reasons))))))
(defun code-review-section--insert-testimpact-ci (tres)
  "Insert the phase 17 CI-gaming findings from TRES (HOISTED to
the top of the Analysis section: skip markers, `|| true' in CI
commands, newly gated/removed workflow steps, lowered coverage
thresholds), each with a jump button into the worktree."
  (let ((worktree code-review-repo-worktree))
    (pcase-dolist (f (plist-get tres :ci))
      (insert "  ")
      (insert (propertize
               (format "ci-gaming: %s in "
                       (code-review-testimpact--kind-label
                        (plist-get f :kind)))
               'font-lock-face 'font-lock-warning-face))
      (code-review-section--insert-analysis-jump
       worktree (plist-get f :path) (plist-get f :line))
      (insert (format " — \"%s\"\n" (plist-get f :text))))))

(defun code-review-section--insert-testimpact-notest (tres)
  "Insert the phase 17 no-test-coverage findings from TRES: hunks
whose touched definitions are USED in the worktree but referenced
by no test file.  The button jumps inside the review buffer, like
the delicate-hunk list."
  (dolist (e (plist-get tres :notest))
    (let ((path (plist-get e :path))
          (ranges (plist-get e :ranges))
          (defs (plist-get e :defs)))
      (insert "  ")
      (insert (propertize "no test coverage: "
                          'font-lock-face 'font-lock-warning-face))
      (insert-button (format "%s %s" path ranges)
                     'face 'code-review-url-header-face
                     'follow-link t
                     'help-echo "Jump to this hunk in the review"
                     'action (lambda (&rest _)
                               (code-review-section--goto-hunk-section
                                path ranges)))
      (insert (format " — %s\n" (string-join defs ", "))))))

(defun code-review-section--insert-coupling (cres)
  "Insert the phase 18 change-coupling completeness findings from
CRES: a changed file whose historical CO-CHANGE PEERS the PR does
not touch.  The button jumps to the untouched peer in the
worktree — the thing to go read to decide whether the refactor is
complete."
  (let ((worktree code-review-repo-worktree))
    (dolist (f (plist-get cres :findings))
      (let ((path (plist-get f :path))
            (peer (plist-get f :peer))
            (co (plist-get f :co))
            (degree (plist-get f :degree)))
        (insert "  ")
        (insert (propertize "completeness: "
                           'font-lock-face 'font-lock-warning-face))
        (insert (format "%s changes together with " path))
        (code-review-section--insert-analysis-jump worktree peer 1)
        (insert (format " (%d%% of %s's revisions, %d co-changes) — \
untouched in this PR: %s\n"
                        (round (* 100 degree))
                        path
                        co
                        (if (plist-get f :test-p)
                            "its historically coupled tests were left \
behind"
                          "is the refactor complete?")))))))

(defun code-review-section--criteria-trace-findings ()
  "The phase 24 traceability findings of the current review (or
nil: no diff, no PR, no criteria in the diff, or a disabled or
failed engine — the pass never signals, a failure reads as no
findings).  Cached per (PR, diff) by the engine
(`code-review-criteria--traceability-run')."
  (let* ((diff (code-review-db--pullreq-raw-diff))
         (pr-id (or code-review-db--pullreq-id
                    (and diff
                         (ignore-errors
                           (oref (code-review-db-get-pullreq) id)))))
         (findings (and diff
                        (code-review-criteria--traceability-run
                         pr-id diff))))
    findings))

(defun code-review-section--insert-criteria-trace ()
  "Insert the phase 24 criteria traceability findings (flipped
in phase 29).  STRONG: the requirement's DEFINITION is edited by
this diff — a legacy criteria file, or an edited `;;; req-<id>:'
block header line (TEST EDITED = SPEC CHANGE: editing the
definition IS editing the requirement).  HINT: a test hunk
referencing a req ID whose definition the diff does not touch
(weaker).  Both files are in the diff, so the jump button goes
to the file-section INSIDE the review buffer."
  (dolist (f (code-review-section--criteria-trace-findings))
    (let* ((kind (plist-get f :kind))
           (id (plist-get f :id))
           (path (plist-get f :path))
           (strong-p (eq kind 'strong)))
      (insert "  ")
      (insert (propertize
               (if strong-p "(traceability) " "(traceability, hint) ")
               'font-lock-face (if strong-p
                                   'font-lock-warning-face
                                 'magit-dimmed)))
      (insert (propertize (format "req-%s " id)
                          'font-lock-face
                          (if strong-p
                              'font-lock-warning-face
                            'magit-dimmed)))
      (insert (if strong-p "modified in " "referenced by a test in "))
      (insert-button (format "%s" path)
                     'face 'code-review-url-header-face
                     'follow-link t
                     'help-echo "Jump to this file in the review"
                     'action (lambda (&rest _)
                               (code-review-section--goto-file-section
                                path)))
      (insert (if strong-p
                 " — no test in this diff references it\n"
               " — its criteria file is untouched in this diff\n")))))

(defcustom code-review-analysis-deferred? t
  "Non-nil means the heavy analysis engines compute in a background child.
The render inserts a placeholder Analysis section immediately;
when the engines finish the placeholder is replaced in place with
the findings.  The engines' full-worktree `git grep' passes cost
10s+ EACH on a large PR (#1107: 23.6s synchronous) — deferral
keeps first paint at the skeleton's cost.  Set nil for the old
synchronous behavior (batch tools and tests may want that)."
  :type 'boolean
  :group 'code-review)

(defvar code-review-analysis--deferred-in-flight
  (make-hash-table :test #'equal)
  "Cache keys with a background analysis child currently running.")

(defun code-review-analysis--cache-key ()
  "The (PR id, diff md5) cache key shared by the analysis engines, or nil."
  (ignore-errors
    (let* ((diff (code-review-db--pullreq-raw-diff))
           (pr (and diff (code-review-db-get-pullreq))))
      (and pr (concat (oref pr id) "|" (md5 diff))))))

(defun code-review-analysis--deferred-needed? (key)
  "Non-nil when KEY's engines are enabled and not cached yet.
A stored nil IS a result (no findings): distinguish it from a
never-computed key with a sentinel (`gethash' two-step idiom)."
  (and key
       (or (and code-review-analysis-enabled
                (eq (gethash key code-review-analysis--cache 'missing)
                    'missing))
           (and code-review-testimpact-enabled
                (eq (gethash key code-review-testimpact--cache 'missing)
                    'missing)))))

(defun code-review-section-insert-analysis ()
  "Insert the heuristic Analysis section (phase 5, 15, 17, 18).
Deferred by default: the engines compute in a background child
while the review buffer paints without them (see
`code-review-analysis-deferred?').  Nothing is inserted when no
analysis found anything (or they are disabled, or there is no
worktree)."
  (let* ((key (and code-review-analysis-deferred?
                   (code-review-analysis--cache-key)))
         (canon (and key (code-review-analysis--canonical-key))))
    ;; deferral needs the canonical (owner repo number) key: without
    ;; it the child could never find the row (insert synchronously)
    (if (and key canon (code-review-analysis--deferred-needed? key))
        (code-review-analysis--insert-deferred key canon)
      (code-review-analysis--insert-full))))

(defun code-review-analysis--canonical-key ()
  "The (owner repo number) of the current PR, or nil.
The pullreq row's uuid id is REGENERATED on every re-open, so the
background child must resolve the row by the canonical
owner/repo/number key instead of capturing the volatile id."
  (ignore-errors
    (let ((pr (code-review-db-get-pullreq)))
      (and pr (list (oref pr owner) (oref pr repo) (oref pr number))))))

(defun code-review-analysis--insert-deferred (key canon)
  "Insert a placeholder Analysis section and spawn the compute child.
KEY is the engines' cache key, CANON the (owner repo number)
canonical key the child resolves the row by (the row uuid is
regenerated on every re-open); the child writes the results to a
temp file and the sentinel fills the caches and swaps the
placeholder for the real section."
  (let* ((section (magit-insert-section (code-review-analysis-section)
                    (insert (propertize "Analysis"
                                        'font-lock-face 'magit-section-heading))
                    (insert (propertize " (heuristic)"
                                        'font-lock-face 'magit-dimmed))
                    (magit-insert-heading)
                    (insert (propertize
                             "(computing in background — this section \
fills in when ready)"
                             'font-lock-face 'magit-dimmed))
                    (insert ?\n)))
         (canon (code-review-analysis--canonical-key))
         (db-copy (make-temp-file "cr-analysis-db-" nil ".sqlite"))
         (out (make-temp-file "cr-analysis-out-"))
         (parent (oref section parent))
         (buffer (current-buffer))
         (epoch code-review-lazy--epoch))
    (puthash key t code-review-analysis--deferred-in-flight)
    ;; a consistent db snapshot for the child: a plain copy of a LIVE
    ;; sqlite can catch a mid-write state; the sqlite3 CLI's .backup
    ;; takes a proper snapshot (fallback: plain copy).  The dot-command
    ;; and its filename are ONE argument — macOS's /usr/bin/sqlite3
    ;; parses dot-command arguments from the same string and answers
    ;; "missing FILENAME argument" when they arrive as separate argv
    ;; entries (a silently empty snapshot; exit status MUST be checked).
    (let ((db-file (expand-file-name code-review-db-database-file))
          (st -1))
      (when (executable-find "sqlite3")
        (setq st (call-process "sqlite3" nil nil nil db-file
                               (concat ".backup " db-copy))))
      (unless (and (zerop st) (file-exists-p db-copy)
                   (> (nth 7 (file-attributes db-copy)) 4096))
        ;; .backup unavailable or failed: a plain copy is the fallback
        (copy-file db-file db-copy t)))
    (let ((proc (make-process
                 :name "code-review-analysis"
                 :buffer " *code-review-analysis-child*"
                 :connection-type 'pipe
                 :command (append
                           (list (expand-file-name invocation-name
                                                   invocation-directory)
                                 "--batch" "-Q")
                           (apply #'append
                                  (mapcar (lambda (d) (list "-L" d))
                                          load-path))
                           (list
                            "--eval"
                            ;; assembled with concat + prin1-to-string,
                            ;; NEVER a format string: escaped-quote
                            ;; nesting inside --eval strings is a
                            ;; hand-counting bug farm
                            (concat
                             "(progn (require 'code-review-analysis)"
                             " (require 'code-review-testimpact)"
                             ;; closql rebuilds the row as its concrete
                             ;; subclass, which must be defined here
                             " (require 'code-review-github)"
                             " (require 'code-review-gitlab)"
                             " (require 'code-review-bitbucket)"
                             " (require 'code-review-local)"
                             " (setq code-review-db-database-file "
                             (prin1-to-string db-copy)
                             " code-review-repo-worktree "
                             (prin1-to-string code-review-repo-worktree)
                             " code-review-repo-dir "
                             (prin1-to-string code-review-repo-dir)
                             " code-review-history-enabled nil)"
                             ;; the row uuid is volatile (regenerated on
                             ;; re-open): resolve by owner/repo/number,
                             ;; then aim the engines' id at the FOUND
                             ;; row.  Direct emacsql, NOT
                             ;; code-review-db-search: that one filters
                             ;; (= saved 't), but a just-rendered PR row
                             ;; has saved NULL until first save, so the
                             ;; child silently found nothing.
                             " (let* ((db (code-review-db))"
                             " (rows (emacsql db [:select :* :from pullreq"
                             " :where (and (= owner $s1) (= repo $s2)"
                             " (= number $s3))]"
                             " " (prin1-to-string (nth 0 canon))
                             " " (prin1-to-string (nth 1 canon))
                             " " (prin1-to-string (nth 2 canon)) "))"
                             " (pr (and rows (closql--remake-instance"
                             " 'code-review-db-pullreq db (car (last rows))))))"
                             " (when pr"
                             " (setq code-review-db--pullreq-id (oref pr id))"
                             " (let ((diff (code-review-db--pullreq-raw-diff)))"
                             " (with-temp-file " (prin1-to-string out)
                             " (prin1 (list :key (concat (oref pr id) \"|\""
                             " (md5 diff))"
                             " :analysis (code-review-analysis-run)"
                             " :testimpact (code-review-testimpact-run))"
                             ;; to the temp buffer, not `standard-output'
                             ;; (stdout in --batch)
                             " (current-buffer))))))))")))
                 :sentinel #'code-review-analysis--child-sentinel)))
      (process-put proc 'analysis-key key)
      (process-put proc 'analysis-out out)
      (process-put proc 'analysis-db-copy db-copy)
      (process-put proc 'review-buffer buffer)
      (process-put proc 'analysis-section section)
      (process-put proc 'analysis-parent parent)
      (process-put proc 'analysis-epoch epoch))))

(defun code-review-analysis--child-sentinel (proc _msg)
  "Fill the engine caches from the child's output and swap the
placeholder for the real section."
  (when (memq (process-status proc) '(exit signal))
    (let* ((key (process-get proc 'analysis-key))
           (out (process-get proc 'analysis-out))
           (db-copy (process-get proc 'analysis-db-copy))
           (buffer (process-get proc 'review-buffer))
           (sec (process-get proc 'analysis-section))
           (parent (process-get proc 'analysis-parent))
           (epoch (process-get proc 'analysis-epoch))
           (data (and (zerop (process-exit-status proc))
                      (file-exists-p out)
                      (ignore-errors
                        (with-temp-buffer
                          (insert-file-contents out)
                          (read (current-buffer)))))))
      (remhash key code-review-analysis--deferred-in-flight)
      (ignore-errors (delete-file out))
      (ignore-errors (delete-file db-copy))
      ;; the child resolves the row by owner/repo/number, so its key
      ;; is AUTHORITATIVE: it may differ from the spawning render's
      ;; key when the row was regenerated meanwhile (the reopen case)
      (let ((ckey (and (consp data) (plist-get data :key))))
        (if (stringp ckey)
            (progn
              ;; nil results are cached too: no findings is a RESULT,
              ;; not a miss (otherwise every render recomputes)
              (puthash ckey (plist-get data :analysis)
                       code-review-analysis--cache)
              (puthash ckey (plist-get data :testimpact)
                       code-review-testimpact--cache)
              ;; swap the placeholder only if THIS render is still the
              ;; live one (an epoch mismatch means a re-render happened;
              ;; its own insert path picks the results up from cache)
              (when (and (buffer-live-p buffer)
                         (eq epoch
                             (buffer-local-value 'code-review-lazy--epoch
                                                 buffer)))
                (code-review-analysis--replace-placeholder
                 buffer sec parent)))
          ;; failure: never leave a "computing" placeholder that will
          ;; never fill; no retry from the sentinel — the next render
          ;; re-spawns (the cache key is still missing)
          (when (and (buffer-live-p buffer)
                     (eq epoch
                         (buffer-local-value 'code-review-lazy--epoch
                                             buffer)))
            (code-review-analysis--placeholder-note
             buffer sec parent
             "background analysis failed — refresh to retry"))
          (message "code-review: background analysis failed \
(see *code-review-analysis-child*)"))))))

(defun code-review-analysis--placeholder-note (buffer sec parent note)
  "Replace the deferred placeholder SEC with a dimmed NOTE line."
  (with-current-buffer buffer
    (when (and (slot-boundp sec 'start) (markerp (oref sec start))
               (marker-buffer (oref sec start))
               (numberp (marker-position (oref sec start)))
               (numberp (marker-position (oref sec end)))
               (< (marker-position (oref sec start))
                  (marker-position (oref sec end))))
      (let ((inhibit-read-only t))
        (save-excursion
          (goto-char (oref sec start))
          (delete-region (oref sec start) (oref sec end))
          (when parent
            (setf (oref parent children) (delq sec (oref parent children))))
          (let ((magit-insert-section--parent parent))
            (magit-insert-section (code-review-analysis-section)
              (insert (propertize "Analysis"
                                  'font-lock-face 'magit-section-heading))
              (magit-insert-heading)
              (insert (propertize (concat "(" note ")")
                                  'font-lock-face 'magit-dimmed))
              (insert ?\n))))))))

(defun code-review-analysis--replace-placeholder (buffer sec parent)
  "Replace the deferred placeholder SEC with the real section.
The engines are cached at this point, so the synchronous insert
runs instantly; the placeholder span is deleted under the same
parent to keep the section order."
  (with-current-buffer buffer
    (when (and (slot-boundp sec 'start) (markerp (oref sec start))
               (marker-buffer (oref sec start))
               (numberp (marker-position (oref sec start)))
               (numberp (marker-position (oref sec end)))
               (< (marker-position (oref sec start))
                  (marker-position (oref sec end))))
      (let ((inhibit-read-only t))
        (save-excursion
          (goto-char (oref sec start))
          (delete-region (oref sec start) (oref sec end))
          (when parent
            (setf (oref parent children) (delq sec (oref parent children))))
          (let ((magit-insert-section--parent parent))
            (code-review-analysis--insert-full)))))))

(defun code-review-analysis--insert-full ()
  "Run the analysis engines synchronously and insert the section."
  (let* ((res (code-review-analysis-run))
         (tres (code-review-testimpact-run))
         (tres-findings (and tres (or (plist-get tres :ci)
                                      (plist-get tres :notest))))
         (cres (code-review-coupling-run))
         (cfindings (and cres (plist-get cres :findings)))
         (tfindings (code-review-section--criteria-trace-findings)))
    (when (or res tres-findings cfindings tfindings)
      (let ((similar (and res (plist-get res :similar)))
            (dead (and res (plist-get res :dead)))
            (dangling (and res (plist-get res :dangling)))
            (worktree code-review-repo-worktree))
        (magit-insert-section (code-review-analysis-section)
          (insert (propertize "Analysis" 'font-lock-face 'magit-section-heading))
          (insert (propertize " (heuristic)" 'font-lock-face 'magit-dimmed))
          (magit-insert-heading)
          ;; phase 17 first: the CI-gaming findings are the hard tags
          (when tres-findings
            (code-review-section--insert-testimpact-ci tres)
            (code-review-section--insert-testimpact-notest tres))
          ;; phase 18: change-coupling completeness
          (when cfindings
            (code-review-section--insert-coupling cres))
          ;; phase 24: criteria <-> test traceability
          (when tfindings
            (code-review-section--insert-criteria-trace))
          (when res
            (code-review-section--insert-analysis-similar worktree similar)
            (code-review-section--insert-analysis-dead dead)
            (code-review-section--insert-analysis-dangling worktree dangling)
            ;; phase 15: delicate hunks (top-K, hottest first).  The
            ;; buttons jump inside the review buffer: the section is
            ;; inserted there, so the button action runs there.
            (code-review-section--insert-analysis-delicate res))
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
                  (equal (code-review-diff--strip-path
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
(provide 'code-review-section-analysis)
;;; code-review-section-analysis ends here
