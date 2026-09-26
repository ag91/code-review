;;; code-review-history.el --- Repository history harvest and review heat -*- lexical-binding: t; -*-

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

;; Phase 14 (Improvements.org): risk-ranked reading order.
;;
;; Evidence base: code churn x complexity is the best validated
;; "where bugs live" signal (Nagappan & Ball 2005; CodeScene
;; hotspots).  In a 40-file PR the delicate file meets the reviewer
;; at position 37; this library ranks the PR's files by heat so
;; attention lands on the delicate parts first.
;;
;; Layers:
;;
;; - HARVEST (shared by phases 14/16/18): per-file repository facts
;;   (revisions, per-author counts, main dev, last touch) from ONE
;;   `git log --name-only --since=WINDOW' call, parsed in pure elisp
;;   by code-compass (`code-compass--parse-git-log-metrics').
;;   `--name-only' reads tree entries only: nothing is fetched on
;;   phase 1 partial clones (`--filter=blob:none'), unlike
;;   `--numstat' which lazy-fetches blob contents per changed file.
;;   The harvest NEVER runs during the wash (daemon rule): it runs
;;   in a child batch emacs on first encounter of a repository, is
;;   cached on disk per repository with a TTL, and is a hash lookup
;;   on every later render.
;;
;; - HEAT: per changed file, churn percentile (per-repository
;;   normalized) x complexity (code-compass indentation stats of the
;;   file at the PR head) x knowledge factors (author spread, main
;;   dev, PR author's prior commits).  Buckets HOT / WARM / COLD by
;;   churn percentile feed the phase 3 tag machinery, the file
;;   ordering and focus mode.
;;
;; - RENDER: the "Review order (heuristic)" section
;;   (`code-review-section-insert-review-order', phase 5 analysis
;;   slot): ranked files with one-line reasons and jump buttons.
;;
;; Requires code-compass (soft dependency): without it the heat
;; features degrade to a message, nothing is reimplemented here.
;; Local diff reviews work: the repository itself is the worktree
;; and the local git user is the author (phase 12 constraint).

;;; Code:

(require 'cl-lib)
(require 'code-review-utils)

(defvar code-review-repo-worktree)        ; code-review-repo.el
(defvar code-review-bot-author-regexp)   ; code-review-section.el

(declare-function code-review-db-get-pullreq "code-review-db")
(declare-function code-review--diff--split-by-files "code-review-diff")
(declare-function code-review--trigger-hooks "code-review-section")
(declare-function code-review--sync-db-pullreq "code-review-section")

(defgroup code-review-history nil
  "Repository history harvest and review heat."
  :group 'code-review)

(defcustom code-review-history-enabled t
  "When non-nil, rank the review by repository heat (phase 14).
Requires the code-compass package for the metrics parse."
  :group 'code-review-history
  :type 'boolean)

(defcustom code-review-history-window "12 months"
  "Harvest window: any `--since' value git accepts.
Changing it invalidates the per-repository cache."
  :group 'code-review-history
  :type 'string)

(defcustom code-review-history-max-commits 5000
  "Cap on commits in one harvest (`git log --max-count').
git log is reverse-chronological across refs, so the cap keeps
the NEWEST commits in the window.  Bounds the log bytes, the
elisp parse in the child and the cache size: a huge repository
(litellm: 36k commits in the 12-month window, 16MB of
`--name-only' log) made the child harvest run for minutes and
the parse is quadratic-ish in the log.  0 disables the cap."
  :group 'code-review-history
  :type 'integer)

(defcustom code-review-history-ttl-days 7
  "Days a harvested per-repository cache stays fresh."
  :group 'code-review-history
  :type 'integer)

(defcustom code-review-history-hot-percentile 0.9
  "Churn percentile at or above which a changed file is HOT."
  :group 'code-review-history
  :type 'number)

(defcustom code-review-history-warm-percentile 0.7
  "Churn percentile at or above which a changed file is WARM."
  :group 'code-review-history
  :type 'number)

(defcustom code-review-history-complexity-scale 8
  "Complexity divisor: max nesting of SCALE counts as a full extra heat unit."
  :group 'code-review-history
  :type 'number)

(defcustom code-review-history-heat-tags t
  "When non-nil, changed files carry [HOT]/[WARM]/[COLD] heading tags."
  :group 'code-review-history
  :type 'boolean)

(defcustom code-review-history-order-diff-by-heat t
  "When non-nil, files matched by no ordering rule are sorted by heat.
Noise rules still sink to the bottom (phase 3 ordering unchanged)."
  :group 'code-review-history
  :type 'boolean)

(defcustom code-review-history-focus-hide-cold nil
  "When non-nil, focus mode also hides COLD files."
  :group 'code-review-history
  :type 'boolean)

(defconst code-review-history--cache-subdir "code-review")

;;; Harvest

(defvar code-review-history--cache (make-hash-table :test #'equal)
  "Repo key -> plist (:metrics HASH :harvested-at FLOAT).")

(defvar code-review-history--cache-files nil
  "Repo keys whose cache file we have written (for `code-review-history-reset').")

(defvar code-review-history--failed (make-hash-table :test #'equal)
  "Repo keys whose harvest failed this session (loop protection).")

;;;###autoload
(defun code-review-history-reset ()
  "Forget all cached repository history.
The next render re-harvests every repository (async, once)."
  (interactive)
  (clrhash code-review-history--cache)
  (clrhash code-review-history--failed)
  (dolist (key code-review-history--cache-files)
    (ignore-errors (delete-file (code-review-history--cache-file key)))))

(defun code-review-history--git (worktree &rest args)
  "Run git ARGS in WORKTREE; return trimmed stdout."
  (with-temp-buffer
    (apply #'call-process "git" nil t nil
           "-C" (expand-file-name worktree) args)
    (string-trim (buffer-substring-no-properties (point-min) (point-max)))))

(defun code-review-history--repo-key (worktree)
  "Stable per-repository key for WORKTREE: its common git directory.
Two PRs of the same clone share one history cache."
  (let ((dir (apply #'code-review-history--git worktree
                    '("rev-parse" "--git-common-dir"))))
    (if (and (not (string-empty-p dir)) (file-name-absolute-p dir))
        dir
      (expand-file-name (if (string-empty-p dir) ".git" dir) worktree))))

(defun code-review-history--window-slug ()
  "File-name-safe form of the harvest window."
  (replace-regexp-in-string "[^a-zA-Z0-9]" "-" code-review-history-window))

(defun code-review-history--cache-file (key)
  "Cache file for repo KEY."
  (expand-file-name
   (format "history-%s.el" (code-review-history--window-slug))
   (expand-file-name code-review-history--cache-subdir key)))

(defun code-review-history--compass-p ()
  "Non-nil when code-compass provides the metrics parse."
  (and (require 'code-compass nil t)
       (fboundp 'code-compass--parse-git-log-metrics)))

(defun code-review-history--bot-regexp ()
  "Author regexp excluded from the harvest (bots are not churn)."
  (and (boundp 'code-review-bot-author-regexp)
       code-review-bot-author-regexp))

(defun code-review-history--log (worktree)
  "One partial-clone-safe history log for WORKTREE.
`--name-only' reads tree entries only, never blob contents; the
review package's own fetched PR refs are excluded so reviews do
not count as repository churn.  Bounded by
`code-review-history-max-commits' (newest first)."
  (with-temp-buffer
    (apply #'call-process "git" nil t nil
           `("-C" ,(expand-file-name worktree)
             "log" "--exclude=refs/remotes/code-review/*"
             "--all" "--name-only" "--no-renames"
             "--date=short" "--pretty=format:--%h--%ad--%aN"
             "--since" ,code-review-history-window
             ,@(when (> code-review-history-max-commits 0)
                 (list "--max-count"
                       (number-to-string code-review-history-max-commits)))))
    (buffer-string)))

(defun code-review-history--store (key metrics)
  "Persist METRICS for repo KEY (memory + disk cache file)."
  (puthash key (list :metrics metrics :harvested-at (float-time))
           code-review-history--cache)
  (let ((file (code-review-history--cache-file key)))
    (make-directory (file-name-directory file) t)
    (with-temp-file file
      (prin1 (list :metrics metrics
                   :window code-review-history-window
                   :harvested-at (float-time))
             (current-buffer)))
    (push key code-review-history--cache-files)
    (remhash key code-review-history--failed)))

(defun code-review-history--load (key)
  "Fresh cached metrics for repo KEY, nil when absent or stale.
Populates the memory cache from the disk cache file."
  (or (gethash key code-review-history--cache)
      (let* ((file (code-review-history--cache-file key))
             (data (and (file-exists-p file)
                        (ignore-errors
                          (with-temp-buffer
                            (insert-file-contents file)
                            (read (current-buffer)))))))
        (when (and data
                   (equal (plist-get data :window) code-review-history-window)
                   (< (- (float-time) (or (plist-get data :harvested-at) 0))
                      (* code-review-history-ttl-days 86400)))
          (puthash key data code-review-history--cache)
          data))))

(defun code-review-history--harvest-sync (worktree)
  "Synchronously harvest WORKTREE's repository history into the cache.
One bounded `git log --name-only' (see `code-review-history--log')
plus a pure elisp parse; run this in batch emacs or in tests, never
in the live daemon.  Returns the metrics hash, nil when
code-compass is unavailable."
  (when (code-review-history--compass-p)
    (let* ((key (code-review-history--repo-key worktree))
           (metrics (code-compass--parse-git-log-metrics
                     (code-review-history--log worktree)
                     (code-review-history--bot-regexp))))
      (code-review-history--store key metrics)
      metrics)))

(defun code-review-history--harvest-async (worktree buffer key)
  "Harvest WORKTREE's repository in a child batch emacs.
The child writes the disk cache; the sentinel re-renders BUFFER
when it finishes.  KEY is the repo key (failure marking)."
  (message "code-review: harvesting repository history for review \
heat (first time for this repository; newest %s commits, a few seconds)..."
           (if (> code-review-history-max-commits 0)
               code-review-history-max-commits
             "all"))
  (let ((proc (make-process
               :name "code-review-history"
               :buffer " *code-review-history*"
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
                          (format
                           "(progn (require 'code-review-history) \
(setq code-review-history-window %S code-review-history-ttl-days %S \
code-review-history-max-commits %S) \
(setq code-review-bot-author-regexp %S) \
(code-review-history--harvest-sync %S))"
                           code-review-history-window
                           code-review-history-ttl-days
                           code-review-history-max-commits
                           (code-review-history--bot-regexp)
                           (expand-file-name worktree))))
               :sentinel #'code-review-history--sentinel)))
    (process-put proc 'review-buffer buffer)
    (process-put proc 'repo-key key)))

(defun code-review-history--sentinel (proc _msg)
  "Re-render the review buffer when the harvest child finishes."
  (when (memq (process-status proc) '(exit signal))
    (let* ((key (process-get proc 'repo-key))
           (buffer (process-get proc 'review-buffer)))
      (if (not (file-exists-p (code-review-history--cache-file key)))
          (progn
            (puthash key t code-review-history--failed)
            (message "code-review: history harvest failed \
(see *code-review-history*); heat disabled for this session"))
        (when (and (bufferp buffer) (buffer-live-p buffer))
          (with-current-buffer buffer
            ;; the user may have opened another PR while the
            ;; harvest ran: re-assert this buffer's PR before
            ;; re-rendering (the db pullreq id is a global)
            (code-review--sync-db-pullreq)
            (code-review--trigger-hooks (buffer-name))))))))

(defun code-review-history-metrics (worktree &optional buffer)
  "Metrics for WORKTREE's repository: hash path -> facts plist.
Serves from the cache; when cold, kicks the async harvest (BUFFER
is re-rendered by the sentinel) and returns nil."
  (let* ((key (code-review-history--repo-key worktree))
         (data (or (code-review-history--load key)
                   (unless (or (gethash key code-review-history--failed)
                               (not (or (bufferp buffer) (null buffer))))
                     (code-review-history--harvest-async worktree buffer key)
                     nil))))
    (and data (plist-get data :metrics))))

;;; Heat

(defun code-review-history--percentile (revisions all-revisions)
  "Fraction of ALL-REVISIONS at or below REVISIONS."
  (if (null all-revisions)
      0.0
    (/ (cl-count-if (lambda (r) (<= r revisions)) all-revisions)
       (float (length all-revisions)))))

(defun code-review-history--complexity (path worktree)
  "Max logical indentation of PATH at the PR head, nil when unknown."
  (and worktree
       (require 'code-compass nil t)
       (fboundp 'code-compass-file-complexity)
       (cdr (assq 'max
                  (code-compass-file-complexity
                   (expand-file-name path worktree))))))

(defun code-review-history--author-revs (pr-author authors)
  "PR-AUTHOR's commits among AUTHORS (alist name -> count), 0 when none."
  (or (cdr (cl-assoc pr-author authors :test #'string-equal-ignore-case))
      0))

(defun code-review-history--author-in-repo-p (pr-author metrics)
  "Non-nil when PR-AUTHOR appears as an author anywhere in METRICS.
Guards the `never touched it' claim: a login with no matching git
author name may simply commit under a different name."
  (and pr-author
       (cl-some (lambda (entry)
                  (cl-some (lambda (a)
                             (string-match-p (regexp-quote pr-author)
                                             (car a)))
                           (plist-get entry :authors)))
                (hash-table-values metrics))))

(defun code-review-history--entry (path metrics all-revisions
                                        worktree pr-author author-pool)
  "Heat entry plist for one changed file PATH."
  (let* ((m (or (gethash path metrics)
                (list :revisions 0 :authors nil
                      :main-dev nil :last-touch nil)))
         (revisions (plist-get m :revisions))
         (authors (plist-get m :authors))
         (authors-n (length authors))
         (main-dev (plist-get m :main-dev))
         (churn-pct (code-review-history--percentile
                     revisions all-revisions))
         (cmax (code-review-history--complexity path worktree))
         (author-revs (code-review-history--author-revs
                       pr-author authors))
         (human-author (and pr-author author-pool
                            (not (string-match-p
                                  (or (code-review-history--bot-regexp)
                                      "\\<\\>")
                                  pr-author))))
         (never-touched (and human-author (zerop author-revs)))
         (score (* churn-pct
                   (+ 1 (/ (min (or cmax 0) (* 3 code-review-history-complexity-scale))
                           (float code-review-history-complexity-scale)))
                   (if never-touched 1.25 1)
                   (if (>= authors-n 5) 1.1 1)))
         (bucket (cond
                  ((>= churn-pct code-review-history-hot-percentile) "HOT")
                  ((>= churn-pct code-review-history-warm-percentile) "WARM")
                  (t "COLD")))
         (reason (string-join
                  (delq nil
                        (list (when (> revisions 0)
                                (format "top %d%% churn (%d revisions)"
                                        (max 1 (round (* 100 (- 1 churn-pct))))
                                        revisions))
                              (when (and cmax (>= cmax 8))
                                (format "deep nesting (%d)" cmax))
                              (when (>= authors-n 5)
                                (format "%d authors" authors-n))
                              (when main-dev
                                (format "main dev: %s" main-dev))
                              (when never-touched
                                (format "%s never touched it" pr-author))))
                  "; ")))
    (list :path path :score score :bucket bucket
          :churn-pct churn-pct :revisions revisions
          :authors-n authors-n :main-dev main-dev
          :author-revs author-revs :complexity cmax
          :reason reason)))

(defun code-review-history-heat (metrics files worktree pr-author)
  "Heat entries for the changed FILES of a PR, hottest first.
METRICS is the repository hash from the harvest; WORKTREE gives
the PR head contents for complexity; PR-AUTHOR is the author
login/name (knowledge factor; bots are skipped).  Each entry is
the plist of `code-review-history--entry'.  Pure: no git, no
harvest (read-only worktree access bounded by the PR's files)."
  (let* ((all-revisions
          (mapcar (lambda (entry) (plist-get entry :revisions))
                  (hash-table-values metrics)))
         (author-pool (code-review-history--author-in-repo-p
                       pr-author metrics))
         (entries (mapcar (lambda (path)
                            (code-review-history--entry
                             path metrics all-revisions
                             worktree pr-author author-pool))
                          files)))
    (sort entries (lambda (a b) (> (plist-get a :score)
                                   (plist-get b :score))))))

(defun code-review-history--pr-author (pr worktree)
  "Author of PR (a pullreq object) or, locally, the git user of WORKTREE."
  (or (and (slot-boundp pr 'raw-infos)
           (a-get-in (oref pr raw-infos) '(author login)))
      (and worktree (code-review-history--git worktree "config" "user.name"))))

(defun code-review-history--files (diff)
  "Changed file paths of DIFF (the raw diff text)."
  (mapcar #'car (code-review--diff--split-by-files diff)))

(defvar code-review-history--order nil
  "Heat entries for the current review render, hottest first.
Set by `code-review-history-prepare' before the wash; read by the
tag/order/focus machinery and the review-order section.")

(defun code-review-history--order-cache (make-key entries)
  "Memoize ENTRIES under MAKE-KEY (a function returning a string)."
  (let ((key (funcall make-key)))
    (or (gethash key code-review-history--order-table)
        (let ((res (funcall entries)))
          (puthash key res code-review-history--order-table)
          res))))

(defvar code-review-history--order-table (make-hash-table :test #'equal)
  "Per (PR, diff) computed heat entries (phase 5 caching pattern).")

(defun code-review-history-prepare (diff worktree)
  "Set `code-review-history--order' for this render (DIFF, WORKTREE).
nil (heat off for this render) when disabled, code-compass is
missing, or the metrics cache is cold: then the async harvest is
kicked (message; the sentinel re-renders this buffer)."
  (setq code-review-history--order nil)
  (when (and code-review-history-enabled worktree diff
             (code-review-history--compass-p))
    (let* ((pr (code-review-db-get-pullreq))
           (metrics (code-review-history-metrics
                     worktree (current-buffer))))
      (when metrics
        (setq code-review-history--order
              (condition-case err
                  (code-review-history--order-cache
                   (lambda ()
                     (concat (oref pr id) "|" (md5 diff)))
                   (lambda ()
                     (code-review-history-heat
                      metrics (code-review-history--files diff)
                      worktree (code-review-history--pr-author pr worktree))))
                (error
                 (code-review-utils--log
                  "code-review-history"
                  (format "history heat failed, skipping section (%s)"
                          (error-message-string err)))
                 nil)))))))

(defun code-review-history--tag-for (path)
  "HOT/WARM/COLD tag for PATH per the current render's heat, else nil."
  (when (and code-review-history--order code-review-history-heat-tags)
    (plist-get (cl-find path code-review-history--order
                        :key (lambda (e) (plist-get e :path))
                        :test #'equal)
               :bucket)))

(defun code-review-history--score-for (path)
  "Heat score for PATH (0.0 when unknown), for diff ordering."
  (if (and code-review-history--order
           code-review-history-order-diff-by-heat)
      (or (plist-get (cl-find path code-review-history--order
                              :key (lambda (e) (plist-get e :path))
                              :test #'equal)
                     :score)
          0.0)
    0.0))

(provide 'code-review-history)

;;; code-review-history.el ends here
