;;; code-review-budget-focus.el --- measured review time -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026 Andrea <andrea-dev@hotmail.com>
;;
;; Author: Andrea <andrea-dev@hotmail.com>
;; Keywords: git, tools, vc
;; Homepage: https://github.com/wandersoncferreira/code-review

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

;; Phase 25b follow-up (the reviewer's finding on the finished-today
;; booking): MEASURED minutes replace the estimate.  This file tracks
;; how long a code-review buffer holds the selected window, per PR,
;; and books that time through the budget state; the diff-shape
;; estimate becomes the FALLBACK for rows the tracker never saw
;; (daemon restarts, tracking disabled mid-review).  The estimate
;; keeps its own job: the queue's ex-ante decisions (rank, bounce,
;; per-PR cap) happen before any focus second exists.
;;
;; Mechanics: `post-command-hook' ticks (buffer-list-update-hook does
;; NOT fire while you sit in one buffer reading — a whole reading
;; session would tick once, read as one huge idle gap, and be
;; wrongly discarded).  Each tick attributes the elapsed gap to the
;; PREVIOUSLY focused PR (when the gap is within
;; `code-review-budget-focus-idle-seconds'), then flushes to the
;; state file at most every 15 seconds.  Bounded per tick, no timers,
;; no polling loops.
;;
;; Attribution key: the CANONICAL owner/repo#N key, resolved once per
;; buffer from its db row.  The buffer's `code-review-review-buffer-pr-id'
;; is a closql uuid that `code-review-db--pullreq-create' REGENERATES
;; on every re-open — keying on it would orphan the accrued minutes
;; of any review whose row got rebuilt between open and finish.

;;; Code:

(require 'cl-lib)
(require 'code-review-budget)

(declare-function code-review-utils--log "code-review-utils")

(defvar code-review-db--pullreq-id)         ; code-review-db.el, global
(defvar code-review-review-buffer-pr-id)    ; code-review-section.el, local

(defvar-local code-review-budget-focus--key nil
  "Canonical pr key (owner/repo#N) of this review buffer.
Resolved once from the buffer's db row: the buffer's pr id is a
regenerated uuid, the canonical key survives re-opens.")

(defgroup code-review-budget-focus nil
  "Measured review time for the budget (phase 25b)."
  :group 'code-review)

(defcustom code-review-budget-focus-enabled t
  "Whether review-buffer focus time is measured and booked.
nil disables the tracker; finished rows fall back to the
estimated cost."
  :group 'code-review-budget-focus
  :type 'boolean)

(defcustom code-review-budget-focus-idle-seconds 60
  "Focus gaps longer than this many seconds do not count.
The buffer being selected is not the same as you looking at it:
reading the screen with hands off stays under this threshold,
leaving for lunch does not.  The calibration dial for the
tracker's honesty."
  :group 'code-review-budget-focus
  :type 'number)

(defvar code-review-budget-focus--last nil
  "The last focus tick: plist (:time FLOAT :pr KEY-OR-NIL).")

(defvar code-review-budget-focus--accrued nil
  "Unflushed seconds: alist (PR-KEY . SECONDS).")

(defvar code-review-budget-focus--last-flush 0.0)

(defun code-review-budget-focus--resolve-key (uuid)
  "Canonical owner/repo#N key for the db row UUID, or nil.
nil means \"not resolvable yet\" (row not built, db mid-reset):
the next tick retries."
  (and uuid
       (condition-case err
           (let* ((db (code-review-db))
                  (pr (and db (slot-boundp db :connection)
                           (closql-get db uuid
                                       'code-review-db-pullreq))))
             (and pr (code-review-budget--pr-key pr)))
         (error
          (code-review-utils--log
           "code-review-budget-focus"
           (format "key resolution failed: %s"
                   (error-message-string err)))
          nil))))

(defun code-review-budget-focus--pr-of-buffer (buf)
  "The canonical pr key of BUFFER, or nil.
Only code-review-mode buffers are attributable.  The key is
resolved lazily from the buffer's db row and cached in the
buffer: the buffer's pr id is a closql uuid that row
re-creation regenerates, while the canonical key is stable
across re-opens."
  (and (buffer-live-p buf)
       (with-current-buffer buf
         (and (derived-mode-p 'code-review-mode)
              (or code-review-budget-focus--key
                  (setq code-review-budget-focus--key
                        (code-review-budget-focus--resolve-key
                         code-review-review-buffer-pr-id)))))))

(defun code-review-budget-focus--buffer-pr-id ()
  "The pr key of the selected window's buffer, or nil."
  (code-review-budget-focus--pr-of-buffer
   (window-buffer (selected-window))))

(defun code-review-budget-focus--accrue (pr-id seconds)
  "Add SECONDS of focus onto PR-ID's unflushed total."
  (let ((total (+ seconds
                  (or (cdr (assoc pr-id
                                  code-review-budget-focus--accrued))
                      0))))
    (setq code-review-budget-focus--accrued
          (cons (cons pr-id total)
                (cl-remove-if (lambda (e) (equal (car e) pr-id))
                              code-review-budget-focus--accrued)))))

(defun code-review-budget-focus--flush ()
  "Merge unflushed seconds into the budget state's :focus key."
  (setq code-review-budget-focus--last-flush (float-time))
  (when code-review-budget-focus--accrued
    (let* ((state (code-review-budget--state-load))
           (focus (plist-get state :focus))
           (merged focus))
      (dolist (e code-review-budget-focus--accrued)
        (setq merged
              (cons (cons (car e)
                          (+ (cdr e)
                             (or (cdr (assoc (car e) focus)) 0)))
                    (cl-remove-if (lambda (x) (equal (car x) (car e)))
                                  merged))))
      (plist-put state :focus merged)
      (code-review-budget--state-save state)
      (setq code-review-budget-focus--accrued nil))))

(defun code-review-budget-focus--note (now &optional pr)
  "Account the gap up to NOW for the focused PR PR, then start
the next focus interval.  The hook entry passes no PR: the
selected window's buffer is extracted; tests pass one
explicitly."
  (let ((pr (or pr (code-review-budget-focus--buffer-pr-id))))
    (when-let (last code-review-budget-focus--last)
      (let ((gap (- now (plist-get last :time))))
        (when (and (plist-get last :pr) pr
                   (>= gap 0)
                   (<= gap code-review-budget-focus-idle-seconds))
          (code-review-budget-focus--accrue (plist-get last :pr) gap)))))
  (setq code-review-budget-focus--last (list :time now :pr pr))
  (when (> (- now code-review-budget-focus--last-flush) 15.0)
    (code-review-budget-focus--flush)))

(defun code-review-budget-focus--hook ()
  "The `buffer-list-update-hook' entry: bounded, no-ops when off."
  (when code-review-budget-focus-enabled
    (condition-case err
        (code-review-budget-focus--note (float-time))
      (error (code-review-budget-focus--disable)
             (code-review-utils--log
              "code-review-budget-focus"
              (format "tracker disabled after error: %s"
                      (error-message-string err)))))))

(defun code-review-budget-focus--disable ()
  "Stop the tracker (hook removal is process-local to this session)."
  (remove-hook 'post-command-hook #'code-review-budget-focus--hook)
  (remove-hook 'kill-emacs-hook #'code-review-budget-focus--flush))

(defun code-review-budget-focus-minutes-for (pr-key)
  "Measured focus MINUTES for PR-KEY, or nil when never tracked.
Flushes first, so a submission right after a buffer switch is
current."
  (when (fboundp 'code-review-budget--state-load)
    (code-review-budget-focus--flush)
    (let ((seconds (cdr (assoc pr-key
                               (plist-get
                                (code-review-budget--state-load)
                                :focus)))))
      (and seconds (/ seconds 60.0)))))

(add-hook 'post-command-hook #'code-review-budget-focus--hook)
(add-hook 'kill-emacs-hook #'code-review-budget-focus--flush)

(provide 'code-review-budget-focus)
;;; code-review-budget-focus.el ends here
