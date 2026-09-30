;;; code-review-hunkhighlight-intraline.el --- Intra-line changed-token marking -*- lexical-binding: t; -*-
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

;;  Phase 20c: mark WHICH TOKENS of a modified line actually
;;  changed, right in the diff surface.  Whole-line green treatment
;;  leaves the reviewer scanning the line by hand for the delta —
;;  and most edits are small token edits INSIDE a line
;;  (ChangeDistiller, TSE 2007; GumTree, ASE 2014).  The
;;  scan-then-fixate eye-tracking pattern (Uwano et al. 2006)
;;  wants the fixation target as small as possible, so an
;;  underline on exactly the changed tokens completes the story
;;  the semantic faces (phase 10/20a/20b) start.
;;
;;  Zero external tools: no treesit, no worktree, no git call —
;;  the OLD side of every change is already inside the hunk.
;;  Local diff reviews (phase 12 constraint) inherit it for free.
;;  Complementary to =D= (difftastic drill-down): on-demand,
;;  per-file and structural on one side; always-on and in-surface
;;  here on the other.
;;
;;  How it works, per hunk (each step is one small function):
;;   1. walk the raw prefixed hunk lines, group change blocks (a
;;      run of `-' lines followed by a run of `+' lines) and pair
;;      them positionally —
;;      `code-review-hunkhighlight--intra-line-ranges' driving the
;;      per-row-kind handlers `--see-minus' / `--see-plus' /
;;      `--see-context';
;;   2. tokenize both lines of each pair with a grammar-free
;;      tokenizer (identifier runs, digit runs, quoted strings,
;;      single characters; whitespace dropped) —
;;      `code-review-hunkhighlight--tokenize';
;;   3. trim the common token prefix/suffix
;;      (`--trim-middles'), LCS-align the middles
;;      (`--lcs-table' / `--lcs-matched'), and keep the new-side
;;      tokens the alignment does not match —
;;      `code-review-hunkhighlight--changed-token-cols'.
;;
;;  Guards (all by design): pure-add `+' lines (no old
;;  counterpart) stay unmarked — the whole line IS the delta;
;;  whitespace-only differences mark nothing (the tokenizer drops
;;  whitespace); monster lines are capped before tokenizing (the
;;  phase 5 litellm incident — notebooks/minified bundles are
;;  data, not reviewable code); oversized middles skip the DP.

;;; Code:

(require 'cl-lib)
(require 'code-review-hunkhighlight-queries)

(defcustom code-review-hunkhighlight-intra-line t
  "When non-nil, mark the CHANGED TOKENS of modified added lines.
An underline (`code-review-changed-token-face') on exactly the
tokens that differ from the paired old line (phase 20c).  Works
with no grammar and no external tools, in every language (the
pairing and tokenizing are grammar-free).  The ranges cache key
includes this: toggling it recomputes on the next render."
  :type 'boolean
  :group 'code-review-hunkhighlight)

(defcustom code-review-hunkhighlight-intra-line-max-length 2000
  "Line-length cap for intra-line changed-token marking.
Longer lines (old or new) mark nothing: monster single lines are
data, not reviewable code (notebooks, minified bundles — the
litellm phase 5 incident), and capping keeps every per-line cost
bounded no matter what the diff contains."
  :type 'natnum
  :group 'code-review-hunkhighlight)

(defcustom code-review-hunkhighlight-intra-line-max-tokens 250
  "Per-line token cap for the intra-line LCS alignment.
Pairs whose token middles (after the common prefix/suffix trim)
exceed this on either side mark nothing: the DP stays quadratic
and bounded, and lines this size are machine-generated anyway.
Typical edits trim down to a handful of tokens each."
  :type 'natnum
  :group 'code-review-hunkhighlight)

;;; The tokenizer

(defun code-review-hunkhighlight--ident-char-p (c)
  "Non-nil when C can continue an identifier run (letters, digits, `_')."
  (or (<= ?a c ?z) (<= ?A c ?Z) (<= ?0 c ?9) (= c ?_)))

(defun code-review-hunkhighlight--ident-start-p (c)
  "Non-nil when C can START an identifier run (a letter or `_')."
  (or (<= ?a c ?z) (<= ?A c ?Z) (= c ?_)))

(defun code-review-hunkhighlight--digit-char-p (c)
  "Non-nil when C is a digit."
  (<= ?0 c ?9))

(defun code-review-hunkhighlight--tokenize-run (line start pred)
  "Token for the run of PRED chars starting at column START in LINE.
Whitespace is never part of a run: the tokenizer driver drops it
before dispatching, so the run stops at the first non-PRED char."
  (let ((end (min (length line)
                  (or (cl-position-if-not pred line :start start)
                      (length line)))))
    (list (substring line start end) start end)))

(defun code-review-hunkhighlight--tokenize-string (line start)
  "Token for the quoted string starting at column START in LINE.
Escape-aware; an unterminated fragment consumes to the end of the
line (hunks cut strings mid-line all the time)."
  (let* ((n (length line))
         (q (aref line start))
         (i (1+ start))
         (closed nil))
    (while (and (< i n) (not closed))
      (cond
       ((= (aref line i) ?\\) (setq i (min n (+ i 2))))
       ((= (aref line i) q) (setq i (1+ i) closed t))
       (t (setq i (1+ i)))))
    (list (substring line start i) start i)))

(defun code-review-hunkhighlight--tokenize (line)
  "Return the tokens of LINE as ((TEXT BEG END)...), in order.
A grammar-free tokenizer (phase 20c needs zero external tools):
identifier runs, digit runs, quoted strings (escape-aware,
unterminated fragments consume to the end of the line), and every
other non-whitespace character as a single token.  Whitespace is
dropped, so whitespace-only line differences tokenize identically
(and mark nothing).  The caller length-caps (see
`code-review-hunkhighlight-intra-line-max-length')."
  (let ((n (length line))
        (i 0)
        (res nil))
    (while (< i n)
      (let ((tok
             (pcase (aref line i)
               ;; whitespace: dropped
               ((or ?\s ?\t ?\r) nil)
               ;; identifier / digit run
               ((pred code-review-hunkhighlight--ident-start-p)
                (code-review-hunkhighlight--tokenize-run
                 line i #'code-review-hunkhighlight--ident-char-p))
               ((pred code-review-hunkhighlight--digit-char-p)
                (code-review-hunkhighlight--tokenize-run
                 line i #'code-review-hunkhighlight--digit-char-p))
               ;; quoted string
               ((or ?\" ?' ?`)
                (code-review-hunkhighlight--tokenize-string line i))
               ;; everything else: single-character token
               (_ (list (substring line i (1+ i)) i (1+ i))))))
        (if tok
            (progn (push tok res) (setq i (nth 2 tok)))
          (setq i (1+ i)))))
    (nreverse res)))

;;; The alignment

(defun code-review-hunkhighlight--common-prefix-len (a b)
  "Count of common LEADING elements of the lists A and B."
  (let ((i 0))
    (while (and (< i (length a)) (< i (length b))
                (equal (nth i a) (nth i b)))
      (setq i (1+ i)))
    i))

(defun code-review-hunkhighlight--common-suffix-len (a b)
  "Count of common TRAILING elements of the lists A and B."
  (let ((i 0))
    (while (and (< i (length a)) (< i (length b))
                (equal (nth (- (length a) 1 i) a)
                       (nth (- (length b) 1 i) b)))
      (setq i (1+ i)))
    i))

(defun code-review-hunkhighlight--trim-middles (old new)
  "Split token lists OLD and NEW at their common ends.
Return (PREFIX OLD-MID NEW-MID): the shared leading-token count
and the middles left after trimming the common prefix AND suffix.
The middles are what the alignment sees — typically a handful of
tokens even for long lines."
  (let* ((prefix (code-review-hunkhighlight--common-prefix-len old new))
         (old-mid (nthcdr prefix old))
         (new-mid (nthcdr prefix new))
         (suffix (code-review-hunkhighlight--common-suffix-len
                  old-mid new-mid)))
    (list prefix
          (seq-take old-mid (- (length old-mid) suffix))
          (seq-take new-mid (- (length new-mid) suffix)))))

(defun code-review-hunkhighlight--token-cols (tokens from count)
  "Column ranges of the COUNT tokens of TOKENS starting at index FROM."
  (let ((cols nil))
    (dotimes (j count)
      (let ((rec (nth (+ from j) tokens)))
        (push (list (nth 1 rec) (nth 2 rec)) cols)))
    (nreverse cols)))

(defun code-review-hunkhighlight--lcs-table (old-mid new-mid)
  "LCS length table for the token middles OLD-MID and NEW-MID.
Table[(1+m)][(1+k)] holds the LCS length of the first m OLD-MID
and first k NEW-MID tokens (the classic DP)."
  (let* ((m (length old-mid))
         (k (length new-mid))
         (dp (make-vector (1+ m) nil)))
    (dotimes (a (1+ m))
      (aset dp a (make-vector (1+ k) 0)))
    (dotimes (a m)
      (dotimes (b k)
        (let ((row (aref dp (1+ a))))
          (aset row (1+ b)
                (if (equal (nth a old-mid) (nth b new-mid))
                    (1+ (aref (aref dp a) b))
                  (max (aref (aref dp a) (1+ b))
                       (aref row b)))))))
    dp))

(defun code-review-hunkhighlight--lcs-matched (old-mid new-mid)
  "Which NEW-MID tokens the token-LCS of OLD-MID matches.
A boolean vector, one entry per NEW-MID token: t when the
alignment matches it.  Backtracks
`code-review-hunkhighlight--lcs-table'; ties prefer eating an
OLD-MID token first (the original backtrack's choice, kept
byte-for-byte so the marked tokens stay stable)."
  (let* ((dp (code-review-hunkhighlight--lcs-table old-mid new-mid))
         (m (length old-mid))
         (k (length new-mid))
         (matched (make-vector k nil))
         (a m)
         (b k))
    (while (and (> a 0) (> b 0))
      (if (equal (nth (1- a) old-mid) (nth (1- b) new-mid))
          (progn
            (aset matched (1- b) t)
            (setq a (1- a) b (1- b)))
        (if (>= (aref (aref dp (1- a)) b)
                (aref (aref dp a) (1- b)))
            (setq a (1- a))
          (setq b (1- b)))))
    matched))

(defun code-review-hunkhighlight--lcs-changed-cols (old-mid new-mid
                                                          tokens from)
  "Columns of the NEW-MID tokens the LCS leaves unmatched.
TOKENS is the full new-line token list; FROM is the index of its
first middle token (see `--trim-middles')."
  (let* ((matched (code-review-hunkhighlight--lcs-matched
                   old-mid new-mid))
         (cols nil))
    (dotimes (j (length new-mid))
      (unless (aref matched j)
        (let ((rec (nth (+ from j) tokens)))
          (push (list (nth 1 rec) (nth 2 rec)) cols))))
    (nreverse cols)))

(defun code-review-hunkhighlight--changed-token-cols (old new)
  "Column ranges ((BEG END)...) of NEW-line tokens changed vs OLD.
BEG/END are 0-based columns within NEW (prefix excluded).  Nil
when nothing changed or the pair is too big for the caps.

Grammar-free alignment: trim the common token prefix and suffix
(`--trim-middles'), then LCS-align the middles; every new-side
token the alignment does not match is changed.  Empty old middle
= pure insertion (all inserted tokens marked); empty new middle =
pure deletion (nothing to mark on the new side)."
  (when (and (<= (length old)
                 code-review-hunkhighlight-intra-line-max-length)
             (<= (length new)
                 code-review-hunkhighlight-intra-line-max-length))
    (let* ((tokens (code-review-hunkhighlight--tokenize new))
           (trim (code-review-hunkhighlight--trim-middles
                  (mapcar #'car
                          (code-review-hunkhighlight--tokenize old))
                  (mapcar #'car tokens)))
           (prefix (nth 0 trim))
           (old-mid (nth 1 trim))
           (new-mid (nth 2 trim)))
      (cond
       ;; nothing changed
       ((and (null old-mid) (null new-mid)) nil)
       ;; pure deletion: nothing to mark on the new side
       ((null new-mid) nil)
       ;; over-budget middles: mark NOTHING (machine-generated
       ;; middles would underline the whole line - noise)
       ((or (> (length old-mid)
               code-review-hunkhighlight-intra-line-max-tokens)
            (> (length new-mid)
               code-review-hunkhighlight-intra-line-max-tokens))
        nil)
       ;; pure insertion: all new middle tokens are changed
       ((null old-mid)
        (code-review-hunkhighlight--token-cols
         tokens prefix (length new-mid)))
       ;; LCS-align the middles
       (t (code-review-hunkhighlight--lcs-changed-cols
           old-mid new-mid tokens prefix))))))

;;; The hunk walk

(defun code-review-hunkhighlight--intra-line-rows (body)
  "The prefixed rows of raw hunk BODY, `\\ No newline' markers dropped."
  (cl-remove-if (lambda (row) (string-prefix-p "\\" row))
                (split-string body "\n")))

(defun code-review-hunkhighlight--row-kind (row)
  "Return `minus', `plus' or `context' for a prefixed hunk ROW."
  (let ((c (and (> (length row) 0) (aref row 0))))
    (cond ((eq c ?-) 'minus)
          ((eq c ?+) 'plus)
          (t 'context))))

(defun code-review-hunkhighlight--token-marks (new-no cols)
  "Change-token range rows for hunk line NEW-NO from COLS.
Each is (LINE BEG END FACE), the shape
`code-review-hunkhighlight--apply' consumes."
  (mapcar (lambda (col)
            (list new-no (nth 0 col) (nth 1 col)
                  'code-review-changed-token-face))
          cols))

(defun code-review-hunkhighlight--see-minus (row state)
  "Handle one `-' hunk row: return (NEXT-STATE . nil).
A `-' after `+' rows starts a NEW block; the row content joins the
block's old-side list (front-pushed; `--see-plus' reverses it)."
  (let ((dels (if (plist-get state :frozen)
                  nil
                (plist-get state :dels))))
    (cons (list :new-no (plist-get state :new-no)
                :dels (push (substring row 1) dels)
                :k 0
                :frozen nil)
          nil)))

(defun code-review-hunkhighlight--see-plus (row state)
  "Handle one `+' hunk row: return (NEXT-STATE . MARKS).
The row counts on the new side and pairs with the block's i-th
`-' row (i = the block's K so far); no counterpart means pure
add - the whole line IS the delta, so no marks."
  (let* ((new-no (1+ (plist-get state :new-no)))
         (dels (if (plist-get state :frozen)
                   (plist-get state :dels)
                 (nreverse (plist-get state :dels))))
         (k (plist-get state :k))
         (old (and (< k (length dels)) (nth k dels)))
         (cols (and old
                    (code-review-hunkhighlight--changed-token-cols
                     old (substring row 1)))))
    (cons (list :new-no new-no
                :dels dels
                :k (1+ k)
                :frozen t)
          (code-review-hunkhighlight--token-marks new-no cols))))

(defun code-review-hunkhighlight--see-context (_row state)
  "Handle one context/blank hunk row: return (NEXT-STATE . nil).
Context lines count on the new side (the reconstruction's row
numbering) and reset the change block."
  (cons (list :new-no (1+ (plist-get state :new-no))
              :dels nil
              :k 0
              :frozen nil)
        nil))

(defun code-review-hunkhighlight--intra-line-ranges (body)
  "Return ((LINE BEG END FACE)...) changed-token marks for raw BODY.
BODY is the raw prefixed hunk body text (with the +/-/space
prefixes, exactly what the wash feeds the region core).  LINE is
the 1-based NEW-side line number — it matches the row list of
`code-review-hunkhighlight--reconstruct', so
`code-review-hunkhighlight--apply' maps it — and BEG/END are
0-based columns within the line's content, prefix excluded.

Walks the prefixed rows with the per-row-kind handlers (see
`--see-minus'/`--see-plus'/`--see-context'): change blocks are a
run of `-' lines followed by a run of `+' lines, paired
positionally — the i-th `+' of the block against the i-th `-'."
  (let ((state (list :new-no 0 :dels nil :k 0 :frozen nil))
        (ranges nil))
    (dolist (row (code-review-hunkhighlight--intra-line-rows body))
      (let* ((result
              (pcase (code-review-hunkhighlight--row-kind row)
                (`minus (code-review-hunkhighlight--see-minus row state))
                (`plus (code-review-hunkhighlight--see-plus row state))
                (_ (code-review-hunkhighlight--see-context row state))))
             (next-state (car result))
             (marks (cdr result)))
        (setq state next-state)
        (dolist (mark marks)
          (push mark ranges))))
    (nreverse ranges)))

(provide 'code-review-hunkhighlight-intraline)
;;; code-review-hunkhighlight-intraline.el ends here
