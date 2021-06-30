;;; subtitles-mode.el --- Mode for editing subtitles -*- lexical-binding: t -*-

;; Copyright (C) 2016 Gustaf Waldemarson

;; Author: Gustaf Waldemarson <gustaf.waldemarson at gmail.com>
;; Keywords: multimedia, tools
;; Package-Requires: ((hydra))

;;; Commentary:
;; Major mode for editing subtitles of various formats.

;;; Code:


(defgroup subtitles nil
  "Customization group for `subtitles-mode'"
  :group 'multimedia)


(defcustom subtitles-type 'srt
  "The type of subtitle to edit."
  :group 'subtitles
  :type 'symbol
  :safe #'symbolp)


;;;; Fontification.


(defvar subtitles-srt-seq-rx
  (rx (and bol (+ digit) eol))
  "Regular expression to match the `srt' sequence indicator.")


(defvar subtitles-srt-time-rx
  (rx (and (group-n 1 (>= 2 digit)) (group-n 5 ":")
           (group-n 2 (=  2 digit)) (group-n 6 ":")
           (group-n 3 (=  2 digit)) (group-n 7 ",")
           (group-n 4 (=  3 digit))))
  "Regular expression to match the `srt' time specifiers.")


(defvar subtitles-srt-separator-rx
  (rx (and
       (= 2 digit) ":" (= 2 digit) ":" (= 2 digit) "," (= 3 digit)
       (* space) (group-n 1 "-->") (* space)
       (= 2 digit) ":" (= 2 digit) ":" (= 2 digit) "," (= 3 digit)))
  "Regular expression to match the `srt' time specifiers.")


(defvar subtitles-srt-func-rx
  (rx (and (* space) (or "<" "{") (? "/")
           (group-n 1 (in alpha "_")
                    (* (in alnum "_" "-" ".")))))
  "Regular expression to match the `html' functions in `srt' subtitles.")


(defvar subtitles-srt-variable-rx
  (rx (and (* space) (group-n 1 (in alpha "_" ":")
                              (* (in alnum "_" "-" "." ":")))
           "=" (or "\"" "'")))
  "Regular expression to match the `html' variables in `srt' subtitles.")


(defvar subtitles-srt-tags-alist
  '(("b" . 'bold)
    ("i" . 'italic)
    ("u" . 'underline))
  "Association list over the various tags `srt' supports.")


(defvar subtitles-srt-tags-keywords-alist
  (cl-loop for (key . val) in subtitles-srt-tags-alist
           collect
           (list (eval
                  `(rx (and (group-n 1 (or "<" "{"))
                            (group-n 2 ,key) (* space)
                            (group-n 3 (or ">" "}"))
                            (group-n 4 (*? not-newline))
                            (backref 1) "/"
                            (backref 2)
                            (backref 3))))
                 (list 4 val)))
  "Font-locking keywords for `html' tags inside `srt' subtitles.")


(defvar subtitles-srt-font-lock-keywords
  `((,@subtitles-srt-tags-keywords-alist
     (,subtitles-srt-separator-rx (1 'bold))
     (,subtitles-srt-func-rx (1 font-lock-function-name-face))
     (,subtitles-srt-variable-rx (1 font-lock-variable-name-face))
     (,subtitles-srt-time-rx (1 'bold) (2 'bold) (3 'bold) (4 'bold))
     (,subtitles-srt-seq-rx . 'bold)))
  "List over `font-lock' specifiers for `srt'.")


(defconst subtitles-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?< "(>"  table)
    (modify-syntax-entry ?> ")<"  table)
    (modify-syntax-entry ?{ "(}"  table)
    (modify-syntax-entry ?} "){"  table)
    table))


(defun subtitles-match-to-integers (indicies)
  "Convert the `match-data' in INDICIES to a list of integers."
  (cl-loop for i in indicies collect
           (string-to-number (match-string-no-properties i))))


(defun subtitles-srt-add-time (time diff)
  "With subtitle TIME on format (H M S MS), add the time difference DIFF.

Note that the time will saturate at (99 59 59 999)."
  (cl-destructuring-bind (h m s ms) (cl-mapcar #'+ time diff)
    (let* ((msc (/ ms 1000))
           (ms  (% ms 1000))
           (s   (+ s msc))
           (sc  (/ s 60))
           (s   (% s 60))
           (m   (+ m sc))
           (mc  (/ m 60))
           (m   (% m 60))
           (h   (+ h mc)))
      (if (>= h 100)
          (list 99 59 59 999)
        (list h m s ms)))))


(defun subtitles-backfill-diff (diff)
  "Compute the new backfilling time difference from DIFF."
  (let ((i     (cl-position-if-not #'zerop diff))
        (ndiff (cl-mapcar (lambda (x) (/ x 10)) diff)))
    (cond
     ((not i)
      (list 0 0 0 0))
     ((equal i 3)                 ; No carrying behavior.
      ndiff)
     ((equal (nth i diff) 1)
      (append (make-list (+ i 1) 0)
              (list (if (equal (+ i 1) 3) 100 10))
              (make-list (- 3 (+ i 1)) 0)))
     (t
      ndiff))))


(defun subtitles-srt-backfill-add (time diff)
  "With subtitle TIME on format (H M S MS), add the time difference DIFF.

Note that this version performs \"backfilling\". That is, when
the time starts to saturate, it will gradually back-fill rather
than immediately saturate the time."
  (let ((ndiff diff)
        (ntime (subtitles-srt-add-time time diff))
        (limit (list 99 59 59 999))
        (zero  (list 0 0 0 0)))
    (while (and (equal ntime limit)
                (not (equal ndiff zero)))
      (setq ndiff (subtitles-backfill-diff ndiff))
      (setq ntime (if (equal ndiff zero)
                      limit
                    (subtitles-srt-add-time time ndiff))))
    ntime))


(defun subtitles-srt-sub-time (time diff)
  "With subtitle TIME on format (H M S MS), subtract the time difference DIFF.

Note that the time will saturate at (0 0 0 0)."
  (cl-destructuring-bind (h m s ms) (cl-mapcar #'- time diff)
    (let* ((msc (if (>= ms 0) 0 -1))
           (ms  (if (>= ms 0) ms (+ 1000 ms)))
           (s   (+ s msc))
           (sc  (if (>= s 0) 0 -1))
           (s   (if (>= s 0) s (+ 60 s)))
           (m   (+ m sc))
           (mc  (if (>= m 0) 0 -1))
           (m   (if (>= m 0) m (+ 60 m)))
           (h   (+ h mc)))
      (if (>= h 0)
          (list h m s ms)
        (list 0 0 0 0)))))


(defun subtitles-srt-backfill-sub (time diff)
  "With subtitle TIME on format (H M S MS), add the time difference DIFF.

Note that this version performs \"backfilling\". That is, when
the time starts to saturate, it will gradually back-fill rather
than immediately saturate the time."
  (let ((ndiff diff)
        (ntime (subtitles-srt-sub-time time diff))
        (zero  (list 0 0 0 0)))
    (while (and (equal ntime zero)
                (not (equal ndiff zero)))
      (setq ndiff (subtitles-backfill-diff ndiff))
      (setq ntime (if (equal ndiff zero)
                      zero
                    (subtitles-srt-sub-time time ndiff))))
    ntime))


(defun subtitles-to-srt-time-string (time)
  "Convert an `elisp' TIME list to a `srt' time string."
  (cl-destructuring-bind (h m s ms) time
    (format "%02d:%02d:%02d,%03d" h m s ms)))


(defun subtitles-find-time-diff (pos)
  "Compute the time difference to use based on POS inside the `match-data'."
  (cond
   ((and (>= pos (match-beginning 1))
         (<= pos (match-end 1)))
    (list (expt 10 (- (match-end 1) pos 1)) 0 0 0))
   ((and (>= pos (match-beginning 2))
         (<= pos (match-end 2)))
    (list 0 (expt 10 (- (match-end 2) pos 1)) 0 0))
   ((and (>= pos (match-beginning 3))
         (<= pos (match-end 3)))
    (list 0 0 (expt 10 (- (match-end 3) pos 1)) 0))
   (t
    (list 0 0 0 (expt 10 (- (match-end 4) pos 1))))))


(defcustom subtitles-srt-acceleration 1
  "The current `acceleration' used by the `srt' time tickers.

Each consecutive call to the `subtitles-srt-incr-time' or
  `subtitles-srt-decr-time' function adds this value to the
  `subtitles-srt-speed' thus gradually accelerating the ticker.")


(defvar subtitles-srt-speed 0
  "The current `speed' of the `srt' time ticker.")


(defun subtitles-srt-modify-time (pos function)
  "If POS is at an `srt' time value, modify it by using FUNCTION.

Note that time values will carry over and that consecutive calls
will gradually accelerate the time modifications.

FUNCTION should probably only be `time-add' or `time-subtract',
but any function that operates on a time point and time
difference and returns a new time point will work.

If point is not currently on a time-value, this function does nothing."
  (save-excursion
    (save-match-data
      (skip-chars-backward ":,0123456789")
      (when (and (/= (char-after pos) ?:)
                 (/= (char-after pos) ?,)
                 (looking-at subtitles-srt-time-rx)
                 (>= pos (match-beginning 0))
                 (<= pos (match-end 0)))
        (if (equal last-command this-command)
            (cl-incf subtitles-srt-speed subtitles-srt-acceleration)
          (setq subtitles-srt-speed 0))
        (let* ((time (subtitles-match-to-integers '(1 2 3 4)))
               (diff  (subtitles-find-time-diff pos))
               (ntime (funcall function time diff)))
          (cl-loop repeat subtitles-srt-speed do
                   (setq ntime (funcall function ntime diff)))
          (replace-match (subtitles-to-srt-time-string ntime))))))
  ;; Replacing the text will change the buffer and move point, so
  ;; unconditionally return to `pos'.
  (goto-char pos))


(defun subtitles-srt-incr-time (pos)
  "If POS is at an `srt' time value, increase the number under point.

Note that time values will carry over and that consecutive calls
will gradually accelerate the time increments.

If point is not currently on a time-value, this function does nothing."
  (interactive "d")
  (subtitles-srt-modify-time pos #'subtitles-srt-backfill-add))


(defun subtitles-srt-decr-time (pos)
  "If POS is at an `srt' time value, decrease the number under point.

Note that time values will carry over and that consecutive calls
will gradually accelerate the time decrements.

If point is not currently on a time-value, this function does nothing."
  (interactive "d")
  (subtitles-srt-modify-time pos #'subtitles-srt-backfill-sub))


(defvar subtitles-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-n") #'subtitles-srt-incr-time)
    (define-key map (kbd "M-p") #'subtitles-srt-decr-time)
    map)
  "Keymap for `subtitles-mode'.")


(define-derived-mode subtitles-mode text-mode "Subs"
  "Major mode for editing subtitle files.

\\{subtitles-mode-map}"
  :group 'subtitles
  :syntax-table subtitles-syntax-table
  (setq-local font-lock-defaults subtitles-srt-font-lock-keywords)
  (font-lock-flush))


;;;###autoload
(add-to-list 'auto-mode-alist '("\\.srt\\'" . subtitles-mode))


(provide 'subtitles-mode)

;;; subtitles-mode.el ends here
