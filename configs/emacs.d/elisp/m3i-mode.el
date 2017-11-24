;;; m3i-mode.el --- Major mode for ARM m3i files -*- lexical-binding:t -*-
;;
;;; Commentary:
;; Major mode for editing ARM m3i files.
;;
;;; Code:

(defgroup m3i-mode nil
  "M3I register editing."
  :prefix "m3i"
  :group 'files)


(defconst m3i-reg-auto-rx
  (rx ?\[ (group  (+ digit)) ?: (group (+ digit)) ?\]
      (+ space) ?=
      (+ space) (group (or (and "0b" (+ (or ?0 ?1)))
                           (and "0x" (+ hex))
                           (+ digit))))
  "Regular expression to detect and update register programming values.")


(defconst m3i-reg-write-rx
  (rx "AW"
      (+ space) (group "0x" (+ hex))
      (+ space) "L1"
      (+ space) "size32"
      (+ space) "incr"
      (+ space) "nolock"
      (+ space) "C0000"
      (+ space) "P010"
      (+ space) "ID0"
      (+ space) "V4100"
      (+ space) "data"
      (+ space) (group "0x" (+ hex)))
  "Regular expression to match a register programming line.")


(defun m3i-bins-to-hex (bins)
  "Convert a list of BINS to hex format or return an error string."
  ;; TODO: Detect overlap and bin overruns.
  (let ((hex  #x0)
        (mask #x0))
    (cl-loop for (min max val) in bins do
             (setq mask (1- (lsh 1 (1+ (- max min)))))
             (setq hex (logior hex (lsh (logand val mask) min))))
    (format "0x%08x" hex)))


(defun m3i-parse-bin ()
  "Parse the m3i bin write.

Note that this expects the match data to be properly set."
  (let* ((max (string-to-number (match-string 1)))
         (min (string-to-number (match-string 2)))
         (val  (match-string 3))
         (base (if (>= (length val) 2)
                   (substring val 0 2)
                 "")))
    (list min max (cond
                   ((string= base "0b") (string-to-number (substring val 2)  2))
                   ((string= base "0x") (string-to-number (substring val 2) 16))
                   (t (string-to-number val))))))


(defun m3i-update-registers ()
  "Update the value to be programmed into each of the registers."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    ;; Find first matching register programmer.
    (while (re-search-forward m3i-reg-auto-rx nil t)
      (let ((bins (list (m3i-parse-bin))))

        ;; Find the rest of the bins on this line.
        (while (re-search-forward m3i-reg-auto-rx (line-end-position) t)
          (push (m3i-parse-bin) bins))

        ;; Update the register write value.
        (if (re-search-forward m3i-reg-write-rx (line-end-position 3) t)
            (replace-match (m3i-bins-to-hex bins) nil t nil 2)
          (message "No register write line found within 3 lines of bins."))))))


(defvar m3i-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'m3i-update-registers)
    map)
  "Keymap for `m3i-mode'.")


(defconst m3i-font-lock-keywords
  `(((,m3i-reg-write-rx (1 font-lock-constant-face)
                        (2 font-lock-constant-face))))
  "Syntax highlighting for `m3i-mode'.")


(defconst m3i-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?@  "w" table)
    (modify-syntax-entry ?#  "<" table)
    (modify-syntax-entry ?\n ">" table)
    table)
  "Syntax table for `m3i-mode'.")


;;;###autoload
(define-derived-mode m3i-mode prog-mode "M3I"
  "Major mode for editing ARM m3i APB register programming files.

Provides basic syntax highlighting and basic navigation support.

Will automatically detect register sizes and configuration in the
comments and update the value to be programmed into the register.

\\{m3i-mode-map}"
  :group 'm3i-mode
  (setq-local comment-start "# ")
  (setq-local comment-start-skip "#+\\s-*")
  (setq-local font-lock-defaults m3i-font-lock-keywords))


;;;###autoload
(add-to-list 'auto-mode-alist '("\\.m3i\\'" . m3i-mode))


(provide 'm3i-mode)

;;; m3i-mode.el ends here
