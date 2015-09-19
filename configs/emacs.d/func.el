;; This file contains various useful functions and aliases.
(eval-when-compile (require 'rect))

(defun indent-defun ()
  "Indent the currently active defun."
  (interactive)
  (save-mark-and-excursion
   (mark-defun)
   (delete-trailing-whitespace (point) (mark))
   (indent-region (point) (mark))))

(defalias 'idf 'indent-defun)


(defun indent-whole-buffer ()
  "Indent the whole buffer, fixing indentation and removes whitespace."
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil))

(defalias 'iwb 'indent-whole-buffer)


(defun terminal-whitespace-toggle ()
  "White Space Toggling when in terminals."
  (interactive)
  (if (fboundp 'global-whitespace-mode)
      (if global-whitespace-mode
	  (progn
	    (global-whitespace-mode -1))
	(progn
	  (global-whitespace-mode))))
  (revert-buffer nil t))

(defalias 'wst 'terminal-whitespace-toggle)


(defun hardcopy ()
  "Make a hardcopy of the current buffer. I.e., print it."
  (interactive)
  (ps-print-buffer-with-faces))


(defun dos2unix (buffer)
  "Convert the coding system to unix style line-endings."
  (interactive "*b")
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "\r" nil t)
      (replace-match "")))
  (set-buffer-file-coding-system 'utf-8-unix 't))


(defun eol-append (str p0 p1)
  "prompt for string, add it to end of lines in the region"
  (interactive "sWhat shall we append? \nr")
  (goto-char p1)
  (forward-line -1)
  (while (>= (point) p0)
    (end-of-line)
    (insert str)
    (forward-line -1)))


(defun upcase-rectangle (b e)
  "Change all characters in a rectangle to uppercase."
  (interactive "r")
  (apply-on-rectangle 'upcase-rectangle-line b e))


(defun upcase-rectangle-line (startcol endcol)
  "Change all characters on a line to uppercase."
  (when (= (move-to-column startcol) startcol)
    (upcase-region (point)
		   (progn (move-to-column endcol 'coerce)
			  (point)))))


(defun downcase-rectangle (b e)
  "Change all characters in a rectangle to uppercase."
  (interactive "r")
  (apply-on-rectangle 'downcase-rectangle-line b e))


(defun downcase-rectangle-line (startcol endcol)
  "Change all characters on a line to uppercase."
  (when (= (move-to-column startcol) startcol)
    (downcase-region (point)
		     (progn (move-to-column endcol 'coerce)
			    (point)))))


(defun byte-compile-user-dir ()
  "Compile the Elisp code in user directory."
  (interactive)
  (byte-recompile-directory user-emacs-directory 0))
