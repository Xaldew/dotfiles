;;; func.el --- Provide various utilities and aliases.
;;
;;; Commentary:
;; This file contains various useful functions and aliases. That I use
;; frequently in everyday coding.
;;
;;; Code:

(declare-function apply-on-rectangle "rect")


(defun indent-defun ()
  "Indent the currently active defun."
  (interactive)
  (save-mark-and-excursion
   (mark-defun)
   (delete-trailing-whitespace (point) (mark))
   (indent-region (point) (mark))))

(defalias 'idf 'indent-defun)


(defun indent-whole-buffer ()
  "Indent the whole buffer.

Fixes indentation and removes erroneous whitespace."
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
  "Make a hardcopy of the current buffer.

I.e., print the buffer.  Attempts to keep as much of the
indentation style and syntax highlighting as possible."
  (interactive)
  (ps-print-buffer-with-faces))


(defun dos2unix (buffer)
  "Convert the BUFFER coding system to Unix style line-endings."
  (interactive "*b")
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "\r" nil t)
      (replace-match "")))
  (set-buffer-file-coding-system 'utf-8-unix 't))


(defun eol-append (string p0 p1)
  "Prompt for STRING and add it to end of lines in the region [P0,P1]."
  (interactive "sWhat shall we append? \nr")
  (goto-char p1)
  (forward-line -1)
  (while (>= (point) p0)
    (end-of-line)
    (insert string)
    (forward-line -1)))


(defun upcase-rectangle (beg end)
  "Change all characters in a rectangle between BEG and END to uppercase."
  (interactive "r")
  (apply-on-rectangle 'upcase-rectangle-line beg end))


(defun upcase-rectangle-line (startcol endcol)
  "Change all characters on a line from STARTCOL to ENDCOL to uppercase."
  (when (= (move-to-column startcol) startcol)
    (upcase-region (point)
		   (progn (move-to-column endcol 'coerce)
			  (point)))))


(defun downcase-rectangle (beg end)
  "Change all characters in a rectangle between BEG and END to lowercase."
  (interactive "r")
  (apply-on-rectangle 'downcase-rectangle-line beg end))


(defun downcase-rectangle-line (startcol endcol)
  "Change all characters on a line from STARTCOL to ENDCOL to lowercase."
  (when (= (move-to-column startcol) startcol)
    (downcase-region (point)
		     (progn (move-to-column endcol 'coerce)
			    (point)))))


(defun byte-compile-user-dir ()
  "Compile the Elisp code in user directory."
  (interactive)
  (byte-recompile-directory user-emacs-directory 0))


(defun locate-directory (dir-name path &optional suffix predicate)
  "Locate the directory DIR-NAME in the list of directories in PATH.

Optional argument SUFFIX is a list of valid directory suffixes.

Optional argument PREDICATE is a function to test the directory
with prior to accepting it.

All arguments are treated similar to to the `locate-file' function."
  (locate-file
   dir-name path suffix
   (lambda (f)
     (when (and (file-directory-p f)
                (or (not predicate) (funcall predicate f)))
       'dir-ok))))


;;; func.el ends here
