;; This file contains various useful functions.

;; This function indents and cleans up the whole buffer.
(defun iwb ()
  "Indent the whole buffer."
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil))


(defun wst ()
  "White Space Toggling when in terminals."
  (interactive)
  (if (and (boundp 'global-whitespace-mode) (boundp 'global-whitespace-mode))
      (if (and global-whitespace-mode global-ethan-wspace-mode)
	  (progn
	    (global-whitespace-mode -1)
	    (global-ethan-wspace-mode -1))
	(progn
	  (global-whitespace-mode)
	  (global-ethan-wspace-mode)) ))
  (revert-buffer nil t))


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


(defun create-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (shell-command
   (format
    "find %s -name \"*.c\" -print -or -name \"*.h\" -print -or -name \"*.cpp\" |
 xargs etags --append" dir-name)))



;; (defun find-file-upwards (file-to-find)
;;   "Recursively searches each parent directory starting from the
;; default-directory. looking for a file with name file-to-find.  Returns the path
;; to it or nil if not found."
;;   (labels
;;       ((find-file-r (path)
;; 		    (let* ((parent (file-name-directory path))
;;                            (possible-file (concat parent file-to-find)))
;;                       (cond
;;                        ((file-exists-p possible-file) possible-file) ; Found
;; 		       ;; The parent of ~ is nil and the parent of / is itself.
;; 		       ;; Thus the terminating condition for not finding the
;; 		       ;; file accounts for both.
;; 		       ((or (null parent)
;; 			    (equal parent (directory-file-name parent))) nil)
;; 					; Not found
;;                        (t (find-file-r (directory-file-name parent)))))))
;; 					; Continue
;;     (find-file-r default-directory)))
;; (let ((my-tags-file (find-file-upwards "TAGS")))
;;   (when my-tags-file
;;     (message "Loading tags file: %s" my-tags-file)
;;     (visit-tags-table my-tags-file)))


;; (defun find-next-func ()
;;   "Returns a list describing next function declaration, or nil if not found.

;; (cdr (assoc 'func (doxymacs-find-next-func))) is the function name (string).
;; (cdr (assoc 'args (doxymacs-find-next-func))) is a list of arguments.
;; (cdr (assoc 'return (doxymacs-find-next-func))) is the return type (string).

;; The argument list is a list of strings."
;;   (interactive)
;;   (save-excursion
;;     (if (re-search-forward
;; 	 (concat
;; 	  ;; return type
;; 	  "\\(\\(const[ \t\n]+\\)?[a-zA-Z0-9_]+[ \t\n*&]+\\)?"

;; 	  ;; name
;; 	  "\\(\\([a-zA-Z0-9_~:<,>*&]\\|\\([ \t\n]+::[ \t\n]+\\)\\)+"
;; 	  "\\(o?perator[ \t\n]*.[^(]*\\)?\\)[ \t\n]*("
;; 	  ) nil t)

;; 	(let* ((func (buffer-substring (match-beginning 3) (match-end 3)))
;; 	       (args (buffer-substring (point) (progn
;;                                                 (backward-char 1)
;;                                                 (forward-list)
;;                                                 (backward-char 1)
;;                                                 (point))))
;; 	       (ret (cond
;; 		     ;; Return type specified
;; 		     ((match-beginning 1)
;; 		      (buffer-substring (match-beginning 1) (match-end 1)))
;; 		     ;;Constructor/destructor
;; 		     ((string-match
;; 		       "^\\([a-zA-Z0-9_<,>:*&]+\\)[ \t\n]*::[ \t\n]*~?\\1$"
;; 		       func) "void")
;; 		     ;;Constructor in class decl.
;; 		     ((save-match-data
;; 			(re-search-backward
;; 			 (concat
;; 			  "class[ \t\n]+" (regexp-quote func) "[ \t\n]*{")
;; 			 nil t))
;; 		      "void")
;; 		     ;;Destructor in class decl.
;; 		     ((save-match-data
;; 			(and (string-match "^~\\([a-zA-Z0-9_]+\\)$" func)
;; 			     (save-match-data
;; 			       (re-search-backward
;; 				(concat
;; 				 "class[ \t\n]+" (regexp-quote
;; 						  (match-string 1 func))
;; 				 "[ \t\n]*{") nil t))))
;; 		      "void")
;; 		     ;;Default
;; 		     (t "int"))))
;; 	  (list (cons 'func func)
;; 		(cons 'args (doxymacs-extract-args-list args))
;; 		(cons 'return (doxymacs-core-string ret))))
;;     nil)))
