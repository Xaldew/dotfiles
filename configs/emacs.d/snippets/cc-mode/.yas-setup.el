(defvar yas-start-point 0 "Point at the beginning of snippet expansion.")
(add-hook 'yas-before-expand-snippet-hook
	  (lambda () (setq-local yas-start-point (point))))


(defun c++-split-args (fn-name arg-string)
  "Split a C++ argument string into a list of names."
  (if (and fn-name (bound-and-true-p semantic-mode))
      (let* ((tag (or (semantic-current-tag)
		      (semantic-analyze-find-tag fn-name 'function)))
	     (class   (if tag (semantic-tag-class tag) 'other))
	     (attrs   (if tag (semantic-tag-attributes tag) '()))
	     (members (if attrs (plist-get attrs :members) '()))
	     (found (not members))
	     (args nil))
	(while (and (not found) members) ; Find the member inside a class.
	  (setq tag (pop members))
	  (when (string= (semantic-tag-name tag) fn-name)
	    (setq found t)))
	(setq attrs (semantic-tag-attributes tag))
	(setq args (delete "" (mapcar 'car (plist-get attrs :arguments)))))
    (mapcar (lambda (x)
	      (nth 1 (split-string x)))
	    (split-string arg-string "[[:blank:]]*,[[:blank:]]*" t))))


(defun c++-doxy-docstring (text fn-name return-value &optional make-fields)
  "Return docstring format for the C++ arguments in `text'."
  (let* ((indent (concat "\n" (make-string (current-column) 32)))
         (args (c++-split-args fn-name text))
         (max-len (if args
		      (apply 'max (mapcar (lambda (x) (length x)) args))
		    0))
	 (nr 0)
         (formatted-args
	  (mapconcat
	   (lambda (x) (concat
			"@param "
			x (make-string (- max-len (length x)) ? )
			(if make-fields (format " ${%d:arg%d}" (incf nr) nr))))
	   args
	   indent)))
    (concat
     (unless (string= formatted-args "")
       (concat
	indent (mapconcat 'identity (list formatted-args) indent)))
     (if (and (> (length args) 3)
	      (stringp return-value)
	      (string-match-p "^.*void" return-value))
	 indent)
     (unless (and (stringp return-value)
		  (string-match-p "^.*void" return-value))
       (format "%s@return ${%d:Returns...}" indent (1+ nr))))))


(defun yas-cc-mode-brace (brace syntax)
  "Determine the open brace and newline locations inside a snippet.

   `brace' is a string with the brace character(s) to insert.

   `syntax' is the syntactic symbol of the brace location. Note that the
    position is slightly unreliable since it is based on an non-expanded
    snippet.

   This uses cc-mode commands to read from `c-hanging-braces-alist' to retrieve
   a list with any combination of the symbols: `before' and `after'. These are
   used to determine if a newline should be placed before and/or after
   the opening brace(s).
"
  (let* ((indent (concat "\n" (make-string (current-column) 32)))
	 (ctx `((,syntax ,yas-start-point)))
	 (actions (c-brace-newlines ctx)))
    (concat
     (if (memq 'before actions)
	 indent
       " ")
     brace
     (if (memq 'after actions)
	 indent))))
