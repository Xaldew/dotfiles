(defun c++-split-args (fn-name arg-string)
  "Split a C++ argument string into a list of names."
  (if (and (bound-and-true-p semantic-mode)
	   (fboundp 'semantic-analyze-find-tag))
      (let*
	  ((sem-tag (when fn-name (semantic-analyze-find-tag fn-name)))
	   (args (when sem-tag
		   (mapcar 'car (getf (nth 2 sem-tag) :arguments)))))
	args)
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
