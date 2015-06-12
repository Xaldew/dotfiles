(defun c++-split-args (fn-name arg-string)
  "Split a C++ argument string into a list of names."
  (if (fboundp 'semantic-analyze-find-tag)
      (let*
	  ((sem-tag (when fn-name (semantic-analyze-find-tag fn-name)))
	   (args (when sem-tag
		   (mapcar 'car (getf (nth 2 sem-tag) :arguments)))))
	args)
    (mapcar (lambda (x)
	      (nth 1 (split-string x)))
	    (split-string arg-string "[[:blank:]]*,[[:blank:]]*" t))))

(defun c++-doxy-docstring (fn-name return-value)
  "return docstring format for the python arguments in yas-text"
  (let* ((indent (concat "\n" (make-string (current-column) 32)))
         (args (c++-split-args fn-name yas-text))
         (max-len (if args
		      (apply 'max (mapcar (lambda (x) (length x)) args))
		    0))
         (formatted-args
	  (mapconcat
	   (lambda (x) (concat
			"@param "
			x
			(make-string (- max-len (length x)) ? )
			" "))
	   args
	   indent)))
    (concat
     (unless (string= formatted-args "")
       (concat
	indent (mapconcat 'identity (list formatted-args) indent)))
     (unless (string= "void" return-value)
       (concat indent "@return ")))))
