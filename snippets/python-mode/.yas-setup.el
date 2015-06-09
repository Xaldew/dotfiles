(defun python-args-to-reST-docstring ()
  "Return a reST docstring format for the python arguments in yas-text."
  (let* ((indent (concat "\n" (make-string (current-column) 32)))
         (args (python-split-args yas-text))
         (max-len (if args
		      (apply 'max (mapcar
				   (lambda (x) (length (nth 0 x))) args)) 0))
         (formatted-args
	  (mapconcat
	   (lambda (x)
	     (concat ":param: " (nth 0 x) ":"
		     (if (nth 1 x) (concat "\(default " (nth 1 x) "\)"))))
	   args
	   indent))
	 (formatted-types
	  (mapconcat
	   (lambda (x)
	     (concat ":type: " (nth 0 x) ":"))
	   args
	   indent)))
    (unless (string= formatted-args "")
      (concat
       indent
       (mapconcat 'identity
		  (list ".. Keyword Arguments:" formatted-args ""
			".. Types:" formatted-types)
		  indent)
       indent))))
