(defun python-args-to-reST-docstring (text &optional make-fields)
  "Return a reST docstring format for the python arguments in yas-text."
  (let* ((indent (concat "\n" (make-string (current-column) 32)))
         (args (python-split-args text))
	 (nr 0)
         (formatted-args
	  (mapconcat
	   (lambda (x)
	     (concat ":param " (nth 0 x) ":"
		     (if make-fields (format " ${%d:arg%d}" (cl-incf nr) nr))
		     (if (nth 1 x) (concat " \(default " (nth 1 x) "\)"))))
	   args
	   indent)))
    (unless (string= formatted-args "")
      (concat
       indent
       (mapconcat 'identity
		  (list ".. Keyword Arguments:" formatted-args)
		  indent)
       indent))))


(defun python-arg-types-to-reST-docstring (text &optional make-fields)
  "Return a reST docstring format for the python argument types in text."
  (let* ((indent (concat "\n" (make-string (current-column) 32)))
         (args (python-split-args text))
	 (i 0)
	 (nr (length args))
	 (formatted-types
	  (mapconcat (lambda (x)
		       (concat ":type " (nth 0 x) ":"
			       (if make-fields
				   (format " ${%d:type%d}"
					   (cl-incf nr) (cl-incf i)))))
		     args indent)))
    (unless (string= formatted-types "")
      (concat
       (mapconcat 'identity (list  ".. Types:" formatted-types) indent)
       indent))))


(defun python-args-to-google-docstring (text &optional make-fields)
  "Return a reST docstring format for the python arguments in yas-text."
  (let* ((indent (concat "\n" (make-string (current-column) 32)))
         (args (python-split-args text))
    	 (nr 0)
         (formatted-args
    	  (mapconcat
    	   (lambda (x)
    	     (concat "   " (nth 0 x)
    		     (if make-fields (format " ${%d:arg%d}" (cl-incf nr) nr))
    		     (if (nth 1 x) (concat " \(default " (nth 1 x) "\)"))))
    	   args
    	   indent)))
    (unless (string= formatted-args "")
      (concat
       (mapconcat 'identity
    		  (list "" "Args:" formatted-args)
    		  indent)
       "\n"))))
