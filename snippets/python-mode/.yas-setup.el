(defun python-args-to-reST-docstring (text &optional make-fields)
  "Return a reST docstring format for the python arguments in yas-text."
  (let* ((indent (concat "\n" (make-string (current-column) 32)))
         (args (python-split-args text))
	 (nr 0)
         (formatted-args
	  (mapconcat
	   (lambda (x)
	     (concat ":param: " (nth 0 x) ":"
		     (if make-fields (format " ${%d:arg%d} " (incf nr) nr))
		     (if (nth 1 x) (concat " \(default " (nth 1 x) "\)"))))
	   args
	   indent)))
    (setq python-reST-snippet text)
    (unless (string= formatted-args "")
      (concat
       indent
       (mapconcat 'identity
		  (list ".. Keyword Arguments:" formatted-args)
		  indent)
       indent))))


(defun python-arg-types-to-reST-docstring ()
  "Return a reST docstring format for the python argument types in yas-text."
  (let* ((indent (concat "\n" (make-string (current-column) 32)))
         (args (python-split-args yas-text))
	 (formatted-types
	  (mapconcat (lambda (x) (concat ":type: " (nth 0 x) ":"))
		     args indent)))
    (unless (string= formatted-types "")
      (concat
       (mapconcat 'identity (list  ".. Types:" formatted-types) indent)
       indent))))


(defvar python-reST-snippet nil "Variable to store snippet content in.")

(defun yas-stacked-snippet (fun snippet-sym)
  "Expand a snippet after passing it through the given function."
  (let ((snippet (symbol-value snippet-sym)))
    (print yas-text)
    (print fun)
    (print snippet-sym)
    (print snippet)
    (print (concat (funcall fun snippet t) "$0"))
    (yas-expand-snippet (concat (funcall fun snippet t) "$0"))))
