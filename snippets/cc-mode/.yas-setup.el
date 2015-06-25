(defun yas-cc-mode-newlines ()
  "Read from `c-hanging-braces-alist' to determine newline locations."
  (let* ((indent (concat "\n" (make-string (current-column) 32)))
	 (actions (c-brace-newlines '(substatement-open . (point)))))
    (print (point))
    (concat
     (if (memq 'before actions)
	 indent
       " ")
     "{"
     (if (memq 'after actions)
	 indent))))
