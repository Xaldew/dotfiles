(defvar yas-start-point 0 "Point at the beginning of snippet expansion.")
(add-hook 'yas-before-expand-snippet-hook
	  (lambda () (setq-local yas-start-point (point))))


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
