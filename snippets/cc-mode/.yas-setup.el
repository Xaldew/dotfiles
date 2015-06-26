(defun yas-cc-mode-brace (brace syntax)
  "Determine the open brace and newline locations.

   `brace' is a string with the brace character(s) to insert.

   `syntax' is the syntactic context of the brace location. Note that the
    position is slightly unreliable since it is based on an non-expanded
    snippet.

   This uses cc-mode commands to read from `c-hanging-brace-alist' to retrieve
   a list with any combination of the symbols: `before' and `after'. These are
   used to determine if a newline should be placed before and/or after
   the opening brace.
"
  (let* ((indent (concat "\n" (make-string (current-column) 32)))
	 (actions (c-brace-newlines syntax)))
    (concat
     (if (memq 'before actions)
	 indent
       " ")
     brace
     (if (memq 'after actions)
	 indent))))
