;; Setup various C-styles for different projects.

(defconst misra-c-style
  '("bsd"
    (indent-tabs-mode . nil)
    (tab-width . 4)
    (c-basic-offset . 4)
    (c-hanging-braces-alist . ((brace-list-open)
			       (brace-entry-open)
			       (statement-cont)
			       (substatement-open before after)
			       (block-close . c-snug-do-while)
			       (extern-lang-open before after)
			       (namespace-open before after)
			       (module-open after)
			       (composition-open after)
			       (inexpr-class-open after)
			       (inexpr-class-close before)
			       (arglist-cont-nonempty)))
    (c-offsets-alist . ((inextern-lang  . 0)
			(innamespace    . 0))))
  "Misra C-style with modifications for the ARM MVE model/firmware/driver.")
(c-add-style "misra" misra-c-style)


;; Linux kernel coding c-style.
(defun c-lineup-arglist-tabs-only (ignored)
  "Line up argument lists by tabs, not spaces."
  (let* ((anchor (c-langelem-pos c-syntactic-element))
	 (column (c-langelem-2nd-pos c-syntactic-element))
	 (offset (- (1+ column) anchor))
	 (steps (floor offset c-basic-offset)))
    (* (max steps 1)
       c-basic-offset)))

(defconst linux-tabs-style
  '("linux"
    (indent-tabs-mode . t)
    (c-offsets-alist  . ((inextern-lang         . 0)
			 (arglist-cont-nonempty .
						(c-lineup-gcc-asm-reg
						 c-lineup-arglist-tabs-only)))))
  "Linux kernel coding style forbidding use of spaces as whitespace.")
(c-add-style "linux-tabs-style" linux-tabs-style)


;; Setup functions and variables to guess the style to use.
(defvar my-c-styles-alist
  (mapc (lambda (elt)
	  (cons (purecopy (car elt)) (cdr elt)))
	'((".*/linux.*/.*\\.[ch]$"    . "linux-tabs-style")
	  (".*/.*kernel.*/.*\\.[ch]$" . "linux-tabs-style")
	  (".*/.*linux/.*\\.[ch]$"    . "linux-tabs-style")
	  (".*/.*mve*.*\\.[ch]$"      . "misra")
	  (".*c-utils.*\\.[ch]$"      . "linux-tabs-style")))
  "A list of regular expressions to match styles for the c-style guesser.")


(defun my-c-style-guesser (filename)
  "Guess the C style we should use based on the path of the buffer"
  (assoc-default filename my-c-styles-alist 'string-match))


(defun my-guess-c-style ()
  "Attempt to figure out the C-style if the is visible.

Note that some modes can get in the way of `buffer-file-name' so
we check that first.

"
  (message (format "looking for style for buffer %s" (buffer-file-name)))
  (when (and buffer-file-name)
    (let ((style (my-c-style-guesser (buffer-file-name))))
      (if style
	  (progn
	    (message (format "Using style: %s." style))
	    (c-set-style style))
        (message "Style not found. Guessing...")
	(c-guess)))))


(defun my-c-mode-hook ()
  "My personal c-mode hook."
  (interactive)
  (auto-fill-mode 1)
  (my-guess-c-style))


;; Add personal c-mode setup function to c-mode-hook.
(add-hook 'c-mode-hook 'my-c-mode-hook)
(add-hook 'c-mode-hook 'cwarn-mode)
