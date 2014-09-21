;; Setup various C-styles for different projects.


;; Misra style used in ARM MVE Model/Firmware/Driver
(defconst misra-c-style
  '("bsd"
    (my-coding-style . "misra")
    (indent-tabs-mode . nil)
    (tab-width . 4)
    (c-basic-offset . 4)
    )
  )

(c-add-style "misra" misra-c-style)


;; Linux kernel coding c-style.
(defun c-lineup-arglist-tabs-only (ignored)
  "Line up argument lists by tabs, not spaces"
  (let* ((anchor (c-langelem-pos c-syntactic-element))
	 (column (c-langelem-2nd-pos c-syntactic-element))
	 (offset (- (1+ column) anchor))
	 (steps (floor offset c-basic-offset)))
    (* (max steps 1)
       c-basic-offset)))

(defconst linux-tabs-style
  '("linux"
    (my-coding-style . "linux")
    (indent-tabs-mode . t)
    (c-offsets-alist
     (arglist-cont-nonempty
      c-lineup-gcc-asm-reg
      c-lineup-arglist-tabs-only))))

(c-add-style "linux-tabs-style" linux-tabs-style)


;; Setup functions and variables to guess the style to use.
(defvar my-c-styles-alist
  (mapc (lambda (elt)
	  (cons (purecopy (car elt)) (cdr elt)))
	'((".*/linux.*/.*\\.[ch]$" . "linux-tabs-style")
	  (".*/.*kernel.*/.*\\.[ch]$" . "linux-tabs-style")
	  (".*/.*linux/.*\\.[ch]$" . "linux-tabs-style")
	  (".*/.*mve*.*\\.[ch]$" . "misra")
	  (".*c-utils.*\\.[ch]$" . "linux-tabs-style")))
  "A list of reg-ex to styles for my-c-style-guesser")

(defun my-c-style-guesser (filename)
  "Guess the C style we should use based on the path of the buffer"
  (message (concat "my-c-style-guesser " filename))
  (assoc-default filename my-c-styles-alist 'string-match))


(defun my-c-mode-hook ()
  "My personal c-mode hook."
  (interactive)
  (message "In my-c-mode-hook")
  (flycheck-mode)
  (turn-on-auto-fill)
 ; Set the c-style if we can. I think mmm-mode gets in the way of
 ; buffer-file-name for setting sub-modes, so check we have one first
  (when buffer-file-name
    (message (format "looking for style for buffer %s" (buffer-file-name)))
    (let ((style (my-c-style-guesser (buffer-file-name))))
      (when style
	(message (format "my-c-mode-hook: found style %s" style))
	(c-set-style style))))
  )

(defun my-brace-placement ()
  "Place the braces based on the currently active c-style."
  (cond ((string-equal my-coding-style "linux")
	 (insert " {"))
	((string-equal my-coding-style "misra")
	 (newline-and-indent)
	 (insert "{"))
	)
  )


;; Add personal c-mode setup function to c-mode-hook.
(add-hook 'c-mode-hook 'my-c-mode-hook)
