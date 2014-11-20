;; Setup various C-styles for different projects.

;; Activate Javadoc comment highlighting for C and C++.
(defun my-cc-init-hook ()
  "Initialization hook for CC-mode runs before any other hooks."
  (setq c-doc-comment-style
	'((java-mode . javadoc)
	  (pike-mode . autodoc)
	  (c-mode    . javadoc)
	  (c++-mode  . javadoc)))
  (set-face-foreground 'font-lock-doc-face
		       (face-foreground font-lock-comment-face)))
(add-hook 'c-initialization-hook 'my-cc-init-hook)

(defvar font-lock-format-specifier-face
  'font-lock-format-specifier-face
  "Face name to use for format specifiers.")

(defface font-lock-format-specifier-face
  '((t (:foreground "OrangeRed1")))
  "Font Lock mode face used to highlight format specifiers."
  :group 'font-lock-faces)

;; Customizations for all modes in CC Mode.
(defun my-cc-mode-common-hook ()
  (font-lock-add-keywords
   nil
   '(("\\<\\(FIXME\\|TODO\\):" 1 font-lock-warning-face prepend)
     ;; Add a printf() modifier highlighter.
     ("[^%]\\(%\\([[:digit:]]+\\$\\)?[-+' #0*]*\\([[:digit:]]*\\|\\*\\|\\*[[:digit:]]+\\$\\)\\(\\.\\([[:digit:]]*\\|\\*\\|\\*[[:digit:]]+\\$\\)\\)?\\([hlLjzt]\\|ll\\|hh\\)?\\([aAbdiuoxXDOUfFeEgGcCsSpn]\\|\\[\\^?.[^]]*\\]\\)\\)"
      1 font-lock-format-specifier-face prepend)))
  )

(add-hook 'c-mode-common-hook 'my-cc-mode-common-hook)


(defconst misra-c-style
  '("bsd"
    (my-coding-style . "misra")
    (indent-tabs-mode . nil)
    (tab-width . 4)
    (c-basic-offset . 4)
    (c-hanging-braces-alist
     (substatement-open . (before after)))
    )
  "Misra coding style with modifications for the ARM MVE model/firmware/driver."
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
      c-lineup-arglist-tabs-only))
    )
  "Linux kernel coding style forbidding use of spaces as whitespace."
  )
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
  (flycheck-mode)
  (setq flycheck-clang-language-standard "c11")
  (turn-on-auto-fill)
  (ggtags-mode)
  ;; Set the c-style if we can. Some modes can get in the way of
  ;; buffer-file-name when setting sub-modes, so check we have one first.
  (when buffer-file-name
    (message (format "looking for style for buffer %s" (buffer-file-name)))
    (let ((style (my-c-style-guesser (buffer-file-name))))
      (if style
	  (progn
	    (message (format "my-c-mode-hook: Using style %s" style))
	    (c-set-style style))
	(c-set-style "linux-tabs-style") ; Default to linux-tab-style.
	)) )
  )

(defun my-brace-placement ()
  "Place the braces based on the currently active c-style."
  (cond ((string-equal my-coding-style "linux")
	 (insert " {"))
	((string-equal my-coding-style "misra")
	 (newline-and-indent)
	 (insert "{"))
	(t (insert " {")) ; Default.
	))

(defun my-alist-test ()
  "Various tests with c-hanging-brace-alist."
  (interactive)
  (message (symbol-name '(assoc 'substatement-open c-hanging-braces-alist)))
  )

;; Add personal c-mode setup function to c-mode-hook.
(add-hook 'c-mode-hook 'my-c-mode-hook)
