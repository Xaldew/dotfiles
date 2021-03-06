;; Setup settings used in C-like languages such as C/C++/Java.
(require 'cc-mode)

(defun my-cc-init-hook ()
  "Initialization hook for CC-mode runs before any other hooks,
   but only once per Emacs session.")
(add-hook 'c-initialization-hook 'my-cc-init-hook)


(defface font-lock-format-specifier-face
  '((t (:foreground "OrangeRed1")))
  "Font Lock mode face used to highlight format specifiers."
  :group 'font-lock-faces)
(defvar font-lock-format-specifier-face
  'font-lock-format-specifier-face
  "Face name to use for format specifiers.")

(defconst doxygen-font-lock-doc-comments
  `(("\\<\\(FIXME\\|TODO\\):?" 1 font-lock-warning-face prepend)
    ,@(copy-sequence javadoc-font-lock-doc-comments))
  "Additional font-lock definitions for doxygen like-fonts.")


(defconst doxygen-font-lock-keywords
  `((,(lambda (limit)
	(c-font-lock-doc-comments "/\\*\\*" limit
	  doxygen-font-lock-doc-comments))))
  "Function to run for doxygen documentation font-locking.")


(defun my-cc-mode-common-hook ()
  "Setup common utilities for all C-like modes."
  (setq-local c-doc-comment-style
	      '((java-mode . javadoc)
		(pike-mode . autodoc)
		(c-mode    . doxygen)
		(c++-mode  . doxygen)))
  (face-remap-add-relative 'font-lock-doc-face
			   :foreground (face-foreground font-lock-comment-face))
  (font-lock-add-keywords
   nil
   '(("\\<\\(FIXME\\|TODO\\):?" 1 font-lock-warning-face prepend)
     ;; Add extra constants for true/false and NULL.
     ("\\<\\(true\\|false\\|NULL\\)" . font-lock-constant-face)
     ;; Add a printf() modifier highlighter.
     ("[^%]\\(%\\([[:digit:]]+\\$\\)?[-+' #0*]*\\([[:digit:]]*\\|\\*\\|\\*[[:digit:]]+\\$\\)\\(\\.\\([[:digit:]]*\\|\\*\\|\\*[[:digit:]]+\\$\\)\\)?\\([hlLjzt]\\|ll\\|hh\\)?\\([aAbdiuoxXDOUfFeEgGcCsSpn]\\|\\[\\^?.[^]]*\\]\\)\\)"
      1 font-lock-format-specifier-face prepend))))
(add-hook 'c-mode-common-hook 'my-cc-mode-common-hook)
