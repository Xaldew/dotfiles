;;; ccmode.el --- Personal cc-mode configuration.
;;
;;; Commentary:
;; Personal configuration and setup for C-like languages such as C, C++ and
;; Java.
;;
;;; Code:

(require 'cc-mode)


(defface font-lock-format-specifier-face
  '((t (:foreground "OrangeRed1")))
  "Font-lock face used to highlight printf format specifiers."
  :group 'font-lock-faces)


(defconst doxygen-font-lock-doc-comments
  `(("\\<\\(FIXME\\|TODO\\):?" 1 font-lock-warning-face prepend)
    ,@(copy-sequence javadoc-font-lock-doc-comments))
  "Additional font-lock definitions for doxygen like-fonts.")


(defvar printf-fmt-regexp
  (concat "[^%]\\(%"
          "\\(?:[[:digit:]]+\\$\\)?"
          "[-+' #0*]*"
          "\\(?:[[:digit:]]*\\|\\*\\|\\*[[:digit:]]+\\$\\)"
          "\\(?:\\.\\(?:[[:digit:]]*\\|\\*\\|\\*[[:digit:]]+\\$\\)\\)?"
          "\\(?:[hlLjzt]\\|ll\\|hh\\)?"
          "\\(?:[aAbdiuoxXDOUfFeEgGcCsSpn]\\|\\[\\^?.[^]]*\\]\\)\\)")
  "Regular expression to capture all possible `printf' formats in C/C++.")


(defun printf-fmt-matcher (end)
  "Search for `printf' format specifiers within strings up to END."
  (let (pos (case-fold-search t))
    (while (and (setq pos (re-search-forward printf-fmt-regexp end t))
                (null (nth 3 (syntax-ppss pos)))))
    pos))


(defconst doxygen-font-lock-keywords
  `((,(lambda (limit)
	(c-font-lock-doc-comments "/\\*\\*" limit
	  doxygen-font-lock-doc-comments))))
  "Function to run for doxygen documentation font-locking.")


(defun my-cc-mode-common-hook ()
  "Setup common utilities for all C-like modes."
  (face-remap-add-relative 'font-lock-doc-face
			   :foreground (face-foreground font-lock-comment-face))
  (font-lock-add-keywords
   nil
   '(("\\<\\(FIXME\\|TODO\\):?" 1 font-lock-warning-face prepend)
     ;; Add extra constants for true/false and NULL.
     ("\\<\\(true\\|false\\|NULL\\)" . font-lock-constant-face)
     ;; Add a printf() modifier highlighter.
     (printf-fmt-matcher 0 'font-lock-format-specifier-face t))))

(add-hook 'c-mode-common-hook #'my-cc-mode-common-hook)


;; Setup the default coding and commenting styles.
(setq c-default-style '((java-mode . "java")
                        (awk-mode  . "awk")
                        (c-mode    . "linux-tabs-style")
                        (c++-mode  . "my-c++-style")
                        (other     . "gnu")))


(setq c-doc-comment-style '((java-mode . javadoc)
                            (pike-mode . autodoc)
                            (c-mode    . doxygen)
                            (c++-mode  . doxygen)))

;;; ccmode.el ends here
