;;; cppmode.el -- C++ mode settings an functions.

;;; Commentary:
;; Functions and utilities for `c++-mode'.

;;; Code:

(require 'cwarn)


;;;; Setup functions for starting header files in the correct modes.

(defun c-header-test-p ()
  "Test if the header file is a C-mode header."
  (and (buffer-file-name)
       (string= (file-name-extension (buffer-file-name)) "h")
       (file-exists-p
	(concat (file-name-sans-extension (buffer-file-name)) ".c"))))


(defun c++-header-test-p ()
  "Test if the header file is a C++-mode header."
  (and (buffer-file-name)
       (string= (file-name-extension (buffer-file-name)) "h")
       (or (file-exists-p
	    (concat (file-name-sans-extension (buffer-file-name)) ".C"))
	   (file-exists-p
	    (concat (file-name-sans-extension (buffer-file-name)) ".cc"))
	   (file-exists-p
	    (concat (file-name-sans-extension (buffer-file-name)) ".cxx"))
	   (file-exists-p
	    (concat (file-name-sans-extension (buffer-file-name)) ".c++"))
	   (file-exists-p
	    (concat (file-name-sans-extension (buffer-file-name)) ".cpp"))
	   (c++-scan-header-p))))


(defun c++-scan-header-p ()
  "Scan the header and return true if any C++ exclusive keywords are detected."
  (let (is-c++-header)
    (save-excursion
      (while (and (not (eobp)) (not is-c++-header))
	(if (looking-at "[ \t]*\\(class\\|namespace\\|template\\)")
	    (setq is-c++-header t)
	  (forward-line))))
    is-c++-header))


(defun c/c++-list-files ()
  "List all C/C++ files in the current folder."
  (directory-files (file-name-directory (buffer-file-name)) nil
		   ".+\.\\(c\\|C\\|cc\\|cxx\\|cpp\\|c\+\+\\)\\'"))


(add-to-list 'magic-mode-alist '(c-header-test-p   . c-mode))
(add-to-list 'magic-mode-alist '(c++-header-test-p . c++-mode))


;;;; C++ coding style configuration.

(defconst my-c++-style
  '("stroustrup"
    (indent-tabs-mode . nil)     ; Use spaces rather than tabs.
    (c-basic-offset   . 4)       ; Indent with 4 spaces.
    (c-offsets-alist  . ((inline-open         . 0)
			 (brace-list-open     . 0)
			 (inextern-lang       . 0)
			 (innamespace         . 0)
			 (statement-case-open . +)))
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
			       (arglist-cont-nonempty)))))
(c-add-style "my-c++-style" my-c++-style)


(defun my-c++-mode-hook ()
  "Personal `c++-mode'-hook."
  (c-set-style "my-c++-style")
  (auto-fill-mode)
  (add-to-list 'cwarn-configuration '(c++-mode (not reference)))
  (cwarn-mode))
(add-hook 'c++-mode-hook 'my-c++-mode-hook)

;;; cppmode.el ends here
