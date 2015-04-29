;; If we are using an appropriate emacs, install packages from
;; repositories.
(require 'package)
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
	     '("elpa" . "http://tromey.com/elpa/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)


;; Needed for important compatibility libraries like cl-lib.
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

;; Activate package library.
(package-initialize)

;; Create a list of packages we want to be installed.
(setq package-list '(ecb
		     nlinum
		     highlight-indentation
		     google-c-style
		     hide-comnt
		     company
		     company-math
		     company-irony
		     company-anaconda
		     company-c-headers
		     company-quickhelp
		     rainbow-mode
		     graphviz-dot-mode
		     glsl-mode
		     autopair
		     cmake-mode
		     xclip
		     yasnippet
		     gitignore-mode
		     gitconfig-mode
		     ssh-config-mode
		     evil
		     elpy
		     anaconda-mode
		     web-mode
		     dart-mode
		     magit
		     auto-complete
		     ac-etags
		     ac-anaconda
		     auto-complete-clang
		     flycheck
		     flycheck-irony
		     flycheck-pos-tip
		     ac-irony
		     irony
		     auto-complete-c-headers
		     c-eldoc
		     css-eldoc
		     irony-eldoc
		     ggtags
		     csv-mode
		     cuda-mode
		     coffee-mode
		     markdown-mode))

;; Create an alist of where we should retrieve certain packages.
(setq package-pinned-packages
      '((coffee-mode . "melpa")))


(or (file-exists-p package-user-dir)
    (package-refresh-contents))


(dolist (package package-list)
  (unless (package-installed-p package)
    (condition-case nil
	(package-install package)
      (error (message "[ERROR]: Failed to install package: %s." package))) ))
