;; If we are using an appropriate emacs, install packages from
;; repositories.
(add-to-list 'package-archives
	     '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
	     '("elpa" . "http://tromey.com/elpa/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))


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
		     srefactor
		     expand-region
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
		     projectile
		     clojure-mode
		     clojure-mode-extra-font-locking
		     cider
		     rainbow-delimiters
		     zenburn-theme
		     sublime-themes
		     markdown-mode
		     dropdown-list
		     ace-window
		     ace-jump-mode
		     form-feed
		     auctex
		     auctex-latexmk
		     auto-complete-auctex
		     company-auctex
		     undo-tree))


;; Create an alist of where we should retrieve certain packages.
(setq package-pinned-packages
      '((coffee-mode . "melpa")
	(form-feed . "melpa")))


(or (file-exists-p package-user-dir)
    (package-refresh-contents))


(dolist (package package-list)
  (unless (package-installed-p package)
    (condition-case nil
	(package-install package)
      (error (message "[ERROR]: Failed to install package: %s." package)))))

;; Remove no longer needed packages.
(when (fboundp 'package-autoremove)
  (package-autoremove))
