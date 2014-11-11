;; If we are using an appropriate emacs, install packages from
;; repositories.
(require 'package)
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
	     '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives
	     '("elpa" . "http://tromey.com/elpa/"))
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/"))


;; Activate all packages.
(package-initialize)

(setq package-list '(ecb
		     flycheck
		     google-c-style
		     hide-comnt
		     ethan-wspace
		     company
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
		     web-mode
		     dart-mode
		     magit
		     auto-complete
		     ac-etags
		     ac-anaconda
		     auto-complete-clang
		     auto-complete-c-headers
		     ggtags
		     ))

(or (file-exists-p package-user-dir)
    (package-refresh-contents))

(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))
