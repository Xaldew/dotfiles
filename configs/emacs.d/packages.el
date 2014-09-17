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

(setq package-list '(company
		     flycheck
		     google-c-style
		     hide-comnt
		     rainbow-mode
		     ecb
		     glsl-mode
		     ethan-wspace
		     autopair
		     graphviz-dot-mode
		     cmake-mode
		     xclip
		     auto-complete
		     ac-etags
		     auto-complete-clang
		     yasnippet
		     gitignore-mode
		     gitconfig-mode
		     ssh-config-mode
		     evil
		     powerline
		     elpy
		     web-mode
		     dart-mode
		     xcscope
		     magit
		     ))

(or (file-exists-p package-user-dir)
    (package-refresh-contents))

(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))
