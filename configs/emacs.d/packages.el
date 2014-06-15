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
		     ecb
		     glsl-mode
		     ))

(or (file-exists-p package-user-dir)
    (package-refresh-contents))

(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; Activate company-mode for all buffers.
(add-hook 'after-init-hook 'global-company-mode)

;; Flymake creates source code copies in temp directories.
(setq flymake-run-in-place nil)

;; I want to see at most the first 4 errors for a line.
(setq flymake-number-of-errors-to-display 4)

;; No limit on number of parallel checks for flymake.
(setq flymake-max-parallel-syntax-checks nil)
