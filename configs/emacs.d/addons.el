;; Activate xclip addon.
(setq load-path (cons (expand-file-name "~/.emacs.d/xclip") load-path))
(require 'xclip)
(xclip-mode 1)

;; Activate cmake mode for editting cmake files.
(setq load-path
      (cons (expand-file-name "~/.emacs.d/emacs-cmake-project") load-path))
(require 'cmake-mode)
(setq auto-mode-alist
      (append '(("CMakeLists\\.txt\\'" . cmake-mode)
		("\\.cmake\\'" . cmake-mode)
		("CMakeLists\\'" . cmake-mode))
	      auto-mode-alist))


;; Activate utilities to work with cmake.
(require 'cmake-project)
(defun maybe-cmake-project-hook ()
  (if (file-exists-p "CMakeLists.txt") (cmake-project-mode)))
(add-hook 'c-mode-hook 'maybe-cmake-project-hook)
(add-hook 'c++-mode-hook 'maybe-cmake-project-hook)


;; Activate linum for neat line- and column numbers.
(require 'linum)
(global-linum-mode 1)
(setq linum-format "%5d ")
(setq flyspell-issue-welcome-flag nil)
(setq line-number-mode t)
(setq column-number-mode t)


;; Activate Ethan whitespace fixer.
(add-to-list 'load-path (expand-file-name "~/.emacs.d/ethan-wspace/lisp"))
(require 'ethan-wspace)
(global-ethan-wspace-mode 1)
(set-default 'ethan-wspace-errors (remove 'tabs ethan-wspace-errors))


;; Activate autopairs addon. I.e. enable autopair in all buffers.
;; Activate cl-lib.
(when (>= emacs-major-version 24)
  (require 'cl)
  (add-to-list 'load-path (expand-file-name "~/.emacs.d/autopair"))
  (require 'autopair)
  (autopair-global-mode))


;; Activate graphviz-dot-mode.
(add-to-list 'load-path (expand-file-name "~/.emacs.d/graphviz-dot-mode"))
(require 'graphviz-dot-mode)


;; Activate whitespace: mark lines longer than 80 columns.
(require 'whitespace)
(setq whitespace-style '(face empty lines-tail trailing))
(global-whitespace-mode t)
