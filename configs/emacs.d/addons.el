;; Activate xclip addon.
(require 'xclip)
(xclip-mode 1)

;; Activate cmake mode for editting cmake files.
(require 'cmake-mode)

;; Activate linum for neat line- and column numbers.
(require 'linum)
(global-linum-mode 1)
(setq linum-format "%5d ")
(setq flyspell-issue-welcome-flag nil)
(setq line-number-mode t)
(setq column-number-mode t)


;; Activate Ethan whitespace fixer.
(require 'ethan-wspace)
(global-ethan-wspace-mode 1)
(set-default 'ethan-wspace-errors (remove 'tabs ethan-wspace-errors))
(setq mode-require-final-newline nil)


;; Activate autopairs addon. I.e. enable autopair in all buffers.
;; Activate cl-lib.
(when (>= emacs-major-version 24)
  (require 'cl)
  (require 'autopair)
  (autopair-global-mode))


;; Activate graphviz-dot-mode.
(require 'graphviz-dot-mode)


;; Activate whitespace: mark lines longer than 80 columns.
(require 'whitespace)
(setq whitespace-style '(face empty lines-tail trailing))
(global-whitespace-mode t)


;; Activate Yasnippet
(require 'yasnippet)
(yas-global-mode t)


;; Activate Evil mode to allow Vim users to use my editors.
(require 'evil)

;; Activate auto-complete add-on.
;(add-to-list 'ac-dictionary-directories "~/.emacs.d/dict")
(require 'auto-complete-config)
(require 'auto-complete-clang)
(ac-config-default)

;; Activate company-mode for all buffers.
;; (add-hook 'after-init-hook 'global-company-mode)

;; Flymake is deprecated, flycheck is far superior.
;; ;; Flymake creates source code copies in temp directories.
;; (setq flymake-run-in-place nil)

;; ;; I want to see at most the first 4 errors for a line.
;; (setq flymake-number-of-errors-to-display 4)

;; ;; No limit on number of parallel checks for flymake.
;; (setq flymake-max-parallel-syntax-checks nil)
