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
