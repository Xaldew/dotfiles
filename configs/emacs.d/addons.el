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
;; binds trigger to C-o to explicitly use yasnippet.
(require 'yasnippet)
(define-key yas-minor-mode-map (kbd "C-o") 'yas-expand)
(define-key yas-keymap (kbd "C-o") 'yas-next-field-or-maybe-expand)
(define-key yas-keymap (kbd "M-n") 'yas-next-field-or-maybe-expand)
(define-key yas-keymap (kbd "M-p") 'yas-prev-field)
(yas-global-mode t)


;; Activate Evil mode to allow Vim users to use my editors.
(require 'evil)

;; Activate auto-complete add-on.
;(add-to-list 'ac-dictionary-directories "~/.emacs.d/dict")
(require 'auto-complete-config)
(require 'auto-complete-clang)
(setq ac-auto-show-menu 0.4)
(setq ac-delay 0.1)
(define-key ac-menu-map (kbd "M-n") 'ac-next)
(define-key ac-menu-map (kbd "M-p") 'ac-previous)
(ac-config-default)

;; Activate auto-complete-etags.
(custom-set-variables '(ac-etags-requires 1))
(eval-after-load "etags" '(progn (ac-etags-setup)))
(add-hook 'cc-mode-common-hook 'ac-etags-ac-setup)

;; Activate Powerline.
(require 'powerline)
(powerline-default-theme)
