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


;; Activate autopairs with dependencies.
(require 'autopair)
(autopair-global-mode)


;; Activate whitespace: mark lines longer than 80 columns.
(require 'whitespace)
(setq whitespace-style '(face empty lines-tail trailing))
(global-whitespace-mode t)


;; Activate Yasnippet
;; binds trigger to C-o to explicitly use yasnippet.
(require 'yasnippet)
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map (kbd "C-o") 'yas-expand)

(define-key yas-keymap [(tab)]       nil)
(define-key yas-keymap (kbd "TAB")   nil)
(define-key yas-keymap [(shift tab)] nil)
(define-key yas-keymap [backtab]     nil)
(define-key yas-keymap (kbd "C-o") 'yas-next-field-or-maybe-expand)
(define-key yas-keymap (kbd "C-u") 'yas-prev-field)
(yas-global-mode t)


;; Activate Auto-complete.
(require 'auto-complete-config)
(require 'auto-complete-clang)
(setq ac-auto-show-menu 0.1)
(setq ac-delay 0.1)
(define-key ac-menu-map (kbd "M-n") 'ac-next)
(define-key ac-menu-map (kbd "M-p") 'ac-previous)
(setq-default ac-sources (push 'ac-source-yasnippet ac-sources))
(ac-config-default)
(global-auto-complete-mode t)
(add-hook 'python-mode-hook 'ac-anaconda-setup)

(defun my:ac-c-headers-init ()
  (require 'auto-complete-c-headers)
  (add-to-list 'ac-sources 'ac-source-c-headers))

(add-hook 'c++-mode-hook 'my:ac-c-headers-init)
(add-hook 'c-mode-hook 'my:ac-c-headers-init)


;; Activate Evil mode to allow Vim users to use my editors.
(require 'evil)


;; Activate Powerline.
(require 'powerline)
(powerline-default-theme)


;; Activate Magit.
(require 'magit)
