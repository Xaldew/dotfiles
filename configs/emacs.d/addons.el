;; Activate xclip addon.
(require 'xclip)
(xclip-mode 1)

;; Activate cmake mode for editting cmake files.
(require 'cmake-mode)
;(autoload 'cmake-mode "cmake-mode" "PBRT Mode." t)

;; Activate linum for neat line- and column numbers.
(require 'linum)
(global-linum-mode 1)
(setq linum-format "%5d ")
(setq line-number-mode t)
(setq column-number-mode t)


;; Activate autopairs with dependencies.
(require 'autopair)
(autopair-global-mode)


;; Activate Ethan whitespace fixer.
(require 'ethan-wspace)
(global-ethan-wspace-mode t)
(set-default 'ethan-wspace-errors (remove 'tabs ethan-wspace-errors))
(setq mode-require-final-newline nil)


;; Activate whitespace: mark lines longer than 80 columns.
;; Note that to disable whitespace mode, you must evalute
;; M-x global-whatespace-mode AND M-x revert-buffer.
(require 'whitespace)
(setq whitespace-style '(face empty lines-tail trailing))
(global-whitespace-mode t)

;; Disable whitespace and ethan-wspace mode in yasnippets mode.
(add-hook 'snippet-mode-hook (lambda ()
			       (global-whitespace-mode -1)
			       (whitespace-mode -1)
			       (ethan-wspace-mode -1)))


;; Activate Yasnippet
;; binds trigger to C-o to avoid stateful behaviours.
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


;; Activate Magit.
(require 'magit)


;; Activate pbrt-mode.
(autoload 'pbrt-mode "pbrt-mode" "PBRT Mode." t)

;; Activate cg-mode.
(autoload 'cg-mode "cg-mode" "CG Mode." t)

;; Activate hlsl-mode.
(autoload 'hlsl-mode "hlsl-mode" "HLSL Mode." t)

;; Add C-c h as toggle command for hide/show-comments.
(require 'hide-comnt)
(global-set-key "\C-ch" 'hide/show-comments-toggle)
