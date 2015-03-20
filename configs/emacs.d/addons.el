;; Activate xclip addon.
(require 'xclip)
(xclip-mode 1)


;; Activate linum for neat line- and column numbers.
(require 'nlinum)
(global-nlinum-mode t)
(setq nlinum-format "%5d ")
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

;; Disable whitespace and ethan-wspace mode in yasnippets mode.
(add-hook 'snippet-mode-hook
	  (lambda ()
	    (setq-local whitespace-style '(face
					   newline
					   newline-mark
					   tabs
					   tab-mark
					   trailing))
	    (ethan-wspace-mode -1)))


;; Activate Auto-complete.
(require 'auto-complete-config)
(require 'auto-complete-c-headers)
(require 'auto-complete-clang)
(ac-config-default)

(defun my/ac-setup-hook ()
  "A hook for my global Autocomplete setup."
  (setq ac-auto-show-menu 0.1)
  (setq ac-delay 0.1)
  (define-key ac-menu-map (kbd "M-n") 'ac-next)
  (define-key ac-menu-map (kbd "M-p") 'ac-previous)
  (add-to-list 'ac-sources 'ac-source-yasnippet))

(defun my/ac-cc-setup-hook ()
  "A hook for Autocomplete settings in any C-like mode."
  (add-to-list 'ac-sources 'ac-source-c-headers))

(defun my/ac-c++-setup-hook ()
  "A hook for Autocomplete settings in C++-mode."
  (add-to-list 'ac-sources 'ac-source-clang))

(add-hook 'auto-complete-mode-hook 'my/ac-setup-hook)
(add-hook 'cc-mode-hook 'ac-cc-mode-setup)
(add-hook 'cc-mode-hook 'my/ac-cc-setup-hook)
(add-hook 'c++-mode-hook 'my/ac-c++-setup-hook)
(add-hook 'python-mode-hook 'ac-anaconda-setup)
(global-auto-complete-mode t)

;; Activate Magit.
(require 'magit)

;; Add C-c h as toggle command for hide/show-comments.
(require 'hide-comnt)
(global-set-key "\C-ch" 'hide/show-comments-toggle)

;; Activate pbrt-mode.
(autoload 'pbrt-mode "pbrt-mode" "PBRT Mode." t)

;; Activate cg-mode.
(autoload 'cg-mode "cg-mode" "CG Mode." t)

;; Activate hlsl-mode.
(autoload 'hlsl-mode "hlsl-mode" "HLSL Mode." t)

;; Activate cmake mode for editting cmake files.
(autoload 'cmake-mode "cmake-mode" "CMake Mode." t)

;; Activate coffee-mode.
(autoload 'coffee-mode "coffee-mode" "Coffee Mode." t)
(setq coffee-tab-width 4)


;; Add Pop-ups for Flycheck errors.
(eval-after-load 'flycheck
  '(custom-set-variables
    '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages)))
