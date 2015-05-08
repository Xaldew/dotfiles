;; Activate xclip addon if available.
(when (executable-find "xclip")
  (require 'xclip)
  (xclip-mode 1))


;; Activate nlinum for line- and column numbers.
(require 'nlinum)
(global-nlinum-mode t)
(setq nlinum-format "%5d ")
(setq line-number-mode t)
(setq column-number-mode t)


;; Activate autopairs with dependencies.
(require 'autopair)
(autopair-global-mode)


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

;; Change whitespace visualization in yasnippets mode.
(defun my/snippet-hook ()
  "Hook for Yasnippet-mode."
  (setq-local whitespace-style '(face
				 newline
				 newline-mark
				 tabs
				 tab-mark
				 trailing)))
(add-hook 'snippet-mode-hook 'my/snippet-hook)


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
(setq magit-last-seen-setup-instructions "1.4.0")
(add-to-list 'auto-mode-alist '("gitignore\\'" . gitignore-mode))

;; Add C-c h as toggle command for hide/show-comments.
(require 'hide-comnt)
(global-set-key "\C-ch" 'hide/show-comments-toggle)

;; Change tab width for coffee-mode.
(setq coffee-tab-width 4)

;; Add Pop-ups for Flycheck errors.
(eval-after-load 'flycheck
  '(custom-set-variables
    '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages)))


;; Activate anaconda-mode.
(defun my/python-mode-hook ()
  "My python mode hook."
  (highlight-indentation-mode)
  (anaconda-mode)
  (eldoc-mode)
  (setq-local whitespace-style '(face
				 tabs
				 tab-mark
				 indentation
				 indentation::space
				 trailing))
  ;; Define a fix for the re-mapping of find-tag.
  (define-key anaconda-mode-map
    [remap xref-find-definitions] 'anaconda-mode-goto))
(add-hook 'python-mode-hook 'my/python-mode-hook)

;; Add CSS-eldoc to the css-hook.
(add-hook 'css-mode-hook 'turn-on-css-eldoc)
(add-hook 'css-mode-hook 'rainbow-mode)


;; Add rainbow-delimiter-mode to most programming modes.
;;(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(add-hook 'c-mode-common-hook #'rainbow-delimiters-mode)
(add-hook 'python-mode-hook #'rainbow-delimiters-mode)
(add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
