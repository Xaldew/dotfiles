;; Activate xclip addon if available.
(when (executable-find "xclip")
  (require 'xclip)
  (xclip-mode 1))


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


;; Enable Auto-complete but don't activate the mode.
(require 'auto-complete-config)
(ac-config-default)
(global-auto-complete-mode -1)

(defun my/ac-setup-hook ()
  "A hook for my global Autocomplete setup."
  (setq ac-auto-show-menu 0.1)
  (setq ac-delay 0.1)
  (define-key ac-menu-map (kbd "M-n") 'ac-next)
  (define-key ac-menu-map (kbd "M-p") 'ac-previous)
  (add-to-list 'ac-sources 'ac-source-yasnippet))
(add-hook 'auto-complete-mode-hook 'my/ac-setup-hook)

(defun my/ac-elisp-hook ()
  "Enable autocomplete for Emacs lisp and disable company mode."
  (company-mode -1)
  (auto-complete-mode t))
(add-hook 'emacs-lisp-mode-hook 'my/ac-elisp-hook)


;; Activate company-mode for all buffers but Emacs lisp ones.
(setq company-idle-delay .2)
(add-hook 'after-init-hook 'global-company-mode)


;; Activate Magit.
(require 'magit)
(setq magit-last-seen-setup-instructions "1.4.0")
(global-set-key (kbd "C-c g") 'magit-status)
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


;; Add CSS-eldoc to the css-hook.
(add-hook 'css-mode-hook 'turn-on-css-eldoc)
(add-hook 'css-mode-hook 'rainbow-mode)


;; Add rainbow-delimiter-mode to most programming modes.
(add-hook 'c-mode-common-hook #'rainbow-delimiters-mode)
(add-hook 'python-mode-hook #'rainbow-delimiters-mode)


;; Visualize the (deprecated) form-feed character (\f or ^L).
(add-hook 'emacs-lisp-mode-hook 'form-feed-mode)
(add-hook 'help-mode-hook 'form-feed-mode)

;; Use undo-tree instead of the regular undo-chain.
(global-undo-tree-mode)


;; Enable projectile-mode globally.
(projectile-global-mode)
(setq projectile-mode-line '(:eval (format " Prj[%s]"
					   (projectile-project-name))))
(when (executable-find "ex-ctags")
  (setq projectile-tags-command "ex-ctags -Re -f \"%s\" %s"))

;; Add the Google C/C++ style to list of all styles.
(require 'google-c-style)
(c-add-style "google" google-c-style)


;; Add Ace-window configuration.
(global-set-key (kbd "C-x o") 'ace-window)
(unless (display-graphic-p) (setq aw-scope 'frame))
(global-set-key (kbd "C-c SPC") 'ace-jump-mode)


;; Add expand-region configuration.
(global-set-key (kbd "C-c e") 'er/expand-region)


;; Add srefactor configuration.
(global-set-key (kbd "C-c r") 'srefactor-refactor-at-point)


;; Enable paredit in lisp-like languages.
(defun my-lisp-mode-hook ()
  "Hook to run for lisp-like languages."
  (rainbow-delimiters-mode)
  (paredit-mode))
(add-hook 'emacs-lisp-mode-hook 'my-lisp-mode-hook)
(add-hook 'lisp-mode-hook       'my-lisp-mode-hook)
(add-hook 'scheme-mode-hook     'my-lisp-mode-hook)
(add-hook 'clojure-mode-hook    'my-lisp-mode-hook)


;; Configure Clojure-mode with some additional font-locking.
(require 'clojure-mode-extra-font-locking)


;; Configure Delight to reduce the number of minor mode lighters.
(require 'delight)
(delight '((abbrev-mode     " Abv" "abbrev")
           (smart-tab-mode  " \\t" "smart-tab")
           (form-feed-mode  nil "form-feed")
           (eldoc-mode      nil "eldoc")
           (rainbow-mode)
	   (paredit-mode    nil "paredit")
	   (subword-mode    nil "subword")
	   (undo-tree-mode  nil "undo-tree")
	   (company-mode    " Comp" "company")
	   (yas-minor-mode  nil "yasnippet")
           (emacs-lisp-mode "Elisp" :major)))


;; Configure Emacs multimedia system.
(require 'emms-setup)
(emms-all)
(emms-default-players)
(global-set-key (kbd "C-c m") 'emms-smart-browse)
(setq emms-source-file-default-directory "~/Music/")
