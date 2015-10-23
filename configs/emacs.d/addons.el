(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

(use-package xclip
  :ensure t
  :if (executable-find "xclip") ; Activate xclip if possible.
  :config (xclip-mode 1))


;; Activate Yasnippet
(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :commands (yas-global-mode yas-minor-mode)
  :init
  (add-hook 'after-init-hook 'yas-global-mode)
  :config
  ;; rebind trigger to C-o to avoid stateful behaviors.
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  (define-key yas-minor-mode-map (kbd "C-o") 'yas-expand)

  (define-key yas-keymap [(tab)]       nil)
  (define-key yas-keymap (kbd "TAB")   nil)
  (define-key yas-keymap [(shift tab)] nil)
  (define-key yas-keymap [backtab]     nil)
  (define-key yas-keymap (kbd "C-o") 'yas-next-field-or-maybe-expand)
  (define-key yas-keymap (kbd "C-u") 'yas-prev-field)
  (defun my/snippet-hook ()
    "Hook for Yasnippet-mode."
    (setq-local whitespace-style '(face
                                   newline
                                   newline-mark
                                   tabs
                                   tab-mark
                                   trailing)))
  ;; Change whitespace visualization in yasnippets mode.
  (add-hook 'snippet-mode-hook 'my/snippet-hook))



(use-package auto-complete
  :ensure t
  :commands (auto-complete-mode global-auto-complete-mode)
  :init
  (add-hook 'emacs-lisp-mode-hook 'auto-complete-mode)
  :config
  (defun my/ac-setup-hook ()
    "A hook for my global Autocomplete setup."
    (setq ac-auto-show-menu 0.1)
    (setq ac-delay 0.1)
    (define-key ac-menu-map (kbd "M-n") 'ac-next)
    (define-key ac-menu-map (kbd "M-p") 'ac-previous)
    (add-to-list 'ac-sources 'ac-source-yasnippet))

  (defun my/disable-company-hook ()
    "Enable autocomplete for Emacs lisp and disable company mode."
    (when (fboundp 'company-mode)
      (company-mode -1)))

  ;; Enable Auto-complete but don't activate the mode.
  (ac-config-default)
  (global-auto-complete-mode -1)

  (add-hook 'auto-complete-mode-hook 'my/disable-company-hook)
  (add-hook 'auto-complete-mode-hook 'my/ac-setup-hook)

  (use-package ac-etags                :defer t :ensure t)
  (use-package ac-anaconda             :defer t :ensure t)
  (use-package auto-complete-clang     :defer t :ensure t)
  (use-package auto-complete-c-headers :defer t :ensure t))


;; Activate company-mode for all buffers but Emacs lisp ones.
(use-package company
  :ensure t
  :diminish (company-mode . "Cp")
  :commands (company-mode global-company-mode)
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (use-package company-math :defer t :ensure t)
  (use-package company-irony :defer t :ensure t)
  (use-package company-anaconda
    :defer t
    :ensure t
    :config
    (add-to-list 'company-backends 'company-anaconda))
  (use-package company-c-headers :defer t :ensure t)
  (use-package company-quickhelp :defer t :ensure t)
  (setq company-idle-delay .1))


;; Activate Magit.
(use-package magit
  :ensure t
  :demand t
  :init
  (setq magit-last-seen-setup-instructions "1.4.0")
  :bind ("C-c g" . magit-status)
  :config
  (use-package magit-gerrit :ensure t)
  (add-to-list 'auto-mode-alist '("gitignore\\'" . gitignore-mode)))


;; Add C-c h as toggle command for hide/show-comments.
(use-package hide-comnt
  :ensure t
  :bind ("C-c h" . hide/show-comments-toggle))


;; Change tab width for coffee-mode.
(use-package coffee-mode
  :ensure t
  :pin melpa
  :config (setq coffee-tab-width 4))


;; Add Pop-ups for Flycheck errors.
(use-package flycheck
  :ensure t
  :commands flycheck-mode
  :config
  (use-package flycheck-pos-tip
    :ensure t
    :config
    (setq flycheck-display-errors-function
          #'flycheck-pos-tip-error-messages))
  (use-package flycheck-irony :ensure t)

  (setq-local flycheck-gcc-language-standard   "c11")
  (setq-local flycheck-clang-language-standard "c11")
  (setq-local flycheck-gcc-language-standard   "c++11")
  (setq-local flycheck-clang-language-standard "c++11")
  (add-hook 'c-mode-hook 'flycheck-mode)
  (add-hook 'c++-mode-hook 'flycheck-mode))


;; Add CSS-eldoc to the css-hook.
(use-package css-eldoc
  :ensure t
  :commands turn-on-css-eldoc
  :init
  (add-hook 'css-mode-hook 'turn-on-css-eldoc))


(use-package rainbow-mode
  :ensure t
  :commands rainbow-mode
  :diminish rainbow-mode
  :init
  (add-hook 'css-mode-hook 'rainbow-mode))


(use-package c-eldoc
  :ensure t
  :commands c-turn-on-eldoc-mode
  :init
  ;; Enable C-eldoc for C/C++.
  (add-hook 'c-mode-hook   'c-turn-on-eldoc-mode)
  (add-hook 'c++-mode-hook 'c-turn-on-eldoc-mode))


;; Add rainbow-delimiter-mode to most programming modes.
(use-package rainbow-delimiters
  :ensure t
  :commands rainbow-delimiters-mode
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))


;; Visualize the (deprecated) form-feed character (\f or ^L).
(use-package form-feed
  :ensure t
  :pin melpa
  :commands form-feed-mode
  :diminish form-feed-mode
  :init
  (add-hook 'emacs-lisp-mode-hook 'form-feed-mode)
  (add-hook 'help-mode-hook 'form-feed-mode)
  (add-hook 'compilation-mode-hook 'form-feed-mode))


;; Use undo-tree instead of the regular undo-chain.
(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode))



;; Enable projectile-mode globally.
(use-package projectile
  :ensure t
  :config
  (projectile-global-mode)
  (setq projectile-mode-line '(:eval (format " Prj[%s]"
                                             (projectile-project-name))))
  (when (executable-find "ex-ctags")
    (setq projectile-tags-command "ex-ctags -Re -f \"%s\" %s")))


;; Add the Google C/C++ style to list of all styles.
(use-package google-c-style
  :ensure t
  :config (c-add-style "google" google-c-style))


;; Add ace-jump and ace-window configuration.
(use-package ace-window
  :ensure t
  :bind ("C-x o" . ace-window)
  :config
  (unless (display-graphic-p) (setq aw-scope 'frame)))
(use-package ace-jump-mode
  :ensure t
  :bind ("C-c SPC" . ace-jump-mode))


;; Add expand-region configuration.
(use-package expand-region
  :functions (er/contract-region)
  :ensure t
  :bind ("C-c e" . er/expand-region))


;; Add srefactor configuration.
(use-package srefactor
  :ensure t
  :bind ("C-c r" . srefactor-refactor-at-point))


;; Enable paredit for lisp-like languages.
(use-package paredit
  :ensure t
  :pin melpa
  :commands paredit-mode
  :diminish paredit-mode
  :init
  (add-hook 'emacs-lisp-mode-hook 'paredit-mode)
  (add-hook 'lisp-mode-hook       'paredit-mode)
  (add-hook 'scheme-mode-hook     'paredit-mode)
  (add-hook 'clojure-mode-hook    'paredit-mode))


;; Configure Clojure-mode with some additional font-locking.
(use-package clojure-mode
  :ensure t
  :commands clojure-mode
  :config
  (use-package clojure-mode-extra-font-locking :ensure t))


;; Configure Emacs multimedia system.
(use-package emms
  :ensure t
  :pin melpa
  :defer t
  :bind ("C-c m" . emms-smart-browse)
  :init
  (setq emms-source-file-default-directory "~/Music/")
  :config
  (use-package emms-player-mpv :ensure t :pin melpa)
  (emms-all)
  (emms-default-players))


(use-package highlight-indentation
  :ensure t
  :commands highlight-indentation-mode
  :config
  (add-hook 'python-mode-hook 'highlight-indentation-mode))


(use-package anaconda-mode
  :ensure t
  :commands anaconda-mode
  :config
  ;; Minor fix for the re-mapping of find-tag in 25.1.
  (when (fboundp 'xref-find-definitions)
    (define-key anaconda-mode-map
      [remap xref-find-definitions] 'anaconda-mode-goto))
  (add-hook 'python-mode-hook 'anaconda-mode))


(use-package ssh-config-mode
  :ensure t
  :mode (("ssh_config\\'"      . ssh-config-mode)
         ("\.*ssh\.*config\\'" . ssh-config-mode)))


(use-package clang-format
  :ensure t
  :init
  (global-set-key (kbd "C-c f") 'clang-format-region))


;; Fix LaTeX settings and AucTeX.
(use-package auctex
  :ensure t
  :mode ("\\.tex\\'" . latex-mode)
  :commands (latex-mode LaTeX-mode plain-tex-mode)
  :init
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq reftex-plug-into-AUCTeX t)

  ;; Disable Toolbar if XPM images aren't supported.
  (unless (image-type-available-p 'xpm)
    (setq LaTeX-enable-toolbar nil))

  (add-hook 'LaTeX-mode-hook 'visual-line-mode)
  (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)

  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)   ; with AUCTeX LaTeX mode
  (add-hook 'latex-mode-hook 'turn-on-reftex)   ; with Emacs latex mode

  :config
  (use-package auctex-latexmk :defer t :ensure t)
  (use-package auto-complete-auctex :defer t :ensure t)
  (use-package company-auctex :defer t :ensure t))


;; On Windows, use the ssh agency package for ssh-agents.
(use-package ssh-agency
  :if (windows-os-p)
  :ensure t
  :init
  (let* ((git-exe (executable-find "git.exe"))
	 (git-dir (directory-file-name
                   (file-name-directory
                    (directory-file-name
                     (file-name-directory git-exe)))))
         (pub-keys (directory-files
                    (expand-file-name "~/.ssh/") 'full ".*\\.pub\\'")))
    (setq ssh-agency-bin-dir (concat git-dir "/usr/bin"))
    (setq ssh-agency-keys (mapcar 'file-name-sans-extension pub-keys))))


;; Semantic need this variable to be defined.
(setq ecb-minor-mode nil)
(use-package ecb
  :defines ecb-minor-mode
  :functions ecb-minor-mode
  :ensure t
  :commands (ecb-activate ecb-minor-mode))


;; Setup the langtool library if the LanguageTool library is available.
(use-package langtool
  :if (locate-directory "languagetool" exec-path)
  :defer t
  :ensure t
  :init
  (let ((lt-dir (or (locate-directory "languagetool" exec-path) ""))
        (lt-jar "languagetool-commandline.jar"))
    (setq langtool-language-tool-jar
          (concat (file-name-directory lt-dir)
                  "languagetool/"
                  lt-jar)))
  (setq langtool-mother-tongue "en")
  (setq langtool-default-language "en-US"))


;; Install various major-mode packages and defer where it is possible.
(use-package graphviz-dot-mode :ensure t :defer t)
(use-package glsl-mode         :ensure t :defer t)
(use-package cmake-mode        :ensure t :defer t)
(use-package git-commit        :ensure t)
(use-package gitignore-mode    :ensure t :defer t)
(use-package gitconfig-mode    :ensure t :defer t)
(use-package markdown-mode     :ensure t :defer t)
(use-package dart-mode         :ensure t :defer t)
(use-package web-mode          :ensure t :defer t)
(use-package cuda-mode         :ensure t :defer t)
(use-package csv-mode          :ensure t :defer t)

(use-package powershell :ensure t)
;;(use-package ahk-mode   :ensure t)


;; Add various themes.
(use-package aurora-theme   :defer t :ensure t)
(use-package zenburn-theme  :defer t :ensure t)
(use-package sublime-themes :defer t :ensure t)

;; Install miscellaneous packages.
(use-package evil :functions (evil-ace-jump-exit-recursive-edit) :defer t :ensure t)
(use-package elpy  :defer t :ensure t)
(use-package irony :defer t :ensure t)
(use-package irony-eldoc :defer t :ensure t)
(use-package ggtags :defer t :ensure t)
(use-package cider  :defer t :ensure t)
(use-package dropdown-list :defer t :ensure t)

;; Remove the lighter for a number of built in packages.
(use-package flyspell :diminish flyspell-mode)
(use-package subword  :diminish subword-mode)
(use-package whitespace :diminish global-whitespace-mode)
(use-package eldoc :diminish eldoc-mode)
(use-package cwarn :commands cwarn-mode :diminish cwarn-mode)
(use-package abbrev :diminish abbrev-mode)
