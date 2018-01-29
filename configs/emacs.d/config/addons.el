;;; addons.el --- File for third party add-on configuration.
;;
;;; Commentary:
;; All third party add-on configuration is gathered in this file.
;;
;;; Code:

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

(setq use-package-verbose t)
(setq use-package-minimum-reported-time 0.01)

;; Define prefix commands for personal key binding groups.
(defmacro my-define-group (prefix name &optional map)
  "Define a group at PREFIX with NAME in MAP."
  (let ((command (intern (format "group:%s" name))))
    `(progn
       (define-prefix-command ',command)
       (bind-key ,prefix #',command ,map))))

(defmacro set-variable-in-hook (hook variable value &optional name)
  "Create a proper HOOK function for setting VARIABLE to VALUE.

NAME can be used to set the name of the defined function."
  (let* ((hname (symbol-name hook))
         (vname (symbol-name variable))
         (fname (intern (or name (format "set-%s-in-%s" vname hname)))))
    `(progn
       (defun ,fname ()
         (setq-local ,variable ,value))
       (add-hook (quote ,hook) (function ,fname)))))

(my-define-group "C-c p" projects)
(my-define-group "C-c v" version-control)
(my-define-group "C-c x" text)
(my-define-group "C-c x a" align)


(use-package anyclip-mode
  :if (and (not (display-graphic-p))
           (executable-find "anyclip"))
  :init
  (anyclip-mode t))


(use-package which-key ; Show help popups for prefix keys
  :ensure t
  :diminish (which-key-mode)
  :init (which-key-mode)
  :config
  (unless which-key-dont-use-unicode
    (setq which-key-replacement-alist
          (append which-key-replacement-alist
                  '((("up")         . ("↑"))
                    (("down")       . ("↓"))
                    (("DEL")        . ("⌫"))
                    (("RET")        . ("⏎"))
                    (("deletechar") . ("⌦"))))))
  (setq which-key-idle-delay 0.8))


(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :commands (yas-global-mode yas-minor-mode)
  :init
  (setq yas-verbosity 1)
  (defvar yas-active-p nil "Is yasnippet currently expanding a snippet?")
  (defvar yas-active-region nil "The start and end of the active snippet.")

  (defun yas-in-snippet-p (tbeg tend)
  "Test if the region [TBEG, TEND] overlaps the active yasnippet region."
  (cl-destructuring-bind (ybeg yend) yas-active-region
    (not (or (and (< ybeg tbeg) (< yend tbeg))
             (and (< tbeg ybeg) (< tend ybeg))))))

  (add-hook 'after-init-hook #'yas-global-mode)
  :config
  ;; Rebind trigger to C-o to avoid stateful behaviors.
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  (define-key yas-minor-mode-map (kbd "C-o") yas-maybe-expand)

  (define-key yas-keymap [(tab)]       nil)
  (define-key yas-keymap (kbd "TAB")   nil)
  (define-key yas-keymap [(shift tab)] nil)
  (define-key yas-keymap [backtab]     nil)
  (define-key yas-keymap (kbd "C-o") #'yas-next-field-or-maybe-expand)
  (define-key yas-keymap (kbd "C-u") #'yas-prev-field)

  (defun my-before-snippet-hook ()
    "Set `yas-active-p' to t and the active region bounds."
    (setq yas-active-p t)
    (setq yas-active-region (list yas-snippet-beg yas-snippet-end)))

  (defun my-after-snippet-hook ()
    "Set `yas-active-p' to nil and clear active region bounds."
    (setq yas-active-p nil)
    (setq yas-active-region nil))

  (add-hook 'yas-before-expand-snippet-hook #'my-before-snippet-hook)
  (add-hook 'yas-after-exit-snippet-hook    #'my-after-snippet-hook)

  (defun my/snippet-hook ()
    "Hook for Yasnippet-mode."
    (setq-local whitespace-style '(face
                                   newline
                                   newline-mark
                                   tabs
                                   tab-mark
                                   trailing)))
  ;; Change whitespace visualization in yasnippets mode.
  (add-hook 'snippet-mode-hook #'my/snippet-hook))

(use-package yasnippet-snippets
  :ensure t
  :defer t)


(use-package auto-complete
  :ensure t
  :commands (auto-complete-mode global-auto-complete-mode)
  :init
  (add-hook 'emacs-lisp-mode-hook #'auto-complete-mode)
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

  (add-hook 'auto-complete-mode-hook #'my/disable-company-hook)
  (add-hook 'auto-complete-mode-hook #'my/ac-setup-hook)

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
  (add-hook 'after-init-hook #'global-company-mode)
  :config
  (setq company-idle-delay .1)
  (setq company-tooltip-align-annotations t))

(use-package company-template
  :ensure company
  :after company
  :defer t
  :config
  (let ((map company-template-nav-map))
    (define-key map (kbd "C-o") #'company-template-forward-field)
    (define-key map [tab] nil)
    (define-key map (kbd "TAB") nil)))

(use-package company-math      :defer t :ensure company)
(use-package company-c-headers :defer t :ensure company)
(use-package company-quickhelp :defer t :ensure company)
(use-package company-irony     :defer t :ensure company)

(use-package company-anaconda
  :defer t
  :ensure company
  :config
  (add-to-list 'company-backends 'company-anaconda))


(use-package magit
  :ensure t
  :defer t
  :mode ("gitignore\\'" . gitignore-mode)
  :bind ("C-c g" . magit-status)
  :functions (magit-stash-pop magit-define-popup-action)
  :config
  (defun magit-stash-and-pull ()
    "Use `magit' to stash, pull, and pop the stash."
    (interactive)
    (magit-call-git "stash")
    (magit-call-git "pull")
    (magit-stash-pop "stash@{0}"))

  (magit-define-popup-action 'magit-pull-popup
    ?s
    "Stash and pull"
    #'magit-stash-and-pull))

(use-package git-commit
  :ensure t
  :defer t
  :mode (("COMMIT_EDITMSG\\'" . global-git-commit-mode))
  :config
  (set-variable-in-hook git-commit-setup-hook fill-column 70)
  (setq git-commit-style-convention-checks
        '(non-empty-second-line overlong-summary-line)))

(use-package magit-gerrit :ensure t :defer t)
(use-package magit-annex  :ensure t :defer t)
(use-package magit-svn    :ensure t :defer t)


(use-package coffee-mode
  :defer t
  :ensure t
  :mode (("\\.coffee\\'" . coffee-mode))
  :pin melpa
  :config (setq coffee-tab-width 4))


(use-package flycheck
  :defer t
  :ensure t
  :commands flycheck-mode
  :init
  (add-hook 'prog-mode-hook #'flycheck-mode)
  :config
  (defun my-flycheck-hook ()
    "Personal hook for per-buffer flycheck settings."
    (cond
     ((eq major-mode 'c-mode)
      (setq flycheck-gcc-language-standard   "c11")
      (setq flycheck-clang-language-standard "c11"))
     ((eq major-mode 'c++-mode)
      (setq flycheck-gcc-language-standard   "c++11")
      (setq flycheck-clang-language-standard "c++11"))))

  (add-hook 'flycheck-mode-hook #'my-flycheck-hook))

(use-package flycheck-pos-tip
  :ensure t
  :defer t
  :after flycheck
  :init
  (unless (fboundp #'x-hide-tip)
    (defalias #'x-hide-tip #'ignore))
  (add-hook 'flycheck-mode-hook #'flycheck-pos-tip-mode)
  :config
  (setq flycheck-pos-tip-display-errors-tty-function
        #'flycheck-popup-tip-show-popup))


(use-package flycheck-popup-tip
  :ensure t
  :defer t
  :commands flycheck-popup-tip-show-popup)


(use-package flycheck-irony :ensure t :defer t :after flycheck)
(use-package flycheck-package :ensure t :defer t :after package-lint)
(use-package package-lint :ensure t :defer t)


;; Add CSS-eldoc to the css-hook.
(use-package css-eldoc
  :ensure t
  :commands turn-on-css-eldoc
  :init
  (add-hook 'css-mode-hook #'turn-on-css-eldoc))


(use-package rainbow-mode
  :ensure t
  :commands rainbow-mode
  :diminish rainbow-mode
  :init
  (add-hook 'css-mode-hook #'rainbow-mode))


(use-package c-eldoc
  :ensure t
  :commands c-turn-on-eldoc-mode
  :init
  ;; Enable C-eldoc for C/C++.
  (add-hook 'c-mode-hook   #'c-turn-on-eldoc-mode)
  (add-hook 'c++-mode-hook #'c-turn-on-eldoc-mode))


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
  (add-hook 'emacs-lisp-mode-hook #'form-feed-mode)
  (add-hook 'help-mode-hook #'form-feed-mode)
  (add-hook 'compilation-mode-hook #'form-feed-mode))


;; Use undo-tree instead of the regular undo-chain.
(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode))


(use-package projectile
  :ensure t
  :defer t
  :init
  (add-hook 'text-mode-hook #'projectile-mode)
  (add-hook 'prog-mode-hook #'projectile-mode)
  :config
  (setq projectile-mode-line
        '(:eval (format " Prj[%s]" (projectile-project-name))))
  (when (executable-find "uctags")
    (setq projectile-tags-command "uctags -Re -f \"%s\" %s")))


;; Add the Google C/C++ style to list of all styles.
(use-package google-c-style
  :ensure t
  :defer t
  :after (cc-mode)
  :config (c-add-style "google" google-c-style))


(use-package ace-window
  :ensure t
  :defer t
  :bind ("C-x o" . ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (unless (display-graphic-p) (setq aw-scope 'frame)))

(use-package ace-link
  :ensure t
  :defer t
  :init
  (ace-link-setup-default))

(use-package avy
  :ensure t
  :defer t
  :bind (("C-c SPC" . avy-goto-char)
         ("C-c n"   . avy-goto-char))
  :config
  (setq avy-all-windows t))


;; Add expand-region configuration.
(use-package expand-region
  :functions (er/contract-region)
  :ensure t
  :defer t
  :bind ("C-c e" . er/expand-region))


;; Add srefactor configuration.
(use-package srefactor
  :ensure t
  :defer t
  :bind ("C-c r" . srefactor-refactor-at-point))


;; Enable paredit for lisp-like languages.
(use-package paredit
  :ensure t
  :pin melpa
  :defer t
  :commands paredit-mode
  :diminish paredit-mode
  :init
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  (add-hook 'lisp-mode-hook       #'paredit-mode)
  (add-hook 'scheme-mode-hook     #'paredit-mode)
  (add-hook 'clojure-mode-hook    #'paredit-mode)
  :config
  (define-key paredit-mode-map (kbd "C-h") #'paredit-backward-delete)
  (define-key paredit-mode-map (kbd "M-h") #'paredit-backward-kill-word))


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
  (setq emms-source-file-default-directory
        (let ((xdg (getenv "XDG_MUSIC_DIR")))
          (cond
           ((and xdg (file-exists-p xdg))
            xdg)
           ((file-exists-p "~/Music")
            "~/Music")
           ((file-exists-p "~/Musik")
            "~/Musik"))))
  :config
  (emms-all)
  (emms-default-players)
  ;; Add midi formats to VLC-player
  (emms-player-set
   emms-player-vlc
   'regex
   (concat "\\`\\(http[s]?\\|mms\\)://\\|"
           (apply #'emms-player-simple-regexp
                  `("mid" "midi" ,@emms-player-base-format-list)))))

(use-package emms-player-mpv :ensure emms :pin melpa :defer t)


(use-package anzu
  :defer t
  :ensure t
  :pin melpa
  :init
  (setq anzu-mode-lighter "")
  (add-hook 'after-init-hook #'global-anzu-mode))


(use-package ein
  :ensure t
  :defer t)


(use-package ob-ipython
  :ensure t
  :defer t)


(use-package pyimport
  :ensure t
  :defer t
  :init
  (defun my-pyimport-hook ()
    "Personal hook for pyimport keys."
    (define-key python-mode-map (kbd "C-c C-u") #'pyimport-remove-unused)
    (define-key python-mode-map (kbd "C-c C-i") #'pyimport-insert-missing))
  (add-hook 'python-mode-hook #'my-pyimport-hook))


(use-package pyvenv
  :ensure t
  :defer t
  :init
  (add-hook 'python-mode-hook #'pyvenv-mode))


(use-package highlight-indentation
  :ensure t
  :defer t
  :commands highlight-indentation-mode
  :init
  (add-hook 'python-mode-hook #'highlight-indentation-mode))


(use-package anaconda-mode
  :ensure t
  :defer t
  :if (executable-find "pip")
  :diminish anaconda-mode
  :commands anaconda-mode
  :init
  (add-hook 'python-mode-hook #'anaconda-mode))


(use-package ssh-config-mode
  :ensure t
  :defer t
  :mode (("ssh_config\\'"     . ssh-config-mode)
         ("\\.ssh.*config\\'" . ssh-config-mode)))


(use-package clang-format
  :if (executable-find "clang-format")
  :ensure t
  :defer t
  :bind ("C-c f" . clang-format-create-style)
  :init
  (defvar my-clang-styles
    (directory-files
     (concat user-emacs-directory "styles/") :full "[^.]")
    "My collection of clang-format styles.")

  (defun clang-format--find-or-create-style ()
    "Find or create a `.clang-format' file with style and root directory."
    (let* ((dir default-directory)
           (prefix current-prefix-arg)
           (styles my-clang-styles)
           (found (unless prefix (locate-dominating-file dir ".clang-format")))
           (root  (or found (read-directory-name "Root directory: " nil nil t)))
           (style (or found (completing-read "Style: " styles nil t nil nil styles))))
      (unless found
        (copy-file style (concat root ".clang-format")))
      (list (region-beginning) (region-end))))

  (defun clang-format-create-style (beg end)
    "Use clang-format to automatically format the selected region.

Creates a `.clang-format' file at a selected root directory and
with the selected style before formatting the region [BEG, END]
if such a file does not already exist.

When `universal-argument' is set, always query for root directory
and style."
    (interactive (clang-format--find-or-create-style))
    (clang-format-region beg end)))


;; Fix LaTeX settings and AucTeX.
(use-package auctex
  :ensure t
  :defer t
  :mode ("\\.tex\\'" . latex-mode)
  :defines LaTeX-enable-toolbar
  :commands (latex-mode LaTeX-mode plain-tex-mode)
  :init
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq reftex-plug-into-AUCTeX t)

  ;; Rename generated local AUCTeX directories.
  (setq TeX-style-local ".auctex_style")
  (setq TeX-auto-local ".auctex_auto")

  ;; Disable Toolbar if XPM images aren't supported.
  (unless (image-type-available-p 'xpm)
    (setq LaTeX-enable-toolbar nil))

  (add-hook 'LaTeX-mode-hook #'visual-line-mode)
  (add-hook 'LaTeX-mode-hook #'flyspell-mode)
  (add-hook 'LaTeX-mode-hook #'LaTeX-math-mode)
  (add-hook 'LaTeX-mode-hook #'prettify-symbols-mode)

  (add-hook 'LaTeX-mode-hook #'turn-on-reftex)
  (add-hook 'latex-mode-hook #'turn-on-reftex))

(use-package cdlatex
  :ensure t
  :defer t
  :init
  (add-hook 'LaTeX-mode-hook #'turn-on-cdlatex)
  (add-hook 'latex-mode-hook #'turn-on-cdlatex)
  (add-hook 'org-mode-hook #'turn-on-org-cdlatex))

(use-package auctex-latexmk       :ensure auctex :defer t)
(use-package auto-complete-auctex :ensure auctex :defer t)
(use-package company-auctex       :ensure auctex :defer t)


;; On Windows without Cygwin, use the ssh agency package for ssh-agents.
(when (and (not (cygwin-p))
           (windows-os-p))
  (use-package ssh-agency
    :if (and (not (cygwin-p))
             (windows-os-p))
    :ensure t
    :init
    (setenv "SSH_ASKPASS" "git-gui--askpass")
    (let ((pub-keys (directory-files
                     (expand-file-name "~/.ssh/") 'full ".*\\.pub\\'")))
      (setq ssh-agency-keys (mapcar 'file-name-sans-extension pub-keys)))))


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
  :functions (langtool-check
              langtool-check-buffer
              langtool-switch-default-language)
  :ensure t
  :defer t
  :config

  (defun langtool-cygwin-advice (args)
    "Convert the buffer-file-name to a Windows compatible path."
    (cons (cygwin-windows-path (car args)) (cdr args)))

  (defun langtool-filter-advice (proc event)
    "Delete trailing carriage returns from the process-buffer before parsing."
    (with-current-buffer (process-buffer proc)
      (goto-char (point-min))
      (while (search-forward "\r\n" nil t)
        (replace-match "\n"))))

  (when (cygwin-p)
    (advice-add 'langtool--invoke-process :filter-args #'langtool-cygwin-advice)
    (advice-add 'langtool--process-filter :before #'langtool-filter-advice))

  (let* ((lt-dir (file-name-as-directory
                  (or (locate-directory "languagetool" exec-path) "")))
         (lt-jar "languagetool-commandline.jar")
         (lt-jar-path (concat lt-dir lt-jar))
         (lt-cp (list lt-jar-path
                      (concat lt-dir (file-name-as-directory "libs") "*")))
         (lt-cp-str (mapconcat 'identity lt-cp ":")))

    ;; Run path through cygpath to correct the Path. (Assumes Oracle JDK.)
    (if (cygwin-p)
        (progn
          (setq langtool-java-classpath (cygwin-windows-path lt-cp-str 'list))
          (setq langtool-process-coding-system 'prefer-utf-8))
      (setq langtool-language-tool-jar lt-jar-path))
    (setq langtool-mother-tongue "en")
    (setq langtool-default-language "en-US")))


(use-package ggtags
  :if (executable-find "gtags")
  :defer t
  :functions ggtags-mode
  :pin melpa
  :ensure t)


(use-package ivy
  :ensure t
  :defer t)


(use-package dumb-jump
  :ensure t
  :defer t
  :after ivy
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g i" . dumb-jump-go-prompt)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :config
  (setq dumb-jump-selector 'ivy))


(use-package smart-jump
  :ensure t
  :defer t
  :after dumb-jump
  :init
  (setq smart-jump-bind-keys t)
  (defconst smart-jump-mode-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "M-.") #'smart-jump-go)
      (define-key map (kbd "M-,") #'smart-jump-references)
      (define-key map (kbd "M-*") #'smart-jump-back)
      (define-key map (kbd "M-P") #'smart-jump-peek)
      map)
    "Keymap for the `smart-jump' minor mode.")
  (define-minor-mode smart-jump-mode
    "Minor mode for the `smart-jump' functions.

\\{smart-jump-mode-map}"
    :group 'smart-jump
    :lighter " sj"
    :global t
    :keymap smart-jump-mode-map)
  (add-hook 'after-init-hook #'smart-jump-mode)
  :config
  (smart-jump-setup-default-registers))


(use-package sx                         ; StackExchange client for Emacs
  :defer t
  :ensure t)

(use-package sx-compose ; Write questions/answers for Stack Exchange
  :ensure sx
  :defer t
  :config
  (add-hook 'sx-compose-mode-hook #'turn-off-auto-fill)
  (add-hook 'sx-compose-mode-hook #'visual-line-mode)
  ;; Clean up whitespace before sending questions
  (add-hook 'sx-compose-before-send-hook #'delete-trailing-whitespace))


(use-package sx-question-mode ; Show Stack
  :ensure sx
  :defer t
  ;; Display questions in the same window
  :config (setq sx-question-mode-display-buffer-function #'switch-to-buffer))


(use-package browse-kill-ring
  :ensure t
  :defer t
  :bind ("C-c x k" . browse-kill-ring)
  :config
  (setq browse-kill-ring-highlight-current-entry t))


(use-package iedit
  :ensure t
  :defer t
  :bind ("C-c x i" . iedit-mode))


(use-package mmm-mode
  :ensure t
  :defer t
  :commands mmm-mode
  :config
  ;; Add python + rst major mode configuration.
  (defun rst-python-statement-is-docstring (begin)
    "Return true if beginning of statement is BEGIN."
    (save-excursion
      (save-match-data
        (python-nav-beginning-of-statement)
        (looking-at-p begin))))

  (defun rst-python-front-verify ()
    "Verify that we're looking at a python docstring."
    (and (rst-python-statement-is-docstring (match-string 0))
         (null yas-active-p)))

  (setq mmm-parse-when-idle 't)
  (add-to-list 'mmm-save-local-variables 'adaptive-fill-regexp)
  (add-to-list 'mmm-save-local-variables 'fill-paragraph-function)

  (mmm-add-classes
   '((rst-python-docstrings
      :submode rst-mode
      :face mmm-comment-submode-face
      :front "u?\\(\"\"\"\\|\'\'\'\\)"
      :front-verify rst-python-front-verify
      :back "~1"
      :end-not-begin t
      :save-matches 1
      :insert ((?d embdocstring nil @ "u\"\"\"" @ _ @ "\"\"\"" @))
      :delimiter-mode nil)))

  (mmm-add-mode-ext-class 'python-mode nil 'rst-python-docstrings))


(use-package smart-mode-line
  :ensure t
  :defer t
  :init
  (setq sml/theme 'dark)
  ;; `sml/setup' must be called manually the first time for the Emacs server.
  (add-hook 'emacs-startup-hook #'sml/setup))


(use-package gnus-desktop-notify
  :ensure t
  :defer t
  :after alert
  :init
  (defun my/gnus-notifications ()
    "Personal function for setting appropriate mail scanning times."
    (gnus-demon-add-handler #'gnus-demon-scan-news 5 2)
    (gnus-demon-add-handler #'gnus-demon-scan-mail 5 2))
  (add-hook 'gnus-startup-hook #'gnus-desktop-notify-mode)
  (add-hook 'gnus-startup-hook #'my/gnus-notifications))


(use-package alert
  :ensure t
  :defer t
  :init
  (customize-set-variable
   'alert-default-style
   (cond
    ((executable-find "notify-send")
     'libnotify)
    ((eq system-type 'gnu/linux)
     'notifications)
    ((executable-find "growlnotify")
     (if (windows-os-p)
         'growl-windows
       'growl))
    ((executable-find "toaster")
     'toaster)
    (t
     'mode-line)))
  :config
  (alert-define-style
   'growl-windows
   :title "Growl for Windows"
   :notifier
   (lambda (info)
     (let ((title (plist-get info :title))
           (msg (plist-get info :message))
           (prio (cdr (assq (plist-get info :severity)
                            alert-growl-priorities))))
       (windows-growlnotify prio title msg)))))


(use-package beacon
  :ensure t
  :pin melpa
  :defer t
  :diminish (beacon-mode)
  :init
  (add-hook 'emacs-startup-hook #'beacon-mode))



(use-package modern-cpp-font-lock
  :ensure t
  :defer t
  :init
  (add-to-list 'c++-mode-hook #'modern-c++-font-lock-mode))


(use-package org
  :defer t
  :diminish (orgstruct-mode   . "Org")
  :init
  (setq org-export-backends '(ascii html latex odt))
  (setq org-directory (file-name-as-directory
                       (concat user-emacs-directory "org")))
  (unless (file-directory-p org-directory)
    (message "Warning: `%s' directory doesn't exist." org-directory))
  (defun my-org-hook ()
    "Personal hook for `org-mode'."
    (linum-mode -1))
  (add-hook 'org-mode-hook #'my-org-hook)
  (add-hook 'text-mode-hook #'orgstruct-mode)
  (add-hook 'latex-mode-hook #'orgstruct-mode)
  :config
  (let ((ditaa    (executable-find "ditaa"))
        (plantuml (executable-find "plantuml")))
    (when ditaa
      (setq org-ditaa-jar-path    ditaa))
    (when plantuml
      (setq org-plantuml-jar-path plantuml)))
  (setq org-confirm-babel-evaluate nil)

  (setq org-capture-templates
        '(("j" "PhD logbook" entry (file+olp+datetree "~/git/phd/logbook.org")
           "* %<%R: >%? %^g\n%t")))

  (org-babel-do-load-languages
   'org-babel-load-languages
   `((emacs-lisp . t)
     (dot . t)
     (ditaa . t)
     (python . t)
     (gnuplot . t)
     ,(when (>= emacs-major-version 26)
        '(shell . t))
     (org . t)
     (plantuml . t)
     ,(when (executable-find "jupyter")
        (ipython . t))
     (latex . t)))

  (add-to-list
   'org-src-lang-modes '("plantuml" . plantuml))

  (define-key org-mode-map (kbd "M-h") #'backward-kill-word)

  (customize-set-variable 'org-highlight-latex-and-related '(latex))
  (customize-set-variable 'org-src-fontify-natively t)
  (customize-set-variable 'org-export-with-sub-superscript '{})
  (customize-set-variable 'org-use-sub-superscript '{})
  (customize-set-variable 'org-log-into-drawer t))


(use-package org-table
  :defer t
  :commands (orgtbl-mode)
  :diminish (orgtbl-mode)
  :init
  (add-hook 'text-mode-hook #'orgtbl-mode)
  (add-hook 'latex-mode-hook #'orgtbl-mode))


(use-package htmlize
  :ensure t
  :pin melpa
  :defer t)


(use-package plantuml-mode
  :ensure t
  :pin melpa
  :mode ("\\.plantuml\\'" . plantuml-mode)
  :defer t
  :config
  (setq plantuml-jar-path (executable-find "plantuml")))


(use-package bbdb
  :ensure t
  :defer t
  :init
  (setq bbdb-file (concat org-directory "bbdb"))
  (bbdb-initialize 'gnus 'mail 'message)
  :config
  (setq bbdb-send-mail-style 'gnus
        bbdb-complete-name-full-completion t
        bbdb-completion-type 'primary-or-name
        bbdb-complete-name-allow-cycling t
        bbdb-offer-save 1
        bbdb-use-pop-up t
        bbdb-electric-p t
        bbdb-popup-target-lines 1))

(use-package bbdb-vcard :defer t :ensure t)


(when (>= emacs-major-version 25)
  (use-package ebdb
    :ensure t
    :defer t
    :init
    (setq ebdb-sources `(,(locate-user-emacs-file "ebdb")
                         ,(concat org-directory "ebdb")))))


(use-package erc
  :defer t
  :init
  (defun bitlbee-close ()
    "Close the SSH tunnel to the `Bitlbee' server."
    (delete-process "bitlbee")
    (remove-hook 'erc-kill-server-hook #'bitlbee-close))

  (defun bitlbee ()
    "Start SSH tunnel to the `Bitlbee' server and then start `ERC'.

When `ERC' exits the SSH process is killed from `erc-kill-server-hook'."
    (interactive)
    (start-process "bitlbee" "*bitlbee*" "ssh" "-N" "bitlbee")
    (add-hook 'erc-kill-server-hook #'bitlbee-close)
    (call-interactively #'erc)))


(use-package calendar
  :defer t
  :config
  (setq calendar-week-start-day 1)
  (setq calendar-date-style 'iso))


(use-package holidays
  :defer t
  :after calendar
  :config
  (setq holiday-local-holidays
        '((holiday-fixed 1 1 "Nyårsdagen")
          (holiday-fixed 1 6 "Trettondedag jul")
          (holiday-fixed 1 13 "Tjogondag Knut")

          ;; Easter
          (holiday-easter-etc -47 "Fettisdagen")
          (holiday-easter-etc -46 "Askonsdagen")
          (holiday-easter-etc  -3 "Skärtorsdagen")
          (holiday-easter-etc  -2 "Långfredagen")
          (holiday-easter-etc  -1 "Påskafton")
          (holiday-easter-etc   0 "Påskdagen")
          (holiday-easter-etc  +1 "Annandag påsk")
          (holiday-easter-etc +39 "Kristi himmelfärdsdag")
          (holiday-easter-etc +49 "Pingstdagen")
          (holiday-easter-etc +50 "Annandag pingst")

          (holiday-fixed 4 30 "Valborgsmässoafton")
          (holiday-fixed 5 1 "Första maj")
          (holiday-fixed 6 6 "Sveriges Nationaldag")

          (holiday-sexp '(calendar-gregorian-from-absolute
                          (1- (calendar-dayname-on-or-before
                               6 (calendar-absolute-from-gregorian
                                  (list 6 26 year)))))
                        "Midsommarafton")
          (holiday-sexp '(calendar-gregorian-from-absolute
                          (calendar-dayname-on-or-before
                           6 (calendar-absolute-from-gregorian
                              (list 6 26 year))))
                        "Midsommardagen")
          (holiday-sexp '(calendar-gregorian-from-absolute
                          (calendar-dayname-on-or-before
                           6 (calendar-absolute-from-gregorian
                              (list 11 6 year))))
                        "Alla helgons dag")

          (holiday-float 12 0 -4 "Första advent" 24)
          (holiday-float 12 0 -3 "Andra advent"  24)
          (holiday-float 12 0 -2 "Tredje advent" 24)
          (holiday-float 12 0 -1 "Fjärde advent" 24)
          (holiday-fixed 12 10 "Nobeldagen")
          (holiday-fixed 12 13 "Lucia")

          (holiday-fixed 12 24 "Julafton")
          (holiday-fixed 12 25 "Juldagen")
          (holiday-fixed 12 26 "Annandag jul")
          (holiday-fixed 12 31 "Nyårsafton"))))


(use-package calfw
  :ensure t
  :defer t
  :commands (cfw:open-calendar-buffer)
  :init
  (autoload #'cfw:org-create-source "calfw-org")
  (autoload #'cfw:cal-create-source "calfw-cal")
  (autoload #'cfw:ical-create-source "calfw-ical")
  (defun my-public-calfw ()
    "Open my public `calfw' calendar."
    (interactive)
    (cfw:open-calendar-buffer
     :contents-sources
     (cl-remove-if
      #'null
      (list
       (cfw:org-create-source "Green")
       (when (file-exists-p diary-file)
         (cfw:cal-create-source "Orange"))
       (cfw:ical-create-source
        "gcal"
        "https://calendar.google.com/calendar/ical/gustaf.waldemarson%40gmail.com/public/basic.ics"
        "Gray")
       (cfw:ical-create-source
        "office365"
        "https://outlook.office365.com/owa/calendar/0bfa28483d924c06ac17b2b35a0e37f0@arm.com/a67bb5853e7c4ec2b36228bac2c64dbd10275722062663688238/calendar.ics"
        "IndianRed"))))))


(use-package uimage
  :ensure t
  :defer t
  :init
  (defun my-uimage-hook ()
    "Check if are using a graphical frame before starting `uimage-mode'."
    (when (display-graphic-p)
      (uimage-mode)))
  (add-hook 'Info-mode-hook #'my-uimage-hook)
  (add-hook 'wiki-mode-hook #'my-uimage-hook))


(use-package image+
  :ensure t
  :defer t)


(use-package helm-dash
  :ensure t
  :defer t
  :pin melpa
  :defines (helm-dash-docsets)
  :init
  (set-variable-in-hook python-mode-hook  helm-dash-docsets '("Python 3" "Matplotlib" "OpenCV_Python"))
  (set-variable-in-hook rust-mode-hook    helm-dash-docsets '("Rust"))
  (set-variable-in-hook shell-script-hook helm-dash-docsets '("Bash"))
  (set-variable-in-hook c-mode-hook       helm-dash-docsets '("C"))
  (set-variable-in-hook c++-mode-hook     helm-dash-docsets '("C++" "C"))
  (set-variable-in-hook cmake-mode-hook   helm-dash-docsets '("CMake"))
  :config
  (define-key helm-map (kbd "C-h") #'backward-delete-char))


(use-package polymode :ensure t :defer t)

(use-package poly-markdown
  :ensure polymode
  :defer t
  :mode ("\\.md" . poly-markdown-mode))


(use-package nameless
  :ensure t
  :defer t
  :init
  (add-hook 'emacs-lisp-mode-hook #'nameless-mode)
  :config
  (setq nameless-affect-indentation-and-filling nil)
  (define-key nameless-mode-map (kbd "_") #'nameless-insert-name-or-self-insert))


(use-package highlight-escape-sequences
  :ensure t
  :defer t
  :init
  (add-hook 'prog-mode-hook #'hes-mode))


(when (>= emacs-major-version 25)
  (use-package perspeen
    :ensure t
    :defer t
    :init
    (add-hook 'after-init-hook #'perspeen-mode)))


(use-package jira-markup-mode
  :ensure t
  :defer t
  :commands (jira-markup-mode)
  :mode (("\\.confluence$" . jira-markup-mode)
         ("/itsalltext/.*jira.*\\.txt$" . jira-markup-mode)))


(use-package markdown-mode
  :ensure t
  :defer t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\'"      . markdown-mode)
         ("README\\.md\\'" . gfm-mode)
         ("\\.md\\'"       . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config
  (define-key markdown-mode-map (kbd "M-h") #'backward-kill-word))


(use-package racer
  :ensure t
  :defer t
  :init
  (add-hook 'rust-mode-hook #'racer-mode)
  :config
  (setq racer-rust-src-path (getenv "RUST_SRC_PATH"))
  (add-hook 'racer-mode-hook #'eldoc-mode))

(use-package cargo
  :ensure t
  :defer t
  :init
  (add-hook 'rust-mode-hook #'cargo-minor-mode))


(use-package jenkins
  :ensure t
  :defer t
  :functions jenkins)


(use-package lsp-mode
  :ensure t
  :defer t
  :disabled
  :init
  (add-hook 'rust-mode-hook   #'lsp-mode)
  (add-hook 'java-mode-hook   #'lsp-mode)
  (add-hook 'python-mode-hook #'lsp-mode))


(use-package lsp-flycheck
  :disabled
  :ensure lsp-mode
  :after lsp-mode)


(use-package lsp-rust
  :disabled
  :ensure t
  :defer t
  :after lsp-mode)


(use-package lsp-python
  :disabled
  :ensure t
  :defer t
  :after lsp-mode)


(use-package lsp-java
  :disabled
  :ensure t
  :defer t
  :after lsp-mode)


(use-package eclim
  :defer t
  :ensure t
  :init
  (add-hook 'java-mode-hook #'eclim-mode)
  :config
  (add-to-list 'eclim-eclipse-dirs
               (concat (file-name-as-directory (getenv "XDG_DATA_HOME"))
                       "eclipse"))
  (setq eclim-executable (eclim-executable-find)))


(use-package atomic-chrome
  :defer t
  :ensure t
  :init
  (defun my-atomic-chrome-start ()
    "Ignore errors when starting the server."
    (condition-case nil
        (atomic-chrome-start-server)
      ((error nil))))
  (add-hook 'after-init-hook #'my-atomic-chrome-start)
  :config
  (setq atomic-chrome-url-major-mode-alist '(("github\\.com" . gfm-mode)))
  (setq atomic-chrome-buffer-open-style 'frame)
  (setq atomic-chrome-default-major-mode 'markdown-mode))


(use-package unfill
  :ensure t
  :defer t)


(use-package nov
  :ensure t
  :defer t
  :mode (("\\.epub\\'" . nov-mode)))


(use-package ruby-mode
  :defer t
  :config
  (customize-set-variable 'ruby-indent-level 4))


(use-package octave
  :defer t
  :mode (("\\.m\\'" . octave-mode))
  :config
  (setq octave-block-offset 4)
  (define-key octave-mode-map (kbd "C-h") #'backward-delete-char))


(use-package csv-mode
  :defer t
  :init
  (setq csv-separators '(";" ":" "," "\t")))


(use-package wdired
  :defer t
  :config
  (setq wdired-allow-to-change-permissions t))


(use-package fortune
  :defer t
  :init
  (setq fortune-dir "/usr/share/games/fortunes/")
  (setq fortune-file (expand-file-name "wisdom" fortune-dir))

  (defvar my-fortune-file (expand-file-name "fortunes" user-emacs-directory)
    "My personal `fortune' file.")

  (defun fortune-random ()
    "Display a random `fortune'."
    (interactive)
    (let* ((sys (directory-files fortune-dir :full "^[^.]+$"))
           (my  (and (file-exists-p my-fortune-file) my-fortune-file))
           (all (append sys (list my)))
           (n   (random (length all))))
      (fortune (nth n all)))))


(use-package semantic
  :defer t
  :disabled
  :init
  (add-hook 'prog-mode-hook #'semantic-mode)
  :config
  (setq semantic-idle-scheduler-idle-time 60)
  (add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode))

(use-package srecode/map
  :defer t
  :disabled
  :init
  (add-hook 'semantic-mode-hook #'srecode-minor-mode)
  :config
  (setq srecode-insert-ask-variable-method 'field)
  (let ((tdir (file-name-as-directory
               (expand-file-name "templates" user-emacs-directory))))
    (add-to-list 'srecode-map-load-path tdir 'append)))


(use-package glsl-mode
  :ensure t
  :defer t
  :mode (("\\.vs\\'" . glsl-mode)
         ("\\.fs\\'" . glsl-mode)))


(use-package opencl-mode
  :ensure t
  :defer t
  :mode (("\\.cl\\'" . opencl-mode)))


(use-package nxml-mode
  :defer t
  :config
  (define-key nxml-mode-map (kbd "M-h") #'backward-kill-word))


(use-package slime
  :ensure t
  :defer t
  :after xdg
  :commands (slime)
  :config
  (load (expand-file-name (concat (getenv "XDG_CONFIG_HOME")
                                  "/quicklisp/slime-helper.el")))
  (setq inferior-lisp-program "sbcl"))


(use-package guess-language
  :ensure t
  :defer t
  :diminish guess-language-mode
  :init
  (add-hook 'text-mode-hook #'guess-language-mode)
  :config
  (setq guess-language-langcodes '((en . ("en_US"   "English"))
                                   (sv . ("svenska" "Swedish"))))
  (setq guess-language-languages '(en sv))
  (setq guess-language-min-paragraph-length 60))


(use-package eshell
  :defer t
  :config
  (defun my-eshell-pyvenv ()
    "Return the propertized python virtualenv prompt string."
    (when (and (boundp 'pyvenv-virtual-env-name)
               pyvenv-virtual-env-name)
      (concat
       (propertize " |" 'face nil)
       (propertize " \ue73c " 'face '(:foreground "forest green"))
       (propertize
        pyvenv-virtual-env-name 'face nil))))

  (defun my-eshell-git-branch ()
    "Return the propertized git prompt string."
    (when (and (fboundp #'magit-get-current-branch)
               (magit-get-current-branch))
      (concat
       (propertize " |" 'face nil)
       (propertize " \ue702 " 'face '(:foreground "#f03c2e"))
       (propertize
        (magit-get-current-branch) 'face '(:foreground "#8787af")))))

  (defun my-eshell-user ()
    "Return the propertized user prompt string."
    (concat
     (propertize " | " 'face nil)
     (propertize (user-login-name) 'face '(:foreground "#8787af"))
     (propertize (my-eshell-system-symbol) 'face nil)
     (propertize (system-name) 'face '(:foreground "#8787af"))))

  (defun my-eshell-system-symbol ()
    "Return a propertized symbol that can represent the system."
    (pcase system-type
      ('gnu/linux    (propertize " \ue712 " 'face nil))
      ('darwin       (propertize " \ue711 " 'face nil))
      ('gnu/kfreebsd (propertize " \uf30e " 'face nil))
      ('windows-nt   (propertize " \ue70f " 'face nil))
      ('cygwin       (propertize " \ue61e " 'face nil))
      (_            (propertize " \ufffd " 'face nil))))

  (defun my-eshell-prompt-function ()
    "Function used to set the prompt in `eshell'."
    (concat
     (propertize "┌─" 'face nil)
     (propertize " \uf07c " 'face '(:foreground "yellow"))
     (propertize
      (abbreviate-file-name (eshell/pwd)) 'face '(:foreground "#8787af"))
     (my-eshell-user)
     (my-eshell-git-branch)
     (my-eshell-pyvenv)
     (propertize "\n" 'face nil)
     (propertize "└─" 'face nil)
     (if (= (user-uid) 0)
         (propertize "#" 'face '(:foreground "red"))
       (concat
        (propertize ">" 'face '(:foreground "#f75f5f"))
        (propertize ">" 'face '(:foreground "#ffaf5f"))
        (propertize ">" 'face '(:foreground "#87af5f"))))
     (propertize " " 'face nil)))

  (setq eshell-prompt-function #'my-eshell-prompt-function)
  (setq eshell-prompt-regexp "└─\\(>>>\\|#\\) "))


;; Install various major-mode packages and defer where it is possible.
(use-package abc-mode          :ensure t :defer t)
(use-package graphviz-dot-mode :ensure t :defer t)
(use-package cmake-mode        :ensure t :defer t)
(use-package gitignore-mode    :ensure t :defer t)
(use-package gitconfig-mode    :ensure t :defer t)
(use-package dart-mode         :ensure t :defer t)
(use-package web-mode          :ensure t :defer t)
(use-package cuda-mode         :ensure t :defer t)
(use-package csv-mode          :ensure t :defer t)
(use-package rust-mode         :ensure t :defer t)
(use-package flycheck-rust     :ensure t :defer t)
(use-package powershell        :ensure t :defer t)
(use-package ahk-mode          :ensure t :defer t)
(use-package cask-mode         :ensure t :defer t)
(use-package gl-conf-mode      :ensure t :defer t)
(use-package dts-mode          :ensure t :defer t :pin melpa)
(use-package go-mode           :ensure t :defer t :pin melpa)
(use-package yaml-mode
  :ensure t
  :defer t
  :pin melpa
  :mode (("\\.clang-format\\'" . yaml-mode)))
(use-package toml-mode
  :ensure t
  :defer t
  :mode (("Cargo.lock\\'" . toml-mode)))
(use-package json-mode
  :ensure t
  :defer t
  :mode (("\\.repo\\'" . json-mode)))


;; Add various themes.
(use-package zenburn-theme   :defer t :ensure t)
(use-package zerodark-theme  :defer t :ensure t)
(use-package sublime-themes  :defer t :ensure t)
(use-package solarized-theme :defer t :ensure t)
(use-package spacemacs-theme :defer t :ensure t)
(use-package color-theme-approximate
  :ensure t
  :defer t
  :init
  (add-hook 'after-init-hook #'color-theme-approximate-on))

;; Install miscellaneous packages.
(use-package evil :functions (evil-ace-jump-exit-recursive-edit) :defer t :ensure t)
(use-package debbugs :ensure t :defer t)
(use-package elpy  :defer t :ensure t)
(use-package irony :defer t :ensure t)
(use-package irony-eldoc :defer t :ensure t)
(use-package cider  :defer t :ensure t)
(use-package popup :defer t :ensure t)


;; Remove the lighter for a number of built in packages.
(use-package flyspell :defer t :diminish flyspell-mode)
(use-package subword  :defer t :diminish subword-mode)
(use-package whitespace
  :defer t
  :diminish (whitespace-mode
             global-whitespace-mode
             whitespace-newline-mode))
(use-package eldoc :defer t :diminish eldoc-mode)
(use-package cwarn :defer t :commands cwarn-mode :diminish cwarn-mode)
(use-package abbrev :defer t :diminish abbrev-mode)
(use-package simple :defer t :diminish (auto-fill-function))

;;; addons.el ends here
