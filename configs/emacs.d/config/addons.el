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

;;(my-define-group "C-c o" org)
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
  (setq which-key-idle-delay 0.8
        which-key-key-replacement-alist
        '(("<\\([[:alnum:]-]+\\)>" . "\\1")
          ("up" . "↑")
          ("right" . "→")
          ("down" . "↓")
          ("left" . "←")
          ("DEL" . "⌫")
          ("deletechar" . "⌦")
          ("RET" . "⏎"))
        which-key-description-replacement-alist
        '(("Prefix Command" . "prefix")
          ("\\`\\?\\?\\'" . "λ"))))


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
  (define-key yas-minor-mode-map (kbd "C-o") 'yas-expand)

  (define-key yas-keymap [(tab)]       nil)
  (define-key yas-keymap (kbd "TAB")   nil)
  (define-key yas-keymap [(shift tab)] nil)
  (define-key yas-keymap [backtab]     nil)
  (define-key yas-keymap (kbd "C-o") 'yas-next-field-or-maybe-expand)
  (define-key yas-keymap (kbd "C-u") 'yas-prev-field)

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
  :defer t
  :mode ("gitignore\\'" . gitignore-mode)
  :init
  (setq magit-last-seen-setup-instructions "1.4.0")
  :bind ("C-c g" . magit-status)
  :config
  (defun magit-push-to-gerrit ()
    "Use `magit' to push for `gerrit' review."
    (interactive)
    (magit-git-command "push origin HEAD:refs/for/master" (magit-toplevel)))

  (magit-define-popup-action 'magit-push-popup
    ?G
    "Push to gerrit"
    #'magit-push-to-gerrit)

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

(use-package magit-gerrit :ensure t :defer t)
(use-package magit-annex  :ensure t :defer t)


(use-package hide-comnt
  :ensure t
  :bind ("C-c x h" . hide/show-comments-toggle))


;; Change tab width for coffee-mode.
(use-package coffee-mode
  :ensure t
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
  (unless (fboundp 'x-hide-tip)
    (defalias 'x-hide-tip 'ignore))
  (add-hook 'flycheck-mode-hook #'flycheck-pos-tip-mode)
  :config
  (defun my-flycheck-popup (errors)
    "Display the ERRORS in the old popup-el interface inside terminals."
    (let ((message (mapconcat
		    #'flycheck-error-format-message-and-id
                    errors
                    "\n\n")))
      (popup-tip message)))
  (setq flycheck-pos-tip-display-errors-tty-function 'my-flycheck-popup))

(use-package flycheck-irony :ensure t :defer t :after flycheck)


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
  :init
  (add-hook 'text-mode-hook #'projectile-mode)
  (add-hook 'prog-mode-hook #'projectile-mode)
  :config
  (setq projectile-mode-line
        '(:eval (format " Prj[%s]" (projectile-project-name))))
  (when (executable-find "uni-ctags")
    (setq projectile-tags-command "uni-ctags -Re -f \"%s\" %s")))


;; Add the Google C/C++ style to list of all styles.
(use-package google-c-style
  :ensure t
  :config (c-add-style "google" google-c-style))


;; Add ace-jump and ace-window configuration.
(use-package ace-window
  :ensure t
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
        (cond
         ((file-exists-p "~/Music")
          "~/Music")
         ((file-exists-p "~/Musik")
          "~/Musik")))
  :config
  (use-package emms-player-mpv :ensure t :pin melpa)
  (emms-all)
  (emms-default-players)
  ;; Add midi formats to VLC-player
  (emms-player-set
   emms-player-vlc
   'regex
   (concat "\\`\\(http[s]?\\|mms\\)://\\|"
           (apply #'emms-player-simple-regexp
                  `("mid" "midi" ,@emms-player-base-format-list)))))


(use-package anzu
  :defer t
  :ensure t
  :pin melpa
  :init
  (setq anzu-mode-lighter "")
  (add-hook 'after-init-hook #'global-anzu-mode))


(use-package pyimport
  :ensure t
  :init
  (defun my-pyimport-hook ()
    "Personal hook for pyimport keys."
    (define-key python-mode-map (kbd "C-c C-u") #'pyimport-remove-unused)
    (define-key python-mode-map (kbd "C-c C-i") #'pyimport-insert-missing))
  (add-hook 'python-mode-hook #'my-pyimport-hook))


(use-package pyvenv
  :ensure t
  :init
  (add-hook 'python-mode-hook #'pyvenv-mode))


(use-package highlight-indentation
  :ensure t
  :commands highlight-indentation-mode
  :init
  (add-hook 'python-mode-hook #'highlight-indentation-mode))


(use-package anaconda-mode
  :ensure t
  :if (executable-find "pip")
  :diminish anaconda-mode
  :commands anaconda-mode
  :init
  (add-hook 'python-mode-hook #'anaconda-mode))


(use-package ssh-config-mode
  :ensure t
  :mode (("ssh_config\\'"      . ssh-config-mode)
         ("\.*ssh\.*config\\'" . ssh-config-mode)))


(use-package clang-format
  :if (executable-find "clang-format")
  :ensure t
  :bind ("C-c f" . clang-format-create-style)
  :init
  (defvar my-clang-styles
    (directory-files
     (concat user-emacs-directory "styles/") :full "[^.]")
    "My collection of clang-format styles.")

  (defun clang-format--find-or-create-style ()
    "Find or create a `.clang-format' file with style and root directory."
    (let* ((dir default-directory)
           (styles my-clang-styles)
           (found (locate-dominating-file dir ".clang-format"))
           (root  (or found (read-directory-name "Root directory: " nil nil t)))
           (style (or found (completing-read "Style: " styles nil t nil nil styles))))
      (unless found
        (copy-file style (concat root ".clang-format")))
      (list (region-beginning) (region-end))))

  (defun clang-format-create-style (beg end)
    "Use clang-format to automatically format the selected region.

Creates a `.clang-format' file at a selected root directory and
with the selected style before formatting the region [BEG, END]
if such a file does not already exist."
    (interactive (clang-format--find-or-create-style))
    (clang-format-region beg end)))


;; Fix LaTeX settings and AucTeX.
(use-package auctex
  :ensure t
  :mode ("\\.tex\\'" . latex-mode)
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

  (add-hook 'LaTeX-mode-hook #'turn-on-reftex)   ; with AUCTeX LaTeX mode
  (add-hook 'latex-mode-hook #'turn-on-reftex))  ; with Emacs latex mode

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
  :config

  (when (cygwin-p)
    (defun langtool-cygwin-advice (args)
      "Convert the buffer-file-name to a Windows compatible path."
      (cons (cygwin-windows-path (car args)) (cdr args)))
    (advice-add 'langtool--invoke-process :filter-args #'langtool-cygwin-advice)

    (defun langtool-filter-advice (proc event)
      "Delete trailing carriage returns from the process-buffer before parsing."
      (with-current-buffer (process-buffer proc)
        (goto-char (point-min))
        (while (search-forward "\r\n" nil t)
          (replace-match "\n"))))
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
  :ensure t
  :init
  (add-hook 'prog-mode-hook #'ggtags-mode))


(use-package sx ; StackExchange client for Emacs
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
  :init
  (add-hook 'python-mode-hook 'mmm-mode)
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
  :init
  (add-hook 'emacs-startup-hook #'beacon-mode))


(use-package hydra
  :ensure t
  :pin melpa
  :defer t
  :config
  (require 'hydra-examples)
  (require 'dired)

  (defhydra hydra-dired (:color pink)
    "
Opening^^          ^Marking^                    ^Point/Mark Actions^
--------------------------------------------------------------------------------
_e_: Open file     _m_: Mark          _A_: Search  _P_: Print      _M_: Chmod
_f_: Open file     _u_: Unmark        _C_: Copy    _R_: Rename     _O_: Chown
_o_: Display file  _t_: Toggle marks  _D_: Delete  _S_: Symlink    _X_: Shell
_v_: View file     _U_: Unmark all    _G_: Chgrp   _H_: Hardlink   _c_: Compress

New/Delete^^          ^Navigate^            ^Views^            ^Addons^
--------------------------------------------------------------------------------
_+_: New directory    _nu_: Go up directory  _g_: Refresh       _wd_: Wdired on
_d_: Delete mark       _j_: Goto file        _s_: Sort files    _Ti_: Image-dired
_x_: Execute delete    _i_: Insert subdir   _hd_: Hide details  _Tv_: View Image
_._: Clean directory   ^ ^                  _ha_: Hide all

"
    ("k" dired-prev-line nil)
    ("j" dired-next-line nil)

    ("e" dired-find-file nil)
    ("f" dired-find-file nil)
    ("o" dired-display-file nil)
    ("v" dired-view-file nil)

    ("m" dired-mark nil)
    ("u" dired-unmark nil)
    ("t" dired-toggle-marks nil)
    ("U" dired-unmark-all-marks nil)

    ("A" dired-do-find-regexp nil)
    ("C" dired-do-copy nil)
    ("D" dired-do-delete nil)
    ("G" dired-do-chgrp nil)

    ("P" dired-do-print nil)
    ("R" dired-do-rename nil)
    ("S" dired-do-symlink nil)
    ("H" dired-do-hardlink nil)

    ("M" dired-do-chmod nil)
    ("O" dired-do-chown nil)
    ("X" dired-do-shell-command nil)
    ("c" dired-do-compress nil)

    ("+" dired-create-directory nil)
    ("d" dired-flag-file-deletion nil)
    ("x" dired-do-flagged-delete nil)
    ("." dired-clean-directory nil)

    ("nu" dired-up-directory nil)
    ("j" dired-goto-file nil)
    ("i" dired-maybe-insert-subdir nil)

    ("g" revert-buffer nil)
    ("hd" dired-hide-details-mode nil)
    ("ha" dired-hide-all nil)
    ("s" dired-sort-toggle-or-edit nil)

    ("wd" wdired-change-to-wdired-mode nil :head blue :exit t)
    ("Ti" image-dired-dired-toggle-marked-thumbs nil)
    ("Tv" image-dired-dired-display-external nil)

    ("q" nil "Quit"))
  (define-key dired-mode-map [remap dired-summary] 'hydra-dired/body)


  (defhydra hydra-help (:exit t)
    "
  Describe        ^^Keys                    ^^Search                    ^^Documentation
  ---------------------------------------------------------------------------------------
  _f_: Function     _k_: Keybinding           _a_: Apropros               _i_: Info
  _p_: Package      _w_: Where-is             _d_: Apropos Docstrings     _n_: WoMan
  _m_: Mode         _b_: Show all bindings    _s_: Info by symbol         _h_: _H_elm-_D_ash
  _v_: Variable
  _S_: Symbol

  "
    ;; Describe
    ("f" describe-function nil)
    ("p" describe-package nil)
    ("m" describe-mode nil)
    ("v" describe-variable nil)
    ("S" describe-symbol nil)
    ("y" describe-syntax nil)

    ("b" describe-bindings nil)
    ("c" describe-key-briefly nil)
    ("k" describe-key nil)
    ("w" where-is nil)

    ;; Search
    ("a" apropos-command nil)
    ("d" apropos-documentation nil)
    ("s" info-lookup-symbol nil)

    ;; Documentation
    ("i" info nil)
    ("n" woman nil)
    ("h" helm-dash-at-point nil)
    ("H" helm-dash nil)
    ("D" helm-dash-async-install-docset nil)

    ("e" view-echo-area-messages "messages")
    ("l" view-lossage "lossage")
    ("C" describe-coding-system "coding-system")
    ("I" describe-input-method "input-method")

    ("q" help-quit "quit"))
  (global-set-key (kbd "C-c h") #'hydra-help/body)


  (defhydra hydra-kmacro (:hint nil
                          :color pink
                          :pre (when defining-kbd-macro
                                 (kmacro-end-macro 1)))
  "
  ^Create-Cycle^   ^Basic^           ^Insert^        ^Save^         ^Edit^
╭─────────────────────────────────────────────────────────────────────────╯
     ^_i_^           [_e_] Execute    [_n_] Insert    [_b_] Name      [_'_] Previous
     ^^↑^^           [_d_] Delete     [_t_] Set       [_K_] Key       [_,_] Last
 _j_ ←   → _l_       [_o_] Edit       [_a_] Add       [_x_] Register
     ^^↓^^           [_r_] Region     [_f_] Format    [_B_] Defun
     ^_k_^           [_m_] Step
    ^^   ^^          [_s_] Swap
Last Macro: %(key-description last-kbd-macro)
"
  ("j" kmacro-start-macro :color blue)
  ("l" kmacro-end-or-call-macro-repeat)
  ("i" kmacro-cycle-ring-previous)
  ("k" kmacro-cycle-ring-next)
  ("r" apply-macro-to-region-lines)
  ("d" kmacro-delete-ring-head)
  ("e" kmacro-end-or-call-macro-repeat)
  ("o" kmacro-edit-macro-repeat)
  ("m" kmacro-step-edit-macro)
  ("s" kmacro-swap-ring)
  ("n" kmacro-insert-counter)
  ("t" kmacro-set-counter)
  ("a" kmacro-add-counter)
  ("f" kmacro-set-format)
  ("b" kmacro-name-last-macro)
  ("K" kmacro-bind-to-key)
  ("B" insert-kbd-macro)
  ("x" kmacro-to-register)
  ("'" kmacro-edit-macro)
  ("," edit-kbd-macro)
  ("q" nil :color blue))
  (global-set-key (kbd "C-c k") #'hydra-kmacro/body)


  (defhydra hydra-compilation (:color red :hint nil)
    "
Commands          ^^Navigate           ^^Options
--------------------------------------------------------------------------------
_c_: Compile        _n_: Next            _f_: Follow output [%`compilation-scroll-output]
_r_: Recompile      _p_: Previous        _t_: Threshold     [%`compilation-skip-threshold]
_g_: Grep           _N_: Go to Next      _K_: Always Kill   [%`compilation-always-kill]
_k_: Kill           _P_: Go to Previous  ^ ^                [%`compile-command]
_R_: Rename buffer  _F_: Follow sources
_s_: Compile shell
^^^^
"
    ("c" compile :color blue)
    ("s" (let ((current-prefix-arg '(t))) (call-interactively #'compile)) :color blue)
    ("r" recompile :color blue)
    ("g" grep-find :color blue)
    ("k" kill-compilation :color blue)
    ("R" (with-current-buffer "*compilation*" (rename-uniquely)))

    ("n" compilation-next-error)
    ("p" compilation-previous-error)
    ("N" next-error)
    ("P" previous-error)
    ("F" next-error-follow-minor-mode)

    ("f" (setq compilation-scroll-output (not compilation-scroll-output)))
    ("t" (setq compilation-skip-threshold (% (1+ compilation-skip-threshold) 3)))
    ("K" (setq compilation-always-kill (not compilation-always-kill)))
    ("q" nil))
  (global-set-key (kbd "C-c c") #'hydra-compilation/body)


  (defhydra hydra-windowing (:color red :hint nil)
    "
Navigate^^    ^Splitting^      ^Buffers^      ^Windows/Frames^     ^Scroll^
-------------------------------------------------------------------------
  ^_k_^       _x_: Horizontal  _p_: Previous   _c_: Close Window:      ^_a_^
_h_ + _l_     _v_: Vertical    _n_: Next       _o_: Close other:       ^|^
  ^_j_^       ^ ^              _b_: Select     _N_: New Frame:         ^_z_^
^^^^          ^ ^              _f_: Find file  _D_: Delete Frame:      ^^ ^^
^^^^          ^ ^              ^ ^             ^ ^                      ^ ^
Resize^^     ^Addons^                    ^Save/Restore^       ^Miscellaneous^
-------------------------------------------------------------------------
  ^_K_^       _d_: Ace delete window:      _u_: Winner undo     _q_: Quit
_H_ + _L_     _s_: Ace Swap windows:       _U_: Winner redo
  ^_J_^       _i_: Ace maximize window:   _ri_: Register Save
^^^^          _F_: Projectile find file:  _rj_: Register Jump
^^^^          ^ ^                         ^  ^
"
    ("h" windmove-left)
    ("j" windmove-down)
    ("k" windmove-up)
    ("l" windmove-right)

    ("H" hydra-move-splitter-left)
    ("J" hydra-move-splitter-down)
    ("K" hydra-move-splitter-up)
    ("L" hydra-move-splitter-right)

    ("z" scroll-up-line)
    ("a" scroll-down-line)

    ("x" split-window-below)
    ("v" split-window-right)
    ("u" (progn (winner-undo)
                (setq this-command 'winner-undo)))
    ("U" winner-redo)

    ("p" previous-buffer)
    ("n" next-buffer)
    ("b" ido-switch-buffer)
    ("f" ido-find-file)

    ("c" delete-window)
    ("o" delete-other-windows)
    ("N" my-new-gui-frame)
    ("D" delete-frame)

    ("d" ace-delete-window)
    ("s" ace-swap-window)
    ("i" ace-maximize-window)
    ("F" projectile-find-file)

    ("ri" window-configuration-to-register)
    ("rj" jump-to-register)

    ("q" nil))
  (global-set-key (kbd "C-c w") #'hydra-windowing/body)


  (defhydra hydra-rectangle-mark (:body-pre (rectangle-mark-mode 1)
                                            :hint nil
                                            :color pink
                                            :post (deactivate-mark))
    "
  ^_k_^    _e_: Exchange  _c_: Copy    _o_: Open        _si_: String Insert     _U_: Upcase
_h_   _l_  _r_: Reset     _x_: Kill    _C_: Clear       _sr_: String Replace    _D_: Downcase
  ^_j_^    _u_: Undo      _y_: Yank    _n_: Number      _RR_: Register Read
^^^^       ^ ^            _d_: Delete  _w_: Whitespace  _RI_: Register Insert
"
    ("h" rectangle-backward-char)
    ("l" rectangle-forward-char)
    ("k" rectangle-previous-line)
    ("j" rectangle-next-line)

    ("e" rectangle-exchange-point-and-mark)
    ("r" (if (region-active-p) (deactivate-mark) (rectangle-mark-mode 1)))
    ("C-x SPC" (if (region-active-p) (deactivate-mark) (rectangle-mark-mode 1)))
    ("u" undo)

    ("c" copy-rectangle-as-kill)
    ("x" kill-rectangle)
    ("d" delete-rectangle :color blue)
    ("y" yank-rectangle   :color blue)

    ("o" open-rectangle              :color blue)
    ("C" clear-rectangle             :color blue)
    ("n" rectangle-number-lines      :color blue)
    ("w" delete-whitespace-rectangle :color blue)

    ("si" string-insert-rectangle    :color blue)
    ("sr" string-rectangle           :color blue)
    ("RR" copy-rectangle-to-register :color blue)
    ("RI" insert-register            :color blue)

    ("U" upcase-rectangle   :color blue)
    ("D" downcase-rectangle :color blue)

    ("q" t "Quit" :exit t))
  (global-set-key (kbd "C-x SPC") #'hydra-rectangle-mark/body))


;; Gnus addons configurations.
(use-package message
  :defer t
  :after hydra
  :config
  (defhydra hydra-message (:color blue :hint nil)
    "
Generic Mail^^                ^Editing Commands^           ^Actions^
--------------------------------------------------------------------------------
_ca_: Attach file             _ci_: Insert file            _cc_: Send message
_cb_: Attach Buffer           _ce_: Elide region           _cj_: Send delayed
_cu_: Toggle importance       _cr_: Caesar shift region    _cd_: Suspend message
_cn_: Request read receipt    _cm_: Morse region           _ck_: Abort message
_cw_: Change to wide reply    _cq_: Quote region
"
    ("ca" mml-attach-file)
    ("cb" mml-attach-buffer)
    ("cu" message-insert-or-toggle-importance)
    ("cn" message-insert-disposition-notification-to)
    ("cw" message-insert-wide-reply)

    ("ci" message-mark-insert-file)
    ("ce" message-elide-region)
    ("cr" rot13-region)
    ("cm" morse-region)
    ("cq" comment-dwim)

    ("cc" message-send-and-exit)
    ("cj" gnus-delay-article)
    ("cd" message-dont-send)
    ("ck" message-kill-buffer)
    ("C-c ?" describe-mode)
    ("q" nil "Quit" :exit t))
  (define-key message-mode-map (kbd "C-c ?") #'hydra-message/body))


(use-package gnus-group
  :defer t
  :after hydra
  :config
  (defhydra hydra-gnus-group (:color pink :hint nil)
    "
Views^^                   ^Marking^                 ^Actions^
-------------------------------------------------------------------
_a_: List active groups   _#_: Mark group/topic     _m_: Write mail
_l_: List unread groups   _u_: Unmark group/topic   _c_: Read all
_L_: List all groups      _k_: Kill group           _S_: Search
_g_: Refresh news         _w_: Kill region          _z_: Suspend
_s_: Server view          _y_: Yank group(s)        _q_: Quit
_t_: Topic view

Modify Topics^^           ^Topic Views^
-------------------------------------------------------------------
 _Tn_: Create topic       _<tab>_: Indent topic
 _Tr_: Rename Topic   _<backtab>_: Unindent topic
_DEL_: Delete topic          _Td_: Remove group from topic
^                           ^_Th_: Toggle empty topics
"
    ("a" gnus-group-list-active)
    ("l" gnus-group-list-groups)
    ("L" gnus-group-list-all-groups)
    ("g" gnus-group-get-new-news)
    ("s" gnus-group-enter-server-mode :color blue)
    ("t" gnus-topic-mode)

    ("#" gnus-topic-mark-topic)
    ("u" gnus-group-unmark-topic)
    ("k" gnus-group-kill-group)
    ("w" gnus-group-kill-region)
    ("y" gnus-group-yank-group)

    ("m" gnus-group-mail             :color blue)
    ("c" gnus-topic-catchup-articles :color pink)
    ("S" gnus-group-make-nnir-group  :color blue)
    ("z" gnus-group-suspend          :color blue)
    ("q" nil nil)

    ("Tn" gnus-topic-create-topic)
    ("Tr" gnus-topic-rename)
    ("DEL" gnus-topic-delete)

    ("<tab>" gnus-topic-indent)
    ("<backtab>" gnus-topic-unindent)
    ("Td" gnus-topic-remove-group)
    ("Th" gnus-topic-toggle-display-empty-topics))
  (define-key gnus-group-mode-map "?" #'hydra-gnus-group/body))


(use-package gnus-sum    ;; gnus-summary-mode
  :defer t
  :after hydra
  :config
  (defhydra hydra-gnus-summary (:color pink :hint nil)
    "
Sending/Replying^^             ^Marking^                  ^Actions^
----------------------------------------------------------------------------
_r_: Reply                     _!_: Pin mail              _D_: Download MIME
_R_: Reply with original       _?_: Mark as dormant       _a_: Article Hydra
_w_: Wide reply                _P_: Mark as read          _g_: Refresh
_W_: Wide reply with original  _U_: Unread mail           _q_: Quit
_f_: Forward                   _C_: Read all
_e_: Resend                    _#_: Mark processable
^^                             _u_: Unmark processable
"
    ("r" gnus-summary-reply                    :color blue)
    ("R" gnus-summary-reply-with-original      :color blue)
    ("w" gnus-summary-wide-reply               :color blue)
    ("W" gnus-summary-wide-reply-with-original :color blue)
    ("f" gnus-summary-mail-forward             :color blue)
    ("e" gnus-summary-resend-message-edit      :color blue)

    ("!" gnus-summary-tick-article-forward)
    ("?" gnus-summary-mark-as-dormant)
    ("P" gnus-summary-mark-as-read-forward)
    ("U" gnus-summary-clear-mark-forward)
    ("C" gnus-summary-catchup)
    ("#" gnus-summary-mark-as-processable)
    ("u" gnus-summary-unmark-as-processable)

    ("D" gnus-summary-save-parts)
    ("a" hydra-gnus-article/body :exit t)
    ("g" gnus-summary-insert-new-articles)
    ("q" nil :exit t))
  (define-key gnus-summary-mode-map "?" #'hydra-gnus-summary/body))


(use-package gnus-art   ;; gnus-article-mode
  :defer t
  :after hydra
  :config
  (defhydra hydra-gnus-article (:color pink :hint nil)
    "
Sending/Replying^^             ^Treatment^            ^Display^            ^Actions^
-----------------------------------------------------------------------------------------------
_r_: Reply                     _t_: Toggle headers    _dg_: Gravatar       _s_: Summary Hydra
_R_: Reply with original       _c_: Caesar            _df_: Face           _v_: View MIME
_w_: Wide reply                _m_: Morse             _dx_: X-Face         _o_: Save MIME
_W_: Wide reply with original  _u_: UTF-8 -> ASCII    _ds_: Smileys        _g_: Refresh/Revert
_f_: Forward                   _o_: Deuglify Outlook  _dw_: Show images    _q_: Quit
_e_: Resend                    _F_: Fill long lines   _dd_: Remove images

"
    ("r" gnus-article-reply                    :color blue)
    ("R" gnus-article-reply-with-original      :color blue)
    ("w" gnus-article-wide-reply               :color blue)
    ("W" gnus-article-wide-reply-with-original :color blue)
    ("f" gnus-summary-mail-forward             :color blue)
    ("e" gnus-summary-resend-message-edit      :color blue)

    ("t" gnus-summary-toggle-header)
    ("c" gnus-summary-caesar-message)
    ("m" gnus-summary-morse-message)
    ("u" gnus-article-treat-non-ascii)
    ("o" gnus-article-outlook-deuglify-article)
    ("F" gnus-article-fill-long-lines)

    ("dg" gnus-treat-from-gravatar)
    ("df" gnus-article-display-face)
    ("dx" gnus-article-display-x-face)
    ("ds" gnus-treat-smiley)
    ("dw" gnus-html-show-images)
    ("dd" gnus-article-remove-images)

    ("s" hydra-gnus-summary/body :exit t)
    ("v" gnus-mime-view-part)
    ("o" gnus-mime-save-part)
    ("g" gnus-summary-show-article)
    ("q" nil :exit t))
  (define-key gnus-article-mode-map "?" #'hydra-gnus-article/body))


(use-package delim-col
  :after hydra
  :config
  (defhydra hydra-delim-col (:color pink :hint nil)
    "
Actions^^                ^Presets^               ^Options^
-----------------------------------------------------------------------------------------------
_r_: Apply on region     _pl_: Python lists       _B_: Before all columns     [%`delimit-columns-str-before]
_R_: Apply on rectangle  _pd_: Python dictionary  _s_: Column separator       [%`delimit-columns-str-separator]
^ ^                      _ps_: Python set         _A_: After all columns      [%`delimit-columns-str-after]
^ ^                      _dd_: Default            _b_: Before each column     [%`delimit-columns-before]
^ ^                      ^  ^                     _a_: After each column      [%`delimit-columns-separator]
^ ^                      ^  ^                     _S_: Separator regex        [%`delimit-columns-after]
^ ^                      ^  ^                     _e_: Odd number of columns  [%`delimit-columns-extra]
^ ^                      ^  ^                     _f_: Formatting             [%`delimit-columns-format]
"
    ("B" (lambda (str)
           (interactive "s")
           (setq delimit-columns-str-before str)))
    ("s" (lambda (str)
           (interactive "s")
           (setq delimit-columns-str-separator str)))
    ("A" (lambda (str)
           (interactive "s")
           (setq delimit-columns-str-after str)))
    ("b" (lambda (str)
           (interactive "s")
           (setq delimit-columns-before str)))
    ("S" (lambda (str)
           (interactive "s")
           (setq delimit-columns-separator str)))
    ("a" (lambda (str)
           (interactive "s")
           (setq delimit-columns-after str)))
    ("e" (lambda ()
           (interactive)
           (setq delimit-columns-extra (not delimit-columns-extra))))
    ("f" (lambda ()
           (interactive)
           (setq delimit-columns-format
                 (cond ((eq delimit-columns-format nil)
                        t)
                       ((eq delimit-columns-format t)
                        'separator)
                       ((eq delimit-columns-format 'separator)
                        'padding)
                       ((eq delimit-columns-format 'padding)
                        nil)))))

    ("pl" (lambda ()
            (interactive)
            (setq delimit-columns-str-before "[")
            (setq delimit-columns-str-separator ", ")
            (setq delimit-columns-str-after "]")
            (setq delimit-columns-separator "[[:space:]]")
            (setq delimit-columns-format 'padding)))
    ("pd" (lambda () (interactive)
            (setq delimit-columns-str-before "")
            (setq delimit-columns-str-separator ": ")
            (setq delimit-columns-str-after ",")
            (setq delimit-columns-before "")
            (setq delimit-columns-separator "[[:space:]]")
            (setq delimit-columns-format 'padding)))
    ("ps" (lambda () (interactive) nil))
    ("dd" (lambda () (interactive)
            (custom-reevaluate-setting 'delimit-columns-str-before)
            (custom-reevaluate-setting 'delimit-columns-str-separator)
            (custom-reevaluate-setting 'delimit-columns-str-before)
            (custom-reevaluate-setting 'delimit-columns-before)
            (custom-reevaluate-setting 'delimit-columns-separator)
            (custom-reevaluate-setting 'delimit-columns-after)
            (custom-reevaluate-setting 'delimit-columns-extra)
            (custom-reevaluate-setting 'delimit-columns-format)))

    ("r" delimit-columns-region)
    ("R" delimit-columns-rectangle)
    ("q" nil nil)))


(use-package hydra
  :defer t
  :after (eww sx emms calc)
  :config
  (defhydra hydra-apps (:color blue :hint nil)
    "
Browser/Calculator ^^Music                  ^^Mail/Chat      ^^Calendar           ^^Stack Exchange
---------------------------------------------------------------------------------------------------
_bo_: EWW Open       _mo_: EMMS Open          _go_: GNUS Open  _co_: Calendar Open  _so_: SX Open
_bb_: EWW Bookmarks  _mp_: EMMS Play library  _ge_: ERC        _ch_: Holidays       _sa_: SX Ask
_ca_: Calc           _mf_: EMMS Play local    _gb_: Bitlbee    _cl_: Holidays List  _sn_: SX Newest
"
    ("bo" eww)
    ("bb" eww-list-bookmarks)
    ("ca" calc)

    ("mo" emms-smart-browse)
    ("mp" emms-play-file)
    ("mf" (let (emms-source-file-default-directory)
            (call-interactively #'emms-play-file)))

    ("go" gnus)
    ("ge" erc)
    ("gb" bitlbee)

    ("so" sx-tab-all-questions)
    ("sa" sx-ask)
    ("sn" sx-tab-newest)

    ("co" calendar)
    ("ch" holidays)
    ("cl" holiday-list)

    ("q" nil))
  (global-set-key (kbd "C-c a") #'hydra-apps/body))


(use-package modern-cpp-font-lock
  :ensure t
  :defer t
  :init
  (add-to-list 'c++-mode-hook #'modern-c++-font-lock-mode))


(use-package org
  :defer t
  :init
  (setq org-directory (file-name-as-directory
                       (concat user-emacs-directory "org")))
  (unless (file-directory-p org-directory)
    (message "Warning: `%s' directory doesn't exist." org-directory))
  :config
  (define-key org-mode-map (kbd "M-h") #'backward-kill-word)
  (customize-set-variable 'org-export-with-sub-superscript '{})
  (customize-set-variable 'org-use-sub-superscript '{})
  (customize-set-variable 'org-log-into-drawer t))


(use-package bbdb
  :defer t
  :ensure t
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


(use-package image-mode
  :defer t
  :after (hydra image+)
  :config
  (require 'image+)
  (defhydra imagex-hydra (:color red :hint nil)
    "
Zoom         ^^Manipulate           ^^Rotate           ^^Actions
--------------------------------------------------------------------------------
_+_: Zoom in  _m_: Maximize          _r_: Rotate right  _a_: Auto adjust
_-_: Zoom out _o_: Restore original  _l_: Rotate left   _i_: Imagemagick Hydra
^ ^           _s_: Save              ^ ^                _q_: Quit
"
    ("+" imagex-sticky-zoom-in)
    ("-" imagex-sticky-zoom-out)
    ("m" imagex-sticky-maximize)
    ("o" imagex-sticky-restore-original)
    ("s" imagex-sticky-save-image)
    ("r" imagex-sticky-rotate-right)
    ("l" imagex-sticky-rotate-left)
    ("a" imagex-auto-adjust-mode)
    ("i" imagemagick-hydra/body :exit t)
    ("q" nil))

  (defhydra imagemagick-hydra (:color red :hint nil)
    "
Zoom           ^^Manipulate           ^^Rotate           ^^Actions
--------------------------------------------------------------------------------
_+_: Zoom in    _h_: Fit to height     _r_: Rotate right  _a_: Auto adjust
_-_: Zoom out   _w_: Fit to width      _l_: Rotate left   _i_: Image+ Hydra
_s_: Set scale  _o_: Restore original  _R_: Rotate free   _q_: Quit
"
    ("+" (image-transform-set-scale (* image-transform-scale 1.1)))
    ("-" (image-transform-set-scale (* image-transform-scale 0.9)))
    ("s" image-transform-set-scale)

    ("h" image-transform-fit-to-height)
    ("w" image-transform-fit-to-width)
    ("o" image-transform-reset)

    ("r" (image-transform-set-rotation  (+ image-transform-rotation 90)))
    ("l" (image-transform-set-rotation  (- image-transform-rotation 90)))
    ("R" image-transform-set-rotation)

    ("a" imagex-auto-adjust-mode)
    ("i" imagex-hydra/body :exit t)
    ("q" nil))
  (define-key image-mode-map (kbd "?") #'imagemagick-hydra/body))


(use-package image+
  :ensure t
  :defer t)


(use-package helm-dash
  :ensure t
  :defer t
  :pin melpa
  :init
  (set-variable-in-hook python-mode-hook  helm-dash-docsets '("Python 3"))
  (set-variable-in-hook rust-mode-hook    helm-dash-docsets '("Rust"))
  (set-variable-in-hook shell-script-hook helm-dash-docsets '("Bash"))
  (set-variable-in-hook c-mode-hook       helm-dash-docsets '("C"))
  (set-variable-in-hook c++-mode-hook     helm-dash-docsets '("C++" "C"))
  :config
  (define-key helm-map (kbd "C-h") #'backward-delete-char))


(use-package polymode
  :ensure t
  :defer t
  :config
  (defvar ReST-head-regexp
    (concat "^[ \t]*\\(def\\|class\\).*:" ;; Match start of function/class.
            "\\(?:\n\\)\\{1,\\}"          ;; Match at least 1 newline.
            "[ \t]*"                      ;; Match optional whitespace.
            "u?\\(\"\"\"\\|\'\'\'\\)")    ;; Match start of docstring.
    "Regexp for matching the `head' of a ReST inner mode.")

  (defvar ReST-tail-regexp
    "\\(\"\"\"\\|\'\'\'\\)"
    "Regexp for matching the `tail' of a ReST inner mode.")

  (defun ReST-match-head (ahead)
    (pm--default-matcher ReST-head-regexp ahead))

  (defun ReST-match-tail (ahead)
    (pm--default-matcher ReST-tail-regexp ahead))

  (defcustom pm-host/python
    (pm-bchunkmode "python"
                   :mode 'python-mode
                   :font-lock-narrow nil)
    "Python host chunkmode"
    :group 'hostmodes
    :type 'object)

  (defcustom pm-inner/ReST
    (pm-hbtchunkmode "ReST"
                     :mode 'rst-mode
                     :head-reg ReST-head-regexp
                     :tail-reg ReST-tail-regexp
                     :head-mode 'host
                     :tail-mode 'host)
    "ReST inner chunk."
    :group 'innermodes
    :type 'object)
  (defcustom pm-poly/python+ReST
    (pm-polymode-one "python+ReST"
                     :hostmode 'pm-host/python
                     :innermode 'pm-inner/ReST)
    "Python and ReST polymode."
    :group 'polymodes
    :type 'object)
  (define-polymode poly-python-mode pm-poly/python+ReST))

(use-package poly-markdown
  :ensure polymode
  :defer t
  :mode ("\\.md" . poly-markdown-mode))


(use-package ruby-mode
  :defer t
  :config
  (customize-set-variable 'ruby-indent-level 4))


(use-package csv-mode
  :defer t
  :init
  (setq csv-separators '(";" ":" "," "\t")))


;; Install various major-mode packages and defer where it is possible.
(use-package abc-mode          :ensure t :defer t)
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
(use-package opencl-mode       :ensure t :defer t)
(use-package rust-mode         :ensure t :defer t)
(use-package flycheck-rust     :ensure t :defer t)
(use-package powershell        :ensure t :defer t)
(use-package ahk-mode          :ensure t :defer t)
(use-package cask-mode         :ensure t :defer t)
(use-package dts-mode          :ensure t :defer t :pin melpa)
(use-package go-mode           :ensure t :defer t :pin melpa)
(use-package yaml-mode         :ensure t :defer t :pin melpa)


;; Add various themes.
(use-package aurora-theme    :defer t :ensure t)
(use-package zenburn-theme   :defer t :ensure t)
(use-package niflheim-theme  :defer t :ensure t)
(use-package sublime-themes  :defer t :ensure t)
(use-package solarized-theme :defer t :ensure t)
(use-package color-theme-approximate
  :ensure t
  :init
  (add-hook 'after-init-hook #'color-theme-approximate-on))

;; Install miscellaneous packages.
(use-package evil :functions (evil-ace-jump-exit-recursive-edit) :defer t :ensure t)
(use-package debbugs :ensure t :defer t)
(use-package elpy  :defer t :ensure t)
(use-package irony :defer t :ensure t)
(use-package irony-eldoc :defer t :ensure t)
(use-package cider  :defer t :ensure t)
(use-package dropdown-list :defer t :ensure t)
(use-package popup :defer t :ensure t)


;; Remove the lighter for a number of built in packages.
(use-package flyspell :diminish flyspell-mode)
(use-package subword  :diminish subword-mode)
(use-package whitespace
  :diminish (whitespace-mode
             global-whitespace-mode
             whitespace-newline-mode))
(use-package eldoc :diminish eldoc-mode)
(use-package cwarn :commands cwarn-mode :diminish cwarn-mode)
(use-package abbrev :diminish abbrev-mode)
(use-package simple :diminish (auto-fill-function))

;;; addons.el ends here