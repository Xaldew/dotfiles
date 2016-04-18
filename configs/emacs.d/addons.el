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

(my-define-group "C-c a" applications)
;;(my-define-group "C-c o" org)
(my-define-group "C-c c" compile-and-comments)
(my-define-group "C-c h" help)
(my-define-group "C-c i" insertion)
(my-define-group "C-c p" projects)
(my-define-group "C-c s" search-and-symbols)
(my-define-group "C-c v" version-control)
;;(my-define-group "C-c w" windows-and-frames)
(my-define-group "C-c x" text)
(my-define-group "C-c x a" align)


(use-package anyclip-mode
  :if (not (display-graphic-p))
  :init
  (anyclip-mode t))


(use-package which-key ; Show help popups for prefix keys
  :ensure t
  :diminish (which-key-mode)
  :init (which-key-mode)
  :config
  (setq which-key-idle-delay 0.4
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
  :bind ("C-c c h" . hide/show-comments-toggle))


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
  :config
  (projectile-global-mode)
  (setq projectile-mode-line '(:eval (format " Prj[%s]"
                                             (projectile-project-name))))
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
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  (add-hook 'lisp-mode-hook       #'paredit-mode)
  (add-hook 'scheme-mode-hook     #'paredit-mode)
  (add-hook 'clojure-mode-hook    #'paredit-mode))


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
  (cond
   ((file-exists-p "~/Music")
    (setq emms-source-file-default-directory "~/Music"))
   ((file-exists-p "~/Musik")
    (setq emms-source-file-default-directory "~/Musik")))
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
  :ensure t
  :bind (("C-c a a" . sx-ask)
         ("C-c a s" . sx-tab-all-questions)
         ("C-c a q" . sx-tab-all-questions)
         ("C-c a f" . sx-tab-all-questions)
         ("C-c a n" . sx-tab-newest)))

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
  (add-hook 'markdown-mode-hook 'mmm-mode)
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
    (rst-python-statement-is-docstring (match-string 0)))

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

  (mmm-add-mode-ext-class 'python-mode nil 'rst-python-docstrings)


  ;; Add markdown + 'any' major-mode configuration.
  (defun my-mmm-markdown-auto-class (lang &optional submode)
    "Define a mmm-mode class for LANG in `markdown-mode' using SUBMODE.
 If SUBMODE is not provided, use `LANG-mode' by default."
    (let ((class (intern (concat "markdown-" lang)))
          (submode (or submode (intern (concat lang "-mode"))))
          (front (concat "^```" lang "[\n\r]+"))
          (back "^```"))
      (mmm-add-classes (list (list class
                                   :submode submode
                                   :front front
                                   :back back)))
      (mmm-add-mode-ext-class 'markdown-mode nil class)))

  ;; Add mmm-classes for modes with the same name and major-mode variable.
  (mapc 'my-mmm-markdown-auto-class
        '("awk" "bibtex" "c" "cpp" "css" "html" "latex" "lisp" "makefile"
          "markdown" "python" "r" "ruby" "sql" "stata" "xml"))

  ;; Add mmm-classes for modes with different name and major-mode variable.
  (my-mmm-markdown-auto-class "fortran" 'f90-mode)
  (my-mmm-markdown-auto-class "perl" 'cperl-mode)
  (my-mmm-markdown-auto-class "shell" 'shell-script-mode))


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
  (setq alert-default-style
        (cond
         ((executable-find "notify-send")
          'libnotify)
         ((eq system-type 'gnu/linux)
          'notifications)
         ((executable-find "growlnotify")
          'growl)
         ((executable-find "toaster")
          'toaster)
         (t
          'mode-line))))


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

  (defhydra hydra-rectangle (:body-pre (rectangle-mark-mode 1)
                                       :color pink
                                       :post (deactivate-mark))
    "
  ^_k_^    _e_: Exchange  _c_: Copy    _o_: Open        _si_: String Insert     _U_: Upcase
_h_   _l_  _r_: Reset     _p_: Paste   _C_: Clear       _sr_: String Replace    _D_: Downcase
  ^_j_^    _u_: Undo      _d_: Delete  _n_: Number      _RR_: Register Read
^^^^       ^ ^            _y_: Yank    _w_: Whitespace  _RI_: Register Insert
^^^^       ^ ^            ^ ^          ^ ^              ^  ^
"
    ("h" rectangle-backward-char nil)
    ("l" rectangle-forward-char nil)
    ("k" rectangle-previous-line nil)
    ("j" rectangle-next-line nil)

    ("e" rectangle-exchange-point-and-mark nil)
    ("r" (if (region-active-p) (deactivate-mark) (rectangle-mark-mode 1)) nil)
    ("C-x SPC" (if (region-active-p) (deactivate-mark) (rectangle-mark-mode 1)) nil)
    ("u" undo nil)

    ("c" copy-rectangle-as-kill nil)
    ("p" kill-rectangle nil)
    ("d" delete-rectangle nil)
    ("y" yank-rectangle nil)

    ("o" open-rectangle nil)
    ("C" clear-rectangle nil)
    ("n" rectangle-number-lines nil)
    ("w" delete-whitespace-rectangle nil)

    ("si" string-insert-rectangle nil)
    ("sr" string-rectangle nil)
    ("RR" copy-rectangle-to-register nil)
    ("RI" insert-register nil)

    ("U" upcase-rectangle nil)
    ("D" downcase-rectangle nil)

    ("q" nil "Quit"))
  (global-set-key (kbd "C-x SPC") 'hydra-rectangle/body))


;; Gnus addons configurations.
(use-package message
  :defer t
  :after hydra
  :config
  (defhydra hydra-message (:color blue)
    "Do?"
    ("ca" mml-attach-file "Attach C-c C-a")
    ("cc" message-send-and-exit "Send C-c C-c")
    ("q" nil "cancel")))


(use-package gnus-group
  :defer t
  :after hydra
  :config
  ;; y is not used by default
  (defhydra hydra-gnus-group (:color blue)
    "Do?"
    ("a" gnus-group-list-active "REMOTE groups A A")
    ("l" gnus-group-list-all-groups "LOCAL groups L")
    ("c" gnus-topic-catchup-articles "Read all c")
    ("G" gnus-group-make-nnir-group "Search server G G")
    ("g" gnus-group-get-new-news "Refresh g")
    ("s" gnus-group-enter-server-mode "Servers")
    ("m" gnus-group-new-mail "Compose m OR C-x m")
    ("#" gnus-topic-mark-topic "mark #")
    ("q" nil "cancel"))
  (define-key gnus-group-mode-map "y" 'hydra-gnus-group/body))


(use-package gnus-sum    ;; gnus-summary-mode
  :defer t
  :after hydra
  :config
  ;; y is not used by default
  (defhydra hydra-gnus-summary (:color blue)
    "
Sending/Replying^^             ^Mark^                     ^Actions^
----------------------------------------------------------------
_r_: Reply                     _!_: Pin mail              _g_: Refresh
_R_: Reply with original       _p_: Mark as read          _q_: Quit
_w_: Wide reply                _u_: Unread mail
_W_: Wide reply with original  _mc_: Read all
_f_: Forward                   _mp_: Mark processable
_e_: Resend                    _mu_: Unmark processable

"
    ("r" gnus-summary-reply "r")
    ("R" gnus-summary-reply-with-original "R")
    ("w" gnus-summary-wide-reply "S w")
    ("W" gnus-summary-wide-reply-with-original "S W")
    ("f" gnus-summary-mail-forward "C-c C-f")
    ("e" gnus-summary-resend-message-edit "S D e")

    ("!" gnus-summary-tick-article-forward "!")
    ("p" gnus-summary-mark-as-read-forward "d")
    ("u" gnus-summary-clear-mark-forward "M c")
    ("mc" gnus-summary-catchup "M C")
    ("mp" gnus-summary-mark-processable "#")
    ("mu" gnus-summary-unmark-as-processable "M-#")

    ("g" gnus-summary-insert-new-articles "/ N")
    ("q" nil "Quit"))
  (define-key gnus-summary-mode-map "y" 'hydra-gnus-summary/body))


(use-package gnus-art   ;; gnus-article-mode
  :defer t
  :after hydra
  :config
  ;; y is not used by default
  (defhydra hydra-gnus-article (:color blue)
    "
Sending/Replying^^             ^Treatment^            ^Display^             ^Actions^
----------------------------------------------------------------
_r_: Reply                     _t_: Toggle headers    _wg_: Gravatar        _s_: Summary Hydra
_R_: Reply with original       _r_: Caesar            _wf_: Face            _v_: View MIME
_w_: Wide reply                _m_: Morse             _wx_: X-Face          _o_: Save MIME
_W_: Wide reply with original  _u_: UTF-8 -> ASCII    _ws_: Smileys         _g_: Refresh
_f_: Forward                   _o_: Deuglify Outlook  _ww_: Show images     _q_: Quit
_e_: Resend                    _F_: Fill long lines   _wd_: Remove images

"
    ("r" gnus-article-reply "r")
    ("R" gnus-article-reply-with-original "R")
    ("w" gnus-article-wide-reply "S w")
    ("W" gnus-article-wide-reply-with-original "S W")
    ("f" gnus-summary-mail-forward "Forward")
    ("e" gnus-summary-resend-message-edit "S D e")

    ("t" gnus-summary-toggle-header "t")
    ("r" gnus-summary-caesar-message "W r")
    ("m" gnus-summary-morse-message "W m")
    ("u" gnus-article-treat-non-ascii "W U")
    ("o" gnus-article-outlook-deuglify-article "W Y f")
    ("F" gnus-article-fill-long-lines "W Q")

    ("wg" gnus-treat-from-gravatar "W D h")
    ("wf" gnus-article-display-face "W D d")
    ("wx" gnus-article-display-x-face "W D x")
    ("ws" gnus-treat-smiley "W D s")
    ("wi" gnus-html-show-images "W D W")
    ("wd" gnus-article-remove-images "W D D")

    ("s" hydra-gnus-summary/body nil :exit t)
    ("o" gnus-mime-save-part "o")
    ("g" gnus-summary-show-article "g")
    ("q" nil "Quit"))
  (define-key gnus-article-mode-map "y" 'hydra-gnus-article/body))


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
