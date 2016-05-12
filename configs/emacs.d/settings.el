;;; settings.el --- Package independent default settings.
;;
;;; Commentary:
;; This file contains generic settings that should not require any external
;; packages.
;;
;;; Code:

(eval-when-compile
  (require 'gud)
  (require 'org))

;; Truncate lines by default.
(setq-default truncate-lines t)

;; Do not display the menu-bar.
(menu-bar-mode -1)

;; Don't show the scroll bars.
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; Set the default fill-column.
(setq-default fill-column 80)

(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode t)

;; Automatically save Emacs minibuffer history.
(savehist-mode t)

;; Automatically follow symlink without prompting.
(setq vc-follow-symlinks t)

;; Change large-file threshold to 1GB rather than ~10MB.
(setq large-file-warning-threshold 1073741824)

;; Set the color theme to something better. (Emacs 24+)
(when (>= emacs-major-version 24)
  (load-theme 'tango-dark 'no-prompt))

;; Place all backups and auto-saves in system temporary directory.
(setq backup-directory-alist `(("." . ,temporary-file-directory))
      backup-by-copying t          ; Don't clobber symlinks.
      version-control t            ; Version numbers for backup files.
      delete-old-versions t        ; Delete excess backup files silently.
      delete-by-moving-to-trash t
      kept-old-versions 6          ; Oldest numbered version to keep.
      kept-new-versions 9          ; Newest numbered versions to keep.
      auto-save-default t          ; Auto-save every buffer that visits a file.
      auto-save-timeout 20         ; Seconds of idle time before auto-save.
      auto-save-interval 1000)     ; Number of keystrokes between auto-saves.
(add-to-list 'auto-save-file-name-transforms
	     `(".*" ,temporary-file-directory t) 'append)


;; Changes all yes/no questions to y/n type.
(fset 'yes-or-no-p 'y-or-n-p)


;; Enable downcase/upcase-region functions.
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)


;; Killing a line at the begining of a line removes the line as well.
(setq kill-whole-line t
      kill-read-only-ok t
      kill-do-not-save-duplicates t)


;; Set up spellchecking in text-modes and programming modes.
(add-hook 'prog-mode-hook #'flyspell-prog-mode)
(add-hook 'text-mode-hook #'flyspell-mode)


;; Activate UTF-8 coding for almost everything.
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq buffer-file-coding-system 'utf-8)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;; Activate certain DocView settings.
(setq doc-view-continuous t)

;; Activate terminal mouse-mode.
(xterm-mouse-mode t)

;; Automatically update buffers to reflect changes on disk.
(setq auto-revert-interval 30)  ; Temporary fix for magit-rebasing.
(global-auto-revert-mode t)

;; When cursor is on edge, move to the other side, as in a toroidal space.
(setq windmove-wrap-around t)

;; Activate winner-mode to undo/redo window configurations.
(winner-mode t)

;; Allow shrinking of the windows using
(global-set-key (kbd "C-<left>")  'shrink-window-horizontally)
(global-set-key (kbd "C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-<down>")  'shrink-window)
(global-set-key (kbd "C-<up>")    'enlarge-window)

;; Make scrolling smoother.
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-follow-mouse 't)
(setq scroll-step 1)
(setq scroll-margin 3)
(setq hscroll-margin 2)
(setq scroll-conservatively 10000)

;; Show the matching parenthesis.
(show-paren-mode t)

;; Globally enable subword-mode, i.e., allow CamelCase word traversal.
(global-subword-mode t)

;; Save the point location in each visited file.
(save-place-mode t)

;; Ignore case when looking for files and buffers.
(setq read-buffer-completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)
(setq completion-ignore-case t)


;; Set various clipboard management variables.
(setq x-select-enable-clipboard-manager t)
(setq x-select-enable-clipboard t)
(setq select-enable-clipboard t)
(setq save-interprogram-paste-before-kill t)
(setq mouse-yank-at-point t)


;; Comint - Command interpreter settings.
(setq comint-scroll-to-bottom-on-input t) ; Scroll to bottom on input.
(setq comint-prompt-read-only t) ; Comint buffers are read-only outside prompt.
(setq comint-input-ignoredups t) ; Ignore duplicates in history.


;; Activate whitespace: Mark lines longer than 80 columns.
;; Note that to disable whitespace mode while in a terminal, you must evalute
;; M-x global-whatespace-mode _AND_ M-x revert-buffer. A wrapper function in
;; `func.el' does this automatically.
(add-hook 'before-save-hook #'delete-trailing-whitespace)
(setq whitespace-style '(face empty lines-tail trailing))
(add-hook 'prog-mode-hook #'whitespace-mode)


;; Do not indent with tabs by default anywhere but in Makefiles.
(setq tab-always-indent t)
(setq-default indent-tabs-mode nil)
(defun my-makefile-tab-hook ()
  "Indent Makefiles with tabs."
  (setq-local indent-tabs-mode t))
(add-hook 'makefile-mode-hook #'my-makefile-tab-hook)


;; Use gdb-many-windows by default and start at the main routine.
(setq gdb-many-windows t)
(setq gdb-show-main t)
(setq gdb-display-io-nopopup t)
(setq gdb-non-stop-setting t)

(defun gdb-toggle-non/all-stop ()
  "Toggle between GDB non-stop and all-stop modes.

All-stop mode stops all threads upon hitting a break-point.

Non-stop mode only stops the current thread."
  (interactive)
  (setq gdb-non-stop-setting (not gdb-non-stop-setting))
  (if gdb-non-stop-setting
      (message "Non-stop-mode turned on. Restart GDB session to take effect.")
    (message "All-stop-mode turned on. Restart GDB session to take effect.")))

(defun my-gud-mode-hook ()
  "Personal hook used to initialize global GUD settings."
  (define-key gud-minor-mode-map [f5] 'gud-step)
  (define-key gud-minor-mode-map [f6] 'gud-next)
  (define-key gud-minor-mode-map [f7] 'gud-up)
  (define-key gud-minor-mode-map [f8] 'gud-down)
  (define-key gud-minor-mode-map [f9] 'gdb-many-windows))
(add-hook 'gud-mode-hook #'my-gud-mode-hook)
(add-hook 'gud-mode-hook #'gud-tooltip-mode)

(defun my-gdb-parent-mode-hook ()
  "Additional buffer setup for the GDB MI buffers."
  (toggle-truncate-lines 1)
  (whitespace-mode -1)
  (global-whitespace-mode -1))
(add-hook 'gdb-threads-mode-hook     #'my-gdb-parent-mode-hook)
(add-hook 'gdb-memory-mode-hook      #'my-gdb-parent-mode-hook)
(add-hook 'gdb-disassembly-mode-hook #'my-gdb-parent-mode-hook)
(add-hook 'gdb-breakpoints-mode-hook #'my-gdb-parent-mode-hook)
(add-hook 'gdb-frames-mode-hook      #'my-gdb-parent-mode-hook)
(add-hook 'gdb-locals-mode-hook      #'my-gdb-parent-mode-hook)
(add-hook 'gdb-registers-mode-hook   #'my-gdb-parent-mode-hook)


(defun my-help-mode-hook ()
  "Personal hook for `help-mode'."
  (define-key help-mode-map [f5] #'describe-symbol)
  (define-key help-mode-map [f7] #'describe-function)
  (define-key help-mode-map [f9] #'describe-mode))
(add-hook 'help-mode-hook #'my-help-mode-hook)

;; Search more extensively when using apropos.
(setq apropos-do-all t)

;; Activate Semantic and the Semantic Recoder for all programming modes.
(autoload 'srecode-minor-mode "srecode")
(defun my-srecode-hook ()
  "Hook to run after initializing srecode."
  (setq srecode-insert-ask-variable-method 'field)
  (let ((template-dir (concat (file-name-as-directory
                               (expand-file-name user-emacs-directory))
                              "templates")))
    (add-to-list 'srecode-map-load-path template-dir 'append)))
(setq semantic-idle-scheduler-idle-time 60)
(add-hook 'prog-mode-hook 'semantic-mode)
(add-hook 'semantic-mode-hook 'srecode-minor-mode)
(add-hook 'srecode-minor-mode-hook 'my-srecode-hook)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode)


;; Increase lisp evaluation depth and the number of variable bindings.
(setq max-lisp-eval-depth '40000)
(setq max-specpdl-size '100000)


;; Automatically insert corresponding closing parenthesis.
(electric-pair-mode)


;; Activate line numbers and column markers.
(setq linum-format "%d ")
(setq line-number-mode t)
(setq column-number-mode t)
(add-hook 'after-init-hook 'global-linum-mode)


;; Add org-mode configuration.
(defun my-start-org-mode (prefix ch)
  "Start `org-mode'.

PREFIX arguments are still usable with this command.  They are
simply passed on to the corresponding function.

This function prompts for a character CH to decide which org-mod
function to call."
  (interactive "P\ncORG: <l:link, a:agenda, c:capture, b:switchb>")
  (cond ((= ch ?l) (org-store-link prefix))
        ((= ch ?a) (org-agenda prefix))
        ((= ch ?c) (let ((current-prefix-arg prefix))
                     (call-interactively 'org-capture)))
        ((= ch ?b) (org-switchb prefix))))
(global-set-key (kbd "C-c o") 'my-start-org-mode)


;; GNUS settings.
(setq gnus-home-directory (expand-file-name "gnus/" user-emacs-directory)
      gnus-init-file	  (expand-file-name "gnus.el" user-emacs-directory)
      gnus-directory      (expand-file-name "news/" gnus-home-directory)
      message-directory	  (expand-file-name "mail/" gnus-home-directory))


(setq ediff-diff-options "-w")
(setq ediff-split-window-function #'split-window-horizontally)
(setq ediff-window-setup-function #'ediff-setup-windows-plain)


;; Simple HTML Renderer - Fix bright backgrounds.
(setq shr-color-visible-luminance-min 70)


;;; settings.el ends here
