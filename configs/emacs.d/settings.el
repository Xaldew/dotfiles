;; Do not display the menu-bar.
(menu-bar-mode -1)

;; Don't show the scroll bars.
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; Set the default fill-column.
(setq-default fill-column 80)

;; Automatically follow symlink without prompting.
(setq vc-follow-symlinks t)

;; Change large-file threshold to 1GB rather than ~10MB.
(setq large-file-warning-threshold 1073741824)

;; Set the colour theme to something better. (emacs 24+)
(when (>= emacs-major-version 24)
  (load-theme 'tango-dark t))

;; Place all backups in system temp instead.
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Changes all yes/no questions to y/n type.
(fset 'yes-or-no-p 'y-or-n-p)

;; Killing a line at the begining of a line removes the line as well.
(setq kill-whole-line t)

;; Fix dictionaries for Aspell.
(setq ispell-personal-dictionary (expand-file-name "~/.dicts/personal.dict"))
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(add-hook 'text-mode-hook 'flyspell-mode)


;; Activate UTF-8 coding for almost everything.
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;; Activate certain DocView settings.
(setq doc-view-continuous t)

;; Activate terminal mouse-mode.
(xterm-mouse-mode t)

;; Automatically update buffers to reflect changes on disk.
(global-auto-revert-mode t)

;; When cursor is on edge, move to the other side, as in a toroidal space.
(setq windmove-wrap-around t)

;; Allow movement to the windows using a prefixed arrow key.
(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <down>")  'windmove-down)

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
(setq scroll-conservatively 10000)

;; Show the matching parenthesis.
(show-paren-mode t)

;; Globally enable subword-mode, i.e., allow CamelCase word traversal.
(global-subword-mode t)

;; Ignore case when looking for files and buffers.
(setq read-buffer-completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)
(setq completion-ignore-case t)


;; Set various clipboard management variables.
(setq x-select-enable-clipboard-manager t)
(setq x-select-enable-primary t)
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
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq whitespace-style '(face empty lines-tail trailing))
(global-whitespace-mode t)

;; Use gdb-many-windows by default and start at the main routine.
(setq gdb-many-windows t)
(setq gdb-show-main t)
(setq gdb-display-io-nopopup t)

(defun my-gud-mode-hook ()
  "Personal hook used to initialize global GUD settings."
  (define-key gud-minor-mode-map '[f5] 'gud-step)
  (define-key gud-minor-mode-map '[f6] 'gud-next)
  (define-key gud-minor-mode-map '[f7] 'gud-up)
  (define-key gud-minor-mode-map '[f8] 'gud-down))
(add-hook 'gud-mode-hook 'my-gud-mode-hook)
(add-hook 'gud-mode-hook 'gud-tooltip-mode)

(defun my-gdb-parent-mode-hook ()
  "Additional buffer setup for the GDB MI buffers."
  (toggle-truncate-lines)
  (whitespace-mode -1)
  (global-whitespace-mode -1))
(add-hook 'gdb-threads-mode-hook     'my-gdb-parent-mode-hook)
(add-hook 'gdb-memory-mode-hook      'my-gdb-parent-mode-hook)
(add-hook 'gdb-disassembly-mode-hook 'my-gdb-parent-mode-hook)
(add-hook 'gdb-breakpoints-mode-hook 'my-gdb-parent-mode-hook)
(add-hook 'gdb-frames-mode-hook      'my-gdb-parent-mode-hook)
(add-hook 'gdb-locals-mode-hook      'my-gdb-parent-mode-hook)
(add-hook 'gdb-registers-mode-hook   'my-gdb-parent-mode-hook)

;; Increase lisp evaluation depth and the number of variable bindings.
(setq max-lisp-eval-depth '40000)
(setq max-specpdl-size '100000)
