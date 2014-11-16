;; Do not display the menu-bar.
(menu-bar-mode 0)

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

;; Fix dictionaries for Aspell.
(setq ispell-program-name "aspell")
;; (setq ispell-process-directory (expand-file-name "~/"))
;; (setq ispell-personal-dictionary (expand-file-name "~/.dicts/personal.dict"))
;; (setq ispell-extra-args '("--sug-mode=ultra"))
(setq ispell-dictionary "english")
;;(setq ispell-list-command "list")

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
