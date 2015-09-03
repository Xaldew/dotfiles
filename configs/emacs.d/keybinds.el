;; Set some global key bindings.
(global-set-key [f5]  'compile)
(global-set-key [f6]  'vc-dir)
(global-set-key [f7]  'gdb)
(global-set-key [f8]  'shell)
(global-set-key [f9]  'toggle-truncate-lines)
(global-set-key (kbd "C-h") 'backward-delete-char)
(global-set-key (kbd "M-h") 'backward-kill-word)
(global-set-key (kbd "C-c t") 'toggle-truncate-lines)

;; Add some extra key binding for M-x.
(global-set-key (kbd "C-x C-m") 'execute-extended-command)
(global-set-key (kbd "C-c C-m") 'execute-extended-command)

;; Disable the character transpose (C-t) keybinding, which is used by Tmux.
(global-unset-key (kbd "C-t"))

;; Enable M-* to pop-tag-mark. This is the default in Emacs pre-25.1.
(global-set-key (kbd "M-*") 'pop-tag-mark)
