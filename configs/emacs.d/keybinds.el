;; Set some global keybindings.
(global-set-key [f10] 'compile)
(global-set-key [f9] 'global-linum-mode)
(global-set-key [f8] 'global-font-lock-mode)
(global-set-key [f7] 'global-hl-line-mode)
(global-set-key "\C-h" 'backward-delete-char)
(global-set-key "\M-h" 'backward-kill-word)

;; Disable some commands.
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; Add some extra keybinding for M-x.
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)
