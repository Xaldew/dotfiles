;; Add mode association for files.
(add-to-list 'auto-mode-alist '("emacs\\'" . emacs-lisp-mode))
(add-to-list 'auto-mode-alist '("bashrc\\'" . shell-script-mode))
(add-to-list 'auto-mode-alist '("bash_aliases\\'" . shell-script-mode))

;; OpenCL mode.
(add-to-list 'auto-mode-alist '("\\.cl\\'" . c-mode))
