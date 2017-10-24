;;; file-assoc.el --- Miscellaneous file associations.
;;
;;; Commentary:
;; Add mode associations for various file types.
;;
;;; Code:

;; Various configuration files.
(add-to-list 'auto-mode-alist '("emacs\\'" . emacs-lisp-mode))
(add-to-list 'auto-mode-alist '("bashrc\\'" . shell-script-mode))
(add-to-list 'auto-mode-alist '("bash_aliases\\'" . shell-script-mode))
(add-to-list 'auto-mode-alist '("profile\\'" . shell-script-mode))
(add-to-list 'auto-mode-alist '("gitconfig.*\\'" . gitconfig-mode))
(add-to-list 'auto-mode-alist '("\\.rc\\'" . conf-mode))

;; Add makefile modes.
(add-to-list 'auto-mode-alist '("\\.mak\\'"   . makefile-gmake-mode))
(add-to-list 'auto-mode-alist '("\\.defs\\'"  . makefile-gmake-mode))
(add-to-list 'auto-mode-alist '("\\.build\\'" . makefile-gmake-mode))

;; Set Scons files to python mode.
(add-to-list 'auto-mode-alist '("[Ss][Cc]onstruct\\'" . python-mode))
(add-to-list 'auto-mode-alist '("[Ss][Cc]onscript\\'" . python-mode))

;; Add auto-env `.env' files to `shell-script-mode'.
(add-to-list 'auto-mode-alist '("\\(/\\|\\`\\)\\.env\\'" . shell-script-mode))

;;; file-assoc.el ends here
