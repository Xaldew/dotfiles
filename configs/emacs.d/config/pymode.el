;;; pymode.el --- Personal Python specific settings.
;;
;;; Commentary:
;; Personal Python mode specific settings.
;;
;;; Code:

(defvar python-shell-interpreter)
(defvar python-shell-interpreter-args)
(defvar python-shell-prompt-regexp)
(defvar python-shell-prompt-output-regexp)
(defvar python-shell-setup-codes)
(defvar python-mode-map)

(defun my/python-mode-hook ()
  "Personal Python-mode hook for buffer local settings."
  (when (executable-find "ipython")
    (setq
     python-shell-interpreter "ipython"
     python-shell-interpreter-args ""
     python-shell-prompt-regexp "In \\[[0-9]+\\]: "
     python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
     python-shell-setup-codes '())) ; TODO: Add autoload setup code.

  (setq-local fill-column 79)
  (define-key python-mode-map [f5] 'python-check)
  (define-key python-mode-map [f7] 'pdb)
  (define-key python-mode-map [f8] 'run-python)
  (setq-local whitespace-style '(face
				 tabs
				 tab-mark
				 lines-tail
				 trailing
				 indentation
				 indentation::space)))
(add-hook 'python-mode-hook #'my/python-mode-hook)

;;; pymode.el ends here
