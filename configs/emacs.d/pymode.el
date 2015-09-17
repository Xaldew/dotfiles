;; Python specific settings.
(defun my/python-mode-hook ()
  "My python mode hook."
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
(add-hook 'python-mode-hook 'my/python-mode-hook)
