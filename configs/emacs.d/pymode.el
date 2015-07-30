;; Python specific settings.
(defun my/python-mode-hook ()
  "My python mode hook."
  (global-auto-complete-mode -1)
  (auto-complete-mode -1)
  (define-key python-mode-map [f5] 'python-check)
  (define-key python-mode-map [f7] 'pdb)
  (define-key python-mode-map [f8] 'run-python)
  (add-to-list 'company-backends 'company-anaconda)
  (setq-local whitespace-style '(face
				 tabs
				 tab-mark
				 lines-tail
				 trailing
				 indentation
				 indentation::space))
  ;; Define a fix for the re-mapping of find-tag.
  (define-key anaconda-mode-map
    [remap xref-find-definitions] 'anaconda-mode-goto))
(add-hook 'python-mode-hook 'my/python-mode-hook)
;;(add-hook 'python-mode-hook 'ac-anaconda-setup)
;;(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'eldoc-mode)
(add-hook 'python-mode-hook 'company-mode)
(add-hook 'python-mode-hook 'highlight-indentation-mode)
