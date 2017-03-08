;;; poly-pweave-rest.el --- Polymode for Pweave + ReST. -*- lexical-binding: t -*-
;;
;;; Commentary:
;; A mode for interacting working with `Pweave' in a ReST host-mode with
;; additional `noweb' chunks containing `python' code.
;;
;;; Code:

(require 'polymode)
(require 'poly-noweb)
(require 'poly-rest-mode)


(defcustom pm-inner/noweb+python
  (clone pm-inner/noweb :mode 'python-mode)
  "Inner mode of `noweb' chunks with Python code."
  :group 'innermodes
  :type 'object)


(defcustom pm-poly/Pweave+ReST
  (pm-polymode-multi-auto
   "Pweave"
   :hostmode 'pm-host/ReST
   :innermodes '(pm-inner/noweb+python)
   :auto-innermode 'pm-inner/ReST-code)
  "Polymode for `Pweave' with ReST syntax."
  :group 'polymodes
  :type 'object)


(define-polymode poly-pweave-rest-mode pm-poly/Pweave+ReST
  :lighter " Pw-ReST")


;;;###autoload
(progn
  (autoload #'poly-pweave-rest-mode "poly-pweave-rest")
  (add-to-list 'auto-mode-alist '("\\.rstw\\'" . poly-pweave-rest-mode)))


(provide 'poly-pweave-rest)

;;; poly-pweave-rest.el ends here
