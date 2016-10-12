;;; poly-rest-mode.el -- Polymode for ReST. -*- lexical-binding: t -*-
;;
;;; Commentary:
;; A mode for interacting with multiple kinds of code inside a ReST host-mode.
;;
;;; Code:

(require 'polymode)


(defcustom pm-host/ReST
  (pm-bchunkmode "ReST"
                 :mode 'rst-mode
                 :init-functions '(poly-rest-fixes))
  "Restructued Text host chunkmode."
  :group 'hostmodes
  :type 'object)

(defcustom pm-inner/ReST-code
  (pm-hbtchunkmode-auto "ReST"
                        :head-reg "^[ \t]*\\.\\. code::.*$"
                        :tail-reg "^[^ \t\n]+$"
                        :retriever-regexp "^[ \t]*\\.\\. code:: +\\(.+\\)"
                        :head-mode 'host
                        :tail-mode 'host
                        :font-lock-narrow t)
  "Restructured Text code chunks."
  :group 'innermodes
  :type 'object)

(defcustom pm-poly/ReST
  (pm-polymode-multi-auto "ReST"
                          :hostmode       'pm-host/ReST
                          :auto-innermode 'pm-inner/ReST-code)
  "Restructured Text typical configuration."
  :group 'polymodes
  :type 'object)

;;;###autoload (autoload #'poly-markdown-mode "poly-rest-mode")
(define-polymode poly-rest-mode pm-poly/ReST)

(defun poly-rest-fixes ()
  "Fix various minor issues that can occur in the poly-ReST-mode."
  (remove-hook 'prog-mode-hook   #'whitespace-mode)
  (remove-hook 'python-mode-hook #'mmm-mode))

(provide 'poly-rest-mode)

;;; poly-rest-mode.el ends here
