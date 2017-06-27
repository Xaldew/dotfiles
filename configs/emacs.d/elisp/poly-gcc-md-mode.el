;;; poly-gcc-md-mode.el --- Polymode for GCC .md files. -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; A mode used to combine `lisp-mode' and `c-mode' for editing GCC machine
;; description (.md) files.
;;
;;; Code:

(require 'polymode)


(defcustom pm-host/gcc-md-lisp
  (pm-bchunkmode "MDsc"
                 :mode 'lisp-mode)
  "GCC machine description host `lisp-mode' chunk."
  :group 'hostmodes
  :type 'object)


(defcustom pm-inner/gcc-md-c
  (pm-hbtchunkmode "MDsc"
                   :head-reg #'ignore
                   :tail-reg #'ignore
                   :head-mode 'host
                   :tail-mode 'host
                   :font-lock-narrow t)
  "GCC machine description inner `c-mode' chunk."
  :group 'innermodes
  :type 'object)


(defcustom pm-poly/gcc-md
  (pm-polymode-multi
   "MDsc"
   :hostmode 'pm-host/gcc-md-lisp
   :innermodes '(pm-inner/gcc-md-c)
   "Polymode for GCC Machine description (.md) files.")
  :group 'polymodes
  :type 'object)


(define-polymode poly-gcc-md-mode pm-poly/gcc-md)


;;;###autoload
(add-to-list 'auto-mode-alist '(".*gcc.+\\.md\\'" . poly-gcc-md-mode))


(provide 'poly-gcc-md-mode)

;;; poly-gcc-md-mode.el ends here
