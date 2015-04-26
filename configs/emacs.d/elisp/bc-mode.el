84;0;0c;;; bc-mode.el --- BC code editing commands for Emacs

;; Copyright (C) 2015  Gustaf Waldemarson

;; Author: Gustaf Waldemarson <gustaf.waldemarson@gmail.com>
;; Created: 2015-04-25
;; Keywords: BC languages

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Sets up a derived C-mode with support for the BC language.
;;
;; For autoload, ass the following lines to your .emacs:
;; (autoload 'bc-mode "bc-mode.el" "bc-mode" t 'nil)

;;; Code:

(require 'cc-mode)

(defcustom bc-mode-hook nil
  "Normal hook run when entering bc-mode."
  :type 'hook
  :group 'data)

(defvar bc-command-line "bc --mathlib"
  "Command line executed for `bc'.")

;; Regexps
(defconst bc-font-lock-keywords
  (eval-when-compile
    (list
     '("\\(#.*\\)$"
       (1 font-lock-comment-face))
     ;; Function names.
     '("^[ \t]*\\(define\\)\\>[ \t]*\\(\\sw+\\)?"
       (1 font-lock-keyword-face) (2 font-lock-function-name-face nil t))
     ;; Math library functions.
     '(".*\\([scalej]\\)(\\sw+)" (1 font-lock-function-name-face))
     ;; Variable names.
     (cons (regexp-opt
	    '("SCALE" "IBASE" "OBASE" "LAST") 'words)
	   'font-lock-variable-name-face)
     ;; Keywords.
     (regexp-opt
      '("break" "continue" "delete" "exit" "else" "for" "quit" "auto" "local"
	"if" "print" "return" "while") 'words)
     ;; Builtins.
     (list (regexp-opt
	    '("length" "read" "scale" "sqrt") 'words)
	   1 'font-lock-builtin-face)
     ;; Operators.
     (cons (regexp-opt '("&&" "||" "<=" "<" ">=" ">" "==" "!="))
	   'font-lock-constant-face)))
  "Default expressions to highlight in BC mode.")


(defvar bc-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-j" 'newline-and-indent)
    map)
  "Keymap for the BC major mode")


;; Associate BC files with bc-mode.
;;;###autoload
(progn
  (add-to-list 'auto-mode-alist '("\\.bc\\'" . bc-mode))
  (add-to-list 'interpreter-mode-alist '("bc" . bc-mode)))

;;;###autoload
(define-derived-mode bc-mode c-mode "BC"
  "Major mode for editing BC code.

   This mode is derived from the c-mode with minor modifications to the comment
   syntax. The keymap is inherited from C mode and it has the same variables for
   customizing indentation.  The mode uses `c-mode-syntax-table' for customized
   syntax entries.

   Turning on bc-mode executes the entries in `bc-mode-hook'.
   "
  (set-syntax-table c-mode-syntax-table)
  (use-local-map bc-mode-map)

  (set (make-local-variable 'comment-start) "/* ")
  (set (make-local-variable 'comment-end) " */")
  (set (make-local-variable 'comment-start-skip) "#+ *|/\\* +")
  (setq font-lock-defaults '(bc-font-lock-keywords nil nil ((?_ . "w"))))
  (run-hooks 'bc-mode-hook))

(provide 'bc-mode)

;;; bc-mode.el ends here
