;;; terminfo.el --- Major mode for terminfo sources -*- lexical-binding: t -*-
;;
;;; Commentary:
;; Major mode for editing and viewing `terminfo' source files.
;;
;;; Code:

(require 'smie)


(defgroup terminfo nil
  "Terminfo editing support for Emacs."
  :group 'languages
  :version "25.1")


(defcustom terminfo-indent-offset 2
  "Default indentation offset for `terminfo' source files."
  :group 'terminfo
  :type 'integer
  :safe #'integerp)


(defconst terminfo--terminal-rx
  (rx (group (+? nonl)) ?| (group (* nonl)) ",\n")
  "Regular expression to capture a `terminfo' terminal definition header.")


(defconst terminfo--capability-rx-1
  (rx (group (+ (in "A-Z" "a-z" "0-9"))) (? ?@) ?\,)
  "The first regular expression to capture `terminfo' capabilities.")


(defconst terminfo--capability-rx-2
  (rx (group (+ (in "A-Z" "a-z" "0-9"))) ?# (group (+ digit)) ?\,)
  "The second regular expression to capture `terminfo' capabilities.")


(defconst terminfo--capability-rx-3
  (rx (group (+ (in "A-Z" "a-z" "0-9"))) ?= (group (+ (not (in ",")))))
  "The third regular expression to capture `terminfo' capabilities.")


(defconst terminfo-font-lock-keywords
  `(((,terminfo--terminal-rx
      (1 font-lock-function-name-face)
      (2 font-lock-doc-face))
     (,terminfo--capability-rx-3
      (1 font-lock-variable-name-face)
      (2 font-lock-string-face))
     (,terminfo--capability-rx-2
      (1 font-lock-variable-name-face)
      (2 font-lock-constant-face))
     (,terminfo--capability-rx-1
      (1 font-lock-variable-name-face))))
  "List over `font-lock' rules.")


(defconst terminfo-smie-grammar
  (smie-prec2->grammar
   (smie-bnf->prec2
    '((id)
      (cap (id "=" id))
      (stmt (id) (cap))
      (stmts (stmts "," stmts) (stmt)))
    '((assoc "="))
    '((assoc ","))))
  "Simplified BNF grammar describing the `terminfo' source files.")


(defun terminfo-smie-rules (kind token)
  "Perform indentation of KIND on TOKEN using the `smie' engine."
  (print (format "%s" (cons kind token)))
  (pcase (cons kind token)
    (`(:elem . basic)     terminfo-indent-offset)
    (`(:elem . args)      0)
    (`(:list-intro . ",") 0)
    (`(:after . ",")      (smie-rule-separator kind))
    (`(:before . ",")     (smie-rule-separator kind))))


(defconst terminfo-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?#  "<" table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?\[  "w" table)
    (modify-syntax-entry ?\]  "w" table)
    table)
  "Syntax table rules for `terminfo-mode'.")


(define-derived-mode terminfo-mode prog-mode "terminfo"
  "Major mode for `terminfo' source files."
  :group 'terminfo
  :syntax-table terminfo-syntax-table
  (setq-local comment-start "# ")
  (setq-local comment-start-skip "#+\\s-*")
  (setq-local syntax-propertize-function
              (syntax-propertize-rules ("[^# \t]+\\(#+\\)" (1 "."))))
  (smie-setup terminfo-smie-grammar #'terminfo-smie-rules)
  (setq-local font-lock-defaults terminfo-font-lock-keywords)
  (font-lock-flush))


;;;###autoload
(add-to-list 'auto-mode-alist '("terminfo.src\\'" . terminfo-mode))

(provide 'terminfo)

;;; terminfo.el ends here
