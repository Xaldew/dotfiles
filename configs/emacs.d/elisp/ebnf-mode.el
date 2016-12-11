;;; ebnf-mode.el --- Major mode (E)BNF grammars -*- lexical-binding: t -*-
;;
;;; Commentary:
;; Major mode for editing files containing various styles of (E)BNF grammars.
;; Currently only supports the ISO-EBNF style.
;;
;;; Code:

(require 'smie)

(defgroup ebnf-mode nil
  "(E)BNF editing support for Emacs."
  :group 'languages
  :version "25.1")


;;;; Fontification.


(defvar ebnf-mode-lhs
  (rx (and bol (* space)
           (group-n 1 (in "A-Z" "a-z")
                    (+ (in "A-Z" "a-z" "0-9" "_")))
           (* space) "="))
  "Regular expression to match and fontify the (E)BNF left hand sides.")


(defvar ebnf-mode-font-lock-keywords
  `(((,ebnf-mode-lhs (1 font-lock-variable-name-face))))
  "List over `font-lock' specifiers.")


;;;; SMIE indentation setup.


(defcustom ebnf-mode-indent-offset 4
  "(E)BNF indentation width."
  :group 'ebnf-mode
  :type 'integer
  :safe #'integerp)


(defconst ebnf-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?'  "\""  table) ; String delimiter
    (modify-syntax-entry ?\" "\"" table)  ; String delimiter
    (modify-syntax-entry ?\( "()1" table)  ; Comment start
    (modify-syntax-entry ?*  ". 23" table) ; Comment start/end
    (modify-syntax-entry ?\) ")(4" table)  ; Comment end
    table))


(defvar ebnf-mode-smie-grammar
  (smie-prec2->grammar
   (smie-bnf->prec2
    '((id)
      (lhs (id))
      (rhs (rhs "|" rhs)
           (rhs "," rhs)
           (id))
      (rule (lhs "=" rhs))
      (rules (rules ";" rules) (rule)))
    '((assoc ";"))
    '((assoc ","))
    '((assoc "|"))))
  "Simplfied BNF grammar for `smie' indentation.")


(defun ebnf-mode-smie-rules (kind token)
  "Perform indentation of KIND on TOKEN using the `smie' engine."
  (message "%s" (cons kind token))
  (pcase (cons kind token)
    (`(:elem . basic)            ebnf-mode-indent-offset)
    (`(:elem . args)             ebnf-mode-indent-offset)
    (`(:list-intro . "=")        0)
    (`(:before . ,(or ";" "," "|"))  (smie-rule-separator kind))
    (`(:after . "=") ebnf-mode-indent-offset)))


;;;; Major mode definitions.

(define-derived-mode ebnf-mode prog-mode "EBNF"
  :group 'ebnf-mode
  :syntax-table ebnf-mode-syntax-table
  (setq-local comment-start "(* ")
  (setq-local comment-end   " *)")
  (setq-local comment-start-skip "\\((\\*\\)\\s-*")
  (setq-local comment-end-skip   "\\s-*\\(\\*)\\)")
  (setq-local indent-tabs-mode nil)
  (smie-setup ebnf-mode-smie-grammar #'ebnf-mode-smie-rules)
  (setq-local font-lock-defaults ebnf-mode-font-lock-keywords)
  (font-lock-flush))


;;;###autoload
(add-to-list 'auto-mode-alist '("\\.ebnf\\'" . ebnf-mode))

(provide 'ebnf-mode)

;;; ebnf-mode.el ends here
