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


(defcustom ebnf-mode-style 'iso-ebnf
  "The EBNF style to associate with the current buffer."
  :group 'ebnf-mode
  :type '(radio :tag "Style"
		(const ebnf)
                (const iso-ebnf))
  :safe #'symbolp)


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


;;;; `iso-ebnf' style rules.


(defconst ebnf-mode-iso-ebnf-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?'  "\""  table) ; String delimiter
    (modify-syntax-entry ?\" "\"" table)  ; String delimiter
    (modify-syntax-entry ?\( "()1" table)  ; Comment start
    (modify-syntax-entry ?*  ". 23" table) ; Comment start/end
    (modify-syntax-entry ?\) ")(4" table)  ; Comment end
    table))


(defvar ebnf-mode-smie-iso-ebnf-grammar
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
    '((assoc "|") (assoc ","))))
  "Simplfied BNF grammar for `smie' indentation of the `iso-ebnf' style.")


(defun ebnf-mode-smie-iso-ebnf-rules (kind token)
  "Perform indentation of KIND on TOKEN using the `smie' engine."
  (pcase (cons kind token)
    (`(:elem . basic)            ebnf-mode-indent-offset)
    (`(:elem . args)             ebnf-mode-indent-offset)
    (`(:list-intro . "=")        0)
    (`(:before . ,(or ";" "," "|"))  (smie-rule-separator kind))
    (`(:after . "=") ebnf-mode-indent-offset)))


;;;; `ebnf' style rules.


(defconst ebnf-mode-ebnf-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?'  "\"" table)  ; String delimiter
    (modify-syntax-entry ?\" "\"" table)  ; String delimiter
    (modify-syntax-entry ?\; "<"  table)  ; Comment start
    (modify-syntax-entry ?\n ">"  table)  ; Comment end
    (modify-syntax-entry ??  "$"  table)  ; paired delimiter
    table))


(defvar ebnf-mode-smie-ebnf-grammar
  (smie-prec2->grammar
   (smie-bnf->prec2
    '((id)
      (lhs (id))
      (alts (rhs "||" rhs)
            (rhs))
      (rhs ("{" alts "}")
           (rhs "|" rhs)
           (id))
      (rule (lhs "=" rhs))
      (rules (rules "." rules) (rule)))
    '((assoc "."))
    '((assoc "||") (assoc "|"))))
  "Simplfied BNF grammar for `smie' indentation of the `ebnf' style.")


(defun ebnf-mode-smie-ebnf-rules (kind token)
  "Perform indentation of KIND on TOKEN using the `smie' engine."
  (pcase (cons kind token)
    (`(:elem . basic)            ebnf-mode-indent-offset)
    (`(:elem . args)             ebnf-mode-indent-offset)
    (`(:list-intro . "=")        0)
    (`(:before . ,(or "." "|" "||"))  (smie-rule-separator kind))
    (`(:after . "=") ebnf-mode-indent-offset)))


;;;; Major mode definitions.


(defun ebnf-iso-ebnf-setup ()
  "Setup the `major-mode' for the `iso-ebnf' style."
  (setq-local comment-start "(* ")
  (setq-local comment-end   " *)")
  (setq-local comment-start-skip "\\((\\*\\)\\s-*")
  (setq-local comment-end-skip   "\\s-*\\(\\*)\\)")
  (set-syntax-table ebnf-mode-iso-ebnf-syntax-table)
  (smie-setup ebnf-mode-smie-iso-ebnf-grammar #'ebnf-mode-smie-iso-ebnf-rules))


(defun ebnf-ebnf-setup ()
  "Setup the `major-mode' for the `ebnf' style."
  (setq-local comment-start "; ")
  (setq-local comment-start-skip ";+\\s-*")
  (set-syntax-table ebnf-mode-ebnf-syntax-table)
  (smie-setup ebnf-mode-smie-ebnf-grammar #'ebnf-mode-smie-ebnf-rules))


(defun ebnf-mode-after-hack-local-variables ()
  "Set EBNF style specific settings after parsing file-local variables."
  (pcase (list ebnf-mode-style)
    (`(ebnf)      (ebnf-ebnf-setup))
    (`(iso-ebnf)  (ebnf-iso-ebnf-setup))
    (`(,_)        (ebnf-iso-ebnf-setup))))


(define-derived-mode ebnf-mode prog-mode "EBNF"
  :group 'ebnf-mode
  (print ebnf-mode-style)
  (setq-local indent-tabs-mode nil)
  (setq-local font-lock-defaults ebnf-mode-font-lock-keywords)
  (font-lock-flush)
  (add-hook 'hack-local-variables-hook
            #'ebnf-mode-after-hack-local-variables nil t))


;;;###autoload
(add-to-list 'auto-mode-alist '("\\.ebnf\\'" . ebnf-mode))

(provide 'ebnf-mode)

;;; ebnf-mode.el ends here
