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


(defconst terminfo--terminal-rx-1
  (rx line-start
      (group (+ (in alnum "-+\\.")))
      (+ ?| (group (+ (in alnum space "()-_&\\."))))
      ",")
  "Regular expression to capture a `terminfo' terminal definition header.")

(defconst terminfo--terminal-rx-2
  (rx line-start (group (+ (in alnum "-+"))) ",")
  "Regular expression to capture a `terminfo' terminal definition header.")


(defconst terminfo-use-rx
  (rx (group "use") ?= (group (+ (not (in ",")))))
  "Regular expression to capture a `terminfo' 'use' capability.")


(defconst terminfo--capability-rx-1
  (rx (group (+ alnum)) (? ?@) ?\,)
  "The first regular expression to capture `terminfo' capabilities.")


(defconst terminfo--capability-rx-2
  (rx (group (+ alnum)) ?# (group (+ digit)) ?\,)
  "The second regular expression to capture `terminfo' capabilities.")


(defconst terminfo--capability-rx-3
  (rx (group (+ alnum)) ?= (group (+ (not (in ",")))))
  "The third regular expression to capture `terminfo' capabilities.")


(defconst terminfo-font-lock-keywords
  `(((,terminfo--terminal-rx-1
      (1 font-lock-function-name-face)
      (2 font-lock-doc-face))
     (,terminfo--terminal-rx-2
      (1 font-lock-function-name-face))
     (,terminfo-use-rx
      (1 font-lock-variable-name-face)
      (2 font-lock-preprocessor-face))
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
      (str-cap (id "=" id))
      (cap (id) (str-cap))
      (caps (caps "," caps) (cap))
      (term (caps))
      (terms (terms "TERMSEP" terms) (term)))
    '((assoc "="))
    '((assoc "TERMSEP"))
    '((assoc ","))))
  "Simplified BNF grammar describing the `terminfo' source files.")


(defun terminfo--new-terminal-p ()
  "Check if a new terminal is found by looking forward."
  (save-excursion
    (forward-comment (point-max))
    (or (looking-at-p terminfo--terminal-rx-1)
        (looking-at-p terminfo--terminal-rx-2))))


(defun terminfo-smie-forward ()
  "Search forward for a token to be used by `smie'."
  (let ((pos (point)))
    (forward-comment (point-max))
    (cond
     ((and (< pos (point))              ; Only emit separator if we have moved.
           (or (terminfo--new-terminal-p)
               (eobp)))
      "TERMSEP")
     ((looking-at "=\\|,")
      (goto-char (match-end 0))
      (match-string-no-properties 0))
     (t
      (buffer-substring-no-properties
       (point)
       (progn (skip-syntax-forward "w_-")
              (point)))))))


(defun terminfo-smie-backward ()
  "Search backward for a token to be used by `smie'."
  (let ((pos (point)))
    (forward-comment (- (point)))
    (cond
     ((and (> pos (point))
           (terminfo--new-terminal-p))
      "TERMSEP")
     ((looking-back "=\\|," (- (point) 2) t)
      (goto-char (match-beginning 0))
      (match-string-no-properties 0))
     (t
      (forward-comment (- (point)))
      (buffer-substring-no-properties
       (point)
       (progn (skip-syntax-backward "w_-")
              (point)))))))


(defun terminfo-smie-rules (kind token)
  "Perform indentation of KIND on TOKEN using the `smie' engine."
  (pcase (cons kind token)
    (`(:elem . basic)  terminfo-indent-offset)
    (`(:elem . args)   0)
    (`(:after  . ",") (if (smie-rule-next-p "TERMSEP")
                          (smie-rule-separator kind)
                        terminfo-indent-offset))))


(defconst terminfo-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\n  ">" table)
    (modify-syntax-entry ?\[  "w" table)
    (modify-syntax-entry ?\]  "w" table)
    (modify-syntax-entry ?=   "." table)
    (modify-syntax-entry ?\\  "w" table)
    (modify-syntax-entry ?@   "_" table)
    (modify-syntax-entry ?\;  "_" table)
    (modify-syntax-entry ?\"  "w" table)
    (modify-syntax-entry ?\(  "w" table)
    (modify-syntax-entry ?\)  "w" table)
    table)
  "Syntax table rules for `terminfo-mode'.")


(define-derived-mode terminfo-mode prog-mode "terminfo"
  "Major mode for `terminfo' source files."
  :group 'terminfo
  :syntax-table terminfo-syntax-table
  (setq-local comment-start "# ")
  (setq-local comment-start-skip "#+\\s-*")
  (setq-local syntax-propertize-function
              (syntax-propertize-rules ("^[ \t]*\\(#+\\)" (1 "<"))))
  (smie-setup terminfo-smie-grammar #'terminfo-smie-rules
              :forward-token #'terminfo-smie-forward
              :backward-token #'terminfo-smie-backward)
  (setq-local font-lock-defaults terminfo-font-lock-keywords))


;;;###autoload
(add-to-list 'auto-mode-alist '("terminfo.src\\'" . terminfo-mode))

(provide 'terminfo)

;;; terminfo.el ends here
