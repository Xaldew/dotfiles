;;; smie-new.el --- Major mode for editing .smie-new files. -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; Example for writing SMIE-based major-modes.
;;
;;; Code:

(require 'smie)


;; Allow users to run their own hooks.
(defcustom smie-new-indent-offset 4
  "Indentation width."
  :group 'smie-new-mode
  :type 'integer)


(defvar smie-new-grammar
  (smie-prec2->grammar
   (smie-bnf->prec2
    `((id)
      (inst ("test" id))
      (inst ("AttributeBegin" inst "AttributeEnd")))
    '((assoc ";"))))
  "Sample BNF grammar for `smie'.")


(defun smie-new-rules (kind token)
  "Perform indentation of KIND on TOKEN using the `smie' engine."
  (pcase (list kind token)
    (`(:elem basic) smie-new-indent-offset)
    (`(:elem args) 0)
    (`(:after "AttributeEnd") (smie-rule-parent))))


(defun smie-new-forward-token ()
  "Go forwards to the next SMIE token."
  (let ((start-pos (point)))
    (forward-comment (point-max))
    (cond
     ((and (> (point) start-pos)           ; Emit virtual statement separator.
           (looking-back "AttributeEnd[ \t\n]+" nil))
      ";")
     (t
      (buffer-substring-no-properties
       (point)
       (progn (if (zerop
                   (skip-syntax-forward "."))
                  (skip-syntax-forward "w_'"))
              (point)))))))

(defun smie-new-backward-token ()
  "Go backwards to the previous SMIE token."
  (let ((start-pos (point)))
    (forward-comment (- (point)))
    (cond
     ((and (< (point) start-pos)         ; Emit virtual statement separator.
           (looking-back "AttributeEnd" nil))
      ";")
     (t
      (buffer-substring-no-properties
       (point)
       (progn (if (zerop
                   (skip-syntax-backward "."))
                  (skip-syntax-backward "w_'"))
              (point)))))))


(defvar smie-new-syntax-table
  (let ((table (make-syntax-table)))
    ;; Double quotes used for comments.
    (modify-syntax-entry ?' "\"" table)
    (modify-syntax-entry ?\" "\"" table)
    ;; Shell style comment: “# ...”
    (modify-syntax-entry ?#  "<" table)
    (modify-syntax-entry ?\n ">" table)
    table)
  "Syntax table.")


(defconst smie-new-keywords
  '("AttributeBegin" "AttributeEnd"))

(defvar smie-new-font-lock-keywords
  `((,(regexp-opt smie-new-keywords 'words) . font-lock-keyword-face)))


;;;###autoload
(progn
  (add-to-list 'auto-mode-alist '("\\.smie-new\\'" . smie-new-mode)))


;;;###autoload
(define-derived-mode smie-new-mode prog-mode "SMIE"
  "Major mode for trying out SMIE samples.

\\{smie-new-mode-map}"
  :syntax-table smie-new-syntax-table
  ;; Code for font-locking.
  (setq-local font-lock-defaults '(smie-new-font-lock-keywords))
  ;; Code for managing comments.
  (setq-local comment-start "# ")
  (setq-local comment-start-skip "#+\\s-*")
  ;; Code for indentation.
  (setq-local indent-tabs-mode nil)
  (smie-setup smie-new-grammar #'smie-new-rules
              :forward-token #'smie-new-forward-token
              :backward-token #'smie-new-backward-token))


(provide 'smie-new)

;;; smie-new.el ends here
