;;; smie-sample.el --- Major mode for editing .smie-sample files. -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; Example for writing SMIE-based major-modes.
;;
;;; Code:

(require 'smie)


;; Allow users to run their own hooks.
(defcustom smie-sample-indent-offset 4
  "Indentation width."
  :group 'smie-sample-mode
  :type 'integer)


(defvar smie-sample-grammar
  (smie-prec2->grammar
   (smie-bnf->prec2
    `((insts (insts ";" insts) (inst))
      (inst ("AttributeBegin" inst "AttributeEnd")))
    '((assoc ";"))))
  "Sample BNF grammar for `smie'.")


(defun smie-sample-rules (kind token)
  "Perform indentation of KIND on TOKEN using the `smie' engine."
  (pcase (list kind token)
    (`(:elem basic) smie-sample-indent-offset)
    (`(:elem args) 0)
    (`(:after "AttributeEnd") (smie-rule-parent))))


(defun smie-sample-forward-token ()
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

(defun smie-sample-backward-token ()
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


(defvar smie-sample-syntax-table
  (let ((table (make-syntax-table)))
    ;; Double quotes used for comments.
    (modify-syntax-entry ?' "\"" table)
    (modify-syntax-entry ?\" "\"" table)
    ;; Shell style comment: “# ...”
    (modify-syntax-entry ?#  "<" table)
    (modify-syntax-entry ?\n ">" table)
    table)
  "Syntax table.")


(defconst smie-sample-keywords
  '("AttributeBegin" "AttributeEnd"))

(defvar smie-sample-font-lock-keywords
  `((,(regexp-opt smie-sample-keywords 'words) . font-lock-keyword-face)))


;;;###autoload
(progn
  (add-to-list 'auto-mode-alist '("\\.smie-sample\\'" . smie-sample-mode)))


;;;###autoload
(define-derived-mode smie-sample-mode prog-mode "SMIE"
  "Major mode for trying out SMIE samples.

\\{smie-sample-mode-map}"
  :syntax-table smie-sample-syntax-table
  ;; Code for font-locking.
  (setq-local font-lock-defaults '(smie-sample-font-lock-keywords))
  ;; Code for managing comments.
  (setq-local comment-start "# ")
  (setq-local comment-start-skip "#+\\s-*")
  ;; Code for indentation.
  (setq-local indent-tabs-mode nil)
  (smie-setup smie-sample-grammar #'smie-sample-rules
              :forward-token #'smie-sample-forward-token
              :backward-token #'smie-sample-backward-token))


(provide 'smie-sample)

;;; smie-sample.el ends here
