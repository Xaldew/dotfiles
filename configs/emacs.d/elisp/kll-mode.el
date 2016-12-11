;;; kll-mode.el --- Major mode for editing kll files -*- lexical-binding: t -*-
;;
;;; Commentary:
;; Major mode for editing files using the Keyboard Layout Language (KLL).
;;
;;; Code:


(require 'smie)


(defgroup kll-mode nil
  "Keyboard Layout Language support for Emacs."
  :group 'languages
  :version "25.1")


(defvar kll-mode-constants
  '("None")
  "KLL constants.")


(defvar kll-mode-capabilities
  '("usbKeyOut"
    "sysCtrlOut"
    "consCtrlOut"
    "mouseOut"
    "noneOut"
    "kbdProtocolBoot"
    "kbdProtocolNKRO"
    "layerShift"
    "layerLatch"
    "layerLock"
    "layerState"
    "layerRotate"
    "LCDLayerDisplay"
    "ledControl"
    "blockKey"
    "blockHold"
    "flashMode")
  "KLL common capabilities.")


(defvar kll-mode-delimiters
  '(":"
    "::"
    ":+"
    ":-"
    "i:"
    "i::"
    "i:+"
    "i:-")
  "KLL delimiter variants.")


(defvar kll-mode-animation-modifiers
  '("loop"
    "div"
    "start"
    "stop"
    "interp"
    "frame")
  "Valid KLL animation modifiers.")


(defvar kll-mode-key-regexp
  (rx (and word-start
           (group-n 1 (or "S" "U" "I" "CON" "SYS" "P" "A"))
           (in digit "\"" ?' "["))))


(defvar kll-mode-variable-regexp
  (rx (and (group-n 1 (+ (in "A-Z" "a-z" "0-9" "_")))
           (? "[" (* (in digit hex-digit)) "]")
           (* space) "=" (* space)
           (group-n 2 (+? not-newline))     ; The variable can span lines.
           (or eol ";")))
  "Regular expression to match KLL variables.")


(defvar kll-mode-capability-regexp
  (rx (and (* space)
           (group-n 1 (+ (in "A-Z" "a-z" "0-9" "_")))
           (* space) "=>" (* space)
           (group-n 2 (+ (in "A-Z" "a-z" "0-9" "_")))
           (* space) "(" (group-n 3 (* not-newline))
           (or eol
               (and ")" (* space) ";"))))
  "Regular expression to match KLL capabilities.")


(defvar kll-mode-define-regexp
  (rx (and (* space)
           (group-n 1 (+ (in "A-Z" "a-z" "0-9" "_")))
           (* space) "=>" (* space)
           (group-n 2 (+ (in "A-Z" "a-z" "0-9" "_")))
           (* space)
           (or eol ";")))
  "Regular expression to match KLL defines.")


(defvar kll-mode-animation-regexp
  (rx (and (* space) "A" (* space)
           "["
           (group-n 1 (+? (in "A-Z" "a-z" "0-9" "_"))) ; Animation label.
           (? (and (* space) "," (* space)))
           (group-n 2 (* (in digit)))                  ; Frame number.
           "]"))
  "Regular expression to match KLL animation declarations.")


(defvar kll-mode-required-variables
  '("Name"
    "Version"
    "Date"
    "Author"
    "KLL")
  "Variables required by KLL.")


(defvar kll-mode-font-lock-keywords
  `(((,(regexp-opt kll-mode-constants 'words)           . font-lock-constant-face)
     (,(regexp-opt kll-mode-capabilities 'words)        . font-lock-keyword-face)
     (,(regexp-opt kll-mode-required-variables 'words)  . font-lock-preprocessor-face)
     (,(regexp-opt kll-mode-animation-modifiers 'words) . font-lock-preprocessor-face)
     (,kll-mode-animation-regexp
      (1 font-lock-variable-name-face))
     (,kll-mode-capability-regexp
      (1 font-lock-variable-name-face)
      (2 font-lock-function-name-face))
     (,kll-mode-define-regexp
      (1 font-lock-variable-name-face)
      (2 font-lock-preprocessor-face))
     (,kll-mode-variable-regexp
      (1 font-lock-variable-name-face))
     (,kll-mode-key-regexp
      (1 font-lock-type-face))
     (,(regexp-opt kll-mode-delimiters) . font-lock-negation-char-face)))
  "List over `font-lock' specifiers.")


;;;; SMIE indentation setup.


(defcustom kll-mode-indent-offset 4
  "KLL indentation width."
  :group 'kll-mode
  :type 'integer
  :safe #'integerp)


(defconst kll-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?'  "\""  table) ; String delimiter
    (modify-syntax-entry ?\" "\"" table)  ; String delimiter
    (modify-syntax-entry ?#  "<" table)   ; Comment start
    (modify-syntax-entry ?\n ">" table)   ; Comment end
    table))


(defvar kll-mode-smie-grammar
  (smie-prec2->grammar
   (smie-bnf->prec2
    '((id)                      ; Variable, identifier, integers, strings, etc.
      (stmt (define-stmt)
            (prop-stmt)
            (capa-stmt)
            (var-stmt)
            (keymap-stmt))
      (stmts (stmts ";" stmts) (stmt))
      ;; Property assignments.
      (prop (id ":" id))
      (props (props "," props) (prop))
      (prop-stmt (id "<=" props))
      ;; Define statements.
      (define-stmt (id "=>" id))
      ;; Capability statements.
      (capa-stmt (id "=>" id "(" props ")"))
      ;; Variable statements.
      (var-stmt (id "=" id))
      ;; Key mapping statements.
      (key (id))
      (keys (keys "+" keys) (key))
      (keymap-stmt (keys ":" keys)
                   (keys "::" keys)
                   (keys ":+" keys)
                   (keys ":-" keys)
                   (keys "i:" keys)
                   (keys "i::" keys)
                   (keys "i:+" keys)
                   (keys "i:-" keys)))
    '((assoc "+"))
    '((assoc ";"))
    '((assoc ","))))
  "BNF Grammar describing the KLL language for `smie'.")


(defun kll-mode-smie-rules (kind token)
  "Perform indentation of KIND on TOKEN using the `smie' engine."
  (pcase (cons kind token)
    (`(:elem . basic)            kll-mode-indent-offset)
    (`(:elem . args)             0)
    (`(:list-intro . "=")        0)     ; Aligns to first list element.
    (`(:before . ,(or ";" ","))  (smie-rule-separator kind))
    (`(:after . ,(or "<=" "=>")) kll-mode-indent-offset)))


(defvar kll-keywords-regexp
  (regexp-opt '("+" "," ";"
                "<=" "=>" "="
                ":" "::" ":+" ":-"
                "i:" "i::" "i:+" "i:-"))
  "Keywords the `smie' lexer should look for.")


(defun kll-smie-forward-token ()
  "Search forwards for a token to be used by `smie'."
  (forward-comment (point-max))
  (cond
   ((looking-at kll-keywords-regexp)
    (goto-char (match-end 0))
    (match-string-no-properties 0))
   (t (buffer-substring-no-properties
       (point)
       (progn (skip-syntax-forward "w_")
              (point))))))


(defun kll-smie-backward-token ()
  "Search backwards for a token to be used by `smie'."
  (forward-comment (- (point)))
  (cond
   ((looking-back kll-keywords-regexp (- (point) 2) t)
    (goto-char (match-beginning 0))
    (match-string-no-properties 0))
   (t (buffer-substring-no-properties
       (point)
       (progn (skip-syntax-backward "w_")
              (point))))))


;;;; Major mode definition.

(define-derived-mode kll-mode prog-mode "KLL"
  :group 'kll-mode
  :syntax-table kll-mode-syntax-table
  (setq-local comment-start "# ")
  (setq-local comment-start-skip "#+\\s-*")
  (setq-local indent-tabs-mode nil)
  (smie-setup kll-mode-smie-grammar #'kll-mode-smie-rules
              :backward-token #'kll-smie-backward-token
              :forward-token #'kll-smie-forward-token)
  (setq-local font-lock-defaults kll-mode-font-lock-keywords)
  (font-lock-flush))


;;;###autoload
(add-to-list 'auto-mode-alist '("\\.kll\\'" . kll-mode))

(provide 'kll-mode)

;;; kll-mode.el ends here
