;;; kll-mode.el --- Major mode for editing kll files -*- lexical-binding: t -*-
;;
;;; Commentary:
;; Major mode for editing files using the Keyboard Layout Language (KLL).
;;
;;; Code:


(defgroup kll-mode nil
  "Keyboard Layout Language support for Emacs."
  :group 'languages
  :version "25.1")


(defcustom kll-mode-indent-offset 4
  "KLL indentation width - currently unused."
  :group 'kll-mode
  :type 'integer
  :safe #'integerp)


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


(defvar kll-mode-prefixes
  '("S" "U" "I" "CON" "SYS" "P" "A")
  "KLL control code types.")


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


(defvar kll-mode-variable-regexp
  (rx (and (group-n 1 (+ (in "A-Z" "a-z" "0-9" "_")))
           (* space) "=" (* space)
           (group-n 2 (+? not-newline))     ; The variable can span lines.
           (or eol ";")))
  "Regular expression to match KLL variables.")


(defvar kll-mode-capability-regexp
  (rx (and (* space)
           (group-n 1 (+ (in "A-Z" "a-z" "0-9" "_")))
           (* space) "=>" (* space)
           (group-n 2 (+ (in "A-Z" "a-z" "0-9" "_")))
           (* space) "(" (group-n 3 (+ anything)) ")" (* space)
           ";"))
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
  (concat "[[:space:]]*"
          "\\(A\\)\\["                  ; Animation identifier.
          "\\([a-zA-Z_]+\\)"            ; Animation label.
          "\\(?:[[:space:]]*,[[:space:]]*[0-9]+[[:space:]]*\\)?" ; Frame number.
          "\\]"
          "[[:space:]]*"
          "\\(?:<=\\)"
          "[[:space:]]*"
          "\\(.+\\);")
  "Regular expression to match all KLL variables.")


(defvar kll-mode-required-variables
  '("Name"
    "Version"
    "Date"
    "Author"
    "KLL")
  "Variables required by KLL.")


(defvar kll-mode-font-lock-keywords
  `(((,(regexp-opt kll-mode-constants 'words)          . font-lock-constant-face)
     (,(regexp-opt kll-mode-capabilities 'words)       . font-lock-keyword-face)
     (,(regexp-opt kll-mode-prefixes 'words)           . font-lock-type-face)
     (,(regexp-opt kll-mode-delimiters)                . font-lock-negation-char-face)
     (,(regexp-opt kll-mode-required-variables 'words) . font-lock-preprocessor-face)
     (,kll-mode-capability-regexp
      (1 font-lock-variable-name-face)
      (2 font-lock-function-name-face))
     (,kll-mode-define-regexp
      (1 font-lock-variable-name-face)
      (2 font-lock-preprocessor-face))
     (,kll-mode-variable-regexp
      (1 font-lock-variable-name-face)))))


(defconst kll-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?' "\""  table) ; String delimiter
    (modify-syntax-entry ?\" "\"" table) ; String delimiter
    (modify-syntax-entry ?#  "<" table)  ; Comment start
    (modify-syntax-entry ?\n ">" table)  ; Comment end
    table))


(define-derived-mode kll-mode prog-mode "KLL"
  :group 'kll-mode
  :syntax-table kll-mode-syntax-table
  (setq-local comment-start "# ")
  (setq-local comment-start-skip "#+\\s-*")
  (setq-local indent-tabs-mode nil)
  (setq-local font-lock-defaults kll-mode-font-lock-keywords)
  (font-lock-flush))


;;;###autoload
(add-to-list 'auto-mode-alist '("\\.kll\\'" . kll-mode))

(provide 'kll-mode)

;;; kll-mode.el ends here
