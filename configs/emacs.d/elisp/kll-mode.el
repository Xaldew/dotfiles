;;; kll-mode.el --- Major mode for editing kll files -*- lexical-binding: t -*-
;;
;;; Commentary:
;; Major mode for editing files using the Keyboard Layout Language (KLL).
;;
;;; Code:


(defgroup kll nil
  "Keyboard Layout Language support for Emacs."
  :group 'languages
  :version "25.1")


(defcustom kll-mode-indent-offset 4
  "KLL indentation width - currently unused."
  :group 'kll
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


(defvar kll-mode-variables
  '("STLcdNumber0"
    "STLcdNumber1"
    "STLcdNumber2"
    "STLcdNumber3"
    "STLcdNumber4"
    "STLcdNumber5"
    "STLcdNumber6"
    "STLcdNumber7"
    "STLcdNumber8"
    "STLcdNumber9"

    "STLcdNumber0Color"
    "STLcdNumber1Color"
    "STLcdNumber2Color"
    "STLcdNumber3Color"
    "STLcdNumber4Color"
    "STLcdNumber5Color"
    "STLcdNumber6Color"
    "STLcdNumber7Color"
    "STLcdNumber8Color"
    "STLcdNumber9Color")
  "Various KLL variables.  May be hardware dependent.")


(defvar kll-mode-required-variables
  '("Name"
    "Version"
    "Date"
    "Author"
    "KLL")
  "Variables required by KLL.")


(defvar kll-mode-font-lock-defaults
  `((("\"\\.\\*\\?" . font-lock-string-face)  ;; String are between quotes.
     (,(regexp-opt kll-mode-required-variables  'words) . font-lock-builtin-face)
     (,(regexp-opt kll-mode-constants 'words)           . font-lock-constant-face))))


(defconst kll-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?' "\""  table) ; String delimiter
    (modify-syntax-entry ?\" "\"" table) ; String delimiter
    (modify-syntax-entry ?#  "<" table)  ; Comment start
    (modify-syntax-entry ?\n ">" table)  ; Comment end
    table))


(define-derived-mode kll-mode prog-mode "Keyboard Layout Language mode"
  :syntax-table kll-mode-syntax-table
  (setq-local comment-start "# ")
  (setq-local comment-start-skip "#+\\s-*")
  (font-lock-flush))


(provide 'kll-mode)

;;; kll-mode.el ends here
