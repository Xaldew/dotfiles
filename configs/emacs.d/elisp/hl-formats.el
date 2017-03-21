;;; hl-formats.el --- Highlighting format strings. -*- lexical-binding: t -*-
;;
;;; Commentary:
;; Minor mode for displaying highlights for output formatting strings.
;;
;; Currently supports:
;;
;; - `printf', `scanf' %-style formts.
;; - Most `Rust' and `Python' style formats.
;; - Some Shell script variable expansions and expressions.
;;
;;; Code:

(require 'font-lock)


(defface hl-formats-format-face
  '((t . (:inherit font-lock-regexp-grouping-backslash
         :foreground "OrangeRed1")))
  "Font-lock face used to highlight printf format specifiers."
  :group 'font-lock-faces)


(defface hl-formats-printf-face
  '((t . (:inherit font-lock-regexp-grouping-backslash
         :foreground "OrangeRed1")))
  "Font-lock face used to highlight printf format specifiers."
  :group 'font-lock-faces)


(defconst hl-formats--format-rx
  (rx "{"
      (? (group                         ; Field-name.
          (? (group (+ word)))
          (* (or
              (and "." (+ word))
              (and "[" (+ word) "]"))))
         (? "!" (in "rsa")))
      (? ":")
      (? (group                         ; Format-specifier.
          (? (? nonl) (in "<^>="))
          (? (in "+" "-" " "))
          (? "#")
          (? "0")
          (? (or (+ digit)
                 (and (+ word) "$")))
          (? (in "_,"))
          (? "." (or (+ digit)
                     (and "*" (+ word) "$")))
          (? (in "bcdeEfFgGnosxX%?"))))
      "}")
  "Regular expression to match Python-style format specifiers.")


(defconst hl-formats--printf-rx
  (rx "%"
      (? (group (+ digit) "$"))
      (* (in "-" "+" "'" " " "#" "*"))
      (or (* digit)
          "*"
          (and "*" (+ digit) "$"))
      (? "." (or (* digit)
                 "*"
                 (and "*" (+ digit) "$")))
      (? (or (in "hlLjzt")
             "hh"
             "ll"))
      (or (in "aAbdiuoxXDOUfFeEgGcCsSpn")
          (and "[" (? "^") nonl (* (not (in "]"))) "]")))
  "Regular expression to match printf-style format specifiers.")


(defconst hl-formats--sh-var-rx
  (rx (group-n 1 "$")
      (or
       (group-n 2 (in "0-9$*@#!?_-"))
       (and (group-n 2 (+ (in "a-z" "A-Z" "_") (* (in "a-z" "A-Z" "0-9" "_")))))
       (and "{" (group-n 2 (+ (in "a-z" "A-Z" "0-9" "_"))) "}")
       (and "{!"
            (group-n 2 (+ (in "a-z" "A-Z" "_") (* (in "a-z" "A-Z" "0-9" "_"))))
            (? (in "*@"))
            "}")
       (and "{#" (group-n 2 (+ (in "a-z" "A-Z" "0-9" "_"))) "}")))
  "Regular expression to match shell variables.")


(defconst hl-formats--sh-array-rx
  (rx (group-n 1 "$")
      (or
       (and "{#"
            (group-n 2 (+ (in "a-z" "A-Z" "0-9" "_")))
            "[" (+? nonl) "]" "}")
       (and "{"
            (group-n 2 (+ (in "a-z" "A-Z" "0-9" "_")))
            "[" (+? (not (in "%#/-"))) "]"
            "}")))
  "Regular expression to match shell array expressions.")


(defconst hl-formats--sh-param-exp-rx
  (rx (group-n 1 "$")
      "{"
      (group-n 2 (+ (in "a-z" "A-Z" "0-9" "_"))) (? "[" (+? nonl) "]")
      (or ":" "-" "=" "+" "?" ":-" ":=" ":+" ":?" "#" "##" "%" "%%")
      (+? nonl)
      "}")
  "Regular expression to match parameter expansion commands.")


(defconst hl-formats--sh-string-sub-rx
  (rx (group-n 1 "$")
      "{"
      (group-n 2 (+ (in "a-z" "A-Z" "0-9" "_"))) (? "[" (+? nonl) "]")
      (or "/" "//" "/#" "/%")
      (*? (not (in "/")))
      "/"
      (*? (not (in "}")))
      "}")
  "Regular expression to match string substitution commands.")


(defmacro hl-formats--create-matcher (name regexp)
  "Create a inside-string font-lock matcher with NAME for REGEXP."
  (let ((fname (intern (format "hl-formats--%s-matcher" name))))
    `(defun ,fname (end)
       "Search for format specifiers within strings up to END."
       (let ((pos)
             (case-fold-search nil))
         (while (and (setq pos (re-search-forward ,regexp end t))
                     (null (nth 3 (syntax-ppss pos)))))
         pos))))


(hl-formats--create-matcher "printf" hl-formats--printf-rx)
(hl-formats--create-matcher "format" hl-formats--format-rx)
(hl-formats--create-matcher "sh-var"        hl-formats--sh-var-rx)
(hl-formats--create-matcher "sh-array"      hl-formats--sh-array-rx)
(hl-formats--create-matcher "sh-param-exp"  hl-formats--sh-param-exp-rx)
(hl-formats--create-matcher "sh-string-sub" hl-formats--sh-string-sub-rx)


(defvar hl-formats--python-matchers
  '((hl-formats--format-matcher (0 'hl-formats-format-face prepend))
    (hl-formats--printf-matcher (0 'hl-formats-printf-face prepend)))
  "Font-lock keyword matchers for `python-mode'.")


(defvar hl-formats--c/c++-matchers
  '((hl-formats--printf-matcher (0 'hl-formats-printf-face prepend)))
  "Font-lock keyword matchers for `c-mode' and `c++-mode'.")


(defvar hl-formats--rust-matchers
  '((hl-formats--format-matcher (0 'hl-formats-format-face prepend)))
  "Font-lock keyword matchers for `rust-mode'.")


(defvar hl-formats--shell-matchers
  '((hl-formats--sh-var-matcher
     (1 'hl-formats-format-face prepend)
     (2 'hl-formats-format-face prepend))
    (hl-formats--sh-array-matcher
     (1 'hl-formats-format-face prepend)
     (2 'hl-formats-format-face prepend))
    (hl-formats--sh-param-exp-matcher
     (1 'hl-formats-format-face prepend)
     (2 'hl-formats-format-face prepend))
    (hl-formats--sh-string-sub-matcher
     (1 'hl-formats-format-face prepend)
     (2 'hl-formats-format-face prepend)))
  "Font-lock keyword matchers for `sh-script-mode'.")


(defvar hl-formats--keywords-alist
  `((python-mode . ,hl-formats--python-matchers)
    (c-mode      . ,hl-formats--c/c++-matchers)
    (c++-mode    . ,hl-formats--c/c++-matchers)
    (rust-mode   . ,hl-formats--rust-matchers)
    (sh-mode     . ,hl-formats--shell-matchers))
  "Keywords given to `font-lock-add-keywords' and `font-lock-remove-keywords'.")


(defun hl-formats--enable ()
  "Enable additional format specifier highlighting."
  (cl-loop for (mode . matchers) in hl-formats--keywords-alist
           do (font-lock-add-keywords mode matchers)))


(defun hl-formats--disable ()
  "Disable the additional format specifier highlighting."
  (cl-loop for (mode . matchers) in hl-formats--keywords-alist
           do (font-lock-remove-keywords mode matchers)))


(define-minor-mode hl-formats-mode
  "Provide extra highlighting of format specifiers inside strings."
  :group 'highlighting
  :lighter ""
  :global t
  (if hl-formats-mode
      (hl-formats--enable)
    (hl-formats--disable)))


(provide 'hl-formats)

;;; hl-formats.el ends here
