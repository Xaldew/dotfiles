;;; format-minor-mode.el --- Highlighting format strings. -*- lexical-binding: t -*-
;;
;;; Commentary:
;; Minor mode for displaying highlights for Python-like format strings.
;;
;;; Code:

(require 'font-lock)


(defface format-minor-format-face
  '((t . (:inherit font-lock-regexp-grouping-backslash
         :foreground "OrangeRed1")))
  "Font-lock face used to highlight printf format specifiers."
  :group 'font-lock-faces)


(defconst format-minor--format-rx
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
          (? (in "bcdeEfFgGnosxX%"))))
      "}")
  "Regular expression to match Python-style format specifiers.")


(defconst format-minor--printf-rx
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


(defun format-minor--format-matcher (end)
  "Search for `printf' format specifiers within strings up to END."
  (let ((pos)
        (case-fold-search nil))
    (while (and (setq pos (re-search-forward format-minor--format-rx end t))
                (null (nth 3 (syntax-ppss pos)))))
    pos))


(defun format-minor--printf-matcher (end)
  "Search for `printf' format specifiers within strings up to END."
  (let ((pos)
        (case-fold-search nil))
    (while (and (setq pos (re-search-forward format-minor--printf-rx end t))
                (null (nth 3 (syntax-ppss pos)))))
    pos))


(defvar format-minor-font-lock-keywords
  '((format-minor--format-matcher (0 'format-minor-format-face prepend))
    (format-minor--printf-matcher (0 'format-minor-printf-face prepend)))
  "Keywords given to `font-lock-add-keywords' and `font-lock-remove-keywords'.")


(defun format-minor--enable ()
  "Enable additional format specifier highlighting."
  (font-lock-add-keywords nil format-minor-font-lock-keywords))


(defun format-minor--disable ()
  "Disable the additional format specifier highlighting."
  (font-lock-remove-keywords nil format-minor-font-lock-keywords))


(define-minor-mode format-minor-mode
  "Provide extra highlighting of format specifiers inside strings."
  :group 'highlighting
  (if format-minor-mode
      (format-minor--enable)
    (format-minor--disable)))


(provide 'format-minor-mode)

;;; format-minor-mode.el ends here
