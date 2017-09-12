;;; hasklig-ligatures.el --- Hasklig font ligatures -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; Code for appending the correct Hasklig code-points from the Unicode Private
;; Use Area (PUA) to prettify-symbols.
;;
;; A complete list of the available ligatures can be found here:
;;
;;   - https://github.com/i-tu/Hasklig/blob/master/GlyphOrderAndAliasDB#L1588
;;
;;; Code:


(defconst hasklig-ligatures
  '("&&" "***" "*>" "\\\\" "||" "|>" "::"
    "==" "===" "==>" "=>" "=<<" "!!" ">>"
    ">>=" ">>>" ">>-" ">-" "->" "-<" "-<<"
    "<*" "<*>" "<|" "<|>" "<$>" "<>" "<-"
    "<<" "<<<" "<+>" ".." "..." "++" "+++"
    "/=" ":::" ">=>" "->>" "<=>" "<=<" "<->")
  "Strings that correspond to a ligature in the Hasklig font.")


(defconst hasklig-unicode-start #Xe100
  "Start of the Unicode PUA used by the Hasklig font.")


(defun hasklig-replacements (ligatures uc-beg)
  "Create a list of replacement strings from LIGATURES.

The Unicode code-points start from UC-BEG."
  (let ((code-points (number-sequence uc-beg (+ uc-beg (length ligatures) -1))))
    (cl-loop for (str . cp) in (cl-mapcar #'cons ligatures code-points)
             collect (cons str (string ?\t cp)))))


(defun hasklig-setup-prettify-symbol ()
  "Add Hasklig ligatures for use with `mode/prettify-symbols-mode'."
  (let ((replacements (hasklig-replacements hasklig-ligatures
                                            hasklig-unicode-start)))
    (setq prettify-symbols-alist (append replacements prettify-symbols-alist))))


(provide 'hasklig-ligatures)

;;; hasklig-ligatures.el ends here
