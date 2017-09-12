;;; ligature-mode.el --- Enable ligature fonts -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'prog-mode)


(defgroup ligatures nil
  "Ligature support for GUI Emacs."
  :group 'fonts
  :version "25.1")


(defcustom ligature-mode-alist
  '(("www"             #Xe100)
    ("**"              #Xe101)
    ("***"             #Xe102)
    ("**/"             #Xe103)
    ("*>"              #Xe104)
    ("*/"              #Xe105)
    ("\\\\\\\\"        #Xe106)
    ("\\\\\\\\\\\\"    #Xe107)
    ("{-"              #Xe108)
    ("\\[\\]"          #Xe109)
    ("::"              #Xe10a)
    (":::"             #Xe10b)
    ("[^=]:="          #Xe10c)
    ("!!"              #Xe10d)
    ("!="              #Xe10e)
    ("!=="             #Xe10f)
    ("-}"              #Xe110)
    ("--"              #Xe111)
    ("---"             #Xe112)
    ("-->"             #Xe113)
    ("->"              #Xe114)
    ("->>"             #Xe115)
    ("-<"              #Xe116)
    ("-<<"             #Xe117)
    ("-~"              #Xe118)
    ("#{"              #Xe119)
    ("#["              #Xe11a)
    ("##"              #Xe11b)
    ("###"             #Xe11c)
    ("####"            #Xe11d)
    ("#("              #Xe11e)
    ("#?"              #Xe11f)
    ("#_"              #Xe120)
    ("#_("             #Xe121)
    (".-"              #Xe122)
    (".="              #Xe123)
    (".."              #Xe124)
    ("..<"             #Xe125)
    ("..."             #Xe126)
    ("?="              #Xe127)
    ("??"              #Xe128)
    (";;"              #Xe129)
    ("/*"              #Xe12a)
    ("/**"             #Xe12b)
    ("/="              #Xe12c)
    ("/=="             #Xe12d)
    ("/>"              #Xe12e)
    ("//"              #Xe12f)
    ("///"             #Xe130)
    ("&&"              #Xe131)
    ("||"              #Xe132)
    ("||="             #Xe133)
    ("|="              #Xe134)
    ("|>"              #Xe135)
    ("^="              #Xe136)
    ("$>"              #Xe137)
    ("++"              #Xe138)
    ("+++"             #Xe139)
    ("+>"              #Xe13a)
    ("=:="             #Xe13b)
    ("=="              #Xe13c)
    ("==="             #Xe13d)
    ("==>"             #Xe13e)
    ("=>"              #Xe13f)
    ("=>>"             #Xe140)
    ("<="              #Xe141)
    ("=<<"             #Xe142)
    ("=/="             #Xe143)
    (">-"              #Xe144)
    (">="              #Xe145)
    (">=>"             #Xe146)
    (">>"              #Xe147)
    (">>-"             #Xe148)
    (">>="             #Xe149)
    (">>>"             #Xe14a)
    ("<*"              #Xe14b)
    ("<*>"             #Xe14c)
    ("<|"              #Xe14d)
    ("<|>"             #Xe14e)
    ("<$"              #Xe14f)
    ("<$>"             #Xe150)
    ("<!--"            #Xe151)
    ("<-"              #Xe152)
    ("<--"             #Xe153)
    ("<->"             #Xe154)
    ("<+"              #Xe155)
    ("<+>"             #Xe156)
    ("<="              #Xe157)
    ("<=="             #Xe158)
    ("<=>"             #Xe159)
    ("<=<"             #Xe15a)
    ("<>"              #Xe15b)
    ("<<"              #Xe15c)
    ("<<-"             #Xe15d)
    ("<<="             #Xe15e)
    ("<<<"             #Xe15f)
    ("<~"              #Xe160)
    ("<~~"             #Xe161)
    ("</"              #Xe162)
    ("</>"             #Xe163)
    ("~@"              #Xe164)
    ("~-"              #Xe165)
    ("~="              #Xe166)
    ("~>"              #Xe167)
    ("~~"              #Xe168)
    ("~~>"             #Xe169)
    ("%%"              #Xe16a)
    ;; This ended up being hard to do properly so I'm leaving it out.
    ;; "x"             #Xe16b)
    (":"               #Xe16c)
    ("+"               #Xe16d)
    ("*"               #Xe16f))
  "Strings to format as ligatures."
  :group 'ligatures
  :type 'list)


(defun ligature-mode--frame-hook ()
  "Ensure that new frames receive the font settings."
  (set-fontset-font t '(#Xe100 . #Xe16f) "Fira Code Symbol"))


(defun ligature-mode--enable ()
  "Function called when enabling `ligature-mode'."
  (set-fontset-font t '(#Xe100 . #Xe16f) "Fira Code Symbol")
  (cl-loop for (str cp) in ligature-mode-alist do
           (push (cons str (string ?\t cp)) prettify-symbols-alist))
  (add-hook 'after-make-frame-functions #'ligature-mode--frame-hook))


(defun ligature-mode--disable ()
  "Function called when disabling `ligature-mode'."
  (cl-loop for (str cp) in ligature-mode-alist do
           (setq prettify-symbols-alist
                 (remove (cons str (string ?\t cp)) prettify-symbols-alist)))
  (remove-hook 'after-make-frame-functions #'ligature-mode--frame-hook))


(define-minor-mode ligature-mode
  "Minor mode to enable font ligatures in GUI Emacs."
  :group 'ligatures
  :lighter ""
  :global t
  (if ligature-mode
      (ligature-mode--enable)
    (ligature-mode--disable)))


(provide 'ligature-mode)

;;; ligature-mode.el ends here
