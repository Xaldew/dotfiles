;;; fira-code-ligatures.el --- Fira font ligatures -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; Code for appending the correct Fira Code Symbol code-points from the Unicode
;; Private Use Area (PUA) to `prettify-symbols-mode' and `font-lock-mode'.
;;
;;; Code:


(defconst fira-code-font-name
  "Fira Code"
  "The human readable name of the Fira Code font.")


(defconst fira-code-font-symbols-name
  "Fira Code Symbol"
  "The human readable name of the Fira Code Symbols font.

I.e., the font that only contain the ligature symbols.")


(defconst fira-code-font-ligatures
  '(("www"           .  #Xe100)
    ("**"            .  #Xe101)
    ("***"           .  #Xe102)
    ("**/"           .  #Xe103)
    ("*>"            .  #Xe104)
    ("*/"            .  #Xe105)
    ("\\\\\\\\"      .  #Xe106)
    ("\\\\\\\\\\\\"  .  #Xe107)
    ("{-"            .  #Xe108)
    ("[]"            .  #Xe109)
    ("::"            .  #Xe10a)
    (":::"           .  #Xe10b)
    ("[^=]:="        .  #Xe10c)
    ("!!"            .  #Xe10d)
    ("!="            .  #Xe10e)
    ("!=="           .  #Xe10f)
    ("-}"            .  #Xe110)
    ("--"            .  #Xe111)
    ("---"           .  #Xe112)
    ("-->"           .  #Xe113)
    ("->"            .  #Xe114)
    ("->>"           .  #Xe115)
    ("-<"            .  #Xe116)
    ("-<<"           .  #Xe117)
    ("-~"            .  #Xe118)
    ("#{"            .  #Xe119)
    ("#["            .  #Xe11a)
    ("##"            .  #Xe11b)
    ("###"           .  #Xe11c)
    ("####"          .  #Xe11d)
    ("#("            .  #Xe11e)
    ("#?"            .  #Xe11f)
    ("#_"            .  #Xe120)
    ("#_("           .  #Xe121)
    (".-"            .  #Xe122)
    (".="            .  #Xe123)
    (".."            .  #Xe124)
    ("..<"           .  #Xe125)
    ("..."           .  #Xe126)
    ("?="            .  #Xe127)
    ("??"            .  #Xe128)
    (";;"            .  #Xe129)
    ("/*"            .  #Xe12a)
    ("/**"           .  #Xe12b)
    ("/="            .  #Xe12c)
    ("/=="           .  #Xe12d)
    ("/>"            .  #Xe12e)
    ("//"            .  #Xe12f)
    ("///"           .  #Xe130)
    ("&&"            .  #Xe131)
    ("||"            .  #Xe132)
    ("||="           .  #Xe133)
    ("|="            .  #Xe134)
    ("|>"            .  #Xe135)
    ("^="            .  #Xe136)
    ("$>"            .  #Xe137)
    ("++"            .  #Xe138)
    ("+++"           .  #Xe139)
    ("+>"            .  #Xe13a)
    ("=:="           .  #Xe13b)
    ("=="            .  #Xe13c)
    ("==="           .  #Xe13d)
    ("==>"           .  #Xe13e)
    ("=>"            .  #Xe13f)
    ("=>>"           .  #Xe140)
    ("<="            .  #Xe141)
    ("=<<"           .  #Xe142)
    ("=/="           .  #Xe143)
    (">-"            .  #Xe144)
    (">="            .  #Xe145)
    (">=>"           .  #Xe146)
    (">>"            .  #Xe147)
    (">>-"           .  #Xe148)
    (">>="           .  #Xe149)
    (">>>"           .  #Xe14a)
    ("<*"            .  #Xe14b)
    ("<*>"           .  #Xe14c)
    ("<|"            .  #Xe14d)
    ("<|>"           .  #Xe14e)
    ("<$"            .  #Xe14f)
    ("<$>"           .  #Xe150)
    ("<!--"          .  #Xe151)
    ("<-"            .  #Xe152)
    ("<--"           .  #Xe153)
    ("<->"           .  #Xe154)
    ("<+"            .  #Xe155)
    ("<+>"           .  #Xe156)
    ("<="            .  #Xe157)
    ("<=="           .  #Xe158)
    ("<=>"           .  #Xe159)
    ("<=<"           .  #Xe15a)
    ("<>"            .  #Xe15b)
    ("<<"            .  #Xe15c)
    ("<<-"           .  #Xe15d)
    ("<<="           .  #Xe15e)
    ("<<<"           .  #Xe15f)
    ("<~"            .  #Xe160)
    ("<~~"           .  #Xe161)
    ("</"            .  #Xe162)
    ("</>"           .  #Xe163)
    ("~@"            .  #Xe164)
    ("~-"            .  #Xe165)
    ("~="            .  #Xe166)
    ("~>"            .  #Xe167)
    ("~~"            .  #Xe168)
    ("~~>"           .  #Xe169)
    ("%%"            .  #Xe16a)
    (":"             .  #Xe16c)
    ("+"             .  #Xe16d)
    ("*"             .  #Xe16f))
  "Mapping of strings to a Unicode point in the Fira Code Symbols font.")


(defconst fira-code-font-lock-keywords
  '(("[[:xdigit:]]\\(x\\)[[:xdigit:]]" . #Xe16b))
  "Mapping of Regular expressions to a Unicode point in the Fira Code font.

Some ligatures need to be aware of their environment which is not
possible with `mode/prettify-symbols-mode'.  These are therefore
passed on to the `font-lock-add-keywords' instead.")


(provide 'fira-code-ligatures)

;;; fira-code-ligatures.el ends here
