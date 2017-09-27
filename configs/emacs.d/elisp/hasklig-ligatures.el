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


(defconst hasklig-font-name
  "hasklig"
  "The human readable name of the Hasklig font.")


(defconst hasklig-font-symbols-name
  "hasklig"
  "The human readable name of the Hasklig font.")


(defconst hasklig-font-ligatures
  '(("&&"   . #xe100)
    ("***"  . #xe101)
    ("*>"   . #xe102)
    ("\\\\" . #xe103)
    ("||"   . #xe104)
    ("|>"   . #xe105)
    ("::"   . #xe106)
    ("=="   . #xe107)
    ("==="  . #xe108)
    ("==>"  . #xe109)
    ("=>"   . #xe10a)
    ("=<<"  . #xe10b)
    ("!!"   . #xe10c)
    (">>"   . #xe10d)
    (">>="  . #xe10e)
    (">>>"  . #xe10f)
    (">>-"  . #xe110)
    (">-"   . #xe111)
    ("->"   . #xe112)
    ("-<"   . #xe113)
    ("-<<"  . #xe114)
    ("<*"   . #xe115)
    ("<*>"  . #xe116)
    ("<|"   . #xe117)
    ("<|>"  . #xe118)
    ("<$>"  . #xe119)
    ("<>"   . #xe11a)
    ("<-"   . #xe11b)
    ("<<"   . #xe11c)
    ("<<<"  . #xe11d)
    ("<+>"  . #xe11e)
    (".."   . #xe11f)
    ("..."  . #xe120)
    ("++"   . #xe121)
    ("+++"  . #xe122)
    ("/="   . #xe123)
    (":::"  . #xe124)
    (">=>"  . #xe125)
    ("->>"  . #xe126)
    ("<=>"  . #xe127)
    ("<=<"  . #xe128)
    ("<->"  . #xe129))
  "Mapping of strings to a Unicode point in the Hasklig font.")


(provide 'hasklig-ligatures)

;;; hasklig-ligatures.el ends here
