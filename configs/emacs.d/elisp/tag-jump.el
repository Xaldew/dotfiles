;;; tag-jump.el --- Front-end for jump engines. -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;
;;; Code:

(require 'hydra)
(require 'dumb-jump)
(require 'ag)
(require 'rg)
(require 'grep)
(require 'vc-git)
(require 'ggtags)


(defcustom tag-jump-active-hydra
  #'tag-jump-dumb-jump/body
  "The currently active tag-jumping hydra."
  :type 'choice
  :options '(tag-jump-menu/body
             tag-jump-xref/body
             tag-jump-dumb-jump/body
             tag-jump-grep+ag/body
             tag-jump-ggtags/body
             tag-jump-rg/body))


;;;###autoload
(defun tag-jump ()
  "Use the currently set hydra to jump or search."
  (interactive)
  (funcall tag-jump-active-hydra))


(defhydra tag-jump-menu (:color blue :columns 4)
  "Reference Jumping"
  ("x" tag-jump-xref/body      "Xref"       :exit t)
  ("d" tag-jump-dumb-jump/body "dumb-jump"  :exit t)
  ("g" tag-jump-ggtags/body    "GNU Global" :exit t)
  ("a" tag-jump-grep+ag/body   "Grep/Ag"    :exit t)
  ("r" tag-jump-rg/body        "Ripgrep"    :exit t)
  ("q" nil                     "Quit"       :exit t))


(defhydra tag-jump-xref (:color pink :columns 4)
  "Xref Jumping"
  ("d" xref-find-definitions              "Definitions")
  ("f" xref-find-definitions-other-frame  "Definitions other frame")
  ("w" xref-find-definitions-other-window "Definitions other window")
  ("r" xref-find-references               "References")
  ("a" xref-find-apropos                  "Apropos")
  ("b" tag-jump-menu/body                 "Go Back" :exit t)
  ("q" nil                                "Quit"    :exit t))


(defhydra tag-jump-dumb-jump (:color pink :columns 4)
  "Dumb-Jumping"
  ("g" dumb-jump-go              "Definitions")
  ("o" dumb-jump-go-other-window "Definitions other window")
  ("p" dumb-jump-back            "Pop stack")
  ("l" dumb-jump-quick-look      "Quick look")
  ("b" tag-jump-menu/body        "Go Back" :exit t)
  ("q" nil                       "Quit"    :exit t))


(defhydra tag-jump-grep+ag (:color pink :columns 4)
  "Grep/Ag Jumping"
  ("G" grep               "Grep")
  ("g" grep-find          "Grep-Find")
  ("v" vc-git-grep        "Git Grep")
  ("s" ag                 "Ag Search")
  ("p" ag-project         "Ag Project")
  ("f" ag-files           "Ag Files")
  ("b" tag-jump-menu/body "Go Back" :exit t)
  ("q" nil                "Quit"    :exit t))


(defhydra tag-jump-ggtags (:color pink :columns 4)
  "GNU Global Jumping"
  ("d" ggtags-find-definition "Definition")
  ("r" ggtags-find-reference  "Reference")
  ("R" ggtags-query-replace   "Replace")
  ("c" ggtags-create-tags     "Create")
  ("u" ggtags-update-tags     "Update")
  ("b" tag-jump-menu/body     "Go Back" :exit t)
  ("q" nil                    "Quit"    :exit t))


(defhydra tag-jump-rg (:color blue :columns 4)
  "Ripgrep Jumping"
  ("r" rg                   "Search File")
  ("p" rg-project           "Search Project")
  ("d" rg-dwim              "Search Symbol")
  ("b" tag-jump-menu/body   "Go Back" :exit t)
  ("q" nil                  "Quit"    :exit t))


;;; tag-jump.el ends here
