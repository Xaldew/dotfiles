;;; terminals.el --- Terminal initialization -*- lexical-binding: t -*-
;;
;;; Commentary:
;; This file contain custom terminal initialization functions.  Typically,
;; mirroring the functionality of `xterm'.
;;
;;; Code:

(defun terminal-init-vte ()
  "Terminal initialization function for `vte' based terminals."
  (tty-run-terminal-initialization (selected-frame) "xterm"))

(defun terminal-init-gnome ()
  "Terminal initialization function for `gnome-terminal'."
  (tty-run-terminal-initialization (selected-frame) "xterm"))

(defun terminal-init-konsole ()
  "Terminal initialization function for `konsole'."
  (tty-run-terminal-initialization (selected-frame) "xterm"))

(defun terminal-init-tmux ()
  "Terminal initialization function for `tmux'."
  (let ((xterm-extra-capabilities '(modifyOtherKeys)))
    (tty-run-terminal-initialization (selected-frame) "xterm")))


(provide 'terminals)

;;; terminals.el ends here
