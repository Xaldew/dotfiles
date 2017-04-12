;;; terminals.el --- Terminal initialization -*- lexical-binding: t -*-
;;
;;; Commentary:
;; This file contain custom terminal initialization functions.  Typically,
;; mirroring the functionality of `xterm'.
;;
;;; Code:

(defun terminal-init-xfce4 ()
  "Terminal initialization function for `xfce4-terminal'."
  (tty-run-terminal-initialization (selected-frame) "xterm"))

(defun terminal-init-gnome ()
  "Terminal initialization function for `gnome-terminal'."
  (tty-run-terminal-initialization (selected-frame) "xterm"))

(defun terminal-init-konsole ()
  "Terminal initialization function for `konsole'."
  (tty-run-terminal-initialization (selected-frame) "xterm"))

(provide 'terminals)

;;; terminals.el ends here
