;;; eshell-marks.el --- eshell bookmarking -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; Various tools for bookmarking directories in `eshell'.
;;
;;; Code:

(require 'eshell)
(require 'em-dirs)


(defcustom eshell-bookmarks-dir
  (expand-file-name "eshell-bookmarks/" user-emacs-directory)
  "Directory where the bookmark directory should be located."
  :group 'eshell
  :type 'string)


(defun eshell/marks ()
  "List all bookmarks that are currently available."
  (make-directory eshell-bookmarks-dir t)
  (directory-files eshell-bookmarks-dir nil "^[^.]+$"))

(defun eshell/mark (mark)
  "MARK the current directory and create a symlink to jump to it."
  (make-directory eshell-bookmarks-dir t)
  (make-symbolic-link
   (eshell/pwd)
   (concat eshell-bookmarks-dir mark) 1))

(defun eshell/unmark (mark)
  "Remove the symlink specified by MARK, if any."
  (make-directory eshell-bookmarks-dir t)
  (delete-file (concat eshell-bookmarks-dir mark)))

(defun eshell/jump (mark)
  "Jump to a directory marked by a symlink in ~/.emacs.d/eshell-bookmarks/MARK."
  (eshell/cd
   (file-symlink-p (concat eshell-bookmarks-dir mark))))

(defun pcomplete/jump ()
  "Autocomplete for the `eshell/jump' command."
  (pcomplete-here* (directory-files eshell-bookmarks-dir nil "^[^.]+$")))

(defun pcomplete/unmark ()
  "Autocomplete for the `eshell/unmark' command."
  (pcomplete-here* (directory-files eshell-bookmarks-dir nil "^[^.]+$")))


(provide 'eshell-marks)

;;; eshell-marks.el ends here
