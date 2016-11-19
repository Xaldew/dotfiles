;;; android.el --- Android specific configuration. -*- lexical-binding: t -*-

;;; Commentary:
;; Settings specific to running Emacs inside Android and Termux.

;;; Code:

(defun android-termux-p ()
  "Return t if the system is running on android and termux."
  (string= system-configuration "arm-unknown-linux-androidabi"))

(defun android-browse-url (url &rest _)
  "Browse a URL with the native Android browser."
  (start-process-shell-command
   "open-url"
   nil
   (concat "am start --user 0 -a android.intent.action.VIEW -d " url)))

(when (android-termux-p)
  (advice-add 'browse-url-default-browser :override #'android-browse-url))

;;; android.el ends here
