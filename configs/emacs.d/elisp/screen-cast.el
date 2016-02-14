;;; screen-cast.el -- Record screen casts in Emacs.
;;
;;; Commentary:
;; Record screen-cast using the Python utilities and enhance the output
;; with the executed Emacs actions.
;;
;;; Code:

(require 'command-log-mode)

(defvar screen-cast-clm-mode-global nil
  "Old value of `command-log-mode-global'.")

(defvar screen-cast-clm/logging-dir nil
  "Old value of `clm/logging-dir'.")

(defun screen-cast ()
  "Record a screen-cast with Emacs keys and actions recorded."
  (command-log-mode t)
  (let ((tmp-dir (make-temp-file "screen-cast" 'dir)))
    (start-process "screen-cast" "*screen-cast*"
                   "screen_cast.py"
                   "--record-keys"
                   "--save-temporaries" tmp-dir)))

;;; screen-cast.el ends here
