;;; xcb-rectsel.el --- Select a region of the screen with xcb_rectsel. -*- lexical-binding: t -*-
;;
;;; Commentary:
;; Use the xcb_rectsel tool to generally select a region of the screen.
;;
;;; Code:

(defun xcb-rectsel ()
  "Select a region of the screen and return the coordinates and region size."
  (interactive)
  (with-temp-buffer
    (call-process "xrectsel" nil t nil "%x %y %w %h")
    (mapcar 'string-to-number
            (split-string (buffer-string)))))


(defun xcb-rectsel-display ()
  "Retrieve the active display where applicable."
  (getenv "DISPLAY"))


(provide 'xcb-rectsel)

;;; xcb-rectsel.el ends here
