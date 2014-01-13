;; This file contains various useful functions.

;; This function indents and cleans up the whole buffer.
(defun iwb ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil))
