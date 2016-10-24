;;; poly-python-mode.el -- Polymode for Python + ReST. -*- lexical-binding: t -*-
;;
;;; Commentary:
;; A mode used to combine ReST and Python editing.
;;
;;; Code:

(require 'polymode)

(defvar poly-python-ReST-head-regexp
  (concat "^[ \t]*\\(def\\|class\\).*:" ;; Match start of function/class.
          "\\(?:\n\\)\\{1,\\}"          ;; Match at least 1 newline.
          "[ \t]*"                      ;; Match optional whitespace.
          "u?\\(\"\"\"\\|\'\'\'\\)")    ;; Match start of docstring.
  "Regexp for matching the `head' of a ReST inner mode.")

(defvar poly-python-ReST-tail-regexp
  "\\(\"\"\"\\|\'\'\'\\)"
  "Regexp for matching the `tail' of a ReST inner mode.")


(defun poly-python-ReST-match-head (ahead)
  "Function to match the Python ReST header.

AHEAD is passed directly to the default matcher."
  (pm--default-matcher poly-python-ReST-head-regexp ahead))

(defun poly-python-ReST-match-tail (ahead)
  "Function to match the Python ReST tailer.

AHEAD is passed directly to the default matcher."
  (pm--default-matcher poly-python-ReST-tail-regexp ahead))


(defcustom poly-python-host/python
    (pm-bchunkmode "python"
                   :mode 'python-mode
                   :init-functions '(poly-python-fixes)
                   :font-lock-narrow nil)
    "Python host chunkmode."
    :group 'hostmodes
    :type 'object)

  (defcustom poly-python-inner/ReST
    (pm-hbtchunkmode "ReST"
                     :mode 'rst-mode
                     :head-reg poly-python-ReST-head-regexp
                     :tail-reg poly-python-ReST-tail-regexp
                     :head-mode 'host
                     :tail-mode 'host)
    "ReST inner chunk."
    :group 'innermodes
    :type 'object)
  (defcustom poly-python-poly/python+ReST
    (pm-polymode-one "python+ReST"
                     :hostmode 'poly-python-host/python
                     :innermode 'poly-python-inner/ReST)
    "Python and ReST polymode."
    :group 'polymodes
    :type 'object)


;;;###autoload (autoload #'poly-python-mode "poly-python-mode")
(define-polymode poly-python-mode poly-python-poly/python+ReST)


(defun poly-python-fixes ()
  "Fix various minor issues that can occur in the poly-python-mode."
  (remove-hook 'prog-mode-hook #'whitespace-mode))

(provide 'poly-python-mode)

;;; poly-python-mode.el ends here
