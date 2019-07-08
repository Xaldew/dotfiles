;;; anyclip-mode.el --- Use anyclip to copy & paste  -*- lexical-binding: t; -*-

;; Copyright (C) 2015 Free Software Foundation, Inc.

;; Author: Gustaf Waldemarson <gustaf.waldemarson@gmail.com>
;; Keywords: convenience, tools
;; Created: 2015-12-29
;; Version: 0.0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package allows Emacs to copy to and paste from the system clipboard(s)
;; when running in terminal mode.  It uses the external command-line tool
;; anyclip which interacts with various other tools depending on the underlying
;; platform.
;;
;; To use: (anyclip-mode)

;;; Code:


(defcustom anyclip-clip-program "anyclip"
  "Name of the anyclip program."
  :type 'string
  :group 'killing)

(defcustom anyclip-paste-program "anypaste"
  "Name of the anypaste program."
  :type 'string
  :group 'killing)

(defcustom anyclip-clipboard "clipboard"
  "String indicating the default system clipboard when multiple exists."
  :type 'string
  :group 'killing)


(defvar anyclip-last-output-text ""
  "The value of the last string output from Emacs through anyclip.")

(defvar anyclip-saved-cut-func nil
  "Saved value of `interprogram-cut-function'.")

(defvar anyclip-saved-paste-func nil
  "Saved value of `interprogram-paste-function'.")


(defun anyclip-set-value (data)
  "DATA is the string to be sent to anyclip."
  (let* ((process-connection-type nil)
         (proc (start-file-process "anyclip" nil "anyclip"
                                   "--clipboard" anyclip-clipboard)))
    (when proc
      (process-send-string proc data)
      (process-send-eof proc))
    (setq anyclip-last-output-text data)))


(defun anyclip-get-value ()
  "See `x-selection-value'."
  (let ((clip-text
         (with-output-to-string
           (process-file anyclip-paste-program nil
                         '(t nil) nil anyclip-clipboard))))
    (setq clip-text
          (cond
           ;; Ignore invalid and empty strings.
           ((or (not clip-text)
                (string= clip-text ""))
            (setq anyclip-last-output-text nil))
           ((eq clip-text anyclip-last-output-text)
            nil)
           ((string= clip-text anyclip-last-output-text)
            ;; Record the newer string so subsequent calls can use the
            ;; faster `eq' test.
            (setq anyclip-last-output-text clip-text)
            nil)
           (t
            (setq anyclip-last-output-text clip-text))))))


(defun anyclip-turn-on ()
  "Turn on the `anyclip-mode'.

This is done by binding `interprogram-cut-function' and
`interprogram-paste-function' to the corresponding `anyclip'
functions."
  (setq anyclip-saved-cut-func interprogram-cut-function)
  (setq anyclip-saved-paste-func interprogram-paste-function)
  (setq interprogram-cut-function 'anyclip-set-value)
  (setq interprogram-paste-function 'anyclip-get-value))


(defun anyclip-turn-off ()
  "Turn off the `anyclip-mode'.

This is done by restoring `interprogram-cut-function' and
`interprogram-paste-function' to their original values."
  (setq interprogram-cut-function anyclip-saved-cut-func)
  (setq interprogram-paste-function anyclip-saved-paste-func))


(defun anyclip-check-programs ()
  "Verify that `anyclip' and `anypaste' are available."
  (or (or (executable-find anyclip-clip-program)
          (signal 'file-error (list "Searching for program: "
                                    anyclip-clip-program
                                    "no such file.")))
      (or (executable-find anyclip-paste-program)
          (signal 'file-error (list "Searching for program: "
                                    anyclip-paste-program
                                    "no such file.")))))


;;;###autoload
(define-minor-mode anyclip-mode
  "Minor mode to use the `anyclip' and `anypaste' programs to copy & paste."
  :global t
  (if anyclip-mode
      (progn
        (anyclip-check-programs)
        ;; NOTE: See `tty-run-terminal-initialization' and term/README.
        (add-hook 'terminal-init-xterm-hook 'anyclip-turn-on))
    (anyclip-turn-off)
    (remove-hook 'terminal-init-xterm-hook 'anyclip-turn-on)))


(provide 'anyclip-mode)
;;; anyclip-mode.el ends here
