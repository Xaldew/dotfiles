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

(defvar screen-cast-clm-on nil
  "Was `command-log-mode' active when starting the screen-cast?.")

(defvar screen-cast-tmp-dir nil
  "Temporary directory were the current screen cast data is saved.")

(defvar screen-cast-output nil
  "The name of the file output screen-cast file.")

(defvar screen-cast-start-time nil
  "The start time of the last screen-cast.")


(defun screen-cast-sentinel (process event)
  "Process sentinel for the screen-cast.

PROCESS: The process that received EVENT."
  (cond
   ((equal event "finished\n")
    (unless screen-cast-clm-on
      (command-log-mode -1))
    (setq clm/logging-dir screen-cast-clm/logging-dir)
    (setq command-log-mode-is-global screen-cast-clm-mode-global)
    (when clm/command-log-buffer
      (with-current-buffer clm/command-log-buffer
        (print (buffer-substring (point-min) (point-max)))))
    (delete-directory screen-cast-tmp-dir t nil))
   (t
    (princ (format "Process: %s had the event `%s'" process event)))))


(defun screen-cast (output-file)
  "Record a screen-cast with Emacs keys and actions recorded.

Output screen-cast GIF is saved to OUTPUT-FILE."
  (interactive "F")
  (setq screen-cast-clm-on (member 'command-log-mode minor-mode-list))
  (setq screen-cast-clm-logging-dir clm/logging-dir)
  (setq screen-cast-clm-mode-global command-log-mode-is-global)

  (setq screen-cast-start-time (current-time))
  (setq screen-cast-output output-file)
  (setq screen-cast-tmp-dir (make-temp-file "screen-cast-" 'dir))
  (setq clm/logging-dir screen-cast-tmp-dir)
  (setq command-log-mode-is-global t)
  (command-log-mode t)
  (save-window-excursion
    (clm/open-command-log-buffer))
  (let* ((output-gif (concat (file-name-as-directory screen-cast-tmp-dir)
                             "out.gif"))
         (process (start-process "screen-cast"
                                 "*screen-cast*"
                                 "screen_cast.py"
                                 "--record-keys"
                                 "--save-intermediates"
                                 screen-cast-tmp-dir
                                 "--output-file" output-gif)))
    (set-process-sentinel process 'screen-cast-sentinel)))


(provide 'screen-cast)

;;; screen-cast.el ends here
