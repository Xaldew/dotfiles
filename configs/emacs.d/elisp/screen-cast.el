;;; screen-cast.el -- Record screen casts in Emacs.
;;
;;; Commentary:
;; Record screen-cast using the Python utilities and enhance the output
;; with the executed Emacs actions.
;;
;;; Code:


(defvar screen-cast-tmp-dir nil
  "Temporary directory were the current screen cast data is saved.")

(defvar screen-cast-output nil
  "The name of the file output screen-cast file.")

(defvar screen-cast-start-time nil
  "The start time of the last screen-cast.")

(defvar screen-cast-cmd-list nil
  "List of commands and time of their execution since starting the screencast.")

(cl-defstruct screen-cast-command time keys command)

(defmacro screen-cast--save-command-environment (&rest body)
  "Save and restore `this-command' and `last-command' after saving the command.

BODY: Forms to be executed."
  (declare (indent 0))
  `(let ((deactivate-mark nil)  ; Do not deactivate mark in transient mark mode.
         ;; Do not let random commands scribble over {THIS,LAST}-COMMAND
	 (this-command this-command)
	 (last-command last-command))
     ,@body))

(defvar screen-cast-cmd-exceptions
  '(nil self-insert-command backward-char forward-char
        delete-char delete-backward-char backward-delete-char
        backward-delete-char-untabify
        universal-argument universal-argument-other-key
        universal-argument-minus universal-argument-more
        beginning-of-line end-of-line recenter
        move-end-of-line move-beginning-of-line
        handle-switch-frame
        newline previous-line next-line)
  "A list commands which should not be logged.

Frequently used non-interesting commands (like cursor movements)
should be put here.")


(defun screen-cast--log-command-p (cmd)
  "Determines whether the given command CMD should be logged."
  (null (member cmd screen-cast-cmd-exceptions)))


(defun screen-cast-log-command (&optional cmd)
  "Hook into `pre-command-hook' to intercept all Emacs commands.

CMD: TODO."
  (screen-cast--save-command-environment
   (setq cmd (or cmd this-command))
   (when (screen-cast--log-command-p cmd)
     (setq screen-cast-cmd-list
           (cons (make-screen-cast-command
                  :time (current-time)
                  :keys (key-description (this-command-keys))
                  :command cmd)
                 screen-cast-cmd-list)))))


(defun screen-cast-sentinel (process event)
  "Process sentinel for the screen-cast.

PROCESS: The process that received EVENT."
  (cond
   ((equal event "finished\n")
    (remove-hook 'pre-command-hook 'screen-cast-log-command)
    (print screen-cast-cmd-list)
    (delete-directory screen-cast-tmp-dir t nil))
   (t
    (princ (format "Process: %s had the event `%s'" process event)))))


(defun screen-cast (output-file)
  "Record a screen-cast with Emacs keys and actions recorded.

Output screen-cast GIF is saved to OUTPUT-FILE."
  (interactive "F")
  (setq screen-cast-start-time (current-time))
  (setq screen-cast-output output-file)
  (setq screen-cast-tmp-dir (make-temp-file "screen-cast-" 'dir))
  (add-hook 'pre-command-hook 'screen-cast-log-command)
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
