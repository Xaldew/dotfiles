;;; screen-cast.el -- Record screen casts in Emacs.
;;
;;; Commentary:
;; Record screen-cast using the Python utilities and enhance the output
;; with the executed Emacs actions.
;;
;;; Code:

(defgroup screen-cast nil
  "Group for customizing the screen-casting."
  :group 'tools)

(defcustom screen-cast-kill-sequence "KILL"
  "Character sequence that stops the screen-cast recording."
  :group 'screen-cast
  :type 'string)

(defvar screen-cast-tmp-dir nil
  "Temporary directory were the current screen cast data is saved.")

(defvar screen-cast-output nil
  "The name of the file output screen-cast file.")

(defvar screen-cast-start-time nil
  "The start time of the last screen-cast.")

(defvar screen-cast-cmd-list nil
  "List of commands and time of their execution since starting the screencast.")

(defvar screen-cast-process nil
  "The currently active screen-cast process.")

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

(defvar screen-cast--history nil
  "The most recently written text.")


(cl-defstruct screen-cast-command time keys command)


(defmacro screen-cast--save-command-environment (&rest body)
  "Save and restore `this-command' and `last-command' after saving the command.

BODY: Forms to be executed."
  (declare (indent 1))
  `(let ((deactivate-mark nil)  ; Do not deactivate mark in transient mark mode.
         ;; Do not let random commands scribble over {THIS,LAST}-COMMAND
	 (this-command this-command)
	 (last-command last-command))
     ,@body))


(defun screen-cast--log-command-p (cmd)
  "Determines whether the given command CMD should be logged."
  (null (member cmd screen-cast-cmd-exceptions)))


(defun screen-cast--check-kill-sequence (key)
  "Add KEY to the seen commands seen so far and check for the kill sequence."
  ;; Add the most recent key.
  (setq screen-cast--history (concat screen-cast--history key))
  ;; Truncate history once history length exceeds kill-sequence.
  (when (> (length screen-cast--history)
           (length screen-cast-kill-sequence))
    (setq screen-cast--history
          (substring screen-cast--history
                     (- (length screen-cast-kill-sequence)))))
  ;; Stop the screen-cast when we match the kill-sequence.
  (when (string= screen-cast--history
                 screen-cast-kill-sequence)
    (screen-cast-stop)))


(defun screen-cast-log-command (&optional cmd)
  "Hook into `pre-command-hook' to intercept all Emacs commands.

CMD: TODO."
  (screen-cast--save-command-environment
      (setq cmd (or cmd this-command))
    (screen-cast--check-kill-sequence (key-description (this-command-keys)))
    (when (screen-cast--log-command-p cmd)
      (setq screen-cast-cmd-list
            (nconc screen-cast-cmd-list
                   (list (make-screen-cast-command
                          :time (current-time)
                          :keys (key-description (this-command-keys))
                          :command (symbol-name cmd))))))))


(defun screen-cast-sentinel (process event)
  "Process sentinel for the screen-cast.

PROCESS: The process that received EVENT."
  (cond
   ((equal event "finished\n")
    (screen-cast--ffmpeg-sendcmd-script "el_script.txt"
                                        screen-cast-start-time
                                        screen-cast-cmd-list)
    (screen-cast--tear-down))
   (t
    (princ (format "Process: %s had the event `%s'" process event)))))


(defun screen-cast--setup ()
  "Prepare all variables and hooks for screen-casting."
  (setq screen-cast-start-time (current-time))
  (setq screen-cast-tmp-dir (concat (file-name-as-directory
                                     (make-temp-file "screen-cast-" 'dir))))
  (setq screen-cast-cmd-list '())
  (add-hook 'post-self-insert-hook 'screen-cast--recent-history))


(defun screen-cast--tear-down ()
  "Reset all variables and hooks used for the screen-casting."
  (remove-hook 'pre-command-hook 'screen-cast-log-command)
  (delete-directory screen-cast-tmp-dir 'recursive nil))


(defun screen-cast (output-file)
  "Record a screen-cast with Emacs keys and actions recorded.

Output screen-cast GIF is saved to OUTPUT-FILE."
  (interactive "F")
  (setq screen-cast-output output-file)
  (screen-cast--setup)
  (let* ((output-gif (concat screen-cast-tmp-dir "out.gif"))
         (process (start-process "screen-cast"
                                 "*screen-cast*"
                                 "screen_cast.py"
                                 "--kill-sequence"
                                 ""
                                 "--record-keys"
                                 "--save-intermediates"
                                 screen-cast-tmp-dir
                                 "--output-file" output-gif)))
    (set-process-sentinel process 'screen-cast-sentinel)
    (setq screen-cast-process process)))


(defun screen-cast-stop ()
  "Stop an active screen-cast, if any."
  (interactive)
  (when screen-cast-process
    (interrupt-process screen-cast-process)))


(defun screen-cast--ffmpeg-sendcmd-script (output-file start-time cmd-list)
  "Generate a ffmpeg compatible 'sendcmd' script.

OUTPUT-FILE: Name of the file to write to.
START-TIME: The start-time of the screen cast.
CMD-LIST: List over the commands to be written as a 'sendcmd' script"
  (with-temp-file output-file
    (dolist (cmd (nreverse cmd-list))
      (insert
       (format "%s %s %s\n"
               (format-time-string "%-S.%3N"
                                   (subtract-time (screen-cast-command-time cmd)
                                                  start-time))
               (screen-cast-command-keys cmd)
               (symbol-name (screen-cast-command-command cmd)))))))


(provide 'screen-cast)

;;; screen-cast.el ends here
