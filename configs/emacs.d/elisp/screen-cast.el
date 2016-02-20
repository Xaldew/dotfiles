;;; screen-cast.el -- Record screen casts in Emacs.
;;
;;; Commentary:
;; Record screen-cast using the Python utilities and enhance the output
;; with the executed Emacs actions.
;;
;;; Code:

(require 'ffmpeg)
(require 'xcb-rectsel)


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


(defun screen-cast--post-process ()
  "Perform screen-cast post-processing."
  (let* ((sendcmd-list (screen-cast--drawtext-list screen-cast-start-time
                                                   screen-cast-cmd-list))
         (tmp-dir screen-cast-tmp-dir)
         (output (concat tmp-dir "out.avi")))
    (ffmpeg-drawtext sendcmd-list output output)
    ;;(ffmpeg-extend-frame output output 1.0)
    (ffmpeg-create-gif output screen-cast-output)))


(defun screen-cast-sentinel (process event)
  "Process sentinel for the screen-cast.

PROCESS: The process that received EVENT."
  (screen-cast--post-process)
  (screen-cast--tear-down))


(defun screen-cast--tear-down ()
  "Reset all variables and hooks used for the screen-casting."
  (remove-hook 'pre-command-hook 'screen-cast-log-command)
  (delete-directory screen-cast-tmp-dir 'recursive nil))


(defun screen-cast (output-file)
  "Record a screen-cast with Emacs keys and actions recorded.

Output screen-cast GIF is saved to OUTPUT-FILE."
  (interactive "F")
  (setq screen-cast-tmp-dir (concat (file-name-as-directory
                                     (make-temp-file "screen-cast-" 'dir))))
  (setq screen-cast-output output-file)
  (setq screen-cast-cmd-list '())
  (setq screen-cast--history '())
  (cl-destructuring-bind (x y w h) (xcb-rectsel)
    (let* ((output-avi (concat screen-cast-tmp-dir "out.avi"))
           (display (xcb-rectsel-display))
           (process (ffmpeg-screen-grab x y w h display output-avi)))
      (set-process-sentinel process 'screen-cast-sentinel)
      (setq screen-cast-process process)
      (setq screen-cast-start-time (current-time))
      (add-hook 'pre-command-hook 'screen-cast-log-command))))


(defun screen-cast-stop ()
  "Stop an active screen-cast, if any."
  (interactive)
  (when screen-cast-process
    (interrupt-process screen-cast-process)))

(defun screen-cast--drawtext-list (start-time cmd-list)
  "Generate a ffmpeg compatible 'drawtext' list of 'sendcmd' commands.

The list contains a start-time for when a string should be
displayed on the video and finish time when it should no longer
be displayed.

OUTPUT-FILE: Name of the file to write to.
START-TIME: The start-time of the screen cast.
CMD-LIST: List over the commands to be written as a 'sendcmd' script"
  (let ((cmd-script '())
        (i 0)
        (cmd0 nil)
        (cmd1 nil)
        (start 0)
        (end 0)
        (string ""))
    (while (< i (- (length cmd-list) 1))
      (setq cmd0 (nth i cmd-list))
      (setq cmd1 (nth (+ i 1) cmd-list))
      (setq start (subtract-time (screen-cast-command-time cmd0) start-time))
      (setq end (subtract-time (screen-cast-command-time cmd1) start-time))
      (setq string (screen-cast-command-command cmd0))
      (setq cmd-script
            (nconc cmd-script
                   (list (make-ffmpeg-sendcmd :start start
                                              :end end
                                              :string string))))
      (cl-incf i))
    ;; Add the last command and hold last command for a second.
    (setq cmd-script
          (nconc cmd-script
                 (list (make-ffmpeg-sendcmd
                        :start end
                        :end (subtract-time (time-add
                                             (screen-cast-command-time cmd1)
                                             (number-to-time 1.0)))
                        :string (scren-cast-command-command cmd1)))))))


(provide 'screen-cast)

;;; screen-cast.el ends here
