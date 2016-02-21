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

(defcustom screen-cast-debug nil
  "Debug the screen-casting by saving intermediate files."
  :group 'screen-cast
  :type 'boolean)

(defvar screen-cast--tmp-dir nil
  "Temporary directory were the current screen cast data is saved.")

(defvar screen-cast--output nil
  "The name of the file output screen-cast file.")

(defvar screen-cast--start-time nil
  "The start time of the last screen-cast.")

(defvar screen-cast--cmd-list nil
  "List of commands and time of their execution since starting the screencast.")

(defvar screen-cast--process nil
  "The currently active screen-cast process.")

(defvar screen-cast--cmd-exceptions
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

(defvar screen-cast--finish-time nil
  "Finish time of the screen-cast.
Used to truncate video to before the kill-sequence.")


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
  (null (member cmd screen-cast--cmd-exceptions)))


(cl-defstruct screen-cast-key-history key time)


(defun screen-cast--check-kill-sequence (key)
  "Add KEY to the seen commands seen so far and check for the kill sequence."

  ;; Add the most recent key.
  (setq screen-cast--history (nconc screen-cast--history
                                    (list (make-screen-cast-key-history
                                           :key key
                                           :time (current-time)))))

  ;; Truncate history once history length exceeds kill-sequence.
  (when (> (length screen-cast--history)
           (length screen-cast-kill-sequence))
    (pop screen-cast--history))

  ;; Check if the last few keys match the kill-sequence.
  (when (string= (mapconcat 'screen-cast-key-history-key
                            screen-cast--history "")
                 screen-cast-kill-sequence)
    (screen-cast-stop (screen-cast-key-history-time
                       (car screen-cast--history)))))


(defun screen-cast-log-command (&optional cmd)
  "Hook into `pre-command-hook' to intercept all Emacs commands.

CMD: TODO."
  (screen-cast--save-command-environment
   (setq cmd (or cmd this-command))
   (screen-cast--check-kill-sequence (key-description (this-command-keys)))
   (when (screen-cast--log-command-p cmd)
     (setq screen-cast--cmd-list
           (nconc screen-cast--cmd-list
                  (list (make-screen-cast-command
                         :time (current-time)
                         :keys (key-description (this-command-keys))
                         :command (symbol-name cmd))))))))


(defun screen-cast--post-process ()
  "Perform screen-cast post-processing."
  (let* ((cmd-list (screen-cast--drawtext-list screen-cast--start-time
                                               screen-cast--cmd-list
                                               'screen-cast-command-time
                                               'screen-cast-command-command))
         (key-list (screen-cast--drawtext-list screen-cast--start-time
                                               screen-cast--cmd-list
                                               'screen-cast-command-time
                                               'screen-cast-command-keys))
         (tmp-dir screen-cast--tmp-dir)
         (output (concat tmp-dir "out.avi"))
         (clip-time (float-time (time-subtract screen-cast--finish-time
                                               screen-cast--start-time))))
    (when screen-cast-debug
      (copy-file output "screen-cast0.avi" t))
    (ffmpeg-clip-time output output 0.0 (- clip-time 0.1))
    (when screen-cast-debug
      (copy-file output "screen-cast1.avi" t))
    (ffmpeg-drawtext cmd-list output output 16 "white" 0.7 'bottom)
    (when screen-cast-debug
      (copy-file output "screen-cast2.avi" t))
    (ffmpeg-drawtext key-list output output 18 "white" 0.7 'top-right)
    (when screen-cast-debug
      (copy-file output "screen-cast3.avi" t))
    (ffmpeg-extend-frame output output 1.0)
    (when screen-cast-debug
      (copy-file output "screen-cast4.avi" t))
    (ffmpeg-create-gif output screen-cast--output)))


(defun screen-cast-sentinel (process event)
  "Process sentinel for the screen-cast.

PROCESS: The process that received EVENT."
  (screen-cast--post-process)
  (screen-cast--tear-down))


(defun screen-cast--tear-down ()
  "Reset all variables and hooks used for the screen-casting."
  (remove-hook 'pre-command-hook 'screen-cast-log-command)
  (delete-directory screen-cast--tmp-dir 'recursive nil))


(defun screen-cast (output-file)
  "Record a screen-cast with Emacs keys and actions recorded.

Output screen-cast GIF is saved to OUTPUT-FILE."
  (interactive "F")
  (setq screen-cast--tmp-dir (concat (file-name-as-directory
                                      (make-temp-file "screen-cast-" 'dir))))
  (setq screen-cast--output output-file)
  (setq screen-cast--cmd-list '())
  (setq screen-cast--history '())
  (cl-destructuring-bind (x y w h) (xcb-rectsel)
    (let* ((output-avi (concat screen-cast--tmp-dir "out.avi"))
           (display (xcb-rectsel-display))
           (process (ffmpeg-screen-grab x y w h display output-avi)))
      (set-process-sentinel process 'screen-cast-sentinel)
      (setq screen-cast--process process)
      (setq screen-cast--start-time (current-time))
      (add-hook 'pre-command-hook 'screen-cast-log-command))))


(defun screen-cast-stop (&optional finish-time)
  "Stop an active screen-cast, if any and set the FINISH-TIME."
  (interactive)
  (if finish-time
      (setq screen-cast--finish-time finish-time)
    (setq screen-cast--finish-time (current-time)))
  (when screen-cast--process
    (interrupt-process screen-cast--process)))


(defun screen-cast--drawtext-list (start-time
                                   cmd-list
                                   time-accessor
                                   string-accessor)
  "Generate a ffmpeg compatible 'drawtext' list of 'sendcmd' commands.

The list contains a start-time for when a string should be
displayed on the video and finish time when it should no longer
be displayed.

OUTPUT-FILE: Name of the file to write to.

START-TIME: The start-time of the screen cast.

CMD-LIST: List over the commands to be written as a 'sendcmd' script.

TIME-ACCESSOR: Function used to access the times stored in CMD-LIST.

STRING-ACCESSOR: Function used to access the strings stored in CMD-LIST."
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
      (setq start (subtract-time (funcall time-accessor cmd0) start-time))
      (setq end (subtract-time (funcall time-accessor cmd1) start-time))
      (setq string (funcall string-accessor cmd0))
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
                                             (funcall time-accessor cmd1)
                                             (seconds-to-time 1.0))
                                            start-time)
                        :string (funcall string-accessor cmd1)))))))


(provide 'screen-cast)

;;; screen-cast.el ends here
