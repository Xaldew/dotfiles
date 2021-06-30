;;; subtitles-player.el --- Displaying subtitles. -*- lexical-binding: t -*-

;;; Commentary:
;; This file provides playback control of VLC or mplayer for displaying the
;; subtitles currently being edited.

;;; Code:


(eval-when (compile run)
	   (require 'comint))


(defcustom srt-backend 'vlc
  "Backend for playing movies along with the srt-files.
Supported backends are MPlayer and VLC."
  :group 'srt
  :type 'symbol)

(defcustom srt-vlc-extra-command-line-args "--fullscreen"
  "Extra command line arguments to be given to VLC."
  :group 'srt
  :type 'string)

(defcustom srt-mplayer-extra-command-line-args "-fs -zoom -osdlevel 2"
  "Extra command line arguments to be given to MPlayer."
  :group 'srt
  :type 'string)


;; Global Variables
(defvar srt-process-buffer nil)
(defvar srt-subfile-buffer nil)
(defvar srt (make-hash-table))
(defvar srt-follow-timer nil
  "Timer to make the .srt-file follow the movie.")

;; Constants
(defconst srt-mode-map (copy-keymap emacs-lisp-mode-map) "Global keymap for `srt'.")
(defconst srt-vlc-commands
  '("add" "enqueue" "playlist" "play"
    "stop" "next" "prev" "goto"
    "clear" "status" "title" "title_n"
    "title_p" "chapter" "chapter_n" "chapter_p"
    "seek" "pause" "fastforward" "rewind"
    "faster" "slower" "normal" "f"
    "info" "get_time" "is_playing" "get_title"
    "get_length" "volume" "volup" "voldown"
    "adev" "achan" "atrack" "vtrack"
    "vratio" "vcrop" "vzoom" "strack"
    "marq-marquee" "marq-x" "marq-y" "marq-position"
    "marq-color" "marq-opacity" "marq-timeout" "marq-size"
    "time-format" "time-x" "time-y" "time-position"
    "time-color" "time-opacity" "time-size" "logo-file"
    "logo-x" "logo-y" "logo-position" "logo-transparency"
    "mosaic-alpha" "mosaic-height" "mosaic-width"
    "mosaic-xoffset" "mosaic-yoffset" "mosaic-align"
    "mosaic-vborder" "mosaic-hborder" "mosaic-position"
    "mosaic-rows" "mosaic-cols" "mosaic-keep-aspect-ratio"
    "check-updates" "help" "longhelp" "logout" "quit"))

(defconst srt-mplayer-commands
  '("[" "]" "{" "}" ">" "<" " " "p" "." "q" "." "+" "-" "/" "*"
    "9" "0" "#" "f" "T" "w" "e" "o" "d" "v" "b" "j" "y" "g" "F"
    "T" "a" "x" "z" "r" "t" "i" "s" "S" "I" "!" "@" "1" "2" "3"
    "4" "5" "6" "7" "8" "2" "l" "t" "c" "p" "r" "h" "k" "n" "u"))


(define-derived-mode srt-comint-mode
  comint-mode "srt-comint-mode"
  "Comint derived mode for interaction with VLC."
  (define-key srt-comint-mode-map "\M-\t" 'srt-vlc-complete-symbol)
  (define-key srt-comint-mode-map "\t" 'srt-vlc-complete-symbol)
  (define-key srt-comint-mode-map "\C-m" 'srt-proc-RET))


(defun srt-completing-read-allow-spaces (prompt table &optional predicate require-match initial-input hist def inherit-input-method)
  "Use `completing-read'.  But allow space input and let case be of no importance."
  (let* ((completion-ignore-case t)
	 (former-function (cdr (assoc 32 minibuffer-local-completion-map))))
					;save former function of space character
    (setcdr (assoc 32 minibuffer-local-completion-map) 'self-insert-command)
					; change space character to simply insert a space
    (unwind-protect
	(completing-read prompt table predicate require-match initial-input hist def inherit-input-method)
      (setcdr (assoc 32 minibuffer-local-completion-map) former-function))))


(defmacro srt-save-pos (&rest body)
  "Execute BODY, restoring point to anchor."
  `(let ((col (current-column))
	 (entry (progn (end-of-line)
		       (re-search-backward srt-anchor-regexp nil t)
		       (match-string-no-properties 0))))
     ,@body
     (goto-char (point-min))
     (re-search-forward entry nil t)
     (goto-char (min (point-max) (+ (point-at-bol) col)))))

;; Process commands
(defmacro srt-get-proc ()
  "Get current srt process."
  `(gethash 'process srt))

(defun srt-set-process-filter (&optional fn)
  "Set process filter for the srt process.
Optional argument FN is a function to override the default function, being the function `srt-filter'."
  (set-process-filter (srt-get-proc) (or fn 'srt-filter)))

(defun srt-output ()
  "Get the last output from the srt process."
  (process-get (srt-get-proc) :output))

(defun srt-send-string (&optional string)
  "Send a STRING to the srt process."
  (interactive)
  (puthash 'last-cmd-interactive-p
	   (if string nil t) srt)
  (process-send-string (srt-get-proc)
		       (or string
			   (format "%s%s"
				   (srt-completing-read-allow-spaces "Send string: "
                                                                     (case srt-backend
                                                                           (vlc srt-vlc-commands)
                                                                           (mplayer srt-mplayer-commands))
                                                                     nil nil)
				   (case srt-backend
                                         ('mplayer "")
                                         ('vlc "\n")))))
  ;; delay to be sure
  (sit-for 0.05)
  (srt-output))

(defun srt-filter (proc string)
  "Filter srt process output."
  (let ((buffer (current-buffer))
	(string (replace-regexp-in-string "" "" string)))
    (with-current-buffer (process-buffer proc)
      (let ((moving (= (point) (process-mark proc))))
	(save-excursion
	  ;; Insert the text, advancing the process marker.
	  (goto-char (process-mark proc))
	  (and (gethash 'last-cmd-interactive-p srt)
	       (insert string))
	  (set-marker (process-mark proc) (point)))
	(if moving (goto-char (process-mark proc)))))
    (setf (process-get (srt-get-proc) :output) string)))


(defun srt-mplayer-filter (proc string)
  "Filter srt process in the case the backend is MPlayer."
  (let ((string (replace-regexp-in-string " \\[J" "" string)))
    (with-current-buffer (process-buffer proc)
      (let ((moving (= (point) (process-mark proc))))
	(save-excursion
	  ;; Insert the text, advancing the process marker.
	  (goto-char (process-mark proc))
	  (insert
	   (cond ((string-match "^A:" string)
		  (delete-region (point-at-bol) (point-at-eol))
		  (substring string 0 (min (+ (progn (string-match "%" string)
						     (match-beginning 0)) 15)
					   (length string))))
		 (t string)))
	  (set-marker (process-mark proc) (point)))
	(if moving (goto-char (process-mark proc)))))
    (setf (process-get (srt-get-proc) :output) string)))


;; Movie control functions
(defun srt-select-and-set ()
  (puthash 'movie (expand-file-name (read-file-name "File: ")) srt)
  (puthash 'subfile (expand-file-name (read-file-name "Subtitle file: "
						      (file-name-directory (gethash 'movie srt))
						      nil
						      nil
						      (format "%s%s" (file-name-sans-extension (file-name-nondirectory (gethash 'movie srt)))
							      ".srt"))) srt))


(defun srt-reopen-movie (&optional seconds)
  (interactive)
  (srt-stop)
  (srt-open-movie (or seconds (srt-get-seconds-at-p))))

(defun srt-open-movie (&optional seconds)
  "Start playing a movie to interact with.
Use the value of `srt-backend' as a backend.  Use functions
`srt-start-proc-vlc' and `srt-start-proc-mplayer' to force using
one or the other.  Subsequent invocations of `srt-open-movie'
will use last used backend."
  (interactive)
  (funcall (intern (concat "srt-open-movie-with-" (symbol-name srt-backend))) seconds))

(defun srt-open-movie-with-vlc (&optional seconds)
  "Open a movie and an srt subtitle file with VLC.
Optional argument SECONDS means re-open current pair of movie and
subtitle at SECONDS from start.  See also the variable
`srt-vlc-extra-command-line-args'"
  (interactive)
  (setq srt-backend 'vlc)
  (or seconds (srt-select-and-set))
  (setf (srt-get-proc) (start-process  "srt" "*srt*"
                                       "vlc" (gethash 'movie srt)
                                       "--sub-file" (gethash 'subfile srt)
                                       (format "--start-time=%d" (or seconds 0))
                                       "--extraintf" "rc")
	srt-process-buffer (get-buffer "*srt*"))
  (srt-set-process-filter)
  (with-current-buffer (get-buffer "*srt*")
    (srt-comint-mode))
  (find-file (gethash 'subfile srt))
  (setq srt-subfile-buffer (current-buffer))
  (srt-follow)
  (srt-mode))

(defun srt-open-movie-with-mplayer (&optional seconds)
  "Open a movie and an srt subtitle file with MPlayer.
Optional argument NO-SELECT means re-open current pair of movie and subtitle."
  (interactive)
  (setq srt-backend 'mplayer)
  (or seconds (srt-select-and-set))
  (setf (srt-get-proc) (start-process "srt" "*srt*" "mplayer"
				      "-sub"
				      (gethash 'subfile srt)
				      (gethash 'movie srt)
				      "-ss"
				      (number-to-string (or seconds 0))
				      "-fs" ;; "-zoom"
				      "-osdlevel" "2"
				      "-subfps" "30" )
	srt-process-buffer (get-buffer "*srt*"))
  (set-process-filter (srt-get-proc) 'srt-mplayer-filter)
  (with-current-buffer (get-buffer "*srt*")
    (srt-comint-mode))
  (find-file (gethash 'subfile srt))
  (setq srt-subfile-buffer (current-buffer))
  (srt-mode))

(defun srt-goto-point-in-movie (&optional and-follow)
  "Tell the movie to move to the time indicated by the time-anchor at point."
  (interactive "P")
  (srt-follow-stop)
  (srt-fix-buffer)
  (when
      (buffer-modified-p)
    (save-buffer))
  (cond ((or (not (gethash 'process srt))
	     (not (eq (process-status (gethash 'process srt)) 'run)))
	 (yes-or-no-p "The movie is not playing, start it? ")
	 (srt-open-movie (srt-get-seconds-at-p)))
	(t (let ((sec (save-excursion
			(cond
			 ((srt-at-entry-number-p)
			  (re-search-forward srt-anchor-regexp nil t))
			 ((srt-at-time-anchor-p))
			 (t (re-search-backward srt-anchor-regexp nil t 2)))
			(srt-get-seconds-at-p))))
	     (case srt-backend
                   (vlc
                    (srt-send-string "status\n")
                    (sit-for 0.05)
                    (and (or (string-match "menu select" (srt-output))
                             (string-match "play state: 2" (srt-output)))
                         (srt-pause))
                    (sit-for 0.05)
                    (srt-send-string
                     (format "seek %d\n"
                             (+ srt-seek-delay sec))))
                   (mplayer
                    (srt-fix-buffer)
                    (save-buffer)
                    (srt-reopen-movie (+ srt-seek-delay sec)))))))
  (if and-follow (srt-follow)))

(defun srt-pause ()
  "Pause or play the movie."
  (interactive)
  (case srt-backend
        (vlc
         (srt-send-string "pause\n"))
        (mplayer
         (srt-send-string " "))))

(defun srt-pause-and-follow ()
  "Pause or play the movie and turn on following mode.
When the purpose is to halt the movie, do not use this function."
  (interactive)
  (srt-pause)
  (srt-follow))

(defun srt-stop ()
  "Quit running current srt backend process.
Same as \\[srt-send-string] RET q RET."
  (interactive)
  (condition-case nil
      (srt-send-string
       (case srt-backend
             (vlc "quit\n")
             (mplayer "q")))
    (error nil)))

;; Windowing/buffer functions
(defun srt-switch-to-subfile-buffer ()
  "Switch to process-associated subtitle file buffer."
  (interactive)
  (switch-to-buffer srt-subfile-buffer))

(defun srt-switch-to-process-buffer ()
  "Switch to srt process buffer."
  (interactive)
  (switch-to-buffer (get-buffer "*srt*"))
  (goto-char (point-max)))

(defun srt-set-current-buffer-as-srt ()
  "Set current buffer as the process-associated subtitle file buffer."
  (interactive)
  (setq srt-subfile-buffer (current-buffer)))

;; Following functions
(defun srt-center-current-time-anchor ()
  "In current buffer, center current time-entry."
  (let* ((time (substring (srt-sec->srt-time (srt-get-time)) 0 8))
         (newpos
          (or (save-excursion (end-of-line 1) (and (re-search-forward time nil t) (1- (point-at-bol))))
              (string-match time (buffer-substring-no-properties
                                  (point-min) (point-max))))))
    (cond  (newpos
	    (srt-propertize-buffer)
	    (goto-char (1+ newpos))
	    (put-text-property
	     (point-at-bol 2)
	     (save-excursion (re-search-forward "^[0-9]*$" nil t))
	     'face
	     '((background-color . "#cccccc")
	       (foreground-color . "#000000")
	       (weight . "bold")))
	    (recenter)))))

(defun srt-follow-function ()
  "When in associated srt subtitle buffer, follow the time of the movie."
  (when (get-buffer-window srt-subfile-buffer)
    (srt-center-current-time-anchor)))

(defun srt-follow ()
  "Follow the playing movie in the buffer `srt-sub-file-buffer'.
Following ONLY takes place when `srt-sub-file-buffer' is the
current buffer."
  (interactive)
  (if (timerp srt-follow-timer)
      (timer-activate srt-follow-timer)
    (setq srt-follow-timer
	  (run-with-timer 1 1 'srt-follow-function)))
  (message "Follow mode on"))

(defun srt-follow-stop ()
  "Stop following the movie in associated subtitle buffer."
  (interactive)
  (when (timerp srt-follow-timer)
    (cancel-timer srt-follow-timer)
    (message "Follow mode off")))

(defun srt-follow-toggle ()
  "Toggle following mode"
  (interactive)
  (cond ((member srt-follow-timer timer-list)
	 (srt-follow-stop))
	(t (srt-follow))))

;; Completion ... bluntly stolen from emacs-lisp mode ... probably overkill
(defun srt-vlc-complete-symbol ()
  "Complete VLC command at point."
  (interactive)
  (if (bolp) nil
    (let ((window (get-buffer-window "*Completions*")))
      (if (and (eq last-command this-command)
	       window (window-live-p window) (window-buffer window)
	       (buffer-name (window-buffer window)))
	  ;; If this command was repeated, and
	  ;; there's a fresh completion window with a live buffer,
	  ;; and this command is repeated, scroll that window.
	  (with-current-buffer (window-buffer window)
	    (if (pos-visible-in-window-p (point-max) window)
		(set-window-start window (point-min))
	      (save-selected-window
		(select-window window)
		(scroll-up))))
	;; Do completion.
	(let* ((end (point))
	       (beg (with-syntax-table emacs-lisp-mode-syntax-table ;fixme: define own syntax-table
		      (save-excursion
			(backward-sexp 1)
			(while (= (char-syntax (following-char)) ?\')
			  (forward-char 1))
			(point))))
	       (pattern (buffer-substring-no-properties beg end))
	       (completion (try-completion pattern srt-vlc-commands)))
	  (cond ((eq completion t))
		((null completion)
		 (message "Can't find completion for \"%s\"" pattern)
		 (ding))
		((not (string= pattern completion))
		 (delete-region beg end)
		 (insert completion)
		 ;; Don't leave around a completions buffer that's out of date.
		 (let ((win (get-buffer-window "*Completions*" 0)))
		   (if win (with-selected-window win (bury-buffer)))))
		(t
		 (let ((minibuf-is-in-use
			(eq (minibuffer-window) (selected-window))))
		   (unless minibuf-is-in-use
		     (message "Making completion list..."))
		   (let ((list (all-completions pattern srt-vlc-commands)))
		     (setq list (sort list 'string<))
		     (if (> (length list) 1)
			 (with-output-to-temp-buffer "*Completions*"
			   (display-completion-list list pattern))
		       ;; Don't leave around a completions buffer that's
		       ;; out of date.
		       (let ((win (get-buffer-window "*Completions*" 0)))
			 (if win (with-selected-window win (bury-buffer))))))
		   (unless minibuf-is-in-use
		     (message "Making completion list...%s" "done"))))))))))


;; srt-comint-mode
(defun srt-proc-RET ()
  (interactive)
  (puthash 'last-cmd-interactive-p t srt)
  (comint-send-input
   (case srt-backend			;do not include newline char (interprets as EOF?)
     (mplayer t)
     (vlc nil))))

(eval-when (compile load)
  (defmacro srt-bind-prc-key (key)
    (define-key srt-comint-mode-map key
      `(lambda ()
;	 (interactive)
	 (case srt-backend
	   (mplayer (srt-send-string ,key))
	   (vlc (insert ,key)))))))

(eval-when (compile load)
  (srt-bind-prc-key "[") (srt-bind-prc-key "]")
  (srt-bind-prc-key "{") (srt-bind-prc-key "}")
  (srt-bind-prc-key ">") (srt-bind-prc-key "<")
  (srt-bind-prc-key " ") (srt-bind-prc-key "p")
  (srt-bind-prc-key ".") (srt-bind-prc-key "q")
  (srt-bind-prc-key ".") (srt-bind-prc-key "+")
  (srt-bind-prc-key "-") (srt-bind-prc-key "/")
  (srt-bind-prc-key "*") (srt-bind-prc-key "9")
  (srt-bind-prc-key "0") (srt-bind-prc-key "#")
  (srt-bind-prc-key "f") (srt-bind-prc-key "T")
  (srt-bind-prc-key "w") (srt-bind-prc-key "e")
  (srt-bind-prc-key "o") (srt-bind-prc-key "d")
  (srt-bind-prc-key "v") (srt-bind-prc-key "b")
  (srt-bind-prc-key "j") (srt-bind-prc-key "y")
  (srt-bind-prc-key "g") (srt-bind-prc-key "F")
  (srt-bind-prc-key "T") (srt-bind-prc-key "a")
  (srt-bind-prc-key "x") (srt-bind-prc-key "z")
  (srt-bind-prc-key "r") (srt-bind-prc-key "t")
  (srt-bind-prc-key "i") (srt-bind-prc-key "s")
  (srt-bind-prc-key "S") (srt-bind-prc-key "I")
  (srt-bind-prc-key "!") (srt-bind-prc-key "@")
  (srt-bind-prc-key "1") (srt-bind-prc-key "2")
  (srt-bind-prc-key "3") (srt-bind-prc-key "4")
  (srt-bind-prc-key "5") (srt-bind-prc-key "6")
  (srt-bind-prc-key "7") (srt-bind-prc-key "8")
  (srt-bind-prc-key "2") (srt-bind-prc-key "l")
  (srt-bind-prc-key "t") (srt-bind-prc-key "c")
  (srt-bind-prc-key "p") (srt-bind-prc-key "r")
  (srt-bind-prc-key "h") (srt-bind-prc-key "k")
  (srt-bind-prc-key "n") (srt-bind-prc-key "u"))

;; Miscellaneous
(defun srt-not-supported-message (&optional fn)
  (error "Function %s not supported with current backend (%s)"
	   (or fn this-command) srt-backend))


(provide 'subtitles-player)

;;; subtitles-player.el ends here
