;;; ffmpeg.el -- Various utilities for interacting with ffmpeg. -*- lexical-binding: t -*-
;;
;;; Commentary:
;; This file provides various functions for interacting with ffmpeg
;; asynchronously.
;;
;;; Code:

(require 'json)

(defun ffprobe (file &rest parameters)
  "Probe the input video FILE for the given PARAMETERS."
  (let (ffprobe result)
    (setq ffprobe
          (with-temp-buffer
            (call-process "ffprobe" nil t nil
                          "-loglevel" "error"
                          "-select_streams" "v:0"
                          "-show_entries"
                          (format "stream=%s"
                                  (mapconcat 'identity parameters ","))
                          "-print_format" "json=compact=1"
                          (file-truename file))
            (goto-char (point-min))
            (json-read)))
    (dolist (param parameters result)
      (setq param (alist-get (intern param)
                             (aref (alist-get 'streams ffprobe) 0)))
      (setq result (nconc result (list param))))))


(defun ffmpeg-screen-grab (x y width height display output-file)
  "Record a region of the screen using ffmpeg.

The region starts at window coordinate [X, Y] with the dimensions
WIDTH and HEIGHT from the device DISPLAY.  The output is saved in
the file named OUTPUT-FILE.

Returns the asynchronously running ffmpeg process."
  (let ((device (if (eq system-type 'windows-nt) "dshow" "x11grab"))
        (input  (if (eq system-type 'windows-nt)
                    "video='screen-capture-recorder'"
                  (format "%s+%d,%d" display x y))))
    (start-process "ffmpeg-screen-grab"
                   "*ffmpeg*"
                   "ffmpeg"
                   "-loglevel" "error"
                   "-y"
                   "-f" device
                   "-show_region" "1"
                   "-framerate" "15"
                   "-video_size" (format  "%dx%d" width height)
                   "-i" input
                   "-codec:v" "huffyuv"
                   "-filter:v" "crop=iw-mod(iw\\,2):ih-mod(ih\\,2)"
                   (file-truename output-file))))


(cl-defstruct ffmpeg-sendcmd start end string)


(defun ffmpeg-drawtext-filter-options (font-size font-color font-alpha location)
  "Compute and return appropriate ffmpeg drawtext filter options.

FONT-SIZE: The size of the font used to draw the text.

FONT-COLOR: Color to use for the font.

FONT-ALPHA: Alpha blending value to use for the text.

LOCATION: The location of where the text should be written.  Can
be any of the symbols: `top-left', `top', `top-right',
`bottom-left', `bottom', `bottom-right' or `center'."
  (let (x y)
    (cond
     ((eq location 'top-left)
      (setq x "5")
      (setq y "5"))
     ((eq location 'top)
      (setq x "(w-text_w)/2")
      (setq y "text_h + 5"))
     ((eq location 'top-right)
      (setq x "w - text_w - 5")
      (setq y "5"))
     ((eq location 'bottom-left)
      (setq x "5")
      (setq y "h - text_h - 5"))
     ((eq location 'bottom)
      (setq x "(w-text_w)/2")
      (setq y "h - text_h - 5"))
     ((eq location 'bottom-right)
      (setq x "w - text_w - 5")
      (setq y "h - text_h - 5"))
     ((eq location 'center)
      (setq x "(w-text_w)/2")
      (setq y "(h-text_h)/2")))
    (mapconcat 'identity `("drawtext=font=Sans"
                           ,(format "fontsize=%d" font-size)
                           ,(format "fontcolor=%s" font-color)
                           ,(format "alpha=%1.3f" font-alpha)
                           ,(format "x=%s" x)
                           ,(format "y=%s" y)
                           "text=''") ":")))


(defun ffmpeg-drawtext (sendcmd-list
                        input output
                        font-size font-color
                        font-alpha font-location)
  "Add the text in SENDCMD-LIST to the INPUT video.

INPUT: Name of the input video file to be filtered.

OUTPUT: Name of the filtered output video file.

FONT-SIZE, FONT-COLOR, FONT-ALPHA and FONT-LOCATION are passed on
to `ffmpeg-drawtext-filter-options'."
  (if (null sendcmd-list)
      (unless (string= input output)
        (copy-file input output t))
    (let* ((sendcmd-script (make-temp-file "sendcmd-" nil ".txt"))
           (time-str nil)
           (filter-opts (ffmpeg-drawtext-filter-options font-size
                                                        font-color
                                                        font-alpha
                                                        font-location))
           (ext (file-name-extension input t))
           (tmp-file (make-temp-file "drawtext" nil ext)))
      (with-temp-file sendcmd-script
        (dolist (cmd sendcmd-list)
          (setq time-str
                (format "%s-%s"
                        (format-time-string "%-S.%3N" (ffmpeg-sendcmd-start cmd))
                        (format-time-string "%-S.%3N" (ffmpeg-sendcmd-end cmd))))
          (insert
           (format "%s [enter] drawtext reinit text='%s',\n"
                   time-str (ffmpeg-sendcmd-string cmd))
           (format "%s [leave] drawtext reinit text='';\n\n"
                   (make-string (length time-str) ? )))))
      (call-process "ffmpeg"
                    nil
                    "*ffmpeg*"
                    nil
                    "-loglevel" "error"
                    "-y"
                    "-i" (file-truename input)
                    "-codec:v" "huffyuv"
                    "-filter:v" (format "sendcmd=f=%s,%s"
                                        sendcmd-script filter-opts)
                    (file-truename tmp-file))
      (copy-file tmp-file output t)
      (delete-file tmp-file))))


(defun ffmpeg-extend-frame (input output length)
  "Extend the INPUT video by holding the last frame for a time.

OUTPUT: Name of the output video file.
LENGTH: The amount of time to add to the duration of the video."
  (cl-destructuring-bind (w h dur) (ffprobe input "width" "height" "duration")
    (let* ((new-duration (+ (string-to-number dur) length))
           (filter-graph (format (concat
                                  "nullsrc=size=%dx%d:duration=%f [null]; "
                                  "[null][0:v] overlay=eof_action=repeat [out]")
                                 w h new-duration))
           (ext (file-name-extension input t))
           (tmp-file (make-temp-file "extend" nil ext)))
      (call-process "ffmpeg"
                    nil
                    "*ffmpeg*"
                    nil
                    "-loglevel" "error"
                    "-y"
                    "-i" (file-truename input)
                    "-codec:v" "huffyuv"
                    "-filter_complex" filter-graph
                    "-map" "[out]" tmp-file)
      (copy-file tmp-file output t)
      (delete-file tmp-file))))


(defun ffmpeg-clip-time (input output start-time finish-time)
  "Clip the INPUT video file in time and write it to OUTPUT.

START-TIME is the desired start time of the OUTPUT video.
FINISH-TIME is the desired end time of the OUTPUT video."
  (let* ((ext (file-name-extension input t))
         (tmp-file (make-temp-file "clip" nil ext)))
    (call-process "ffmpeg"
                  nil
                  "*ffmpeg*"
                  nil
                  "-loglevel" "error"
                  "-y"
                  "-i" (file-truename input)
                  "-codec:v" "huffyuv"
                  "-ss" (number-to-string start-time)
                  "-to" (number-to-string finish-time)
                  tmp-file)
    (copy-file tmp-file output t)
    (delete-file tmp-file)))


(defun ffmpeg-create-gif (input output)
  "Convert the INPUT video file to the OUTPUT GIF file."
  (let ((palette (make-temp-file "palette" nil ".png")))
    (call-process "ffmpeg"
                  nil
                  "*ffmpeg*"
                  nil
                  "-loglevel" "error"
                  "-y"
                  "-i" (file-truename input)
                  "-filter:v" "palettegen"
                  palette)
    (call-process "ffmpeg"
                  nil
                  "*ffmpeg*"
                  nil
                  "-loglevel" "error"
                  "-y"
                  "-i" palette
                  "-i" (file-truename input)
                  "-filter_complex"
                  "[1:v][0:v] paletteuse"
                  (file-truename output))))


(defun ffmpeg-create-x-face (input)
  "Generate a valid X-Face string from the INPUT file.

Note that the INPUT file must be on an image format that `ffmpeg' supports.
Recommended formats are:
* jpeg
* png
* pam, ppm, pgm, pbm
* xbm"
  (replace-regexp-in-string
   "[^[:print:]]" ""
   (with-temp-buffer
     (call-process "ffmpeg"
                   nil
                   t
                   nil
                   "-loglevel" "error"
                   "-i" (file-truename input)
                   "-filter:v" "scale=48x48"
                   "-codec:v" "xface"
                   "-f" "rawvideo"
                   "pipe:1")
     (buffer-string))))


(defun ffmpeg-x-face-to-pbm (x-face)
  "Use ffmpeg to generate a valid binary `pbm' image from a X-FACE string."
  (with-temp-buffer
    (insert x-face)
    (set-buffer-multibyte nil)   ; Image must be in unibyte format.
    (let ((coding-system-for-read 'raw-text)
          (coding-system-for-write 'binary))
      (call-process-region (point-min)
                           (point-max)
                           "ffmpeg"
                           'delete
                           t
                           nil
                           "-loglevel" "error"
                           "-codec:v0" "xface"
                           "-f" "rawvideo"
                           "-s:v0" "48x48"
                           "-i" "pipe:0"
                           "-codec:v1" "pbm"
                           "-f" "rawvideo"
                           "pipe:1"))
    (buffer-string)))


(provide 'ffmpeg)

;;; ffmpeg.el ends here
