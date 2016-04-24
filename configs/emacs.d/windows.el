;; File containing various Windows related configurations.

(defun cygwin-p ()
  "Return true if this is a variant of Cygwin."
  (eq system-type 'cygwin))


(defun windows-os-p ()
  "Return true if this is a variant of Windows."
  (or (eq system-type 'windows-nt)
      (eq system-type 'cygwin)
      (eq system-type 'ms-dos)))


(defun cygwin-windows-path (path &optional path-list)
  "Convert the given path to a Cygwin compatible Path.

Do not convert the PATH if running under Windows-nt.

PATH is a Cygwin path on unix format.

PATH-LIST indicates that the path is list of semicolon separated unix paths."
  (if (cygwin-p)
      (replace-regexp-in-string
       "[\n\r]*\\'" ""
       (shell-command-to-string
        (concat "cygpath --windows " (when path-list "--path ") path)))
    path))


(defun windows-growlnotify (priority title message)
  "Issue a Growl notification with `growlnotify'.

Send a message with PRIORITY to Growl with the TITLE and MESSAGE."
  (call-process "growlnotify"
                nil
                "*growl*"
                nil
                "/application:emacs"
                (format "/i:%s"
                        (cygwin-windows-path
                         (file-truename
                          (concat
                           invocation-directory
                           "../share/icons/hicolor/48x48/apps/emacs.png"))))
                (format "/p:%S" priority)
                (format "/t:%S" title)
                message))


(when (windows-os-p)
  ;; Change how the bell works in Windows.
  (setq visible-bell t)
  (setq ring-bell-function 'ignore))

(when (and (cygwin-p)
           (not (display-graphic-p)))
  (let ((path "/cygdrive/c/Program Files (x86)/Growl for Windows"))
    (when (file-exists-p path)
      (add-to-list 'exec-path path)))

  ;; Add proper scrolling for Emacs under Cygwin.
  (global-set-key (kbd "<mouse-4>") (lambda () (interactive) (scroll-down 1)))
  (global-set-key (kbd "<mouse-5>") (lambda () (interactive) (scroll-up 1))))

(when (and (not (cygwin-p)) (windows-os-p))
  ;; Setup dictionary for using Hunspell with Flyspell-mode.
  (require 'flyspell)
  (setq ispell-dictionary "english")
  (add-to-list 'ispell-local-dictionary-alist '(("svenska"
                                                 "[[:alpha:]]"
                                                 "[^[:alpha:]]"
                                                 "[']"
                                                 nil
                                                 ("-d" "sv_SE")
                                                 t
                                                 utf-8)
                                                ("english"
                                                 "[[:alpha:]]"
                                                 "[^[:alpha:]]"
                                                 "[']"
                                                 t
                                                 ("-d" "en_US")
                                                 nil
                                                 utf-8)))
  (setq ispell-hunspell-dictionary-alist ispell-local-dictionary-alist)

  ;; Startup the server if not already running.
  (require 'server)
  (unless (server-running-p)
    (server-start)))
