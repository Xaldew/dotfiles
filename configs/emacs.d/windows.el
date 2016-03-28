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

PATH is a Cygwin path on unix format.

PATH-LIST indicates that the path is list of semicolon separated unix paths."
  (replace-regexp-in-string
   "[\n\r]*\\'" ""
   (shell-command-to-string
    (concat "cygpath --windows " (when path-list "--path ") path))))


(when (windows-os-p)
  ;; Change how the bell works in Windows.
  (setq visible-bell t)
  (setq ring-bell-function 'ignore))

(when (and (cygwin-p)
           (not (display-graphic-p)))
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
