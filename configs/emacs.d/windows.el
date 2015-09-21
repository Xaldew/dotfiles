;; File containing various Windows related configurations.

(defun windows-os-p ()
  "Return true if this is a variant of Windows."
  (or (eq system-type 'windows-nt)
      (eq system-type 'cygwin)
      (eq system-type 'ms-dos)))

(when (windows-os-p)
  ;; Change how the bell works in Windows.
  (setq visible-bell t)
  (setq ring-bell-function 'ignore)

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
