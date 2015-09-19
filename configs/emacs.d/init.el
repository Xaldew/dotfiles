;; Prefer the newer files when loading.
(setq load-prefer-newer t)

(defun load-user-file (file)
  "Load a file in current user's configuration directory."
  (interactive "f")
  (load (expand-file-name file user-emacs-directory)))

;; Add `~/.emacs.d/elisp' to the initial load path.
(add-to-list 'load-path (expand-file-name "~/.emacs.d/elisp"))

;; Set path for Custom variables.
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'no-errors)

;; Load settings independent on external plugins.
(load-user-file "keybinds.el")
(load-user-file "settings.el")
(load-user-file "file-assoc.el")

;; Attempt to load packages and their configuration settings.
(when (>= emacs-major-version 24)
  ;; Activate package library.
  (require 'package)
  (package-initialize)
  (load-user-file "packages.el")
  (load-user-file "addons.el"))

(load-user-file "func.el")
(load-user-file "ccmode.el")
(load-user-file "cmode.el")
(load-user-file "cppmode.el")
(load-user-file "pymode.el")
(load-user-file "module-mk-parser.el")
(load-user-file "parse-cdb.el")
(load-user-file "latexmode.el")

;; Update and load autoload cookies for the local files.
(let ((generated-autoload-file (expand-file-name
				"~/.emacs.d/elisp/local-autoloads.el")))
  (unless (file-readable-p generated-autoload-file)
    (update-directory-autoloads (expand-file-name "~/.emacs.d/elisp"))
    (kill-buffer (file-name-nondirectory generated-autoload-file)))
  (load generated-autoload-file 'no-errors 'no-messages))
