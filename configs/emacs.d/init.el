(defconst user-init-dir
  (cond ((boundp 'user-emacs-directory)
	 user-emacs-directory)
	((boundp 'user-init-directory)
	 user-init-directory)
	(t "~/.emacs.d/"))
  "Set the user init directory.")

(defun load-user-file (file)
  "Load a file in current user's configuration directory."
  (interactive "f")
  (load-file (expand-file-name file user-init-dir)))

;; Add elisp inital to the initial load path.
(add-to-list 'load-path (expand-file-name "~/.emacs.d/elisp"))

;; Set path for Custom variables.
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file t)

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

;; Update and load autoload cookies for the local files.
(let ((generated-autoload-file (expand-file-name
				"~/.emacs.d/elisp/local-autoloads.el"))
      (no-errs t)
      (no-msgs t))
  (unless (file-readable-p generated-autoload-file)
    (update-directory-autoloads (expand-file-name "~/.emacs.d/elisp"))
    (kill-buffer (file-name-nondirectory generated-autoload-file)))
  (load generated-autoload-file no-errs no-msgs))
