;;; init.el -- Personal Emacs config toplevel.
;;
;;; Commentary:
;; My personal toplevel Emacs configuration file.  This should be the first file
;; to be loaded by Emacs.
;;
;;; Code:

;; Define the location of user configuration.
(defvar user-emacs-config (concat user-emacs-directory "config/")
  "The directory containing user-specific configuration.")

;; Prefer the newer files when loading.
(setq load-prefer-newer t)

;; Set path for Custom variables.
(setq custom-file "~/.emacs.d/custom.el")

;; Add `~/.emacs.d/elisp' to the initial load path.
(add-to-list 'load-path (expand-file-name "~/.emacs.d/elisp"))


(defun load-user-file (file)
  "Load FILE from the current user's Emacs configuration directory."
  (interactive "f")
  (load (expand-file-name file user-emacs-config)))


;; Load settings independent on external plugins.
(load-user-file "windows")
(load-user-file "terminals")
(load-user-file "keybinds")
(load-user-file "settings")
(load-user-file "file-assoc")
(load-user-file "func")
(load-user-file "ccmode")
(load-user-file "cmode")
(load-user-file "cppmode")
(load-user-file "pymode")


;; Update and load autoload cookies for the local files.
(let ((generated-autoload-file (expand-file-name
				"~/.emacs.d/elisp/local-autoloads.el")))
  (unless (file-readable-p generated-autoload-file)
    (update-directory-autoloads (expand-file-name "~/.emacs.d/elisp"))
    (kill-buffer (file-name-nondirectory generated-autoload-file)))
  (load generated-autoload-file 'no-errors 'no-messages))


;; Activate 'package and install packages from these repositories.
(package-initialize)
(add-to-list 'package-archives
	     '("gnu" . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/"))

(unless (package-installed-p 'use-package)
  (load-user-file "packages"))


;; Load external addons if possible.
(load-user-file "addons")
(load-user-file "my-hydras")

;; Load custom-file for possible local overrides.
(load custom-file 'no-errors)

;;; init.el ends here
