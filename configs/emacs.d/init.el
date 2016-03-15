;;; init.el -- Personal Emacs config toplevel.
;;
;;; Commentary:
;; My personal toplevel Emacs configuration file.  This should be the first file
;; to be loaded by Emacs.
;;
;;; Code:

;; Prefer the newer files when loading.
(setq load-prefer-newer t)

(defun load-user-file (file)
  "Load FILE from the current user's Emacs configuration directory."
  (interactive "f")
  (load (expand-file-name file user-emacs-directory)))


;; Add `~/.emacs.d/elisp' to the initial load path.
(add-to-list 'load-path (expand-file-name "~/.emacs.d/elisp"))

;; Set path for Custom variables.
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'no-errors)


;; Load settings independent on external plugins.
(load-user-file "windows")
(load-user-file "keybinds")
(load-user-file "settings")
(load-user-file "file-assoc")
(load-user-file "func")
(load-user-file "ccmode")
(load-user-file "cmode")
(load-user-file "cppmode")
(load-user-file "pymode")
(load-user-file "module-mk-parser")
(load-user-file "parse-cdb")


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
	     '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))

(unless (package-installed-p 'use-package)
  (load-user-file "packages"))


;; Load external addons if possible.
(load-user-file "addons")

;;; init.el ends here
