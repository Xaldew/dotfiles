;;; packages.el --- Install `use-package' if not already installed.
;;
;;; Commentary:
;; This package is responsible for installing use-package if not already done.
;;
;;; Code:

(defvar my-package-list '(use-package)
  "List of packages to be installed upon initialization.")

(package-refresh-contents)

(dolist (package my-package-list)
  (unless (package-installed-p package)
    (condition-case nil
	(package-install package)
      (error (message "[ERROR]: Failed to install package: %s." package)))))

;;; packages.el ends here
