(defvar my-package-list '(use-package)
  "List of packages to be installed upon initialization.")

(package-refresh-contents)

(dolist (package my-package-list)
  (unless (package-installed-p package)
    (condition-case nil
	(package-install package)
      (error (message "[ERROR]: Failed to install package: %s." package)))))
