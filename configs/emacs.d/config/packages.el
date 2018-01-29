;;; packages.el --- Install `use-package'. -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; This package is responsible for bootstrapping `use-package'.
;;
;;; Code:

(defvar my-package-list '(use-package diminish delight bind-key)
  "List of packages to be installed upon initialization.")

(package-refresh-contents)

(dolist (package my-package-list)
  (unless (package-installed-p package)
    (condition-case nil
	(package-install package)
      (error (message "[ERROR]: Failed to install package: %s." package)))))

;;; packages.el ends here
