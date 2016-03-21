;;; gnus.el -- Personal Emacs GNUS configuration.
;;
;;; Commentary:
;; Personal settings for using GNUS to connect to various mail clients.
;;
;;; Code:

(setq gnus-select-method
      '(nnimap "gmail"
	       (nnimap-address "imap.gmail.com")
	       (nnimap-server-port "imaps")
	       (nnimap-stream ssl)))

(add-to-list 'gnus-secondary-select-methods
             '(nnimap "ARM"
                      (nnimap-address "outlook.office365.com")
                      (nnimap-server-port "imaps")
                      (nnimap-stream tls)
                      (nnir-search-engine imap)))

(setq smtpmail-smtp-service 587
      gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")

;; Always display nnimap mails in topic view.
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

(defun my-posting-from-work-p ()
  "Check if we are posting this GNUS message at work."
  (or (string-match-p "e[0-9]\\{6\\}-lin" (system-name))
      (string= (user-login-name) "guswal01")))

(setq gnus-posting-styles
      '((".*"
         (signature "Written at home from %s." (emacs-version))
         (name "Gustaf Waldemarson")
         ("X-Message-SMTP-Method" "smtp smtp.gmail.com 587")
         ;; (x-face-file "~/.xface")
         ;; (x-url (getenv "WWW_HOME"))
         )
        ((my-posting-from-work-p) ;; A user defined function
         (signature "Written at work from %s." (emacs-version))
         (address "gustaf.waldemarson@arm.com")
         ("X-Message-SMTP-Method" "smtp smtp.office365.com 587")
         (organization "ARM Ltd."))))


(when window-system
  (setq gnus-sum-thread-tree-indent "  ")
  (setq gnus-sum-thread-tree-root "● ")
  (setq gnus-sum-thread-tree-false-root "◯ ")
  (setq gnus-sum-thread-tree-single-indent "◎ ")
  (setq gnus-sum-thread-tree-vertical        "│")
  (setq gnus-sum-thread-tree-leaf-with-other "├─► ")
  (setq gnus-sum-thread-tree-single-leaf     "╰─► "))

(setq gnus-summary-line-format
      (concat
       "%0{%U%R%z%}"
       "%3{│%}" "%1{%d%}" "%3{│%}"  ; Date
       "  "
       "%4{%-20,20f%}"              ; Name
       "  "
       "%3{│%}"
       " "
       "%1{%B%}"
       "%s\n"))
(setq gnus-summary-display-arrow t)

;;; gnus.el ends here
