;;; gnus.el -- Personal Emacs GNUS configuration.
;;
;;; Commentary:
;; Personal settings for using GNUS to connect to various mail clients.
;;
;;; Code:

(require 'gnus)
(require 'ffmpeg)

(setq gnus-select-method
      '(nnimap "gmail"
	       (nnimap-address "imap.gmail.com")
	       (nnimap-server-port "imaps")
	       (nnimap-stream ssl)
               (nnir-search-engine imap)))

(add-to-list 'gnus-secondary-select-methods
             '(nnimap "ARM"
                      (nnimap-address "outlook.office365.com")
                      (nnimap-server-port "imaps")
                      (nnimap-stream tls)
                      (nnir-search-engine imap)))

(add-to-list 'gnus-secondary-select-methods '(nntp "news.gwene.org"))

(setq smtpmail-smtp-service 587
      gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")


;; Always display nnimap mails in topic view.
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)


(defun my-posting-from-work-p ()
  "Check if we are posting this GNUS message at work."
  (or (string-match-p "e[0-9]\\{6\\}-lin" (system-name))
      (string= (user-login-name) "guswal01")))


(when (and (not (executable-find "compface"))
           (executable-find "ffmpeg"))
  (setq gnus-treat-display-x-face 'head)
  (advice-add 'uncompface :override #'ffmpeg-x-face-to-pbm))


(defvar my-gnus-x-face
  (concat
   " \"c-YF%wh2UV[&70j\\TQ\"|I$N2MV5Bl9M#-'b8LY\"Uj&MdHG{>XlY$75f|39nWaV0Hct7_<F"
   " @ph<915nhG[R:lgWJf\"`rhaUXTJ?D$.y[u%<[(q*fl`PR0I;hx!sfbm}Q={Hk0O3M4u\\7b\\"))


(defun my-x-face ()
  "Retrieve a random X-Face or a fallback if none are available."
  (let* ((dir gnus-x-face-directory)
         (files (if (file-directory-p dir)
                    (directory-files dir t ".xbm")
                  '()))
         (rnd-file (if files
                       (nth (random (length files)) files)
                     '()))
         (x-face (if rnd-file
                     (ffmpeg-create-x-face rnd-file)
                   "")))
    (if (not (string= "" x-face))
        x-face
      gnus-x-face)))


(setq gnus-posting-styles
      '((".*"
         (name "Gustaf Waldemarson")
         (address "gustaf.waldemarson@gmail.com")
         ("X-Message-SMTP-Method" "smtp smtp.gmail.com 587")
         (X-Face (my-x-face))
         (signature (format "Written at home from %s."
                            (replace-regexp-in-string "\n" "" (emacs-version))))
         ;; (x-url (getenv "WWW_HOME"))
         )
        ((my-posting-from-work-p)
         (address "gustaf.waldemarson@arm.com")
         ("X-Message-SMTP-Method" "smtp smtp.office365.com 587")
         (signature (format "Written at work from %s."
                            (replace-regexp-in-string "\n" "" (emacs-version))))
         (organization "ARM Ltd."))))


;; Always sort threads by most recent date.
(setq gnus-thread-sort-functions #'gnus-thread-sort-by-most-recent-date)


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
