;; C++
;; Fix mode for header files.
(defun file-in-directory-list-p (file dirlist)
  "Returns true if the file specified is contained within one of
the directories in the list. The directories must also exist."
  (let ((dirs (mapcar 'expand-file-name dirlist))
	(filedir (expand-file-name (file-name-directory file))))
    (and
     (file-directory-p filedir)
     (member-if (lambda (x) ; Check directory prefix matches
		  (string-match
		   (substring x 0 (min(length filedir) (length x))) filedir))
		dirs))))

(defun buffer-standard-include-p ()
  "Returns true if the current buffer is contained within one of
the directories in the INCLUDE environment variable."
  (and (getenv "INCLUDE")
       (file-in-directory-list-p buffer-file-name
				 (split-string
				  (getenv "INCLUDE") path-separator))))

;; C++ Style.
(c-add-style "my-style"
	     '("stroustrup"
	       (my-coding-style . "linux")
	       (indent-tabs-mode . nil)        ; use spaces rather than tabs
	       (c-basic-offset . 4)            ; indent by four spaces
	       (c-offsets-alist . ((inline-open . 0)  ; custom indentation rules
				   (brace-list-open . 0)
				   (statement-case-open . +)))))

(defun my-c++-mode-hook ()
  (c-set-style "my-style")        ; use my-style defined above
  (ggtags-mode)
  (auto-fill-mode)
  (flycheck-mode)
  (setq flycheck-clang-language-standard "c++11")
  )

(add-hook 'c++-mode-hook 'my-c++-mode-hook)
(add-to-list 'magic-fallback-mode-alist '(buffer-standard-include-p . c++-mode))
