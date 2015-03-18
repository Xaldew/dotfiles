;; A module to parse Module.mk, Android.mk and Makefiles for include files.

(defun module-mk-parser ()
  "Parse a Module.mk file and set flycheck options."
  )

(defun module-mk-find-top (path)
  "Find the top-level makefile for the Module.mk system."
  (let (top)
    (setq top (locate-dominating-file path "Makefile"))
    (when top
      (concat top "Makefile"))))


(defun module-mk-parse-include ()
  "Parse an include path. Uses and modifies point."
  )

(defun module-mk-get-make-var (var-name)
  "Attempt to find the contents of the content of a Makefile variable."
  )


(defun makefile-parser (mk)
  "Parse a Module.mk file and set flycheck options."
  (print mk)
  (when (file-readable-p mk)
    (with-temp-buffer
      (insert-file-contents mk)
      (goto-char (point-min))

      (while (not (eobp))
      	(while (looking-at ".*-iquote[ \t]+\\([^ \t\n]+\\)")
	  (what-line)
	  (print (match-string 1))
	  (print (match-string 0))
	  (goto-char (match-end 0)))
	(while (looking-at ".*-isystem")
	  (what-line)
	  (print "Found system header path."))
	(forward-line))
      )))


(defun Android-mk-parser ()
  "Parse a Android.mk file and set flycheck options."
  )

(defun module-mk-parser-start (path)
  "Start parsing at the given path."
  (interactive "DDirectory: ")
  (when (file-exists-p
	 (concat (file-name-as-directory path) "Module.mk"))
    (let (top-mk)
      (setq top-mk (module-mk-find-top path))
      (when top-mk
	;; Add The top-level directory to the include paths.
	(add-to-list 'flycheck-clang-include-path
		     (file-name-directory top-mk))
	(add-to-list 'flycheck-clang-includes
		     (file-name-directory top-mk))
	(add-to-list 'flycheck-gcc-include-path
		     (file-name-directory top-mk))
	(add-to-list 'flycheck-gcc-includes
		     (file-name-directory top-mk))
	;; Parse the top-level Makefile to set additional include paths.
	(makefile-parser top-mk) ))))
