;; Add mode association for files.
(add-to-list 'auto-mode-alist '("emacs\\'" . emacs-lisp-mode))
(add-to-list 'auto-mode-alist '("bashrc\\'" . shell-script-mode))
(add-to-list 'auto-mode-alist '("bash_aliases\\'" . shell-script-mode))

;; OpenCL Mode.
(add-to-list 'auto-mode-alist '("\\.cl\\'" . c-mode))

;; GLSL Mode
(autoload 'glsl-mode "glsl-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.vert\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.frag\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.vs\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.fs\\'" . glsl-mode))

;; Add makefile modes.
(add-to-list 'auto-mode-alist '("\\.mak\\'" . makefile-mode))

;; Set Scons files to python mode.
(add-to-list 'auto-mode-alist '("[Ss][Cc]onstruct" . python-mode))
(add-to-list 'auto-mode-alist '("[Ss][Cc]onscript" . python-mode))

;; Associate PBRT files with pbrt-model.
(add-to-list 'auto-mode-alist '("\\.pbrt\\'" . pbrt-mode))
