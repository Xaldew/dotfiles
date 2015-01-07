;; C++

;; Setup functions for starting header files in the correct modes.
(defun c-header-test-p ()
  "Test if the header file is a C-mode header"
  (file-exists-p (concat (file-name-sans-extension (buffer-file-name)) ".c")))

(defun c++-header-test-p ()
  "Test if the header file is a C++-mode header."
  (or (file-exists-p
       (concat (file-name-sans-extension (buffer-file-name)) ".C"))
      (file-exists-p
       (concat (file-name-sans-extension (buffer-file-name)) ".cc"))
      (file-exists-p
       (concat (file-name-sans-extension (buffer-file-name)) ".cxx"))
      (file-exists-p
       (concat (file-name-sans-extension (buffer-file-name)) ".c++"))
      (file-exists-p
       (concat (file-name-sans-extension (buffer-file-name)) ".cpp"))
      (c++-scan-header-p)))

(defun c++-scan-header-p ()
  "Scan the header and return true if any C++ exclusive keywords are detected."
  (let (is-c++-header)
    (save-excursion
      (while (and (not (eobp)) (not is-c++-header))
	(if (looking-at "[ \t]*\\(class\\|namespace\\|template\\)")
	    (setq is-c++-header t)
	  (forward-line))))
    (setq is-c++-header is-c++-header)))

(defconst my-c++-style
  '("stroustrup"
    (my-coding-style  . "linux")
    (indent-tabs-mode . nil)     ; Use spaces rather than tabs.
    (c-basic-offset   . 4)       ; Indent with 4 spaces.
    (c-offsets-alist  . ((inline-open         . 0)
			 (brace-list-open     . 0)
			 (inextern-lang       . 0)
			 (innamespace         . 0)
			 (statement-case-open . +))) ))
(c-add-style "my-c++-style" my-c++-style)

(defun my-c++-mode-hook ()
  (c-set-style "my-c++-style")
  (ggtags-mode)
  (auto-fill-mode)
  (flycheck-mode)
  (setq flycheck-gcc-language-standard   "c++11")
  (setq flycheck-clang-language-standard "c++11"))
(add-hook 'c++-mode-hook 'my-c++-mode-hook)
(add-to-list 'magic-mode-alist '(c-header-test-p   . c-mode))
(add-to-list 'magic-mode-alist '(c++-header-test-p . c++-mode))
