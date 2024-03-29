;;; ligature-mode.el --- Enable ligature fonts -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; Mode to enable font-ligatures in the graphical Emacs window.
;;
;;; Code:

(require 'prog-mode)
(require 'hasklig-ligatures)
(require 'fira-code-ligatures)
(require 'pragmatapro-ligatures)


(defgroup ligatures nil
  "Ligature support for GUI Emacs."
  :group 'fonts
  :version "25.1")


(defcustom ligature-style
  'fira-code
  "Desired ligature style."
  :group 'ligatures
  :options '(hasklig fira-code pragmatapro)
  :type 'choice)


(defvar ligature-font-name
  (eval (intern-soft (concat (symbol-name ligature-style) "-font-name"))))

(defvar ligature-font-symbols-name
  (eval (intern-soft (concat (symbol-name ligature-style) "-font-symbols-name"))))

(defvar ligature-font-ligatures
  (eval (intern-soft (concat (symbol-name ligature-style) "-font-ligatures"))))

(defvar ligature-font-lock-keywords
  (eval (intern-soft (concat (symbol-name ligature-style) "-font-lock-keywords"))))


(defun ligature-font-available-p (font-name)
  "Check if FONT-NAME is available."
  (cl-member font-name (font-family-list) :test #'cl-search))


(defun ligature-replacements (ligatures)
  "Create a list of replacement strings from LIGATURES."
  (cl-loop for (str . cp) in ligatures
           collect (cons str (string ?\t cp))))


(defun ligature-variable-setup ()
  "Setup ligature font variables."
  (setq ligature-font-lock-keywords
        (eval (intern-soft (concat (symbol-name ligature-style) "-font-lock-keywords")))
        ligature-font-ligatures
        (eval (intern-soft (concat (symbol-name ligature-style) "-font-ligatures")))
        ligature-font-symbols-name
        (eval (intern-soft (concat (symbol-name ligature-style) "-font-symbols-name")))
        ligature-font-name
        (eval (intern-soft (concat (symbol-name ligature-style) "-font-name")))))


(defun ligature-prettify-symbols-setup ()
  "Add font ligatures for use with `mode/prettify-symbols-mode'."
  (let ((replace (ligature-replacements ligature-font-ligatures)))
    (setq prettify-symbols-alist (append replace prettify-symbols-alist))))


(defun ligature-prettify-symbols-teardown ()
  "Remove font ligatures from `mode/prettify-symbols-mode'."
  (cl-loop for (str . cp) in ligature-font-ligatures do
           (setq prettify-symbols-alist
                 (remove (cons str (string ?\t cp))
                         prettify-symbols-alist))))


(defun ligature-font-lock-setup ()
  "Add font ligatures for use with `mode/font-lock-mode'."
  (font-lock-add-keywords
   nil
   (cl-loop for (rgx . cp) in ligature-font-lock-keywords collect
            `(,rgx (0 (progn (compose-region
                              (match-beginning 1)
                              (match-end 1)
                              ,(string ?\t cp))))))))


(defun ligature-font-lock-teardown ()
  "Remove font ligatures from `mode/font-lock-mode'."
  (font-lock-remove-keywords
   nil
   (cl-loop for (rgx . cp) in ligature-font-lock-keywords collect
            `(,rgx (0 (progn (compose-region
                              (match-beginning 1)
                              (match-end 1)
                              ,(string ?\t cp))))))))


(defun ligature-mode--frame-hook ()
  "Ensure that new frames receive the font settings."
  (unless (ligature-font-available-p ligature-font-name)
    (error "The '%s' font is not available!" ligature-font-name))
  (unless (ligature-font-available-p ligature-font-symbols-name)
    (error "The '%s' font is not available!" ligature-font-symbols-name))
  (set-frame-font ligature-font-name)
  (ligature-font-lock-setup)
  (ligature-prettify-symbols-setup)
  (cl-loop for (_ . cp) in ligature-font-ligatures do
           (set-fontset-font t cp ligature-font-symbols-name))
  (cl-loop for (_ . cp) in ligature-font-lock-keywords do
           (set-fontset-font t cp ligature-font-symbols-name)))


(defun ligature-mode--enable ()
  "Function called when enabling `ligature-mode'."
  (ligature-variable-setup)
  (set-frame-font ligature-font-name)
  (ligature-mode--frame-hook)
  (if prettify-symbols-mode
      (prettify-symbols-mode)
    (message "Turn on `prettify-symbols-mode' to enable the ligatures."))
  (add-hook 'after-make-frame-functions #'ligature-mode--frame-hook))


(defun ligature-mode--disable ()
  "Function called when disabling `ligature-mode'."
  (ligature-prettify-symbols-teardown)
  (ligature-font-lock-teardown)
  (remove-hook 'after-make-frame-functions #'ligature-mode--frame-hook))


;;;###autoload
(define-minor-mode ligature-mode
  "Minor mode to enable font ligatures in GUI Emacs."
  :group 'ligatures
  :lighter ""
  :global t
  (if ligature-mode
      (ligature-mode--enable)
    (ligature-mode--disable)))


(provide 'ligature-mode)

;;; ligature-mode.el ends here
