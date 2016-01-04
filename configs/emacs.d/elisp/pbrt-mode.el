;;; pbrt-mode.el --- Major mode for editing .pbrt files. -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; Provide a major mode for manually editting .pbrt files that describe ray
;; tracable scenes in the PBRT format. More information regarding this is
;; provided here:
;; http://www.pbrt.org/fileformat.html
;;
;;; Code:

(eval-when-compile
  (require 'newcomment))

;; Allow users to run their own hooks.
(defvar pbrt-mode-hook nil "User hooks for PBRT mode.")

(defvar pbrt-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-j" 'newline-and-indent)
    (define-key map [remap comment-dwim] 'pbrt-comment-dwim)
    map)
  "Keymap for PBRT major mode.")


(defgroup pbrt-mode nil
  "Major mode for editing"
  :group 'languages)

(defcustom pbrt-indent 4 "PBRT-indentation width."
  :group 'pbrt-mode)

;; Create lists keywords for highlighting.
(defvar pbrt-keywords '("Include" "ActiveTransform" "ObjectInstance"))

(defvar pbrt-types '("bool" "integer" "float" "string" "point" "vector" "normal"
		     "xyz" "color" "rgb" "texture" "spectrum" "blackbody"))

(defvar pbrt-transforms '("Identity" "Translate" "Scale" "Rotate" "LookAt"
			  "Transform" "ConcatTransform" "CoordinateSystem"
			  "CoordSysTransform" "ReverseOrientation"))

(defvar pbrt-states '("WorldBegin" "WorldEnd"
		      "ObjectBegin" "ObjectEnd"
		      "AttributeBegin" "AttributeEnd"
		      "TransformBegin" "TransformEnd"))

(defvar pbrt-render-options '("Camera" "Sampler" "Film" "PixelFilter" "Renderer"
			      "SurfaceIntegrator" "VolumeIntegrator"
			      "Accelerator"))

(defvar pbrt-scene-options '("Shape" "LightSource" "AreaLightSource" "Material"
			     "Texture" "Volume"))
(defvar pbrt-statements
  (append pbrt-transforms pbrt-render-options pbrt-scene-options))


;; Create regular expressions for font-locking.
(defvar pbrt-keywords-regexp        (regexp-opt pbrt-keywords        'words))
(defvar pbrt-types-regexp           (regexp-opt pbrt-types           'words))
(defvar pbrt-transforms-regexp      (regexp-opt pbrt-transforms      'words))
(defvar pbrt-states-regexp          (regexp-opt pbrt-states          'words))
(defvar pbrt-render-options-regexp  (regexp-opt pbrt-render-options  'words))
(defvar pbrt-scene-options-regexp   (regexp-opt pbrt-scene-options   'words))
(defvar pbrt-identifier-regexp
  (format "\\(%s\\)[[:space:]]+\\([a-zA-Z0-9_]+\\)"
	  (mapconcat 'identity pbrt-types "\\|"))
  "A regular expression that can match all PBRT identifiers.")


;; Create the list for font-lock.
;; Each class of keyword is given a particular face.
;; Note that sexps with 4 values use the matchgroup in the second value
;; and overrides previously defined font-locks if the fourth is non-nil.
(defvar pbrt-font-lock-keywords
  `((,"#.*"                       . font-lock-comment-face)
    (,pbrt-keywords-regexp        . font-lock-keyword-face)
    (,pbrt-types-regexp           0 font-lock-type-face           nil)
    (,pbrt-identifier-regexp      2 font-lock-variable-name-face  nil)
    (,pbrt-transforms-regexp      . font-lock-constant-face)
    (,pbrt-states-regexp          . font-lock-builtin-face)
    (,pbrt-render-options-regexp  . font-lock-function-name-face)
    (,pbrt-scene-options-regexp   . font-lock-keyword-face)
    (,"\"[^\"]*\""                . font-lock-string-face)
    (,"\""                        . font-lock-string-face) ))


;; Regex variables for indentation.
(defvar pbrt-block-start-regexp
  "[ \t]*\\(ObjectBegin\\|AttributeBegin\\|TransformBegin\\)")
(defvar pbrt-block-close-regexp
  "[ \t]*\\(ObjectEnd\\|AttributeEnd\\|TransformEnd\\)")
(defvar pbrt-statement-regexp
  (format "[ \t]*#?[ \t]*\\(%s\\)" (mapconcat 'identity pbrt-statements "\\|")))
(defvar pbrt-states-indent-regexp
  (format "[ \t]*\\(%s\\)" (mapconcat 'identity pbrt-states "\\|")))
(defvar pbrt-keywords-indent-regexp
  (format "[ \t]*\\(%s\\)" (mapconcat 'identity pbrt-keywords "\\|")))


;; Create an indentation command.
(defun pbrt-indent-line ()
  "Indent current line as PBRT code.

PBRT will be indented according to the following rules:

1. If we are at the beginning of the buffer, indent to column 0.
2. If we are starting on a line that starts with a comment marker,
   indent to column 0.
3. If we are currently at an 'END' line, de-indent to the previous statement
   line.
4. If we first see an 'END' line before our current line, we should indent the
   current line similarly to it.
5. If we see a 'BEGIN' line before our current line, we should increase our
   indentation relative to the previous line.
6. If we are on a non-whitespace line and a previous line starts with a
   statement, then add another indentation to mark that it belongs to that
   statement.
7. If none of the above applies, do not indent at all.


As an example, see the following PBRT file:

LookAt 0 10 100   0 -1 0 0 1 0
Camera 'perspective' 'float fov' [30]
Film 'image' 'string filename' ['simple.exr']
     'integer xresolution' [200] 'integer yresolution' [200] # Rule 6.

WorldBegin
TransformBegin
    CoordSysTransform 'camera'            # Rule x.
    AttributeBegin                        # Rule y.
        LightSource 'distant'             # Rule x.
                    'point from' [0 0 0]  # Rule 6.
                    'point to'   [0 0 1]  # Rule 6.
                    'rgb L'    [3 3 3]    # Rule 6.
    AttributeEnd                          # Rule xx.
TransformEnd                              # Rule xx.
WorldEnd"
  (interactive)
  (save-excursion
    (beginning-of-line)
    ;; Check for rule 1 and 2.
    (if (or (looking-at-p "#.*\n") (bobp))
	(indent-line-to 0)
      (let ((not-indented t)
	    (cur-indent nil))
	(if (looking-at-p pbrt-block-close-regexp)           ; Check for rule 3.
	    (progn
	      (save-excursion
		(while (not (looking-at-p pbrt-statement-regexp))
		  (forward-line -1))
		(setq cur-indent (max 0 (- (current-indentation) pbrt-indent))))
	      )
	  (save-excursion
	    (while (and not-indented (not (bobp)))           ; Check for rule 7.
	      (forward-line -1)
	      (cond ((looking-at-p pbrt-block-close-regexp)  ; Check for rule 4.
		     (setq cur-indent (current-indentation))
		     (setq not-indented nil))
		    ((looking-at-p pbrt-block-start-regexp)  ; Check for rule 5.
		     (setq cur-indent (+ (current-indentation) pbrt-indent))
		     (setq not-indented nil)))
	      )))
	;; Check for rule 5.
	(save-excursion
	  (while (and (not (bobp))
		      (not (looking-at-p "[ \t]*\n"))
		      (not (looking-at-p pbrt-statement-regexp))
		      (not (looking-at-p pbrt-states-indent-regexp))
		      (not (looking-at-p pbrt-keywords-indent-regexp)))
	    (forward-line -1)
	    (when (looking-at-p pbrt-statement-regexp)
	      (if not-indented
		  (setq cur-indent (+ (current-indentation) pbrt-indent))
		(setq cur-indent (+ cur-indent pbrt-indent))))
	    ))
	;; Do the actual indentation.
	(if cur-indent
	    (indent-line-to cur-indent)
	  (indent-line-to 0)) )))
  ;; Move to the start of the indentation.
  (when (looking-at "[ \t]+")
    (move-to-column (current-indentation)) ))


(defun pbrt-comment-dwim (arg)
  "Comment or uncomment current line or region in a smart way.

ARG is passed down to `comment-dwim'."
  (interactive "*P")
  (let ((comment-start "#") (comment-end ""))
    (comment-dwim arg)))


;; Syntax table.
(defvar pbrt-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Shell style comment: “# ...”
    (modify-syntax-entry ?#  "<" table)
    (modify-syntax-entry ?\n ">" table)
    ;; Dash and underscores are word constituents.
    (modify-syntax-entry ?-  "w" table)
    (modify-syntax-entry ?_  "w" table)
    ;; Brackets are used to group numbers.
    (modify-syntax-entry ?\[ "(]")
    (modify-syntax-entry ?\] ")[")
    table)
  "Syntax table for `pbrt-mode'.")


;; Associate PBRT files with pbrt mode.
;;;###autoload
(progn
  (add-to-list 'auto-mode-alist '("\\.pbrt\\'" . pbrt-mode)))


;;;###autoload
(define-derived-mode pbrt-mode fundamental-mode "PBRT"
  "Major mode for editing PBRT scene files."
  (kill-all-local-variables)
  (set-syntax-table pbrt-mode-syntax-table)
  (use-local-map pbrt-mode-map)

  ;; Code for syntax highlighting.
  (setq-local font-lock-defaults '(pbrt-font-lock-keywords t))

  ;; Code for pbrt indentation.
  (setq-local indent-line-function 'pbrt-indent-line)
  (setq-local indent-tabs-mode nil)

  (setq major-mode 'pbrt-mode)
  (setq mode-name "PBRT")
  (run-hooks 'pbrt-mode-hook))


(provide 'pbrt-mode)

;;; pbrt-mode.el ends here
