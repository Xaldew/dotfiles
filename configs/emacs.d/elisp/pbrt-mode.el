;; Allow users to run their own hooks.
(defvar pbrt-mode-hook nil)

(defvar pbrt-mode-map
  (let ((map (make-keymap)))
    (define-key map "\C-j" 'newline-and-indent)
    (define-key map [remap comment-dwim] 'pbrt-comment-dwim)
    map)
  "Keymap for PBRT major mode")

;; Create lists keywords for highlighting.
(setq pbrt-keywords '("Include" "ActiveTransform" "ObjectInstance"))

(setq pbrt-types '("bool" "integer" "float" "string" "point" "vector" "normal"
		   "xyz" "color" "rgb" "texture" "spectrum" "blackbody"))

(setq pbrt-transforms '("Identity" "Translate" "Scale" "Rotate" "LookAt"
			"Transform" "ConcatTransform" "CoordinateSystem"
			"CoordSysTransform" "ReverseOrientation"))

(setq pbrt-states '("WorldBegin" "WorldEnd"
		    "ObjectBegin" "ObjectEnd"
		    "AttributeBegin" "AttributeEnd"
		    "TransformBegin" "TransformEnd"))

(setq pbrt-render-options '("Camera" "Sampler" "Film" "PixelFilter" "Renderer"
			    "SurfaceIntegrator" "VolumeIntegrator"
			    "Accelerator"))

(setq pbrt-scene-options '("Shape" "LightSource" "AreaLightSource" "Material"
			   "Texture" "Volume"))


;; Transform each of the types, and then concatenate them.
;; (Saved for future reference.)
(setq pbrt-types-regexp
      (mapconcat
       'identity
       (mapcar (lambda (a)
		 (format "%s[[:space:]]+[a-zA-Z_]+" a)) pbrt-types)
       "\\|"
       )
      )


;; Create a regular expression that matches all identifiers.
(setq pbrt-identifier-regexp
      (format "\\(%s\\)[[:space:]]+\\([a-zA-Z_]+\\)"
	      (mapconcat 'identity pbrt-types "\\|")))


;; Create regular expressions from the above lists.
(setq pbrt-keywords-regexp        (regexp-opt pbrt-keywords        'words))
(setq pbrt-types-regexp           (regexp-opt pbrt-types           'words))
(setq pbrt-transforms-regexp      (regexp-opt pbrt-transforms      'words))
(setq pbrt-states-regexp          (regexp-opt pbrt-states          'words))
(setq pbrt-render-options-regexp  (regexp-opt pbrt-render-options  'words))
(setq pbrt-scene-options-regexp   (regexp-opt pbrt-scene-options   'words))


;; Create the list for font-lock.
;; Each class of keyword is given a particular face.
;; Note that sexps with 4 values use the matchgroup in the second value
;; and overrides previously defined font-locks if the fourth is non-nil.
(setq pbrt-font-lock-keywords
      `((,"#.*"                       . font-lock-comment-face)
	(,pbrt-keywords-regexp        . font-lock-type-face)
	(,pbrt-types-regexp           0 font-lock-type-face           nil)
	(,pbrt-identifier-regexp      2 font-lock-variable-name-face  nil)
	(,pbrt-transforms-regexp      . font-lock-constant-face)
	(,pbrt-states-regexp          . font-lock-builtin-face)
	(,pbrt-render-options-regexp  . font-lock-function-name-face)
	(,pbrt-scene-options-regexp   . font-lock-keyword-face)
	(,"\"[^\"]*\""                . font-lock-string-face)
	(,"\""                        . font-lock-string-face)
	))


;; Create an indentation command.
(defun pbrt-indent-line ()
  "Indent current line as PBRT code.

PBRT will be indented according to the following rules:

1. If we are at the beginning of the buffer, indent to column 0.
2. If we are currently at a 'END' line, de-indent to the previous line.
3. If we first see an 'END' line before our current line, we should indent the
   current line similarly to it.
4. If we see a 'BEGIN' line before our current line, we should increase our
   indentation relative to the previous line.
5. If none of the above applies, do not indent at all.

As an example, see the following PBRT file:

LookAt 0 10 100   0 -1 0 0 1 0
Camera 'perspective' 'float fov' [30]
Film 'image' 'string filename' ['simple.exr']
     'integer xresolution' [200] 'integer yresolution' [200] # Rule z.

WorldBegin

TransformBegin

    CoordSysTransform 'camera'            # Rule x.
    AttributeBegin                        # Rule y.

        LightSource 'distant'
                    'point from' [0 0 0]
                    'point to'   [0 0 1]  # Rule z.
                    'rgb L'    [3 3 3]

    AttributeEnd                 # Rule xx.
TransformEnd                     # Rule xx.

WorldEnd

"
  (interactive)
  (beginning-of-line)
  (if (bobp)
      (indent-line-to 0)
    (let ((not-indented t) cur-indent)
      (if (looking-at "regex") ; Check for rule "regex1."
	  (progn
	    (save-excursion
	      (forward-line -1)
	      (setq cur-indent (- (current-indentation) default-tab-width)))

	    (if (< cur-indent 0) ; Safety check - Don't indent past left margin.
		(setq cur-indent 0)))

	(save-excursion
	  (while not-indented
	    (forward-line -1)
	    (if (looking-at "regex2") ; Check for rule "regex2."
		(progn
		  (setq cur-indent (current-indentation))
		  (setq not-indented nil))
	      (if (looking-at "regex3") ; Check for rule "regex3."
		  (progn
		    (setq cur-indent (+ (current-indentation) default-tab-width))
		    (setq not-indented nil))
		)
	      (if (bobp) ; Check for rule xx.
		  (setq not-indented nil)
		)
	      )
	    )
	  )
	)
      )
    )
  )

;; Command to comment/uncomment text in PBRT-mode.
(defun pbrt-comment-dwim (arg)
  "Comment or uncomment current line or region in a smart way.
For detail, see `comment-dwim'."
  (interactive "*P")
  (require 'newcomment)
  (let ((comment-start "#") (comment-end ""))
    (comment-dwim arg)))


;; Syntax table.
(defvar pbrt-syntax-table nil "Syntax table for `pbrt-mode'.")
(setq pbrt-syntax-table
      (let ((synTable (make-syntax-table)))

        ;; Shell style comment: “# ...”
        (modify-syntax-entry ?# "< b" synTable)
        (modify-syntax-entry ?\n "> b" synTable)

        synTable))


;; Define the pbrt-mode.
(define-derived-mode pbrt-mode fundamental-mode "PBRT"
  "Major mode for editing PBRT scene files."
  (kill-all-local-variables)
  (set-syntax-table pbrt-syntax-table)
  (use-local-map pbrt-mode-map)

  ;; Code for syntax highlighting.
  (setq-local font-lock-defaults '(pbrt-font-lock-keywords t))

  (setq major-mode 'pbrt-mode)
  (setq mode-name "pbrt")
  (run-hooks 'pbrt-mode-hook)


  ;; Clear memory from redundant variables.
  ;; (setq pbrt-keywords-regexp nil)
  ;; (setq pbrt-types-regexp nil)
  ;; (setq pbrt-constants-regexp nil)
  ;; (setq pbrt-events-regexp nil)
  ;; (setq pbrt-functions-regexp nil)
  )

(provide 'pbrt-mode)
