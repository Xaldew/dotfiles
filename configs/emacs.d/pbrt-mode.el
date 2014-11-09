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

(setq pbrt-types '("bool" "integer" "float" "string" "point" "vector ""normal"
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


;; Create regular expressions from the above lists.
(setq pbrt-keywords-regexp        (regexp-opt pbrt-keywords        'words))
(setq pbrt-type-regexp            (regexp-opt pbrt-types           'words))
(setq pbrt-transforms-regexp      (regexp-opt pbrt-transforms      'words))
(setq pbrt-states-regexp          (regexp-opt pbrt-states          'words))
(setq pbrt-render-options-regexp  (regexp-opt pbrt-render-options  'words))
(setq pbrt-scene-options-regexp   (regexp-opt pbrt-scene-options   'words))


;; Create the list for font-lock.
;; Each class of keyword is given a particular face.
(setq pbrt-font-lock-keywords
      `((,pbrt-keywords-regexp        . font-lock-type-face)
	(,pbrt-type-regexp            . font-lock-type-face)
	(,pbrt-transforms-regexp      . font-lock-constant-face)
	(,pbrt-states-regexp          . font-lock-builtin-face)
	(,pbrt-render-options-regexp  . font-lock-function-name-face)
	(,pbrt-scene-options-regexp   . font-lock-keyword-face)
	))


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
(define-derived-mode pbrt-mode fundamental-mode
  "pbrt-mode"
  "Major mode for editing PBRT scene files."
  :syntax-table pbrt-syntax-table

  (setq mode-name "pbrt")

  ;; Code for syntax highlighting.
  (setq font-lock-defaults '((pbrt-font-lock-keywords)))


  ;; Clear memory from redundant variables.
  ;; (setq pbrt-keywords-regexp nil)
  ;; (setq pbrt-types-regexp nil)
  ;; (setq pbrt-constants-regexp nil)
  ;; (setq pbrt-events-regexp nil)
  ;; (setq pbrt-functions-regexp nil)
  )

(provide 'pbrt-mode)
