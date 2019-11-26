;;; pbrt-mode.el --- Major mode for editing .pbrt files. -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; Provide a major mode for manually editting .pbrt files that describe ray
;; tracable scenes in the PBRT format.  More information regarding this is
;; provided here:
;;
;; http://www.pbrt.org/fileformat.html
;;
;;; Code:

(require 'smie)


;; Allow users to run their own hooks.
(defvar pbrt-mode-hook nil
  "User hooks for PBRT mode.")

(defvar pbrt-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-j") 'newline-and-indent)
    map)
  "Keymap for PBRT major mode.")


(defgroup pbrt-mode nil
  "Major mode for editing"
  :group 'languages)

(defcustom pbrt-indent 4
  "PBRT-indentation width."
  :group 'pbrt-mode
  :type 'integer)

(defcustom pbrt-use-smie t
  "Use SMIE for indentation/navigation."
  :group 'pbrt-mode
  :type 'boolean)

;; Create lists keywords for highlighting.
(defconst pbrt-keywords
  '("Include" "ActiveTransform" "ObjectInstance"
    "NamedMaterial" "MakeNamedMaterial"))

(defconst pbrt-types
  '("bool" "integer" "float" "string" "point" "vector" "normal"
    "xyz" "color" "rgb" "texture" "spectrum" "blackbody"))

(defconst pbrt-transforms
  '("Identity" "Translate" "Scale" "Rotate" "LookAt"
    "Transform" "ConcatTransform" "CoordinateSystem"
    "CoordSysTransform" "ReverseOrientation"))

(defconst pbrt-openers
  '("WorldBegin" "ObjectBegin" "TransformBegin" "AttributeBegin"))

(defconst pbrt-closers
  '("WorldEnd" "ObjectEnd" "AttributeEnd" "TransformEnd"))

(defconst pbrt-states (append pbrt-openers pbrt-closers))

(defconst pbrt-render-options
  '("Camera" "Sampler" "Film" "PixelFilter" "Renderer"
    "Integrator"
    "SurfaceIntegrator" "VolumeIntegrator"
    "Accelerator"))

(defconst pbrt-scene-options
  '("Shape" "LightSource" "AreaLightSource" "Material"
    "Texture" "Volume"))

(defconst pbrt-statements
  (append pbrt-keywords
          pbrt-openers
          pbrt-transforms
          pbrt-render-options
          pbrt-scene-options))


;; Create regular expressions for font-locking.
(defconst pbrt-keywords-regexp       (regexp-opt pbrt-keywords        'words))
(defconst pbrt-types-regexp          (regexp-opt pbrt-types           'words))
(defconst pbrt-transforms-regexp     (regexp-opt pbrt-transforms      'words))
(defconst pbrt-states-regexp         (regexp-opt pbrt-states          'words))
(defconst pbrt-render-options-regexp (regexp-opt pbrt-render-options  'words))
(defconst pbrt-scene-options-regexp  (regexp-opt pbrt-scene-options   'words))
(defconst pbrt-statements-rx         (regexp-opt pbrt-statements      'words))
(defconst pbrt-openers-rx            (regexp-opt pbrt-openers         'word))
(defconst pbrt-closers-rx            (regexp-opt pbrt-closers         'word))
(defconst pbrt-identifier-regexp
  (format "\\(%s\\)[[:space:]]+\\([a-zA-Z0-9_]+\\)"
	  (mapconcat 'identity pbrt-types "\\|"))
  "A regular expression that can match all PBRT identifiers.")


(defmacro pbrt--create-matcher (fname regexp)
  "Create a inside-string font-lock matcher with FNAME for REGEXP."
  `(defun ,fname (end)
     "Search for format specifiers within strings up to END."
     (let ((pos)
           (case-fold-search nil))
       (while (and (setq pos (re-search-forward ,regexp end t))
                   (null (nth 3 (syntax-ppss pos)))))
       pos)))


(pbrt--create-matcher pbrt--types-matcher pbrt-types-regexp)
(pbrt--create-matcher pbrt--identifier-matcher pbrt-identifier-regexp)


;; Create the list for font-lock.
;; Each class of keyword is given a particular face.
(defvar pbrt-font-lock-keywords
  `((,pbrt-keywords-regexp        . font-lock-keyword-face)
    (pbrt--types-matcher      (0 'font-lock-type-face prepend))
    (pbrt--identifier-matcher (2 'font-lock-variable-name-face prepend))
    (,pbrt-transforms-regexp      . font-lock-constant-face)
    (,pbrt-states-regexp          . font-lock-builtin-face)
    (,pbrt-render-options-regexp  . font-lock-function-name-face)
    (,pbrt-scene-options-regexp   . font-lock-keyword-face)))


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


(defvar pbrt-smie-grammar
  (smie-prec2->grammar
   (smie-bnf->prec2
    `((insts (inst) (insts ";" insts))
      (inst ("WorldBegin" insts "WorldEnd")
            ("AttributeBegin" insts "AttributeEnd")
            ("TransformBegin" insts "TransformEnd")
            ("ObjectBegin" inst "ObjectEnd")))
    '((assoc ";"))))
  "BNF grammar describing the PBRT languge for `smie'.")


(defun pbrt-smie-rules (kind token)
  "Perform indentation of KIND on TOKEN using the `smie' engine."
  (pcase (list kind token)
    (`(:elem basic) pbrt-indent)
    (`(:list-intro "") 0)
    (`(:elem args) 0)
    (`(:after ,(or "WorldEnd" "AttributeEnd" "TransformEnd" "ObjectEnd"))
     (smie-rule-parent))))

(defun pbrt-looking-back-at-opener-p (&optional move)
  "Is there an opening statement behind us after the optional MOVE?"
  (save-excursion
    (when move
      (forward-comment (- (point)))
      (skip-syntax-backward "w"))
    (looking-at-p pbrt-openers-rx)))

(defun pbrt-looking-back-at-closer-p (&optional move)
  "Is there a closing statement behind us after the optional MOVE?"
  (save-excursion
    (when move
      (forward-comment (- (point)))
      (skip-syntax-backward "w"))
    (looking-at-p pbrt-closers-rx)))

(defun pbrt-space-behind-p ()
  "Is there whitespace or comments behind us?"
  (let ((pos (point)))
    (save-excursion
      (forward-comment (- (point)))
      (< (point) pos))))


(defun pbrt-looking-at-new-statement-p (&optional move)
  "Is there a new PBRT statement in front of after the optional MOVE?"
  (save-excursion
    (when move
      (forward-comment (point-max)))
    (looking-at-p pbrt-statements-rx)))


(defun pbrt-smie-forward-token ()
  "Go forwards to the next SMIE token."
  (let ((start-pos (point))
        (space-behind (pbrt-space-behind-p)))
    (forward-comment (point-max))
    (cond

     ((and (> (point) start-pos)       ; Emit virtual statement separator.
           ;; Don't insert another virtual separator if between statements.
           (not space-behind)
           (not (pbrt-looking-back-at-opener-p :move))
           (or
            (pbrt-looking-back-at-closer-p :move)
            (pbrt-looking-at-new-statement-p :move)))
      ";")
     (t
      (smie-default-forward-token)))))

(defun pbrt-smie-backward-token ()
  "Go backwards to the previous SMIE token."
  (let ((start-pos (point)))
    (forward-comment (- (point)))
    (cond
     ((and (< (point) start-pos)        ; Emit virtual statement separator.
           (not (pbrt-looking-back-at-opener-p :move))
           (or
            (pbrt-looking-back-at-closer-p :move)
            (pbrt-looking-at-new-statement-p :move)))
      ";")
     ((bobp)
      ";")
     (t
      (smie-default-backward-token)))))


(defun pbrt-debug-lexer (fun)
  "Debug the lexer FUN."
  (lambda ()
    (let ((tok (funcall fun))
          (nam (symbol-name fun))
          (line (line-number-at-pos))
          (col (current-column))
          (pos (point)))
      (princ (format "%s: '%s' at %d:%d (%d).\n" nam tok line col pos))
      tok)))


;; Syntax table.
(defvar pbrt-syntax-table
  (let ((table (make-syntax-table)))
    ;; Double quotes used for comments.
    (modify-syntax-entry ?' "\"" table)
    (modify-syntax-entry ?\" "\"" table)
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
(define-derived-mode pbrt-mode prog-mode "PBRT"
  "Major mode for editing PBRT scene files.

\\{pbrt-mode-map}"
  :syntax-table pbrt-syntax-table

  ;; Code for syntax highlighting.
  (setq-local font-lock-defaults '(pbrt-font-lock-keywords))

  ;; Code for managing comments.
  (setq-local comment-start "# ")
  (setq-local comment-start-skip "#+\\s-*")

  ;; Code for indentation.
  (setq-local indent-tabs-mode nil)
  (if pbrt-use-smie
      (smie-setup pbrt-smie-grammar #'pbrt-smie-rules
                  :forward-token #'pbrt-smie-forward-token
                  :backward-token #'pbrt-smie-backward-token)
    (setq-local indent-line-function #'pbrt-indent-line)))


(provide 'pbrt-mode)

;;; pbrt-mode.el ends here
