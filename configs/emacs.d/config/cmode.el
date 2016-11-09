;;; cmode.el --- Personal C configuration.
;;
;;; Commentary:
;; Setup various C-styles for different projects.
;;
;;; Code:

(defconst misra-c-style
  '("bsd"
    (indent-tabs-mode . nil)
    (tab-width . 4)
    (c-basic-offset . 4)
    (c-hanging-braces-alist . ((brace-list-open)
			       (brace-entry-open)
			       (statement-cont)
			       (substatement-open before after)
			       (block-close . c-snug-do-while)
			       (extern-lang-open before after)
			       (namespace-open before after)
			       (module-open after)
			       (composition-open after)
			       (inexpr-class-open after)
			       (inexpr-class-close before)
			       (arglist-cont-nonempty)))
    (c-offsets-alist . ((inextern-lang  . 0)
			(innamespace    . 0))))
  "Misra C-style with modifications for the ARM MVE model/firmware.")
(c-add-style "misra" misra-c-style)


;; Linux kernel coding c-style.
(declare-function c-langelem-pos     "cc-defs")
(declare-function c-langelem-2nd-pos "cc-defs")
(defvar c-syntactic-element)
(defvar c-basic-offset)

(defun c-lineup-arglist-tabs-only (&rest ignored)
  "Line up argument lists by tabs, not spaces.

Additional arguments are IGNORED."
  (let* ((anchor (c-langelem-pos c-syntactic-element))
	 (column (c-langelem-2nd-pos c-syntactic-element))
	 (offset (- (1+ column) anchor))
	 (steps (floor offset c-basic-offset)))
    (* (max steps 1) c-basic-offset)))

(defconst linux-tabs-style
  '("linux"
    (indent-tabs-mode . t)
    (c-offsets-alist  . ((inextern-lang         . 0)
			 (arglist-cont-nonempty .
						(c-lineup-gcc-asm-reg
						 c-lineup-arglist-tabs-only))))
    (c-cleanup-list . (brace-else-brace
                       brace-elseif-brace)))
  "Linux kernel coding style forbidding use of spaces as whitespace.")
(c-add-style "linux-tabs-style" linux-tabs-style)


;; Setup functions and variables to guess the style to use.
(defvar my-c-styles-alist
  (copy-tree '((".*/linux.*/.*\\.[ch]$"    . "linux-tabs-style")
               (".*/.*kernel.*/.*\\.[ch]$" . "linux-tabs-style")
               (".*/.*linux/.*\\.[ch]$"    . "linux-tabs-style")
               (".*/.*mve*.*\\.[ch]$"      . "misra")
               (".*c-utils.*\\.[ch]$"      . "linux-tabs-style")))
  "A list of regular expressions to match styles for the c-style guesser.")


(defun my-c-style-guesser (filename)
  "Guess the C style we should use based on the FILENAME of the buffer."
  (assoc-default filename my-c-styles-alist 'string-match))


(defun my-guess-c-style ()
  "Attempt to figure out the C-style for the current buffer.

Note that some modes can get in the way of the variable
`buffer-file-name' so we check that it is non-nil first."
  (when (and buffer-file-name)
    (let ((style (my-c-style-guesser (buffer-file-name))))
      (when style
        (message (format "Using style: %s." style))
        (c-set-style style)))))


(defun my-c-mode-hook ()
  "My personal `c-mode' hook."
  (auto-fill-mode 1)
  (my-guess-c-style))


;; Add personal c-mode setup function to c-mode-hook.
(add-hook 'c-mode-hook #'my-c-mode-hook)
(add-hook 'c-mode-hook #'cwarn-mode)

;;; cmode.el ends here
