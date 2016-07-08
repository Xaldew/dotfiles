;;; anycov.el --- Interface for coverage overlays  -*- lexical-binding: t; -*-

;; Copyright (C) 2016 Free Software Foundation, Inc.

;; Author: Gustaf Waldemarson <gustaf.waldemarson@gmail.com>
;; Keywords: convenience, tools, coverage
;; Created: 2016-07-01
;; Version: 0.0.1
;; Package-Requires: ((async "1.9") (popup "1.5"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package allows Emacs to display coverage information from various
;; sources.  These include:
;; * C/C++ via gcov/lcov.
;; * Go.
;; * Any 'Cobertura' compliant XML schema (kcov, coverage.py).

;;; Code:

(require 'cl-lib)
(require 'xml)
(require 'popup)

;;; Utils:

(defun assoc-recursive (alist &rest keys)
  "Recursively search ALIST for KEYS."
  (while keys
    (setq alist (cdr (assoc (pop keys) alist))))
  alist)


(defvar anycov--comment-faces
  '(font-lock-string-face
    font-lock-comment-face
    font-lock-doc-face
    mmm-default-submode-face)
  "Faces corresponding to comments in various buffers.")


(defun anycov--comment-p (pos)
  "Detect if POS is within a comment."
  (let ((f (get-text-property pos 'face)))
    (memq f anycov--comment-faces)))


;;; Custom variables:

(defgroup anycov nil
  "Test coverage overlay for Emacs."
  :group 'tools)


(defcustom anycov-overlay-priority 10
  "Priority for all overlays created by this package.")


(defface anycov-line-hit
  '((t :inherit font-lock-constant-face))
  "Face used for lines hit by the line coverage analysis."
  :group 'anycov)

(defface anycov-line-miss
  '((t :inherit font-lock-constant-face))
  "Face used for lines missed by the line coverage analysis."
  :group 'anycov)

(defface anycov-branch-hit
  '((t :inherit font-lock-constant-face))
  "Face used for lines hit by the line coverage analysis."
  :group 'anycov)

(defface anycov-branch-miss
  '((t :inherit font-lock-constant-face))
  "Face used for lines missed by the line coverage analysis."
  :group 'anycov)


(defvar anycov--line-hits (make-hash-table :test 'equal)
  "Association list over files and line hits.")

(defvar anycov--branch-hits (make-hash-table :test 'equal)
  "Association list over files and line hits.")

(defvar anycov--loaded-file nil
  "The currently loaded coverage data file.")


(defun anycov--parse-sources (xml)
  "Read the `sources' tag with XML directories from the XML."
  (let ((srcs (assoc-recursive xml 'coverage 'sources))
        (out))
    (dolist (src srcs out)
      (when (and (listp src)
                 (eq 'source (car src)))
        (setq out (append out (cddr src)))))))


(defun anycov--parse-class (class)
  "Read the `Cobertura' CLASS."
  (let ((lines (alist-get 'lines class))
        (lines-out)
        (branch-out))
    (dolist (l lines)
      (when (and l (listp l))
        (setq l (nth 1 l))
        (setq lines-out
              (plist-put lines-out
                         (string-to-number (alist-get 'number l))
                         (string-to-number (alist-get 'hits l))))
        (when (alist-get 'branch l)
          (setq branch-out (plist-put branch-out
                                      (string-to-number (alist-get 'number l))
                                      (alist-get 'missing-branches l))))))
    (list lines-out branch-out)))


(defun anycov--parse-xml (file)
  "Read the `Cobertura' compliant XML schema in FILE."
  (let* ((xml (xml-parse-file file))
         (src-dirs (anycov--parse-sources xml))
         (pkgs (assoc-recursive xml 'coverage 'packages))
         (file)
         (lines))
    (dolist (pkg pkgs)
      (when (and (listp pkg)
                 (eq 'package (car pkg)))
        (dolist (cls (alist-get 'classes pkg))
          (when (and (listp cls)
                     (eq 'class (car cls)))
            (setq file (concat (file-name-as-directory (nth 0 src-dirs))
                               (alist-get 'filename (nth 1 cls))))
            (setq lines (anycov--parse-class cls))
            (puthash file (nth 0 lines) anycov--line-hits)
            (puthash file (nth 1 lines) anycov--branch-hits)))))))


(defun anycov--parse-lcov (file)
  "Read the `lcov' formatted FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (let ((lines '())
          (branches '())
          (total-branches '())
          (funcs '())
          (m0)
          (m1)
          (cnt)
          (src-file))
      (while (not (eobp))
        (cond
         ((looking-at-p "TN:")         ; Test numbers.
          (setq src-file nil))
         ((looking-at "SF:\\(.*\\)")   ; Source file-name.
          (setq src-file (buffer-substring-no-properties
                          (match-beginning 1)
                          (match-end 1))))
         ((looking-at "FN:\\([[:digit:]]+\\),\\(.*\\)")  ; Function name
          (setq m0 (string-to-number (buffer-substring (match-beginning 1)
                                                       (match-end 1))))
          (setq m1 (buffer-substring-no-properties (match-beginning 2)
                                                   (match-end 2)))
          (setq lines (plist-put lines m0 0))
          (setq funcs (plist-put funcs m1 m0)))
         ((looking-at "FNDA:\\([[:digit:]]+\\),\\(.*\\)")  ; Function hit count.
          (setq m0 (string-to-number (buffer-substring (match-beginning 1)
                                                       (match-end 1))))
          (setq m1 (buffer-substring-no-properties (match-beginning 2)
                                                   (match-end 2)))
          (when (plist-member funcs m1)
            (setq lines (plist-put lines (plist-get funcs m1) m0))))
         ((looking-at-p "FNF:\\([[:digit:]]+\\)")  ; Functions found.
          nil)
         ((looking-at-p "FNH:\\([[:digit:]]+\\)")  ; Functions hit.
          nil)
         ((looking-at "BRDA:\\([[:digit:]]+\\),\\([[:digit:]]+\\),\\([[:digit:]]+\\),\\(.*\\)")
          ;; Branch hit.
          (setq m0 (string-to-number (buffer-substring (match-beginning 1)
                                                       (match-end 1))))
          (setq m1 (buffer-substring-no-properties (match-beginning 4)
                                                   (match-end 4)))
          ;; TODO: Save the branch frequency counts.
          (setq cnt (or (plist-get total-branches m0) 0))
          (setq total-branches (plist-put total-branches m0 (1+ cnt)))
          (if (string= m1 "-")
              (progn
                (setq cnt (or (plist-get branches m0) 0))
                (setq branches (plist-put branches m0 cnt)))
            (setq cnt (or (plist-get branches m0) 0))
            (setq branches (plist-put branches m0 (1+ cnt)))))
         ((looking-at-p "BRF:\\([[:digit:]]+\\)") ; Branches Found.
          nil)
         ((looking-at-p "BRH:\\([[:digit:]]+\\)") ; Branches hit.
          nil)
         ((looking-at "DA:\\([[:digit:]]+\\),\\([[:digit:]]+\\)") ; Line found.
          (setq m0 (string-to-number (buffer-substring (match-beginning 1)
                                                       (match-end 1))))
          (setq cnt (string-to-number (buffer-substring (match-beginning 2)
                                                        (match-end 2))))
          (setq lines (plist-put lines m0 cnt)))
         ((looking-at-p "LF:\\([[:digit:]]+\\)")  ; Lines found.
          nil)
         ((looking-at-p "LH:\\([[:digit:]]+\\)")  ; Lines hit.
          nil)
         ((and (looking-at-p "end_of_record") src-file)
          ;; Commit accumulated data and reset record storage.
          (puthash src-file lines    anycov--line-hits)
          (puthash src-file branches anycov--branch-hits)
          (setq funcs '())
          (setq lines '())
          (setq branches '())
          (setq total-branches '())
          (setq src-file nil))
         ((t
           (user-error "Anycov.el: Lcov parsing error: %s"
                       (buffer-substring (line-beginning-position)
                                         (line-end-position))))))
        (forward-line 1)))))


(defun anycov--display-tooltip (window object pos)
  "Display a popup in WINDOW.

OBJECT is either a buffer, overlay or a string.

POS either of:
* Position in the OBJECT buffer.
* Position of the overlay.
* Position inside the string."
  nil)


(defun anycov--clear-overlays ()
  "Clear all overlays in the current buffer."
  (remove-overlays (point-min) (point-max) 'coverage t))


(defun anycov--add-buffer-overlays (buffer)
  "Add coverage overlays to BUFFER."
  (save-excursion
    (with-current-buffer buffer
      (let* ((file     (file-truename (buffer-file-name buffer)))
             (lines    (gethash file anycov--line-hits))
             (branches (gethash file anycov--branch-hits))
             (buffer-lines (line-number-at-pos (point-max))))
        (when (or lines branches)
          (goto-char (point-min))
          (anycov--clear-overlays)
          (overlay-recenter (point-max))
          (cl-loop for i from 1 to (1+ buffer-lines)
                   do
                   (anycov--add-line-overlay buffer
                                             (plist-get lines i)
                                             (plist-member branches i)
                                             (plist-get branches i))
                   (forward-line 1)))))))


(defun anycov--add-line-overlay (buffer hit branch branch-end)
  "Add an overlay to BUFFER the current line if the coverage HIT the line.

If the line contains a missed BRANCH, add an additional overlay
next to the line overlay.  Set the `branch' overlay property to
BRANCH-END.  This value is nil when the branch is fully covered."
  (let* ((beg (line-beginning-position))
         (end (+ 1 beg))
         (line-ovl)
         (branch-ovl))
    (when hit
      (setq line-ovl (make-overlay beg end buffer))
      (overlay-put line-ovl 'coverage t)
      (overlay-put line-ovl 'priority anycov-overlay-priority)
      (overlay-put line-ovl 'face (if (> hit 0)
                                      (cons 'background-color "green4")
                                    (cons 'background-color "red4")))
      (when branch
        (setq branch-ovl (make-overlay end (+ end 1) buffer))
        (overlay-put branch-ovl 'coverage t)
        (when branch-end
          (overlay-put branch-ovl
                       'help-echo
                       (format "Missing branches: %s" branch-end)))
        (overlay-put branch-ovl 'priority anycov-overlay-priority)
        (overlay-put branch-ovl 'face (if branch-end
                                          (cons 'background-color "dark orange")
                                        (cons 'background-color "dark cyan"))))
      )))


(defun anycov--locate-dominating-file (file)
  "Search upwards in the file hierarchy for FILE.

Return the full path to the file if found, otherwise return
nil."
  (let ((dir (locate-dominating-file (buffer-file-name) file)))
    (when dir
      (concat dir file))))


(defun anycov--find-coverage-data ()
  "Search upwards in the file hierarchy for coverage data.

By default, this package will look for a file called
`coverage.xml'.  When found, return the path to the file or nil
if not found."
  (or (anycov--locate-dominating-file "coverage.xml")
      (anycov--locate-dominating-file ".coverage.xml")))


(defun anycov--find-file-hook ()
  "Add coverage data to a buffer after loading if such data exist."
  (let* ((name (buffer-file-name)))
    (message (format "anycov.el: Loading coverage data for file: %s." name))
    (anycov--add-buffer-overlays (current-buffer))))


(defun anycov--clear-all-buffers ()
  "Remove coverage data from all active buffers."
  (maphash (lambda (key &ignore)
             (when (get-file-buffer key)
               (with-current-buffer (get-file-buffer key)
                 (anycov--clear-overlays))))
           anycov--line-hits))


(defun anycov--load-coverage (file)
  "Load coverage data stored in FILE."
  (pcase (file-name-extension file)
    ("lcov" (anycov--parse-lcov file))
    ("xml"  (anycov--parse-xml file))
    (`(,_   (user-error "Anycov.el: Unsupported coverage data format")))))


(defun anycov--apply-coverage ()
  "Apply the loaded coverage data to all applicable buffers."
  (dolist (buf (buffer-list))
    (when (buffer-file-name buf)
      (anycov--add-buffer-overlays buf))))


(defun anycov--turn-on ()
  "Turn on `anycov-mode'."
  (unless anycov--loaded-file
    (setq anycov--loaded-file (anycov--find-coverage-data)))
  (if (not anycov--loaded-file)
      (user-error "Anycov.el: No coverage data found")
    (anycov--load-coverage anycov--loaded-file)
    (anycov--apply-coverage))
  (add-hook 'find-file-hook #'anycov--find-file-hook))


(defun anycov--turn-off ()
  "Turn off `anycov-mode'."
  (remove-hook 'find-file-hook #'anycov--find-file-hook)
  (anycov--clear-all-buffers))


;;;###autoload
(define-minor-mode anycov-mode
  "Minor mode to visualize code coverage information."
  :global t
  :lighter " acov"
  (if anycov-mode
      (anycov--turn-on)
    (anycov--turn-off)))


;;;###autoload
(defun anycov-select-coverage (file &optional no-start)
  "Select FILE as coverage data file.

Also restart `anycov-mode' unless NO-START is set as prefix argument."
  (interactive "fP")
  (setq anycov--loaded-file file)
  (anycov-mode))


(provide 'anycov)

;;; anycov.el ends here
