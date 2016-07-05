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

(defvar anycov--loaded-filepath nil
  "The currently loaded coverage data file.")


(defun anycov--read-sources-tag (xml)
  "Read the `sources' tag with XML directories from the XML."
  (let ((srcs (assoc-recursive xml 'coverage 'sources))
        (out))
    (dolist (src srcs out)
      (when (and (listp src)
                 (eq 'source (car src)))
        (setq out (append out (cddr src)))))
    out))


(defun anycov--read-class-tag (class)
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


(defun anycov-read-xml (file)
  "Read the `Cobertura' compliant XML schema in FILE."
  (let* ((xml (xml-parse-file file))
         (src-dirs (anycov--read-sources-tag xml))
         (pkgs (assoc-recursive xml 'coverage 'packages))
         (i 0)
         (file)
         (lines))
    (dolist (pkg pkgs)
      (when (and (listp pkg)
                 (eq 'package (car pkg)))
        (dolist (cls (alist-get 'classes pkg))
          (when (and (listp cls)
                     (eq 'class (car cls)))
            (setq file (concat (file-name-as-directory (nth i src-dirs))
                               (alist-get 'filename (nth 1 cls))))
            (setq lines (anycov--read-class-tag cls))
            (puthash file (nth 0 lines) anycov--line-hits)
            (puthash file (nth 1 lines) anycov--branch-hits)))
        (cl-incf i)))))


(defun anycov-read-lcov (file)
  "Read the `lcov' formatted FILE."
  nil)


(defun anycov-display-tooltip (window object pos)
  "Display a popup in WINDOW.

OBJECT is either a buffer, overlay or a string.

POS either of:
* Position in the OBJECT buffer.
* Position of the overlay.
* Position inside the string."
  nil)


(defun anycov-clear-overlays ()
  "Clear all overlays in the current buffer."
  (remove-overlays (point-min) (point-max) 'coverage t))


(defun anycov-add-buffer-overlays (buffer)
  "Add coverage overlays to BUFFER."
  (save-excursion
    (with-current-buffer buffer
      (let* ((file     (file-truename (buffer-file-name buffer)))
             (lines    (gethash file anycov--line-hits))
             (branches (gethash file anycov--branch-hits))
             (buffer-lines (line-number-at-pos (point-max))))
        (when (or lines branches)
          (goto-char (point-min))
          (anycov-clear-overlays)
          (overlay-recenter (point-max))
          (print branches)
          (print lines)
          (print buffer-lines)
          (cl-loop for i from 1 to (1+ buffer-lines)
                   do
                   (anycov-add-line-overlay buffer
                                            (plist-get lines i)
                                            (plist-member branches i)
                                            (plist-get branches i))
                   (forward-line 1)))))))


(defun anycov-add-line-overlay (buffer hit branch branch-end)
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


(defun anycov--turn-on ()
  "Turn on `anycov-mode'."
  nil)


(defun anycov--turn-off ()
  "Turn off `anycov-mode'."
  nil)


;;;###autoload
(define-minor-mode anycov-mode
  "Minor mode to visualize code coverage information."
  :global t
  :lighter " acov"
  (if anycov-mode
      (anycov--turn-on)
    (anycov--turn-off)))


(provide 'anycov)

;;; anycov.el ends here
