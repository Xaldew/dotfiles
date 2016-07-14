;;; alcove.el --- Interface for coverage overlays  -*- lexical-binding: t; -*-

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


(defvar alcove--comment-faces
  '(font-lock-string-face
    font-lock-comment-face
    font-lock-doc-face
    mmm-default-submode-face)
  "Faces corresponding to comments in various buffers.")


(defun alcove--comment-p (pos)
  "Detect if POS is within a comment."
  (let ((f (get-text-property pos 'face)))
    (memq f alcove--comment-faces)))


;;; Custom variables:

(defgroup alcove nil
  "Test coverage overlay for Emacs."
  :group 'tools)


(defcustom alcove-overlay-priority 10
  "Priority for all overlays created by this package.")


(defface alcove-line-hit
  '((t (:background "green4")))
  "Face used for lines hit by the line coverage analysis."
  :group 'alcove)

(defface alcove-line-miss
  '((t (:background "red4")))
  "Face used for lines missed by the line coverage analysis."
  :group 'alcove)

(defface alcove-branch-hit-all
  '((t (:background "dark cyan")))
  "Face used for lines with fully covered branches."
  :group 'alcove)

(defface alcove-branch-hit-partial
  '((t (:background "dark orange")))
  "Face used for lines with partially covered branches."
  :group 'alcove)

(defface alcove-branch-miss
  '((t (:background "dark red")))
  "Face used for lines with missed branch coverage."
  :group 'alcove)


(defvar alcove--line-hits (make-hash-table :test #'equal)
  "Association list over files and line hits.")

(defvar alcove--branch-hits (make-hash-table :test #'equal)
  "Association list over files and line hits.")

(defvar alcove--loaded-file nil
  "The currently loaded coverage data file.")


(defun alcove--parse-sources (xml)
  "Read the `sources' tag with XML directories from the XML."
  (let ((srcs (assoc-recursive xml 'coverage 'sources))
        (out))
    (dolist (src srcs out)
      (when (and (listp src)
                 (eq 'source (car src)))
        (setq out (append out (cddr src)))))))


(defun alcove--parse-condition-coverage (string)
  "Parse the `condition-coverage' XML tag inside STRING.

Return a list of of numbers corresponding to the number of
branches covered and the total number of branches.  Return (0 0)
upon failure."
  (if (string-match
       "[[:digit:]]+% ?(\\([[:digit:]]+\\)/\\([[:digit:]]+\\))"
       string)
      (list (string-to-number (match-string 1 string))
            (string-to-number (match-string 2 string)))
  (list 0 0)))


(defun alcove--parse-class (class)
  "Read the `Cobertura' CLASS."
  (let ((lines (alist-get 'lines class))
        (cnt)
        (lines-out)
        (branches))
    (dolist (l lines)
      (when (and l (listp l))
        (setq l (nth 1 l))
        (setq lines-out
              (plist-put lines-out
                         (string-to-number (alist-get 'number l))
                         (string-to-number (alist-get 'hits l))))
        (when (alist-get 'branch l)
          (setq cnt (alcove--parse-condition-coverage
                     (alist-get 'condition-coverage l)))
          (setq branches (plist-put branches
                                    (string-to-number (alist-get 'number l))
                                    cnt)))))
    (list lines-out branches)))


(defun alcove--parse-xml (file)
  "Read the `Cobertura' compliant XML schema in FILE."
  (let* ((xml (xml-parse-file file))
         (src-dirs (alcove--parse-sources xml))
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
            (setq lines (alcove--parse-class cls))
            (puthash file (nth 0 lines) alcove--line-hits)
            (puthash file (nth 1 lines) alcove--branch-hits)))))))


(defun alcove--parse-lcov (file)
  "Read the `lcov' formatted FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (let ((lines '())
          (branches '())
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
          (setq cnt (or (plist-get branches m0) (list 0 0)))
          (cl-incf (nth 1 cnt))
          (unless (string= m1 "-")
            (cl-incf (nth 0 cnt)))
          (setq branches (plist-put branches m0 cnt)))
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
          (puthash src-file lines    alcove--line-hits)
          (puthash src-file branches alcove--branch-hits)
          (setq funcs '())
          (setq lines '())
          (setq branches '())
          (setq src-file nil))
         ((t
           (user-error "Alcove.el: Lcov parsing error: %s"
                       (buffer-substring (line-beginning-position)
                                         (line-end-position))))))
        (forward-line 1)))))


(defun alcove--display-tooltip (window object pos)
  "Display a popup in WINDOW.

OBJECT is either a buffer, overlay or a string.

POS either of:
* Position in the OBJECT buffer.
* Position of the overlay.
* Position inside the string."
  nil)


(defun alcove--clear-overlays ()
  "Clear all overlays in the current buffer."
  (remove-overlays (point-min) (point-max) 'coverage t))


(defun alcove--add-buffer-overlays (buffer)
  "Add coverage overlays to BUFFER."
  (save-excursion
    (with-current-buffer buffer
      (let* ((file     (file-truename (buffer-file-name buffer)))
             (lines    (gethash file alcove--line-hits))
             (branches (gethash file alcove--branch-hits))
             (buffer-lines (line-number-at-pos (point-max))))
        (when (or lines branches)
          (goto-char (point-min))
          (alcove--clear-overlays)
          (overlay-recenter (point-max))
          (cl-loop for i from 1 to (1+ buffer-lines)
                   do
                   (alcove--add-line-overlay buffer
                                             (plist-get lines i)
                                             (plist-get branches i))
                   (forward-line 1)))))))


(defun alcove--add-line-overlay (buffer hit branch)
  "Add an overlay to BUFFER the current line if the coverage HIT the line.

If the line contains a BRANCH add an additional overlay next to
the line overlay to indicate branch coverage."
  (let* ((beg (line-beginning-position))
         (end (+ 1 beg))
         (line-ovl)
         (branch-ovl))
    (when hit
      (setq line-ovl (make-overlay beg end buffer))
      (overlay-put line-ovl 'coverage t)
      (overlay-put line-ovl 'priority alcove-overlay-priority)
      (overlay-put line-ovl 'face (if (> hit 0)
                                      'alcove-line-hit
                                    'alcove-line-miss))
      (when branch
        (setq branch-ovl (make-overlay end (+ end 1) buffer))
        (overlay-put branch-ovl 'coverage t)
        (overlay-put branch-ovl
                     'help-echo
                     (format "Branch coverage: %d%% (%d/%d)"
                             (* 100 (/ (nth 0 branch)
                                       (float (nth 1 branch))))
                             (nth 0 branch)
                             (nth 1 branch)))
        (overlay-put branch-ovl 'priority alcove-overlay-priority)
        (overlay-put branch-ovl 'face (cond
                                       ((equal (nth 0 branch)
                                               (nth 1 branch))
                                        'alcove-branch-hit-all)
                                       ((> (nth 0 branch) 0)
                                        'alcove-branch-hit-partial)
                                       (t
                                        'alcove-branch-miss)))))))


(defun alcove--locate-dominating-file (file)
  "Search upwards in the file hierarchy for FILE.

Return the full path to the file if found, otherwise return
nil."
  (let ((dir (locate-dominating-file (buffer-file-name) file)))
    (when dir
      (concat dir file))))


(defun alcove--find-coverage-data ()
  "Search upwards in the file hierarchy for coverage data.

By default, this package will look for a file called
`coverage.xml'.  When found, return the path to the file or nil
if not found."
  (or (alcove--locate-dominating-file "coverage.xml")
      (alcove--locate-dominating-file ".coverage.xml")))


(defun alcove--find-file-hook ()
  "Add coverage data to a buffer after loading if such data exist."
  (let* ((name (buffer-file-name)))
    (message (format "alcove.el: Loading coverage data for file: %s." name))
    (alcove--add-buffer-overlays (current-buffer))))


(defun alcove--clear-all-buffers ()
  "Remove coverage data from all active buffers."
  (maphash (lambda (key &ignore)
             (when (get-file-buffer key)
               (with-current-buffer (get-file-buffer key)
                 (alcove--clear-overlays))))
           alcove--line-hits))


(defun alcove--load-coverage (file)
  "Load coverage data stored in FILE."
  (pcase (file-name-extension file)
    ("lcov" (alcove--parse-lcov file))
    ("xml"  (alcove--parse-xml file))
    (`(,_   (user-error "Alcove.el: Unsupported coverage data format")))))


(defun alcove--apply-coverage ()
  "Apply the loaded coverage data to all applicable buffers."
  (dolist (buf (buffer-list))
    (when (buffer-file-name buf)
      (alcove--add-buffer-overlays buf))))


(defun alcove--turn-on ()
  "Turn on `alcove-mode'."
  (unless alcove--loaded-file
    (setq alcove--loaded-file (alcove--find-coverage-data)))
  (if (not alcove--loaded-file)
      (user-error "Alcove.el: No coverage data found")
    (alcove--load-coverage alcove--loaded-file)
    (alcove--apply-coverage))
  (add-hook 'find-file-hook #'alcove--find-file-hook))


(defun alcove--turn-off ()
  "Turn off `alcove-mode'."
  (remove-hook 'find-file-hook #'alcove--find-file-hook)
  (alcove--clear-all-buffers))


;;;###autoload
(define-minor-mode alcove-mode
  "Minor mode to visualize code coverage information."
  :global t
  :lighter " acov"
  (if alcove-mode
      (alcove--turn-on)
    (alcove--turn-off)))


;;;###autoload
(defun alcove-select-coverage (file no-restart)
  "Select FILE as coverage data file.

Also restart `alcove-mode' unless NO-RESTART is set as prefix argument."
  (interactive "f\nP")
  (setq alcove--loaded-file file)
  (unless no-restart
    (alcove-mode)))


(provide 'alcove)

;;; alcove.el ends here
