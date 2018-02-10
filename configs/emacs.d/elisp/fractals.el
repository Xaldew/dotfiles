;;; fractals.el --- Fractal generators -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; Collection of various functions that generates fractals.
;;
;;; Code:


(defun fractals-sierpinski-carpet (size)
  "Create an image of the Sierpinski carpet of SIZE pixels."
  (interactive "nSierpinski carpet size: ")
  (with-current-buffer-window
   "*Sierpinski*" nil nil
   (fundamental-mode)
   (erase-buffer)
   (cl-labels ((fill-p (x y)
                       (cond ((or (zerop x) (zerop y)) "0")
                             ((and (= 1 (mod x 3)) (= 1 (mod y 3))) "1")
                             (t (fill-p (/ x 3) (/ y 3))))))
     (insert (format "P1\n%d %d\n" size size))
     (cl-loop for x from 0 below size do
              (cl-loop for y from 0 below size do
                       (insert (fill-p x y)))
              (insert "\n")))
   (image-mode)))


(provide 'fractals)

;;; fractals.el ends here
