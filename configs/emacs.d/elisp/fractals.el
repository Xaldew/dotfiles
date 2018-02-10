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


(defun fractals-mandelbrot-pixel (cx cy depth)
  "Count the Mandelbrot iterations before divergence.

The starting number is CX + CY i and goes through at most DEPTH
iterations."
  (cl-loop for i from 0 below depth
           with zr = 0.0 and zi = 0.0
           when (> (+ (* zr zr) (* zi zi)) 4.0)
           return i do
           (cl-psetq zr (+ (* zr zr) (- (* zi zi)) cx)
                     zi (+ (* zr zi 2.0) cy))
           finally return i))


(defun fractals-mandelbrot (size depth)
  "Visualize the Mandelbrot set in an image of SIZE by SIZE pixels.

Each pixel is recursed to a maximum DEPTH before concluding that
  it is in or out of the set."
  (interactive "nSize: \nP")
  (let* ((w (float size))
         (h (float size))
         (llx -2.0) (lly -1.5)
         (urx  1.0) (ury  1.5)
         (px (/ (- urx llx) w))
         (py (/ (- ury lly) h))
         (d (or depth 32)))
    (with-current-buffer-window
     "*Mandelbrot*" nil nil
     (fundamental-mode)
     (erase-buffer)
     (insert (format "P3\n%d %d\n255\n" w h))
     (cl-loop for y from 0 below size do
              (cl-loop for x from 0 below size do
                       (let ((v (fractals-mandelbrot-pixel
                                 (+ llx (* px x) (* 0.5 px))
                                 (- ury (* py y) (* 0.5 py))
                                 d)))
                         (insert (format "%1$3d %1$3d %1$3d "
                                         (floor (* 256 (/ v 1.0 d)))))))
              (insert "\n"))
     (image-mode))))


(provide 'fractals)

;;; fractals.el ends here
