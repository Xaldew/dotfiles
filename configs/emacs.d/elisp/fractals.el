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


(defun fractals-rand-float (limit)
  "Generate a random positive floating point number less than the given LIMIT."
  (/ (float (random most-positive-fixnum)) (/ most-positive-fixnum limit)))


(defun fractals-rand-range (a b)
  "Generate a random floating point number in the interval [A, B]."
  (+ a (fractals-rand-float (- b a))))


(cl-defstruct (fractals-image
               (:constructor nil)
               (:constructor new-image
                             (width height &aux
                                    (pixels (make-vector (* width height) 0)))))
  width height pixels)


(defun fractals-anti-buddahbrot-pixel (cr ci depth)
  "Perform the Mandelbrot iteration test on the complex point CR + CI i.

If after DEPTH iterations, the point is considered part of the
Mandelbrot set and a list of visited points is returned.
Otherwise, nil is returned."
  (cl-loop for i from 0 below depth
           with cx = cr and cy = ci and
           zr = 0.0 and zt = 0.0 and zi = 0.0
           when (> (+ (* zr zr) (* zi zi)) 10.0)
           return nil do
           (setq zt (+ (* zr zr) (- (* zi zi)) cx))
           (setq zi (+ (* zr zi 2.0) cy))
           (setq zr zt)
           collect
           (list zr zi)))


(defun fractals-buddahbrot-pixel (cr ci depth)
  "Perform the Mandelbrot iteration test on the complex point CR + CI i.

After DEPTH iterations, the point is considered to be part of the
Mandelbrot set and an empty list is returned.  Otherwise, a list
of points visited during the escape to infinity is returned."
  (cl-loop for i from 0 below depth
           with cx = cr and cy = ci and
           zr = 0.0 and zt = 0.0 and zi = 0.0
           when (> (+ (* zr zr) (* zi zi)) 10.0)
           return points do
           (setq zt (+ (* zr zr) (- (* zi zi)) cx))
           (setq zi (+ (* zr zi 2.0) cy))
           (setq zr zt)
           collect (list zr zi) into points
           finally return nil))


(defun fractals-buddahbrot-image (iterations width height depth)
  "Compute the individual pixels for the Buddahbrot image.

Uses the Buddahbrot techinique ITERATIONS times to generate a
nested vector of size WIDTH x HEIGHT.  Each starting point is
iterated at most DEPTH times."
  (let ((img (new-image width height))
        (llx -2.0) (lly -1.5)
        (urx  1.0) (ury  1.5)
        (size (* width height)))
    (cl-loop for i from 0 below iterations
             with sr = 0.0 and si = 0.0 and
             x = 0 and y = 0 and idx = 0 do
             (setq sr (fractals-rand-range llx urx)
                   si (fractals-rand-range lly ury))
             (cl-loop for (zr zi) in (fractals-buddahbrot-pixel sr si depth) do
                      (setq x (truncate (* width  (/ (- zr llx) (abs (- urx llx)))))
                            y (truncate (* height (/ (- zi lly) (abs (- ury lly)))))
                            idx (+ x (* width y)))
                      (when (and (< 0 x width) (< 0 y height))
                        (cl-incf (aref (fractals-image-pixels img) idx)))))
    img))


(defun fractals-buddahbrot (iterations width height depth)
  "Visualize the Mandelbrot set using ITERATIONS of the Buddahbrot technique.

Creates an image of WIDTH x HEIGHT pixels.  Each starting pixel
is iterated at most DEPTH times."
  (interactive "nSize: \nP")
  (let* ((img (fractals-buddahbrot-image iterations width height depth))
         (max (float (seq-max (fractals-image-pixels img))))
         (min (float (seq-min (fractals-image-pixels img)))))
    (with-current-buffer-window
     "*Buddahbrot*" nil nil
     (fundamental-mode)
     (erase-buffer)
     (insert (format "P3\n%d %d\n%d\n" width height 255))
     (cl-loop for y from 0 below height do
              (cl-loop for x from 0 below width
                       with idx = 0 and val = 0 and ramp = 0 and tm = 0 do
                       (setq idx (+ (* width y) x)
                             val (aref (fractals-image-pixels img) idx)
                             ramp (/ (* 2 (- val min)) (- max min))
                             tm (* 255 (sqrt (if (> ramp 1.0) 1.0 ramp))))
                       (insert (format "%1$3d %1$3d %1$3d " tm)))
              (insert "\n"))
     (image-mode))))


(provide 'fractals)

;;; fractals.el ends here
