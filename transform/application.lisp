(in-package :sift/transform)

(serapeum:-> apply-transform-point
             ((sift/core:mat 3) single-float single-float)
             (values single-float single-float &optional))
(defun apply-transform-point (m x y)
  (declare (optimize (speed 3)))
  (let ((x (+ (* (aref m 0 0) x) (* (aref m 0 1) y) (aref m 0 2)))
        (y (+ (* (aref m 1 0) x) (* (aref m 1 1) y) (aref m 1 2))))
    (values x y)))

(serapeum:-> apply-transform ((simple-array single-float (* *)) (sift/core:mat 3) &key
                              (:background single-float)
                              (:shape      list))
             (values (simple-array single-float (* *)) &optional))
(defun apply-transform (array m &key (shape (array-dimensions array)) (background 0.0))
  "Apply affine transform @c(m) (in the form of 3x3 matrix) to an
image. The result has the shape @c(shape)."
  (declare (optimize (speed 3)))
  (let ((result (make-array shape :element-type 'single-float)))
    (sift/core:loop-array (result (i j))
      (multiple-value-bind (x y)
          (apply-transform-point m (float i) (float j))
        (setf (aref result i j)
              (sift/util:interpolate/linear
               (lambda (i j)
                 (if (array-in-bounds-p array i j)
                     (aref array i j)
                     background))
                x y 1 1))))
    result))
