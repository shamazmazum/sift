;; More "high-quality" interpolation than used in the core library

(in-package :sift/util)

(declaim (inline interp1d))
(defun interp1d (v0 v1 x)
  (+ v0 (* (- v1 v0) x)))

(sera:-> interpolate/linear
         ((sera:-> (fixnum fixnum)
                   (values single-float &optional))
          real real
          (real (0)) (real (0)))
         (values single-float &optional))
(declaim (inline interpolate/linear))
(defun interpolate/linear (f x y divisor-x divisor-y)
  "Interpolate F in the point (X/DIVISOR-X, Y/DIVISOR-Y)."
  (let ((divisor-x (float divisor-x))
        (divisor-y (float divisor-y)))
    (sera:mvlet ((qi ri (floor x divisor-x))
                 (qj rj (floor y divisor-y)))
      ;; For code formatting
      (flet ((id (x) x))
        (declare (inline id))
        (let* ((ri (/ ri divisor-x))
               (rj (/ rj divisor-y))

               (v00 (funcall f (id qi) (id qj)))
               (v01 (funcall f (id qi) (1+ qj)))
               (v10 (funcall f (1+ qi) (id qj)))
               (v11 (funcall f (1+ qi) (1+ qj)))

               (v0 (interp1d v00 v01 rj))
               (v1 (interp1d v10 v11 rj))

               (v (interp1d v0 v1 ri)))
          v)))))
