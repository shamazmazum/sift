(in-package :sift/core)

(sera:defconstructor index3
  (i fixnum)
  (j fixnum)
  (k fixnum))

(declaim (inline index3-+))
(sera:-> index3-+ (index3 index3)
         (values index3 &optional))
(defun index3-+ (idx1 idx2)
  (index3 (+ (index3-i idx1)
             (index3-i idx2))
          (+ (index3-j idx1)
             (index3-j idx2))
          (+ (index3-k idx1)
             (index3-k idx2))))

(declaim (inline index3--))
(sera:-> index3-- (index3 index3)
         (values index3 &optional))
(defun index3-- (idx1 idx2)
  (index3 (- (index3-i idx1)
             (index3-i idx2))
          (- (index3-j idx1)
             (index3-j idx2))
          (- (index3-k idx1)
             (index3-k idx2))))

(declaim (inline index3-negate))
(sera:-> index3-negate (index3)
         (values index3 &optional))
(defun index3-negate (idx)
  (index3 (- (index3-i idx))
          (- (index3-j idx))
          (- (index3-k idx))))

(declaim (inline index3-scale))
(sera:-> index3-scale (index3 fixnum)
         (values index3 &optional))
(defun index3-scale (idx s)
  (index3 (* (index3-i idx) s)
          (* (index3-j idx) s)
          (* (index3-k idx) s)))

(declaim (inline aref-index3))
(sera:-> aref-index3 ((simple-array * (* * *)) index3)
         (values t &optional))
(defun aref-index3 (array idx)
  (aref array (index3-i idx) (index3-j idx) (index3-k idx)))

;; Useful macros for iteration which supersede nested loops
(defmacro loop-array ((array indices) &body body)
  (car
   (reduce
    (lambda (entry acc)
      (destructuring-bind (d . idx)
          entry
        `((loop for ,idx below (array-dimension ,array ,d) do
                ,@acc))))
    (loop for idx in indices
          for d from 0 by 1
          collect (cons d idx))
    :from-end t
    :initial-value body)))

(defmacro loop-ranges (specs &body body)
  (car
   (reduce
    (lambda (spec acc)
      (destructuring-bind (idx start end)
          spec
        `((loop for ,idx fixnum from ,start below ,end do
                ,@acc))))
    specs
    :from-end t
    :initial-value body)))

;; Nearest neighbor interpolation
(deftype scalar-field () '(sera:-> (index3) (values single-float &optional)))

(sera:-> interpolate/nn (scalar-field single-float single-float single-float)
         (values single-float &optional))
(declaim (inline interpolate/nn))
(defun interpolate/nn (f x y z)
  (funcall f (index3 (round x) (round y) (round z))))
