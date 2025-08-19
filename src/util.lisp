(in-package :sift)

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

(declaim (inline aref-index3/p))
(sera:-> aref-index3/p ((simple-array * (* * *)) index3)
         (values t &optional))
(defun aref-index3/p (array idx)
  (aref array
        ;; Periodic boundary conditions on the layer coordinate is not
        ;; what I want. On spatial dimensions, though, they are
        ;; acceptable. Moreover, I do image filtering via FFT which
        ;; imposes periodic BC. If this is a problem, we can simply
        ;; discard keypoints which are close (~10 pixels) to the
        ;; boundary.
        (index3-i idx)
        (mod (index3-j idx) (array-dimension array 1))
        (mod (index3-k idx) (array-dimension array 2))))

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
