(in-package :sift/core)

;; Much faster linear algebra for small matrices and vectors compared
;; to magicl.

(deftype mat (n) `(simple-array single-float (,n ,n)))
(deftype vec (n) `(simple-array single-float (,n)))

(declaim (inline vec3))
(defun vec3 (x y z)
  (make-array 3
              :element-type 'single-float
              :initial-contents (list x y z)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (inline mat3))
  (defun mat3 (a00 a01 a02 a10 a11 a12 a20 a21 a22)
    (make-array '(3 3)
                :element-type 'single-float
                :initial-contents (list (list a00 a01 a02)
                                        (list a10 a11 a12)
                                        (list a20 a21 a22)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (inline mat2))
  (defun mat2 (a00 a01 a10 a11)
    (make-array '(2 2)
                :element-type 'single-float
                :initial-contents (list (list a00 a01)
                                        (list a10 a11)))))

(alex:define-constant +mat3-identity+
    (mat3 1f0 0f0 0f0 0f0 1f0 0f0 0f0 0f0 1f0)
  :test #'equalp)

(alex:define-constant +mat2-identity+
    (mat2 1f0 0f0 0f0 1f0)
  :test #'equalp)

;; Vectors

(sera:-> dist ((vec *) (vec *))
         (values single-float &optional))
(declaim (inline dist))
(defun dist (v1 v2)
  (assert (= (length v1) (length v2)))
  (sqrt
   (loop for i below (length v1) sum
         (expt (- (aref v1 i) (aref v2 i)) 2)
         single-float)))

(sera:-> add ((vec *) (vec *))
         (values (vec *) &optional))
(declaim (inline add))
(defun add (v1 v2)
  (assert (= (length v1) (length v2)))
  (let ((result (make-array (length v1) :element-type 'single-float)))
    (loop-array (result (i))
     (setf (aref result i)
           (+ (aref v1 i) (aref v2 i))))
    result))

(sera:-> sub ((vec *) (vec *))
         (values (vec *) &optional))
(declaim (inline sub))
(defun sub (v1 v2)
  (assert (= (length v1) (length v2)))
  (let ((result (make-array (length v1) :element-type 'single-float)))
    (loop-array (result (i))
     (setf (aref result i)
           (- (aref v1 i) (aref v2 i))))
    result))

(sera:-> dot ((vec *) (vec *))
         (values single-float &optional))
(declaim (inline dot))
(defun dot (v1 v2)
  (assert (= (length v1) (length v2)))
  (loop for x1 across v1
        for x2 across v2
        sum (* x1 x2) single-float))

(sera:-> mul-mv ((mat *) (vec *))
         (values (vec *) &optional))
(declaim (inline mul-mv))
(defun mul-mv (m v)
  (assert (= (length v) (array-dimension m 1)))
  (let ((result (make-array (length v) :element-type 'single-float)))
    (loop-array (result (i))
     (setf (aref result i)
           (loop for k below (length v) sum
                 (* (aref m i k)
                    (aref v k))
                 single-float)))
    result))

;; Matrices

(sera:-> shrink3 ((mat 3))
         (values (mat 2) &optional))
(declaim (inline shrink3))
(defun shrink3 (m)
  "Remove the first row and column which are related to the scale
level."
  (mat2 (aref m 1 1) (aref m 1 2)
        (aref m 2 1) (aref m 2 2)))

(sera:-> mul ((mat *) (mat *))
         (values (mat *) &optional))
(declaim (inline mul))
(defun mul (m1 m2)
  (assert (= (array-dimension m1 1)
             (array-dimension m2 0)))
  (let ((result (make-array (list (array-dimension m1 0)
                                  (array-dimension m2 1))
                            :element-type 'single-float)))
    (loop-array (result (i j))
     (setf (aref result i j)
           (loop for k below (array-dimension m1 1) sum
                 (* (aref m1 i k)
                    (aref m2 k j))
                 single-float)))
    result))

(sera:-> mtrace ((mat *))
         (values single-float &optional))
(declaim (inline mtrace))
(defun mtrace (m)
  (loop for i below (array-dimension m 0)
        sum (aref m i i) single-float))
    
(sera:-> det2 ((mat 2))
         (values single-float &optional))
(declaim (inline det2))
(defun det2 (m)
  (- (* (aref m 0 0) (aref m 1 1))
     (* (aref m 1 0) (aref m 0 1))))

(declaim (inline solve))
(defun solve (a b)
  (let ((length (length b)))
    (em:reshape
     (em:solve a (em:reshape-unsafe b (list length 1)))
     (list length))))
