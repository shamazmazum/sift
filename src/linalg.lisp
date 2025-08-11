(in-package :sift)

(deftype mat (n) `(simple-array double-float (,n ,n)))
(deftype vec (n) `(simple-array double-float (,n)))

(declaim (inline make-vec3))
(defun make-vec3 (x y z)
  (make-array 3
              :element-type 'double-float
              :initial-contents (list x y z)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (inline make-mat3))
  (defun make-mat3 (a00 a01 a02 a10 a11 a12 a20 a21 a22)
    (make-array '(3 3)
                :element-type 'double-float
                :initial-contents (list (list a00 a01 a02)
                                        (list a10 a11 a12)
                                        (list a20 a21 a22)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (inline make-mat2))
  (defun make-mat2 (a00 a01 a10 a11)
    (make-array '(2 2)
                :element-type 'double-float
                :initial-contents (list (list a00 a01)
                                        (list a10 a11)))))

(alex:define-constant +mat3-identity+
    (make-mat3 1d0 0d0 0d0 0d0 1d0 0d0 0d0 0d0 1d0)
  :test #'equalp)

(alex:define-constant +mat2-identity+
    (make-mat2 1d0 0d0 0d0 1d0)
  :test #'equalp)

;; Vector 3x1

(sera:-> dist3 ((vec 3) (vec 3))
         (values double-float &optional))
(declaim (inline dist3))
(defun dist3 (v1 v2)
  (sqrt
   (loop for i below 3 sum
         (expt (- (aref v1 i) (aref v2 i)) 2)
         double-float)))

(sera:-> add3 ((vec 3) (vec 3))
         (values (vec 3) &optional))
(declaim (inline add3))
(defun add3 (v1 v2)
  (let ((result (make-array 3 :element-type 'double-float)))
    (loop-array (result (i))
     (setf (aref result i)
           (+ (aref v1 i) (aref v2 i))))
    result))

(sera:-> scalev3 ((vec 3) double-float)
         (values (vec 3) &optional))
(declaim (inline scalev3))
(defun scalev3 (v s)
  (let ((result (make-array 3 :element-type 'double-float)))
    (loop-array (result (i))
     (setf (aref result i)
           (* (aref v i) s)))
    result))

(sera:-> dot3 ((vec 3) (vec 3))
         (values double-float &optional))
(declaim (inline dot3))
(defun dot3 (v1 v2)
  (loop for x1 across v1
        for x2 across v2
        sum (* x1 x2) double-float))

(sera:-> mul-m3v3 ((mat 3) (vec 3))
         (values (vec 3) &optional))
(declaim (inline mul-m3v3))
(defun mul-m3v3 (m v)
  (let ((result (make-array 3 :element-type 'double-float)))
    (loop-array (result (i))
     (setf (aref result i)
           (loop for k below 3 sum
                 (* (aref m i k)
                    (aref v k))
                 double-float)))
    result))

;; Matrix 3x3

(sera:-> shrink3 ((mat 3))
         (values (mat 2) &optional))
(declaim (inline shrink3))
(defun shrink3 (m)
  "Remove the first row and column which are related to the scale
level."
  (make-mat2 (aref m 1 1) (aref m 1 2)
             (aref m 2 1) (aref m 2 2)))

(sera:-> mul3 ((mat 3) (mat 3))
         (values (mat 3) &optional))
(declaim (inline mul3))
(defun mul3 (m1 m2)
  (let ((result (make-array '(3 3) :element-type 'double-float)))
    (loop-array (result (i j))
     (setf (aref result i j)
           (loop for k below 3 sum
                 (* (aref m1 i k)
                    (aref m2 k j))
                 double-float)))
    result))

(sera:-> det3 ((mat 3))
         (values double-float &optional))
(defun det3 (m)
  (declare (optimize (speed 3)))
  (let ((A (- (* (aref m 1 1) (aref m 2 2))
              (* (aref m 1 2) (aref m 2 1))))
        (B (- (* (aref m 1 0) (aref m 2 2))
              (* (aref m 1 2) (aref m 2 0))))
        (C (- (* (aref m 1 0) (aref m 2 1))
              (* (aref m 1 1) (aref m 2 0)))))
    (+ (+ (* A (aref m 0 0)))
       (- (* B (aref m 0 1)))
       (+ (* C (aref m 0 2))))))

(sera:-> trace3 ((mat 3))
         (values double-float &optional))
(declaim (inline trace3))
(defun trace3 (m)
  (+ (aref m 0 0) (aref m 1 1) (aref m 2 2)))

(sera:-> inv3 ((mat 3))
         (values (mat 3) &optional))
(defun inv3 (m)
  (declare (optimize (speed 3)))
  (let* ((A (- (* (aref m 1 1) (aref m 2 2))
               (* (aref m 2 1) (aref m 1 2))))
         (B (- (* (aref m 2 1) (aref m 0 2))
               (* (aref m 0 1) (aref m 2 2))))
         (C (- (* (aref m 0 1) (aref m 1 2))
               (* (aref m 1 1) (aref m 0 2))))
         (D (- (* (aref m 2 0) (aref m 1 2))
               (* (aref m 1 0) (aref m 2 2))))
         (E (- (* (aref m 0 0) (aref m 2 2))
               (* (aref m 2 0) (aref m 0 2))))
         (F (- (* (aref m 1 0) (aref m 0 2))
               (* (aref m 0 0) (aref m 1 2))))
         (G (- (* (aref m 1 0) (aref m 2 1))
               (* (aref m 2 0) (aref m 1 1))))
         (H (- (* (aref m 2 0) (aref m 0 1))
               (* (aref m 0 0) (aref m 2 1))))
         (I (- (* (aref m 0 0) (aref m 1 1))
               (* (aref m 1 0) (aref m 0 1))))
         (det (+ (* (aref m 0 0) A)
                 (* (aref m 0 1) D)
                 (* (aref m 0 2) G)))
         (res (make-mat3 A B C D E F G H I)))
    (loop for i below (array-total-size res) do
          (setf (row-major-aref res i)
                (/ (row-major-aref res i) det)))
    res))
    
;; Matrix 2x2

(sera:-> mul2 ((mat 2) (mat 2))
         (values (mat 2) &optional))
(declaim (inline mul2))
(defun mul2 (m1 m2)
  (let ((result (make-array '(2 2) :element-type 'double-float)))
    (loop-array (result (i j))
     (setf (aref result i j)
           (loop for k below 2 sum
                 (* (aref m1 i k)
                    (aref m2 k j))
                 double-float)))
    result))

(sera:-> det2 ((mat 2))
         (values double-float &optional))
(declaim (inline det2))
(defun det2 (m)
  (- (* (aref m 0 0) (aref m 1 1))
     (* (aref m 1 0) (aref m 0 1))))

(sera:-> trace2 ((mat 2))
         (values double-float &optional))
(declaim (inline trace2))
(defun trace2 (m)
  (+ (aref m 0 0) (aref m 1 1)))

(sera:-> inv2 ((mat 2))
         (values (mat 2) &optional))
(defun inv2 (m)
  (declare (optimize (speed 3)))
  (let ((det (det2 m))
        (res (make-mat2 (+ (aref m 1 1))
                        (- (aref m 0 1))
                        (- (aref m 1 0))
                        (+ (aref m 0 0)))))
    (loop for i below (array-total-size res) do
          (setf (row-major-aref res i)
                (/ (row-major-aref res i) det)))
    res))
