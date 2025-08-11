(in-package :sift)

;; Here we have a function F of type SCALAR-FIELD which takes 3
;; integer arguments (wrapped in INDEX3) and returns a double
;; float. Functions in this file compute its Hessian and gradient via
;; 2nd order finite differences.

(deftype scalar-field () '(sera:-> (index3) (values double-float &optional)))

(alex:define-constant +shift-x+
    (index3 1 0 0)
  :test #'equalp)

(alex:define-constant +shift-y+
    (index3 0 1 0)
  :test #'equalp)

(alex:define-constant +shift-z+
    (index3 0 0 1)
  :test #'equalp)

(sera:-> derivative/1 (scalar-field index3 index3)
         (values double-float &optional))
(declaim (inline derivative/1))
(defun derivative/1 (f index dir)
  (let ((p1 (index3-+ index dir))
        (p2 (index3-- index dir)))
    (declare (dynamic-extent p1 p2))
    (/ (- (funcall f p1) (funcall f p2)) 2)))

(sera:-> gradient (scalar-field index3)
         (values (vec 3) &optional))
(declaim (inline gradient))
(defun gradient (f index)
  (make-vec3
   (derivative/1 f index +shift-x+)
   (derivative/1 f index +shift-y+)
   (derivative/1 f index +shift-z+)))

(declaim (inline gradient/array))
(defun gradient/array (array index)
  (gradient
   (lambda (index)
     (aref-index3/p array index))
   index))

(sera:-> derivative/2m (scalar-field index3 index3 index3)
         (values double-float &optional))
(declaim (inline derivative/2m))
(defun derivative/2m (f index dir1 dir2)
  (let* ((p1 (index3-+ index dir1))
         (p2 (index3-- index dir1))
         (d1 (derivative/1 f p1 dir2))
         (d2 (derivative/1 f p2 dir2)))
    (declare (dynamic-extent p1 p2))
    (/ (- d1 d2) 2)))

;; NB: Does not go out of bounds! This way HESSIAN can inspect only 26
;; neighbors of a point at INDEX.
(sera:-> derivative/2 (scalar-field index3 index3)
         (values double-float &optional))
(declaim (inline derivative/2))
(defun derivative/2 (f index shift)
  (let ((p1 (index3-+ index shift))
        (p2 (index3-- index shift)))
    (declare (dynamic-extent p1 p2))
    (+ (funcall f p1) (funcall f p2) (* (funcall f index) -2))))

(sera:-> hessian (scalar-field index3)
         (values (mat 3) &optional))
(declaim (inline hessian))
(defun hessian (f index)
  (let ((xx (derivative/2  f index +shift-x+))
        (yy (derivative/2  f index +shift-y+))
        (zz (derivative/2  f index +shift-z+))
        (xy (derivative/2m f index +shift-x+ +shift-y+))
        (xz (derivative/2m f index +shift-x+ +shift-z+))
        (yz (derivative/2m f index +shift-y+ +shift-z+)))
    (make-mat3 xx xy xz
               xy yy yz
               xz yz zz)))

(declaim (inline hessian/array))
(defun hessian/array (array index)
  (hessian
   (lambda (index)
     (aref-index3/p array index))
   index))
