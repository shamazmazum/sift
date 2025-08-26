(in-package :sift/core)

;; Functions in this file compute its Hessian and gradient via 2nd
;; order finite differences.

(alex:define-constant +shift-x+
    (index3 1 0 0)
  :test #'equalp)

(alex:define-constant +shift-y+
    (index3 0 1 0)
  :test #'equalp)

(alex:define-constant +shift-z+
    (index3 0 0 1)
  :test #'equalp)

(sera:-> derivative/1 ((simple-array single-float (* * *)) index3 index3)
         (values single-float &optional))
(declaim (inline derivative/1))
(defun derivative/1 (a index dir)
  (let ((p1 (index3-+ index dir))
        (p2 (index3-- index dir)))
    (declare (dynamic-extent p1 p2))
    (/ (- (aref-index3 a p1) (aref-index3 a p2)) 2)))

(sera:-> gradient ((simple-array single-float (* * *)) index3)
         (values (vec 3) &optional))
(declaim (inline gradient))
(defun gradient (a index)
  (vec3
   (derivative/1 a index +shift-x+)
   (derivative/1 a index +shift-y+)
   (derivative/1 a index +shift-z+)))

(sera:-> derivative/2m ((simple-array single-float (* * *))
                        index3 index3 index3)
         (values single-float &optional))
(declaim (inline derivative/2m))
(defun derivative/2m (a index dir1 dir2)
  (let* ((p1 (index3-+ index dir1))
         (p2 (index3-- index dir1))
         (d1 (derivative/1 a p1 dir2))
         (d2 (derivative/1 a p2 dir2)))
    (declare (dynamic-extent p1 p2))
    (/ (- d1 d2) 2)))

;; NB: Does not go out of bounds! This way HESSIAN can inspect only 26
;; neighbors of a point at INDEX.
(sera:-> derivative/2 ((simple-array single-float (* * *)) index3 index3)
         (values single-float &optional))
(declaim (inline derivative/2))
(defun derivative/2 (a index shift)
  (let ((p1 (index3-+ index shift))
        (p2 (index3-- index shift)))
    (declare (dynamic-extent p1 p2))
    (+ (aref-index3 a p1) (aref-index3 a p2) (* (aref-index3 a index) -2))))

(sera:-> hessian ((simple-array single-float (* * *)) index3)
         (values (mat 3) &optional))
(declaim (inline hessian))
(defun hessian (a index)
  (let ((xx (derivative/2  a index +shift-x+))
        (yy (derivative/2  a index +shift-y+))
        (zz (derivative/2  a index +shift-z+))
        (xy (derivative/2m a index +shift-x+ +shift-y+))
        (xz (derivative/2m a index +shift-x+ +shift-z+))
        (yz (derivative/2m a index +shift-y+ +shift-z+)))
    (mat3 xx xy xz
          xy yy yz
          xz yz zz)))
