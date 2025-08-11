(in-package :sift)

(declaim (inline interp))
(defun interp (v1 v2 x)
  (+ v1 (* x (- v2 v1))))

(defmacro bind-coords (coords &body body)
  (car
   (reduce
    (lambda (coord acc)
      (destructuring-bind (q r x)
          coord
        `((multiple-value-bind (,q ,r)
              (floor ,x)
            (declare (type fixnum ,q))
            ,@acc))))
    coords
    :initial-value body
    :from-end t)))

(sera:-> interpolate (scalar-field double-float double-float double-float)
         (values double-float &optional))
(declaim (inline interpolate))
(defun interpolate (f x y z)
  (bind-coords ((qx rx x)
                (qy ry y)
                (qz rz z))
    ;; For better code formatting
    (flet ((id (x) x))
      (let* ((idx000 (index3 (id qx) (id qy) (id qz)))
             (idx001 (index3 (id qx) (id qy) (1+ qz)))
             (idx010 (index3 (id qx) (1+ qy) (id qz)))
             (idx011 (index3 (id qx) (1+ qy) (1+ qz)))
             (idx100 (index3 (1+ qx) (id qy) (id qz)))
             (idx101 (index3 (1+ qx) (id qy) (1+ qz)))
             (idx110 (index3 (1+ qx) (1+ qy) (id qz)))
             (idx111 (index3 (1+ qx) (1+ qy) (1+ qz)))

             (v000 (funcall f idx000))
             (v001 (funcall f idx001))
             (v010 (funcall f idx010))
             (v011 (funcall f idx011))
             (v100 (funcall f idx100))
             (v101 (funcall f idx101))
             (v110 (funcall f idx110))
             (v111 (funcall f idx111))

             (v00 (interp v000 v001 rz))
             (v01 (interp v010 v011 rz))
             (v10 (interp v100 v101 rz))
             (v11 (interp v110 v111 rz))

             (v0 (interp v00 v01 ry))
             (v1 (interp v10 v11 ry))

             (v (interp v0 v1 rx)))
        (declare (dynamic-extent idx000 idx001 idx010 idx011
                                 idx100 idx101 idx110 idx111))
        v))))
