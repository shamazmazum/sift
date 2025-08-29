(in-package :sift/core)

(deftype scalar-field () '(sera:-> (index3) (values single-float &optional)))

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

(sera:-> interpolate (scalar-field single-float single-float single-float)
         (values single-float &optional))
(defun interpolate (f x y z)
  (declare (optimize (speed 3)))
  (bind-coords ((qx rx x)
                (qy ry y)
                (qz rz z))
    (let* ((qx+0     qx)
           (qx+1 (1+ qx))
           (qy+0     qy)
           (qy+1 (1+ qy))
           (qz+0     qz)
           (qz+1 (1+ qz))

           (idx000 (index3 qx+0 qy+0 qz+0))
           (idx001 (index3 qx+0 qy+0 qz+1))
           (idx010 (index3 qx+0 qy+1 qz+0))
           (idx011 (index3 qx+0 qy+1 qz+1))
           (idx100 (index3 qx+1 qy+0 qz+0))
           (idx101 (index3 qx+1 qy+0 qz+1))
           (idx110 (index3 qx+1 qy+1 qz+0))
           (idx111 (index3 qx+1 qy+1 qz+1))

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
      v)))
