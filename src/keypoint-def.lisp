(in-package :sift/core)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +pi+ (float pi 0f0)))

(sera:defconstructor keypoint
  "A structure which describes a keypoint in an image."
  (coord  (vec 3))
  (index  index3)
  (octave alex:non-negative-fixnum)
  (σ      (single-float (0f0)))
  (angle  (single-float 0f0 (#.(* 2 +pi+)))))

(picolens:gen-lenses keypoint
  (keypoint-coord  kp-coord)
  (keypoint-index  kp-index)
  (keypoint-octave kp-octave)
  (keypoint-σ      kp-σ)
  (keypoint-angle  kp-angle))

(sera:-> add-coord (keypoint (vec 3))
         (values keypoint &optional))
(declaim (inline add-coord))
(defun add-coord (keypoint diff)
  (picolens:over #'kp-coord
                 (lambda (x)
                   (add3 x diff))
                 keypoint))

(sera:-> new-angle (keypoint (single-float 0f0 (#.(* 2 +pi+))))
         (values keypoint &optional))
(declaim (inline new-angle))
(defun new-angle (keypoint angle)
  (picolens:set #'kp-angle angle keypoint))

(sera:-> image-coordinate (keypoint)
         (values single-float single-float &optional))
(defun image-coordinate (keypoint)
  "Return coordinates of a keypoint in a coordinate system of the
image (scale level 0)."
  (let ((scale (expt 2 (keypoint-octave keypoint)))
        (coord (keypoint-coord keypoint)))
    (values (* (aref coord 1) scale)
            (* (aref coord 2) scale))))

(sera:-> image-coordinate-vector (keypoint)
         (values (vec 3) &optional))
(defun image-coordinate-vector (keypoint)
  "Return coordinates of a keypoint in a coordinate system of the
image (scale level 0) as an augmented vector (3-component vector with
the last element being equal to 1)."
  (multiple-value-bind (x y)
      (image-coordinate keypoint)
    (make-vec3 x y 1f0)))
