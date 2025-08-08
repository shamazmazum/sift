(in-package :sift)

(sera:defconstructor keypoint
  (coord  (vec 3))
  (index  index3)
  (octave alex:non-negative-fixnum)
  (σ      (double-float (0d0)))
  (angle  (double-float 0d0 (#.(* 2 pi)))))

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

(sera:-> new-angle (keypoint (double-float 0d0 (#.(* 2 pi))))
         (values keypoint &optional))
(declaim (inline new-angle))
(defun new-angle (keypoint angle)
  (picolens:set #'kp-angle angle keypoint))

(sera:-> global-coordinate (keypoint)
         (values double-float double-float &optional))
(defun global-coordinate (keypoint)
  "Return global coordinates (at the scale level 0) of a keypoint."
  (let ((scale (expt 2 (keypoint-octave keypoint)))
        (coord (keypoint-coord keypoint)))
    (values (* (aref coord 1) scale)
            (* (aref coord 2) scale))))
