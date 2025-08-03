(in-package :sift)

(sera:defconstructor keypoint
  (coord  (vec 3))
  (index  index3)
  (octave alex:non-negative-fixnum)
  (σ      (double-float (0d0)))
  (angle  (double-float 0d0 (#.(* 2 pi)))))

;; Keypoint with a descriptor
(deftype descriptor () '(simple-array double-float (128)))
(deftype described-keypoint () '(cons descriptor keypoint))

(picolens:gen-lenses keypoint
  (keypoint-coord  kp-coord)
  (keypoint-index  kp-index)
  (keypoint-octave kp-octave)
  (keypoint-σ      kp-σ)
  (keypoint-angle  kp-angle))

;; Keypoint with a binding to a scale space
(sera:defconstructor space-attachment
  (point    keypoint)
  (dog      (simple-array double-float (* * *)))
  (gaussian (simple-array double-float (* * *))))

(picolens:gen-lenses space-attachment
  (space-attachment-point    sa-point)
  (space-attachment-dog      sa-dog)
  (space-attachment-gaussian sa-gaussian))

(defparameter *coord-lens*
  (picolens:compose #'sa-point #'kp-coord))

(defparameter *angle-lens*
  (picolens:compose #'sa-point #'kp-angle))

(sera:-> add-coord (space-attachment (vec 3))
         (values space-attachment &optional))
(declaim (inline add-coord))
(defun add-coord (attachment diff)
  (picolens:over *coord-lens*
                 (lambda (x)
                   (add3 x diff))
                 attachment))

(sera:-> new-angle (space-attachment (double-float 0d0 (#.(* 2 pi))))
         (values space-attachment &optional))
(declaim (inline new-angle))
(defun new-angle (attachment angle)
  (picolens:set *angle-lens* angle attachment))

(sera:-> global-coordinate (keypoint)
         (values double-float double-float &optional))
(defun global-coordinate (keypoint)
  "Return global coordinates (at the scale level 0) of a keypoint."
  (let ((scale (expt 2 (keypoint-octave keypoint)))
        (coord (keypoint-coord keypoint)))
    (values (* (aref coord 1) scale)
            (* (aref coord 2) scale))))
