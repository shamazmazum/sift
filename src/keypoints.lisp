(in-package :sift)

(declaim (inline make-coord-vector))
(defun make-coord-vector (i j k)
  (make-vec3 (float i 0d0)
             (float j 0d0)
             (float k 0d0)))

(sera:-> keypointp ((simple-array double-float (* * *))
                    alex:non-negative-fixnum
                    alex:non-negative-fixnum
                    alex:non-negative-fixnum)
         (values boolean &optional))
(defun keypointp (array l i j)
  (declare (optimize (speed 3)))
  (let ((min ff:double-float-positive-infinity)
        (max ff:double-float-negative-infinity)
        (h (array-dimension array 1))
        (w (array-dimension array 2))
        (v (aref array l i j)))
    (loop-ranges ((%l -1 2) (%i -1 2) (%j -1 2))
     (when (or (not (zerop %l)) (not (zerop %i)) (not (zerop %j)))
       (let ((v (aref array (+ l %l) (mod (+ i %i) h) (mod (+ j %j) w))))
         (setq min (min min v)
               max (max max v)))))
    (or (< v min) (> v max))))

(sera:-> detect-keypoints/octave ((simple-array double-float (* * *))
                                  (simple-array double-float (*))
                                  alex:non-negative-fixnum)
         (values list &optional))
(defun detect-keypoints/octave (gaussian-space σs octave)
  (declare (optimize (speed 3)))
  (let* ((dog-space (gaussian->dog gaussian-space))
         (n (array-dimension dog-space 0))
         (h (array-dimension dog-space 1))
         (w (array-dimension dog-space 2))
         keypoints)
    (loop-ranges ((l 1 (1- n)) (i 0 h) (j 0 w))
     (when (keypointp dog-space l i j)
       (push
        (space-attachment
         (keypoint
          (make-coord-vector l i j)
          (index3 l i j)
          octave (aref σs l)
          ;; Determine orientation later
          0d0)
         dog-space
         gaussian-space)
        keypoints)))
    keypoints))

(sera:-> detect-keypoints (scale-space)
         (values list &optional))
(defun detect-keypoints (scale-space)
  (declare (optimize (speed 3)))
  (let ((octaves (scale-space-octaves scale-space))
        (σs (scale-space-σs scale-space)))
    (loop for octave from 0 by 1
          for gaussian-space in octaves appending
          (detect-keypoints/octave gaussian-space σs octave))))

(sera:-> shift-ok-p ((vec 3))
         (values boolean &optional))
(defun shift-ok-p (shift)
  (declare (optimize (speed 3)))
  (every
   (lambda (x)
     (< (abs x) 5d-1))
   shift))

(sera:-> adjust-keypoint (space-attachment)
         (values (or space-attachment null) &optional))
(defun adjust-keypoint (attachment)
  (let* ((keypoint (space-attachment-point attachment))
         (dog (space-attachment-dog attachment))
         (index (keypoint-index keypoint)))
    ;; Adjust the coordinate
    (let* ((hessian  (hessian/array  dog index))
           (gradient (gradient/array dog index))
           (diff (scalev3 (mul-m3v3 (inv3 hessian) gradient) -1d0)))
      (if (shift-ok-p diff)
          ;; Drop keypoints with enormous extremum correction
          (let ((value (+ (aref-index3 dog index)
                          (/ (dot3 gradient diff) 2))))
            ;; Discard a keypoint with low contrast
            (if (> (abs value) 3d-2)
                (let* ((subhessian (shrink3 hessian))
                       (trace (trace2 subhessian))
                       (det (det2 subhessian))
                       (r 10d0))
                  ;; Discard a keypoint with big ratio of principal curvatures
                  (if (and (> det 0)
                           (< (/ (expt trace 2) det) (/ (expt (1+ r) 2) r)))
                      (add-coord attachment diff)))))))))

(sera:-> adjust-keypoints (list)
         (values list &optional))
(defun adjust-keypoints (attachments)
  "Remove unstanble keypoints and adjust coordinates of remaining
keypoints."
  ;; Hessian can be not invertible
  (ff:with-float-traps-masked (:overflow :invalid :divide-by-zero)
    (reduce
     (lambda (acc attachment)
       (let ((attachment (adjust-keypoint attachment)))
         (if attachment (cons attachment acc) acc)))
     attachments
     :initial-value nil)))

(sera:-> keypoints (scale-space) (values list &optional))
(defun keypoints (scale-space)
  (reduce
   (lambda (attachment acc)
     ;; One keypoint may spawn more keypoints with the same location
     ;; and different orientations.
     (append
      (mapcar
       (lambda (attachment)
         (let ((descriptor (describe-point attachment))
               (keypoint (space-attachment-point attachment)))
           (cons descriptor keypoint)))
       (determine-orientations attachment))
      acc))
   (adjust-keypoints
    (detect-keypoints
     scale-space))
   :from-end t
   :initial-value nil))
