(in-package :sift/core)

(declaim (inline make-coord-vector))
(defun make-coord-vector (i j k)
  (make-vec3 (float i 0f0)
             (float j 0f0)
             (float k 0f0)))

(sera:-> keypointp ((simple-array single-float (* * *))
                    alex:non-negative-fixnum
                    alex:non-negative-fixnum
                    alex:non-negative-fixnum)
         (values boolean &optional))
(defun keypointp (array l i j)
  (declare (optimize (speed 3)))
  (let ((min ff:single-float-positive-infinity)
        (max ff:single-float-negative-infinity)
        (h (array-dimension array 1))
        (w (array-dimension array 2))
        (v (aref array l i j)))
    ;; TODO: Why I should add this declaration? Why I should not when
    ;; using double-float? Investigate this.
    (declare (type single-float min max))
    (loop-ranges ((%l -1 2) (%i -1 2) (%j -1 2))
     (when (or (not (zerop %l)) (not (zerop %i)) (not (zerop %j)))
       (let ((v (aref array (+ l %l) (mod (+ i %i) h) (mod (+ j %j) w))))
         (setq min (min min v)
               max (max max v)))))
    ;; A point is a keypoint if its value in DoG space is greater than
    ;; all its neighbors or lesser than all of its neighbors.
    (or (< v min) (> v max))))

;; The original paper says something like "If an adjusted coordinate
;; of a keypoint is further than 0.5 at any coordinate, choose a new
;; keypoint and adjust it again". In this library, however, such a
;; keypoint is simply dropped.
(sera:-> shift-ok-p ((vec 3))
         (values boolean &optional))
(defun shift-ok-p (shift)
  (declare (optimize (speed 3)))
  (every
   (lambda (x)
     (< (abs x) 5f-1))
   shift))

(sera:-> adjust-keypoint (keypoint (simple-array single-float (* * *)))
         (values (or keypoint null) &optional))
(defun adjust-keypoint (keypoint dog)
  (let* ((index (keypoint-index keypoint))
         ;; Adjust the coordinate
         (hessian  (hessian/array  dog index))
         (gradient (gradient/array dog index))
         (diff (scalev3 (mul-m3v3 (inv3 hessian) gradient) -1f0)))
    ;; Drop keypoints with enormous extremum correction
    (if (shift-ok-p diff)
        (let ((value (+ (aref-index3 dog index)
                        (/ (dot3 gradient diff) 2))))
          ;; Discard a keypoint with low contrast
          (if (> (abs value) 3f-2)
              (let* ((subhessian (shrink3 hessian))
                     (trace (trace2 subhessian))
                     (det (det2 subhessian))
                     (r 10f0))
                (declare (dynamic-extent subhessian))
                ;; Discard a keypoint with big ratio of principal
                ;; curvatures or negative determinant of Hessian.
                (if (and (> det 0)
                         (< (/ (expt trace 2) det) (/ (expt (1+ r) 2) r)))
                    (add-coord keypoint diff))))))))

(sera:-> detect-keypoints/octave ((simple-array single-float (* * *))
                                  (simple-array single-float (*))
                                  alex:non-negative-fixnum)
         (values list &optional))
(defun detect-keypoints/octave (gaussian-space σs octave)
  (declare (optimize (speed 3)))
  (let* ((dog-space (gaussian->dog gaussian-space))
         (n (array-dimension dog-space 0))
         (h (array-dimension dog-space 1))
         (w (array-dimension dog-space 2))
         keypoints)
    ;; Hessian can be not invertible
    (ff:with-float-traps-masked (:overflow :invalid :divide-by-zero)
      (loop-ranges ((l 1 (1- n)) (i 0 h) (j 0 w))
       (when (keypointp dog-space l i j)
         (let ((keypoint (adjust-keypoint
                          (keypoint
                           (make-coord-vector l i j)
                           (index3 l i j)
                           octave (aref σs l)
                           ;; Determine orientation later
                           0f0)
                          dog-space)))
           (when keypoint
             (push keypoint keypoints))))))
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

(sera:-> keypoints (scale-space) (values list &optional))
(defun keypoints (scale-space)
  "Detect keypoints in a scale space."
  (reduce
   (lambda (keypoint acc)
     ;; One keypoint may spawn more keypoints with the same location
     ;; and different orientations.
     (append
      (determine-orientations
       keypoint
       (nth (keypoint-octave keypoint)
            (scale-space-octaves scale-space)))
      acc))
   (detect-keypoints
    scale-space)
   :from-end t
   :initial-value nil))
