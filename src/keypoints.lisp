(in-package :sift/core)

(declaim (inline make-coord-vector))
(defun make-coord-vector (i j k)
  (vec3 (float i 0f0)
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
        (v (aref array l i j)))
    ;; TODO: Why I should add this declaration? Why I should not when
    ;; using double-float? Investigate this.
    (declare (type single-float min max))
    (loop-ranges ((%l -1 2) (%i -1 2) (%j -1 2))
     (when (or (not (zerop %l)) (not (zerop %i)) (not (zerop %j)))
       (let ((v (aref array (+ l %l) (+ i %i) (+ j %j))))
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

(sera:-> far-from-borders-p
         ((simple-array single-float (* * *))
          index3 (single-float 0.0))
         (values boolean &optional))
(declaim (inline far-from-borders-p))
(defun far-from-borders-p (array index σ)
  ;; NB: Must be consistent with the size of a window in DESCRIBE-POINT
  ;; (SQRT 2) is a scaling factor which can occur as a result of rotation.
  (let ((margin (ceiling (* (sqrt 2) (+ 5f-1 (* 8 (ceiling σ)))))))
    (flet ((check (i d)
             (and (> i margin)
                  (> (- d i) margin))))
      (and (check (index3-j index) (array-dimension array 1))
           (check (index3-k index) (array-dimension array 2))))))

(sera:-> remove-close-to-borders
         (keypoint (simple-array single-float (* * *)))
         (values (or keypoint null) &optional))
(defun remove-close-to-borders (keypoint array)
  (declare (optimize (speed 3)))
  (if (far-from-borders-p array (keypoint-index keypoint) (keypoint-σ keypoint))
      keypoint))

(sera:-> adjust-keypoint ((or keypoint null)
                          (simple-array single-float (* * *)))
         (values (or keypoint null) &optional))
(defun adjust-keypoint (keypoint dog)
  (when keypoint
    (let* ((index (keypoint-index keypoint))
           ;; Adjust the coordinate
           (hessian  (hessian  dog index))
           (gradient (gradient dog index))
           (diff (scalev (solve hessian gradient) -1f0)))
      ;; Drop keypoints with enormous extremum correction
      (if (shift-ok-p diff)
          (let ((value (+ (aref-index3 dog index)
                          (/ (dot gradient diff) 2))))
            ;; Discard a keypoint with low contrast
            (if (> (abs value) 3f-2)
                (let* ((subhessian (shrink3 hessian))
                       (trace (mtrace subhessian))
                       (det (det2 subhessian))
                       (r 10f0))
                  (declare (dynamic-extent subhessian))
                  ;; Discard a keypoint with big ratio of principal
                  ;; curvatures or negative determinant of Hessian.
                  (if (and (> det 0)
                           (< (/ (expt trace 2) det) (/ (expt (1+ r) 2) r)))
                      (add-coord keypoint diff)))))))))

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
      (loop-ranges ((l 1 (1- n)) (i 1 (1- h)) (j 1 (1- w)))
       (when (keypointp dog-space l i j)
         (let ((keypoint (adjust-keypoint
                          (remove-close-to-borders
                           (keypoint
                            (make-coord-vector l i j)
                            (index3 l i j)
                            octave (aref σs l)
                            ;; Determine orientation later
                            0f0)
                           dog-space)
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
