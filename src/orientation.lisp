(in-package :sift)

(sera:-> evaluate-neighbor ((simple-array double-float (* * *))
                            (vec 3))
         (values double-float double-float &optional))
(defun evaluate-neighbor (array coord)
  "Return a direction of the gradient and its magnitude at this
point."
  (declare (optimize (speed 3)))
  (labels ((access (index)
             (aref-index3/p array index))
           (derivative (direction)
             (interpolate
              (lambda (index)
                (derivative/1 #'access index direction))
              (aref coord 0)
              (aref coord 1)
              (aref coord 2))))
    ;; "X" is the second coordinate, "Y" is the third coordinate
    ;; (The first coordinate is the scale parameter).
    (let ((x (derivative +shift-y+))
          (y (derivative +shift-z+)))
      (values (atan y x)
              (sqrt (+ (expt x 2) (expt y 2)))))))

(sera:-> %gaussian ((double-float 0d0) double-float double-float)
         (values double-float &optional))
(defun %gaussian (σ x y)
  (declare (optimize (speed 3)))
  (/ (exp (- (/ (+ (expt x 2) (expt y 2)) 2 (expt σ 2))))
     (sqrt (* 2 pi))
     σ))

(sera:-> gaussian ((double-float 0d0) (vec 3))
         (values double-float &optional))
(declaim (inline gaussian))
(defun gaussian (σ coord)
  (%gaussian σ (aref coord 1) (aref coord 2)))

;; This function distributes a value between the two closest bins with
;; weight between 0 and 1.
(sera:-> angle->bins (double-float alex:positive-fixnum)
         (values alex:non-negative-fixnum alex:non-negative-fixnum double-float &optional))
(defun angle->bins (x nbins)
  (declare (optimize (speed 3)))
  (let* ((w   (/ (* 2 pi) nbins))
         (w/2 (/ w 2)))
    (multiple-value-bind (q r)
        (floor (- x w/2) w)
      (declare (type fixnum q))
      (let ((v (/ r w)))
        (values (mod q nbins)
                (mod (1+ q) nbins)
                (- 1 v))))))

(sera:-> orientation-histogram (keypoint (simple-array double-float (* * *)))
         (values (simple-array double-float (36)) &optional))
(defun orientation-histogram (keypoint gaussian)
  (declare (optimize (speed 3)))
  (let* ((σ (* (keypoint-σ keypoint) 1.5))
         (l (1+ (* (the fixnum (ceiling σ)) 4)))
         (w (floor l 2))
         (hist (make-array 36
                           :element-type 'double-float
                           :initial-element 0d0)))
    (declare (type fixnum l))
    (loop-ranges ((i 0 l) (j 0 l))
     (let* ((diff (make-vec3 0d0 (float (- i w) 0d0) (float (- j w) 0d0)))
            (coord (add3 diff (keypoint-coord keypoint))))
       (multiple-value-bind (phase magnitude)
           (evaluate-neighbor gaussian coord)
         (multiple-value-bind (bin1 bin2 w)
             (angle->bins phase 36)
           (let ((v (* magnitude (gaussian σ diff))))
             (incf (aref hist bin1) (* v w))
             (incf (aref hist bin2) (* v (- 1 w))))))))
    hist))

(defconstant +ori-bin-width+ (/ (* 2 pi) 36))
(defconstant +ori-bin-center+ (/ pi 36))

(sera:-> determine-orientations (keypoint (simple-array double-float (* * *)))
         (values list &optional))
(defun determine-orientations (keypoint gaussian)
  (declare (optimize (speed 3)))
  (loop with histogram = (orientation-histogram keypoint gaussian)
        with max double-float = (reduce #'max histogram)
        for bin below 36
        for c = (aref histogram bin)
        ;; Access pattern can be circular since this is histogram of
        ;; angles in the range [0, 2pi).
        for l = (aref histogram (mod (1- bin) 36))
        for r = (aref histogram (mod (1+ bin) 36))
        ;; When c is a "rough" local extremum in the histogram which
        ;; is big enough...
        when (and (> c (* max 8d-1))
                  (>= c l)
                  (>= c r))
        collect
        ;; Location of the extremum of a parabola
        (let ((extremum (/ (- l r) 2 (+ r l (* c -2)))))
          (assert (< -1/2 extremum 1/2))
          (new-angle
           keypoint
           (+ +ori-bin-center+
              (* (+ bin extremum) +ori-bin-width+))))))
