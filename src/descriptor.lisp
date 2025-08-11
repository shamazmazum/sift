(in-package :sift)

(deftype %descriptor () '(simple-array double-float (128)))

(declaim (inline flatten-descriptor))
(defun flatten-descriptor (descr)
  (let ((result (make-array (* 4 4 8)
                            :element-type 'double-float))
        (flat (make-array (* 4 4 8)
                          :element-type 'double-float
                          :displaced-to descr
                          :displaced-index-offset 0)))
    (replace result flat)))

(sera:-> normalize-descriptor! (%descriptor)
         (values %descriptor &optional))
(defun normalize-descriptor! (descr)
  (declare (optimize (speed 3)))
  (let ((norm (sqrt (loop for x across descr sum (expt x 2) double-float))))
    (map-into
     descr
     (lambda (x) (/ x norm))
     descr)))

(sera:-> descriptor-postprocess! (%descriptor)
         (values %descriptor &optional))
(defun descriptor-postprocess! (descriptor)
  (declare (optimize (speed 3)))
  ;; Here we normalize, clamp to [0, 0.2] and normalize again
  (let ((normalized (normalize-descriptor! descriptor)))
    (normalize-descriptor!
     (map-into normalized
               (lambda (x)
                 (min x 2d-1))
               normalized))))

(sera:defconstructor descriptor
  (keypoint keypoint)
  (array    %descriptor))

(sera:-> describe-point (keypoint scale-space)
         (values %descriptor &optional))
(defun describe-point (keypoint scale-space)
  "Describe a keypoint in a scale space. The descriptor is a vector of 128 float values
which is invariant to linear transformations of the image which produced this scale
space."
  (declare (optimize (speed 3)))
  (let* ((descriptor (make-array '(4 4 8)
                                 :element-type 'double-float
                                 :initial-element 0d0))
         (gaussian (nth (keypoint-octave keypoint)
                        (scale-space-octaves scale-space)))
         (angle (keypoint-angle keypoint))
         (cos (cos angle))
         (sin (sin angle))
         (m (make-mat3 1d0 0d0 0d0
                       0d0 cos (- sin)
                       0d0 sin cos))
         (σ (keypoint-σ keypoint))
         (hist-width/2 (* (the fixnum (ceiling σ)) 2))
         (hist-width   (* hist-width/2 2))
         (window-width (* hist-width 4))
         (shift (- (* hist-width 2) 5d-1)))
    (declare (type fixnum window-width hist-width hist-width/2))
    (flet ((idx->bin (idx)
             (multiple-value-bind (q r)
                 (floor (- idx hist-width/2) hist-width)
               (let ((v (/ (float r 0d0) hist-width)))
                 (values q (- (1- v))))))
           (incf-descr! (i j k v)
             (when (and (<= 0 i 3)
                        (<= 0 j 3))
               (incf (aref descriptor i j k) v))))
      (loop-ranges ((i 0 window-width) (j 0 window-width))
       (let* ((neighbor (make-vec3 0d0 (- i shift) (- j shift)))
              (rotated (mul-m3v3 m neighbor))
              (coord (add3 (keypoint-coord keypoint) rotated)))
         (declare (dynamic-extent neighbor rotated coord))
         (multiple-value-bind (bin-i wi)
             (idx->bin i)
           (declare (type fixnum bin-i))
           (multiple-value-bind (bin-j wj)
               (idx->bin j)
             (declare (type fixnum bin-j))
             (multiple-value-bind (kp-angle norm)
                 (evaluate-neighbor gaussian coord)
               (multiple-value-bind (bin-o bin-o+1 wo)
                   (angle->bins (- kp-angle angle) 8)
                 (let ((v (* norm (gaussian (* σ 4) neighbor))))
                   ;; Here is this "trilinear interpolation" which means that we
                   ;; distribute V between 8 bins (a combination of two bins for each
                   ;; dimension: two space dimensions and the angle).
                   (incf-descr! bin-i bin-j bin-o
                                (* v wi wj wo))
                   (incf-descr! bin-i bin-j bin-o+1
                                (* v wi wj (- 1 wo)))
                   (incf-descr! bin-i (1+ bin-j) bin-o
                                (* v wi (- 1 wj) wo))
                   (incf-descr! bin-i (1+ bin-j) bin-o+1
                                (* v wi (- 1 wj) (- 1 wo)))
                   (incf-descr! (1+ bin-i) bin-j bin-o
                                (* v (- 1 wi) wj wo))
                   (incf-descr! (1+ bin-i) bin-j bin-o+1
                                (* v (- 1 wi) wj (- 1 wo)))
                   (incf-descr! (1+ bin-i) (1+ bin-j) bin-o
                                (* v (- 1 wi) (- 1 wj) wo))
                   (incf-descr! (1+ bin-i) (1+ bin-j) bin-o+1
                                (* v (- 1 wi) (- 1 wj) (- 1 wo)))))))))))
    (descriptor-postprocess!
     (flatten-descriptor descriptor))))

(sera:-> descriptors (scale-space)
         (values list &optional))
(defun descriptors (scale-space)
  "Calculate keypoints and descriptors (128-element vectors) for a scale space."
  (mapcar
   (lambda (keypoint)
     (descriptor keypoint (describe-point keypoint scale-space)))
   (keypoints scale-space)))
