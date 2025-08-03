(in-package :sift)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (sera:-> z-order ((unsigned-byte 32)
                    (unsigned-byte 32))
           (values (unsigned-byte 64) &optional))
  (defun z-order (x y)
    (let ((n (max (integer-length x)
                  (integer-length y))))
      (labels ((%go (acc i)
                 (if (= i n) acc
                     (%go
                      (logior
                       (* (logior (* (ldb (byte 1 i) x))
                                  (* (ldb (byte 1 i) y) 2))
                          (ash 1 (* i 2)))
                       acc)
                      (1+ i)))))
        (%go 0 0)))))

(alex:define-constant +neighborhood+
    (let (points)
      (loop-ranges ((i -8 8) (j -8 8))
       (let ((bin-i (+ (floor i 4) 2))
             (bin-j (+ (floor j 4) 2))
             (coord (make-vec3 0d0
                               (+ i 5d-1)
                               (+ j 5d-1)))
             bins)
         (loop-ranges ((si 0 3) (sj 0 3))
          (let* ((%bin-i (- (1+ bin-i) si))
                 (%bin-j (- (1+ bin-j) sj))
                 (bin-center (make-vec3 0d0
                                        (float (+ 2 (* 4 (- %bin-i 2))) 0d0)
                                        (float (+ 2 (* 4 (- %bin-j 2))) 0d0))))
            (when (and (<= 0 %bin-i 3)
                       (<= 0 %bin-j 3))
              (push (cons (z-order %bin-i %bin-j)
                          (- (1- (/ (dist3 coord bin-center) 8))))
                    bins))))
         (push (cons bins coord) points)))
      points)
  :test #'equalp)

(declaim (inline flatten-descriptor))
(defun flatten-descriptor (descr)
  (let ((result (make-array (* 16 8)
                            :element-type 'double-float))
        (flat (make-array (* 16 8)
                          :element-type 'double-float
                          :displaced-to descr
                          :displaced-index-offset 0)))
    (replace result flat)))

(sera:-> normalize-descriptor! (descriptor)
         (values descriptor &optional))
(defun normalize-descriptor! (descr)
  (declare (optimize (speed 3)))
  (let ((norm (sqrt (loop for x across descr sum (expt x 2) double-float))))
    (map-into
     descr
     (lambda (x) (/ x norm))
     descr)))

(sera:-> descriptor-postprocess! (descriptor)
         (values descriptor &optional))
(defun descriptor-postprocess! (descriptor)
  (declare (optimize (speed 3)))
  (let ((normalized (normalize-descriptor! descriptor)))
    (normalize-descriptor!
     (map-into normalized
               (lambda (x)
                 (min x 2d-1))
               normalized))))

(sera:-> describe-point (space-attachment)
         (values descriptor &optional))
(defun describe-point (attachment)
  (declare (optimize (speed 3)))
  (let* ((descriptor (make-array '(16 8)
                                 :element-type 'double-float
                                 :initial-element 0d0))
         (keypoint (space-attachment-point attachment))
         (gaussian (space-attachment-gaussian attachment))
         (angle (keypoint-angle keypoint))
         (cos (cos angle))
         (sin (sin angle))
         (m (make-mat3 1d0 0d0 0d0
                       0d0 cos (- sin)
                       0d0 sin cos)))
    (loop for (weights . neighbor) in +neighborhood+
          for rotated = (mul-m3v3 m neighbor)
          for coord = (add3 (keypoint-coord keypoint) rotated) do
          (multiple-value-bind (kp-angle norm)
              (evaluate-neighbor gaussian coord)
            (let ((bin (angle->bin (- kp-angle angle) 8)))
              (loop for (hist-idx . weight) in weights
                    for w double-float = weight do
                    (incf (aref descriptor hist-idx bin)
                          ;; Half width of the window?
                          (* norm w (gaussian 4d0 rotated)))))))
    (descriptor-postprocess!
     (flatten-descriptor descriptor))))
