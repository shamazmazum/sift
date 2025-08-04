(in-package :sift/debug)

(sera:-> myaref ((simple-array * (* *)) sift:index3)
         (values t &optional))
(declaim (inline myaref))
(defun myaref (array index)
  (aref array
        (mod (sift:index3-j index) (array-dimension array 0))
        (mod (sift:index3-k index) (array-dimension array 1))))

(sera:-> rotate-array ((simple-array double-float (* *)) double-float)
         (values (simple-array double-float (* *)) &optional))
(defun rotate-array (array ϕ)
  (declare (optimize (speed 3)))
  (assert (= (array-dimension array 0)
             (array-dimension array 1)))
  (let* ((side (array-dimension array 0))
         (cos (cos ϕ))
         (sin (sin ϕ))
         (c (+ sin cos))
         (a (* (/ side 2) (+ (1+ (* c (- sin cos))))))
         (b (* (/ side 2) (- (1- (* c (+ sin cos))))))
         (new-side (floor (* side c)))
         (result (make-array (list new-side new-side)
                             :element-type 'double-float)))
    (sift:loop-array (result (i j))
      (let ((x (+ (* cos i) (- (* sin j)) a))
            (y (+ (* sin i) (+ (* cos j)) b)))
        (setf (aref result i j)
              (if (and (< 0 x side)
                       (< 0 y side))
                  (sift:interpolate
                   (lambda (idx)
                     (myaref array idx))
                   0d0 x y)
                  0d0))))
  result))

(sera:-> scale-array ((simple-array double-float (* *))
                      (double-float 0d0)
                      (double-float 0d0))
         (values (simple-array double-float (* *)) &optional))
(defun scale-array (array s1 s2)
  (declare (optimize (speed 3)))
  (let* ((h (array-dimension array 0))
         (w (array-dimension array 1))
         (result (make-array (list (floor (* s1 h))
                                   (floor (* s2 w)))
                             :element-type 'double-float)))
    (sift:loop-array (result (i j))
      (let ((x (/ i s1))
            (y (/ j s2)))
        (flet ((get-data (idx)
                 (myaref array idx)))
          (setf (aref result i j)
                (sift:interpolate #'get-data 0d0 x y)))))
    result))

(declaim (inline clamp))
(defun clamp (v min max)
  (min (max v min) max))

(sera:-> add-noise ((simple-array double-float (* *)))
         (values (simple-array double-float (* *)) &optional))
(defun add-noise (array)
  (declare (optimize (speed 3)))
  (let ((result (make-array (array-dimensions array)
                            :element-type 'double-float)))
    (sift:loop-array (result (i j))
      (setf (aref result i j)
            (clamp (+ (aref array i j) (random 5d-2)) 0 1)))
    result))

(defun rotation-transform (s ϕ)
  (let* ((cos (cos ϕ))
         (sin (sin ϕ))
         (2/s (/ 2d0 s))
         (s/2 (/ s 2d0))
         (cs/2 (* s/2 (+ cos sin)))
         (a (sift:make-mat3 2/s 0d0 -1d0
                            0d0 2/s -1d0
                            0d0 0d0 1d0))
         (b (sift:make-mat3 cos sin 0d0
                            (- sin) cos 0d0
                            0d0 0d0 1d0))
         (c (sift:make-mat3 s/2 0d0 cs/2
                            0d0 s/2 cs/2
                            0d0 0d0 1d0)))
    (sift:mul3 c (sift:mul3 b a))))

(defun success-rates (data1 data2 m &key (spatial-error 4d0))
  (let* ((kp1 (sift:keypoints (sift:gaussian-scale-space data1)))
         (kp2 (sift:keypoints (sift:gaussian-scale-space data2)))
         (matches (sift:find-matches kp1 kp2))
         (correct 0))
    (loop for (kp1 . kp2) in matches
          for kp-coord = (multiple-value-call #'sift:make-vec3
                           (sift:global-coordinate kp1) 1d0)
          for expected-coord = (sift:mul-m3v3 m kp-coord)
          for match-coord = (multiple-value-call #'sift:make-vec3
                              (sift:global-coordinate kp2) 1d0)
          ;; This match is close enough to the expected position
          when (< (sift:dist3 expected-coord match-coord) spatial-error) do
          (incf correct))
    (cons correct (length matches))))

(defun success-rates-scaling (data)
  (loop for s from 1d0 to 2d0 by 1d-2
        for trans = (scale-array data s s)
        for m = (sift:make-mat3  s  0d0 0d0
                                0d0  s  0d0
                                0d0 0d0 1d0)
        do (format t "Current scale level: ~f~%" s)
        collect (cons s (success-rates data trans m))))

(defun success-rates-rotation (data)
  (loop for ϕ from 0d0 to (/ pi 2) by 1d-2
        for trans = (rotate-array data ϕ)
        for m = (rotation-transform (float (array-dimension data 0) 0d0) ϕ)
        do (format t "Current rotation angle: ~f~%" ϕ)
        collect (cons ϕ (success-rates data trans m))))

