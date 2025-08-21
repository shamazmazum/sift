(in-package :sift/debug)

(sera:-> myaref ((simple-array * (* *)) sift/core:index3)
         (values t &optional))
(declaim (inline myaref))
(defun myaref (array index)
  (aref array
        (mod (sift/core:index3-j index) (array-dimension array 0))
        (mod (sift/core:index3-k index) (array-dimension array 1))))

(sera:-> rotate-array ((simple-array single-float (* *)) single-float)
         (values (simple-array single-float (* *)) &optional))
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
                             :element-type 'single-float)))
    (sift/core:loop-array (result (i j))
      (let ((x (+ (* cos i) (- (* sin j)) a))
            (y (+ (* sin i) (+ (* cos j)) b)))
        (setf (aref result i j)
              (if (and (< 0 x side)
                       (< 0 y side))
                  (sift/core:interpolate
                   (lambda (idx)
                     (myaref array idx))
                   0f0 x y)
                  0f0))))
  result))

(sera:-> scale-array ((simple-array single-float (* *))
                      (single-float 0f0)
                      (single-float 0f0))
         (values (simple-array single-float (* *)) &optional))
(defun scale-array (array s1 s2)
  (declare (optimize (speed 3)))
  (let* ((h (array-dimension array 0))
         (w (array-dimension array 1))
         (result (make-array (list (floor (* s1 h))
                                   (floor (* s2 w)))
                             :element-type 'single-float)))
    (sift/core:loop-array (result (i j))
      (let ((x (/ i s1))
            (y (/ j s2)))
        (flet ((get-data (idx)
                 (myaref array idx)))
          (setf (aref result i j)
                (sift/core:interpolate #'get-data 0f0 x y)))))
    result))

(sera:-> add-noise ((simple-array single-float (* *)))
         (values (simple-array single-float (* *)) &optional))
(defun add-noise (array)
  (declare (optimize (speed 3)))
  (let ((result (make-array (array-dimensions array)
                            :element-type 'single-float)))
    (sift/core:loop-array (result (i j))
      (setf (aref result i j)
            (clamp (+ (aref array i j) (random 5f-2)) 0 1)))
    result))

(defun rotation-transform (s ϕ)
  (let* ((cos (cos ϕ))
         (sin (sin ϕ))
         (2/s (/ 2f0 s))
         (s/2 (/ s 2f0))
         (cs/2 (* s/2 (+ cos sin)))
         (a (sift/core:make-mat3
             2/s 0f0 -1f0
             0f0 2/s -1f0
             0f0 0f0 1f0))
         (b (sift/core:make-mat3
             cos sin 0f0
             (- sin) cos 0f0
             0f0 0f0 1f0))
         (c (sift/core:make-mat3
             s/2 0f0 cs/2
             0f0 s/2 cs/2
             0f0 0f0 1f0)))
    (sift/core:mul3 c (sift/core:mul3 b a))))

(defun scale-transform (s)
  (sift/core:make-mat3  s  0f0 0f0
                   0f0  s  0f0
                   0f0 0f0 1f0))

(defun success-rates (data1 data2 m &key (spatial-error 4f0))
  (let* ((kp1 (sift/core:descriptors (sift/core:gaussian-scale-space data1)))
         (kp2 (sift/core:descriptors (sift/core:gaussian-scale-space data2)))
         (matches (sift/core:find-matches kp1 kp2))
         (correct 0))
    (loop for (kp1 . kp2) in matches
          for kp-coord = (multiple-value-call #'sift/core:make-vec3
                           (sift/core:image-coordinate kp1) 1f0)
          for expected-coord = (sift/core:mul-m3v3 m kp-coord)
          for match-coord = (multiple-value-call #'sift/core:make-vec3
                              (sift/core:image-coordinate kp2) 1f0)
          ;; This match is close enough to the expected position
          when (< (sift/core:dist3 expected-coord match-coord) spatial-error) do
          (incf correct))
    (cons correct (length matches))))

(defun success-rates-scaling (data)
  (loop for s from 1f0 to 2f0 by 1f-2
        for trans = (scale-array data s s)
        for m = (scale-transform s)
        do (format t "Current scale level: ~f~%" s)
        collect (cons s (success-rates data trans m))))

(defun success-rates-rotation (data)
  (loop for ϕ from 0f0 to (/ pi 2) by 1f-2
        for trans = (rotate-array data ϕ)
        for m = (rotation-transform (float (array-dimension data 0) 0f0) ϕ)
        do (format t "Current rotation angle: ~f~%" ϕ)
        collect (cons ϕ (success-rates data trans m))))
