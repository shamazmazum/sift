(in-package :sift)

;; Downsample an image using the ideal low-pass filter

(sera:-> cut-ranges (alex:positive-fixnum alex:positive-fixnum)
         (values alex:non-negative-fixnum
                 alex:non-negative-fixnum
                 alex:non-negative-fixnum
                 alex:non-negative-fixnum
                 &optional))
(defun cut-ranges (h w)
  (declare (optimize (speed 3)))
  (let ((start-h (floor h 4))
        (end-h   (ceiling (* h 3) 4))
        (start-w (floor w 4))
        (end-w   (1+ (floor w 2))))
    (values start-h end-h start-w end-w)))

(sera:-> filter-lowpass ((simple-array double-float (* *)))
         (values (simple-array double-float (* *)) &optional))
(defun filter-lowpass (array)
  (declare (optimize (speed 3)))
  (let ((ft (cl-fftw/double:%rfft array))
        (total (float (array-total-size array) 0d0)))
    (multiple-value-bind (sh eh sw ew)
        (cut-ranges (array-dimension array 0)
                    (array-dimension array 1))
      (loop-ranges ((i sh eh) (j sw ew))
       (setf (aref ft i j) #c(0d0 0d0)))
      (loop-array (ft (i j))
       (setf (aref ft i j) (/ (aref ft i j) total))))
    (cl-fftw/double:%irfft ft (array-dimensions array))))

(sera:-> downsample ((simple-array double-float (* *)))
         (values (simple-array double-float (* *)) &optional))
(defun downsample (array)
  "Downsample an image by a factor of 2."
  (declare (optimize (speed 3)))
  (let ((filtered (filter-lowpass array))
        (result (make-array (list (floor (array-dimension array 0) 2)
                                  (floor (array-dimension array 1) 2))
                            :element-type 'double-float)))
    (loop-array (result (i j))
      (setf (aref result i j)
            (aref filtered (* i 2) (* j 2))))
    result))
