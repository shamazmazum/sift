(in-package :sift/util)

(sera:-> histogram ((simple-array (unsigned-byte 8) (* *)))
         (values (simple-array (unsigned-byte 64) (256)) &optional))
(defun histogram (array)
  (declare (optimize (speed 3)))
  (let ((hist (make-array 256
                          :element-type '(unsigned-byte 64)
                          :initial-element 0)))
    (loop for i below (array-total-size array) do
          (incf (aref hist (row-major-aref array i))))
    hist))

(sera:-> cdf ((simple-array (unsigned-byte 64) (256)))
         (values (simple-array (unsigned-byte 64) (256)) &optional))
(defun cdf (pmf)
  (declare (optimize (speed 3)))
  (let ((cdf (make-array 256 :element-type '(unsigned-byte 64))))
    (setf (aref cdf 0) (aref pmf 0))
    (loop for i from 1 below (length cdf) do
          (setf (aref cdf i) (+ (aref cdf (1- i)) (aref pmf i))))
    cdf))

(sera:-> normalize-cdf ((simple-array (unsigned-byte 64) (256)))
         (values (simple-array single-float (256)) &optional))
(defun normalize-cdf (cdf)
  (declare (optimize (speed 3)))
  (let ((%cdf (make-array (length cdf) :element-type 'single-float))
        (m (float (aref cdf 255))))
    (loop for i below (length cdf) do
          (setf (aref %cdf i) (/ (aref cdf i) m)))
    %cdf))

(sera:-> lookup-table ((simple-array (unsigned-byte 8) (* *)))
         (values (simple-array single-float (256)) &optional))
(defun lookup-table (array)
  (normalize-cdf
   (cdf
    (histogram array))))

;; https://en.wikipedia.org/wiki/Histogram_equalization
(sera:-> enhance-contrast ((simple-array (unsigned-byte 8) (* *)))
         (values (simple-array single-float (* *)) &optional))
(defun enhance-contrast (array)
  "Enhance contrast of an image using histogram equalization
algorithm. The input array has the element type @c((unsigned-byte 8))
(which can be obtained with the help of @c(load-image) function) and
the output contains single floats in the range \\([0, 1]\\)."
  (declare (optimize (speed 3)))
  (let ((table (lookup-table array))
        (result (make-array (array-dimensions array) :element-type 'single-float)))
    (loop for i below (array-total-size array)
          for x = (row-major-aref array i) do
          (setf (row-major-aref result i) (aref table x)))
    result))
