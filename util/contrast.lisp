(in-package :sift/util)

(deftype histograms (type) `(simple-array ,type (* * 256)))
(deftype image      (type) `(simple-array ,type 2))

(sera:defconstructor bin-dimensions
 (h alexandria:positive-fixnum)
 (w alexandria:positive-fixnum))

(declaim (inline bin-size))
(defun bin-size (dim)
  (* (bin-dimensions-h dim)
     (bin-dimensions-w dim)))

(sera:-> histogram-row-major-index
         ((histograms *) fixnum fixnum)
         (values alex:non-negative-fixnum &optional))
(declaim (inline histogram-row-major-index))
(defun histogram-row-major-index (histogram i j)
  (let ((i (clamp i 0 (1- (array-dimension histogram 0))))
        (j (clamp j 0 (1- (array-dimension histogram 1)))))
    (array-row-major-index histogram i j 0)))

(sera:-> histograms
         ((image (unsigned-byte 8)) bin-dimensions)
         (values (histograms (unsigned-byte 64)) &optional))
(defun histograms (image bin-dimensions)
  (declare (optimize (speed 3)))
  (let ((h  (array-dimension image 0))
        (w  (array-dimension image 1))
        (bh (bin-dimensions-h bin-dimensions))
        (bw (bin-dimensions-w bin-dimensions)))
    (let ((nh (floor h bh))
          (nw (floor w bw)))
      (let ((histograms (make-array (list nh nw 256)
                                    :element-type '(unsigned-byte 64)
                                    :initial-element 0)))
        (sift/core:loop-array (image (i j))
          ;; The last histogram bin (along any axis) can be a bit
          ;; larger than other bins
          (let ((base-idx (histogram-row-major-index
                           histograms
                           (floor i bh)
                           (floor j bw))))
            (incf (row-major-aref histograms (+ base-idx (aref image i j))))))
        histograms))))

(sera:-> histograms->cdfs ((histograms (unsigned-byte 64)))
         (values (histograms single-float) &optional))
(defun histograms->cdfs (histograms)
  (declare (optimize (speed 3)))
  (let ((cdfs            (make-array (array-dimensions histograms)
                                     :element-type '(unsigned-byte 64)))
        (normalized-cdfs (make-array (array-dimensions histograms)
                                     :element-type 'single-float)))
    (sift/core:loop-ranges ((i 0 (array-dimension histograms 0))
                            (j 0 (array-dimension histograms 1)))
      (let ((idx (array-row-major-index histograms i j 0)))
        (setf (row-major-aref cdfs idx) (row-major-aref histograms idx))
        (loop for l from 1 below 256
              for %idx = (+ idx l) do
              (setf (row-major-aref cdfs %idx)
                    (+ (row-major-aref cdfs (1- %idx))
                       (row-major-aref histograms %idx))))
        (loop with max = (float (row-major-aref cdfs (+ idx 255)))
              for l below 256
              for %idx = (+ idx l) do
              (setf (row-major-aref normalized-cdfs %idx)
                    (/ (row-major-aref cdfs %idx) max)))))
    normalized-cdfs))

(sera:-> clip-histogram!
         ((histograms (unsigned-byte 64))
          alex:positive-fixnum
          alex:non-negative-fixnum
          (single-float 0.0 1.0))
         (values &optional))
(defun clip-histogram! (histograms bin-size index clip-limit)
  (declare (optimize (speed 3)))
  (let* ((clip-value (floor (* clip-limit bin-size)))
         (clipped-sum
          (loop for i below 256
                for idx = (+ index i)
                for x   = (row-major-aref histograms idx)
                for x-clipped = (min x clip-value)
                for residue   = (- x x-clipped)
                do (setf (row-major-aref histograms idx) x-clipped)
                sum residue of-type (unsigned-byte 64))))
    (declare (type fixnum clip-value))
    (loop with increment = (floor clipped-sum 256)
          for i below 256
          for idx = (+ index i) do
          (incf (row-major-aref histograms idx) increment))
    ;; 5 in each bin
    (if (< clipped-sum (* 256 5))
        (values)
        (clip-histogram! histograms bin-size index clip-limit))))

(sera:-> clip-histograms!
         ((histograms (unsigned-byte 64))
          alex:positive-fixnum
          (single-float 0.0 1.0))
         (values (histograms (unsigned-byte 64)) &optional))
(defun clip-histograms! (histograms bin-size clip-limit)
  (declare (optimize (speed 3)))
  (sift/core:loop-ranges ((i 0 (array-dimension histograms 0))
                          (j 0 (array-dimension histograms 1)))
    (clip-histogram!
     histograms bin-size
     (array-row-major-index histograms i j 0)
     clip-limit))
  histograms)

(serapeum:-> clahe-transform-pixel
             ((histograms single-float)
              (unsigned-byte 8)
              bin-dimensions
              alex:non-negative-fixnum
              alex:non-negative-fixnum)
             (values single-float &optional))
(defun clahe-transform-pixel (table v bin-dimensions i j)
  (declare (optimize (speed 3)))
  (flet ((access-pixel (i j)
           (let ((index (histogram-row-major-index table i j)))
             (row-major-aref table (+ index v)))))
    (declare (inline access-pixel))
    (let ((bin-dim-h (float (bin-dimensions-h bin-dimensions)))
          (bin-dim-w (float (bin-dimensions-w bin-dimensions))))
      (interpolate/linear #'access-pixel
                          (- i (/ bin-dim-h 2))
                          (- j (/ bin-dim-w 2))
                          bin-dim-h bin-dim-w))))

(serapeum:-> default-bin-dimensions ((image *))
             (values bin-dimensions &optional))
(defun default-bin-dimensions (image)
  (bin-dimensions
   (floor (array-dimension image 0) 8)
   (floor (array-dimension image 1) 8)))

;; https://en.wikipedia.org/wiki/Adaptive_histogram_equalization
(sera:-> enhance-contrast ((image (unsigned-byte 8)) &key
                           (:bin-dimensions bin-dimensions)
                           (:clip-limit     (single-float 0.0 1.0)))
         (values (image single-float) &optional))
(defun enhance-contrast (image &key
                                 (bin-dimensions (default-bin-dimensions image))
                                 (clip-limit 0.015))
  "Perform contrast limited adaptive histogram equalization (constrast
enhancement) of an image. The parameter @c(BIN-DIMENSIONS) controls
dimensions of a single histogram."
  (declare (optimize (speed 3)))
  (let ((cdf (histograms->cdfs
              (clip-histograms!
               (histograms image bin-dimensions)
               (bin-size bin-dimensions)
               clip-limit)))
        (result (make-array (array-dimensions image) :element-type 'single-float)))
    (sift/core:loop-array (result (i j))
      (setf (aref result i j)
            (clahe-transform-pixel cdf (aref image i j) bin-dimensions i j)))
    result))
