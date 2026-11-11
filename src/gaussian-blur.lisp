(in-package :sift/core)

(sera:-> gaussian-kernel-1d ((single-float (0f0)))
         (values (simple-array single-float (*)) &optional))
(defun gaussian-kernel-1d (σ)
  (loop with length = (1+ (* (ceiling σ) 4))
        with w = (floor length 2)
        with kernel = (make-array length :element-type 'single-float)
        for i below length
        for x = (- i w)
        for v = (exp (- (/ (expt x 2) (expt σ 2) 2)))
        do (setf (aref kernel i) v)
        sum v into sum
        finally
        (map-into kernel (lambda (x) (/ x sum)) kernel)
        (return kernel)))

(sera:-> gaussian-kernel ((single-float (0f0)))
         (values (simple-array single-float (* *)) &optional))
(defun gaussian-kernel (σ)
  (let* ((kernel-1d (gaussian-kernel-1d σ))
         (length (length kernel-1d))
         (kernel (make-array (list length length)
                             :element-type 'single-float)))
    (loop-array (kernel (i j))
     (setf (aref kernel i j)
           (* (aref kernel-1d i)
              (aref kernel-1d j))))
    kernel))

(sera:-> gaussian-kernel-padded (alex:positive-fixnum
                                 alex:positive-fixnum
                                 (single-float (0f0)))
         (values (simple-array single-float (* *)) &optional))
(defun gaussian-kernel-padded (h w σ)
  (let* ((kernel (make-array (list h w)
                             :element-type 'single-float
                             :initial-element 0f0))
         (kernel-tight (gaussian-kernel σ))
         (size (array-dimension kernel-tight 0))
         (width (floor size 2)))
    (flet ((centered-idx (i q)
             (mod (- i width) q)))
      ;; TODO: Check that tight-dimensions <= dimensions
      (loop-array (kernel-tight (i j))
        (setf (aref kernel (centered-idx i h) (centered-idx j w))
              (aref kernel-tight i j))))
    kernel))

(sera:-> gaussian-blur ((simple-array single-float (* *))
                        (single-float (0f0)))
         (values (simple-array single-float (* *)) &optional))
(defun gaussian-blur (array σ)
  (declare (optimize (speed 3)))
  (let ((dimensions (array-dimensions array))
        (size       (array-total-size array))
        (kernel (gaussian-kernel-padded
                 (array-dimension array 0)
                 (array-dimension array 1)
                 σ)))
    (cl-fftw/single:%irfft
     (cl-fftw/single:with-plan (plan cl-fftw/single:create-rfft-plan dimensions)
       (let ((array-fft  (cl-fftw/single:rfft plan array))
             (kernel-fft (cl-fftw/single:rfft plan kernel)))
         (loop for i below (array-total-size array-fft) do
           (setf (row-major-aref array-fft i)
                 (/ (* (row-major-aref array-fft  i)
                       (row-major-aref kernel-fft i))
                    ;; Normalize
                    size)))
         array-fft))
     dimensions)))
