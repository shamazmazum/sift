(in-package :sift/core)

(sera:-> gaussian-parameter
         ((single-float (0f0)) fixnum alex:positive-fixnum)
         (values (single-float (0f0)) &optional))
(declaim (inline gaussian-parameter))
(defun gaussian-parameter (σ k n)
  "Calculate the parameter of Gaussian filter for the k-th image of
ascale space with N images in octave."
  (* σ (expt 2f0 (/ k n))))

(sera:-> gaussian-parameters/octave
         ((single-float (0f0)) alex:positive-fixnum)
         (values list &optional))
(declaim (inline gaussian-parameters/octave))
(defun gaussian-parameters/octave (σ n)
  "Calculate parameters of Gaussian filter for all images in octave +
one image below it + 2 images above."
  (loop for k from -1 below (+ n 2) collect
        (gaussian-parameter σ k n)))

(sera:-> gaussian-scale-space/octave
         ((simple-array single-float (* *)) list)
         (values (simple-array single-float (* * *)) &optional))
(defun gaussian-scale-space/octave (array σs)
  (declare (optimize (speed 3)))
  (let* ((nl (length σs))
         (h (array-dimension array 0))
         (w (array-dimension array 1))
         (s (array-total-size array))
         (result (make-array (list nl h w) :element-type 'single-float)))
    (loop for i below nl
          for σ in σs
          for blurred = (gaussian-blur array σ)
          do (loop with idx1 = (array-row-major-index result i 0 0)
                   for j below s
                   do (setf (row-major-aref result (+ idx1 j))
                            (row-major-aref blurred j))))
    result))

(sera:defconstructor scale-space
  (octaves list)
  (σs      (simple-array single-float (*))))

(sera:-> gaussian-scale-space
         ((simple-array single-float (* *))
          &key
          (:σ (single-float (0f0)))
          (:n alex:positive-fixnum)
          (:m alex:positive-fixnum))
         (values scale-space &optional))
(defun gaussian-scale-space (a &key (σ 1.6f0) (n 3) (m 3))
  "Compute a set of blurred images of @c(A) which constitutes a scale
space of @c(A) with @c(M) octaves, @c(N) images per octave. Each
octave also contains one image below it and two images above it."
  (declare (optimize (speed 3)))
  (let ((σs (gaussian-parameters/octave σ n)))
    (labels ((%go (a m acc)
               (declare (type fixnum m))
               (if (zerop m) acc
                   (let ((octave (gaussian-scale-space/octave a σs)))
                     (%go (downsample a) (1- m) (cons octave acc))))))
      (scale-space (reverse (%go a m nil))
                   (make-array (+ n 3)
                               :element-type 'single-float
                               :initial-contents σs)))))

(sera:-> gaussian->dog ((simple-array single-float (* * *)))
         (values (simple-array single-float (* * *)) &optional))
(defun gaussian->dog (octave)
  "Convert a Gausian scale space to difference of Gaussians space."
  (declare (optimize (speed 3)))
  (let* ((nl (array-dimension octave 0))
         (h  (array-dimension octave 1))
         (w  (array-dimension octave 2))
         (result (make-array (list (1- nl) h w) :element-type 'single-float)))
    (loop for l below (1- nl)
          for this-octave-idx = (array-row-major-index octave     l  0 0)
          for next-octave-idx = (array-row-major-index octave (1+ l) 0 0)
          for result-idx      = (array-row-major-index result     l  0 0)
          do (loop for i below (* h w) do
            (setf (row-major-aref result (+ result-idx i))
                  (- (row-major-aref octave (+ next-octave-idx i))
                     (row-major-aref octave (+ this-octave-idx i))))))
    result))
