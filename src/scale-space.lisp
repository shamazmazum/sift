(in-package :sift)

(sera:-> gaussian-parameter
         ((double-float (0d0)) fixnum alex:positive-fixnum)
         (values (double-float (0d0)) &optional))
(declaim (inline gaussian-parameter))
(defun gaussian-parameter (σ k n)
  "Calculate the parameter of Gaussian filter for the k-th image of
ascale space with N images in octave."
  (* σ (expt 2d0 (/ k n))))

(sera:-> gaussian-parameters/octave
         ((double-float (0d0)) alex:positive-fixnum)
         (values list &optional))
(declaim (inline gaussian-parameters/octave))
(defun gaussian-parameters/octave (σ n)
  "Calculate parameters of Gaussian filter for all images in octave +
one image below it + 2 images above."
  (loop for k from -1 below (+ n 2) collect
        (gaussian-parameter σ k n)))

(sera:-> gaussian-scale-space/octave
         ((simple-array double-float (* *)) list)
         (values (simple-array double-float (* * *)) &optional))
(defun gaussian-scale-space/octave (array σs)
  (declare (optimize (speed 3)))
  (let* ((nl (length σs))
         (h (array-dimension array 0))
         (w (array-dimension array 1))
         (result (make-array (list nl h w) :element-type 'double-float)))
    (loop for i below nl
          for σ double-float in σs
          for blurred = (gaussian-blur array σ) do
          (loop-array (blurred (j k))
           (setf (aref result i j k) (aref blurred j k))))
    result))

(sera:defconstructor scale-space
  (octaves list)
  (σs      (simple-array double-float (*))))

(sera:-> gaussian-scale-space
         ((simple-array double-float (* *))
          &key
          (:σ (double-float (0d0)))
          (:n alex:positive-fixnum)
          (:m alex:positive-fixnum))
         (values scale-space &optional))
(defun gaussian-scale-space (a &key (σ 1.6d0) (n 3) (m 3))
  "Compute a set of blurred images of A which constitutes a scale
space of A with M octaves, N images per octave. Each octave also
contains one image below it and two images above it."
  (declare (optimize (speed 3)))
  (let ((σs (gaussian-parameters/octave σ n)))
    (labels ((%go (a m acc)
               (declare (type fixnum m))
               (if (zerop m) acc
                   (let ((octave (gaussian-scale-space/octave a σs)))
                     (%go (downsample a) (1- m) (cons octave acc))))))
      (scale-space (reverse (%go a m nil))
                   (make-array (+ n 3)
                               :element-type 'double-float
                               :initial-contents σs)))))

(sera:-> gaussian->dog ((simple-array double-float (* * *)))
         (values (simple-array double-float (* * *)) &optional))
(defun gaussian->dog (octave)
  "Convert a Gausian scale space to difference of Gaussians space."
  (declare (optimize (speed 3)))
  (let* ((nl (array-dimension octave 0))
         (h  (array-dimension octave 1))
         (w  (array-dimension octave 2))
         (result (make-array (list (1- nl) h w) :element-type 'double-float)))
    (loop for l below (1- nl) do
          (loop-ranges ((i 0 h) (j 0 w))
           (setf (aref result l i j)
                 (- (aref octave (1+ l) i j)
                    (aref octave l i j)))))
    result))
