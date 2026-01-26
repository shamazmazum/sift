(in-package :sift/transform)

(sera:-> select-rows ((simple-array single-float (* *)) list)
         (values (simple-array single-float (* *)) &optional))
(defun select-rows (m is)
  (declare (optimize (speed 3)))
  (let* ((cols (array-dimension m 1))
         (result (make-array (list (length is) cols)
                             :element-type 'single-float)))
    (loop for row in is
          for i fixnum from 0 by 1 do
            (loop for j below cols do
              (setf (aref result i j)
                    (aref m row j))))
    result))

;; Convert a list of keypoint pairs (matches) into 2 Nx3 matrices
;; where the first matrix corresponds to the first keypoint in a pair
;; and the second matrix corresponds to the second keypoint in a pair.
(sera:-> matches->matrices (list)
         (values (simple-array single-float (* 3))
                 (simple-array single-float (* 3))
                 &optional))
(defun matches->matrices (matches)
  (flet ((coord-list (kp)
           (multiple-value-bind (x y)
               (sift/core:image-coordinate kp)
             (list x y 1f0))))
    (multiple-value-bind (xs ys n)
        (loop for (kp1 . kp2) in matches
              collect (coord-list kp1) into xs
              collect (coord-list kp2) into ys
              sum 1 into n
              finally (return (values xs ys n)))
      (flet ((%make-array (list)
               (make-array (list n 3)
                           :element-type     'single-float
                           :initial-contents list)))
      (values
       (%make-array xs)
       (%make-array ys))))))

;; Return a matrix βs so that ys ≈ xs * βs using least squares.
(sera:-> least-squares-fit ((simple-array single-float (* 3))
                            (simple-array single-float (* 3)))
         (values (sift/core:mat 3) &optional))
(defun least-squares-fit (xs ys)
  (let ((fit (em:solve
              (em:mult xs xs :ta t)
              (em:mult xs ys :ta t))))
    (if fit fit sift/core:+mat3-identity+)))

(sera:-> fit-error ((sift/core:mat 3)
                    (simple-array single-float (* 3))
                    (simple-array single-float (* 3)))
         (values single-float &optional))
(defun fit-error (βs xs ys)
  (let ((diff (em:sub ys (em:mult xs βs))))
    (flet ((norm (column)
             (em:norm (em:column diff column))))
      (max (norm 0)
           (norm 1)
           (norm 2)))))

(defun random-integers (k n)
  "Collect K random integer from 0 (inclusive) to N (exclusive)
without repetitions."
  (labels ((%go (acc k)
             (if (zerop k) acc
                 (let ((x (random n)))
                   (if (find x acc :test #'=)
                       (%go acc k)
                       (%go (cons x acc) (1- k)))))))
    (%go nil k)))

;; One iteration of RANSAC fit
;; https://en.wikipedia.org/wiki/Random_sample_consensus
;; K — number of points for initial fit
;; D — number of points needed to be fit with the model to treat the model as good.
(sera:-> ransac-iteration ((simple-array single-float (* 3))
                           (simple-array single-float (* 3))
                           alex:positive-fixnum
                           alex:positive-fixnum
                           (single-float 0f0))
         (values boolean &optional (sift/core:mat 3) single-float))
(defun ransac-iteration (xs ys k d err)
  (let* ((length (array-dimension xs 0))
         (is (random-integers k length))
         (%xs (select-rows xs is))
         (%ys (select-rows ys is))
         (βs (least-squares-fit %xs %ys)))
    (multiple-value-bind (n xs ys)
        (loop for i below length
              for xrow = (em:vector->row (em:row xs i))
              for yrow = (em:vector->row (em:row ys i))
              for yfit = (em:mult xrow βs)
              for pair-err = (em:norm (em:row (em:sub yrow yfit) 0))
              when (< pair-err err)
              collect xrow into fit-x-rows and
              collect yrow into fit-y-rows and
              sum 1 into n
              finally (when (not (zerop n))
                        (return
                          (values n
                                  (em:vstack fit-x-rows 'single-float)
                                  (em:vstack fit-y-rows 'single-float)))))
      (when (and n (>= n d))
        (let ((βs (least-squares-fit xs ys)))
          (values t βs (fit-error βs xs ys)))))))

(sera:-> ransac-fit ((simple-array single-float (* 3))
                     (simple-array single-float (* 3))
                     alex:positive-fixnum
                     alex:positive-fixnum
                     alex:positive-fixnum
                     (single-float 0f0))
         (values (or (sift/core:mat 3) null)
                 single-float
                 &optional))
(defun ransac-fit (xs ys n k d err)
  (labels ((%go (best-fit best-err n)
             (if (zerop n)
                 (values best-fit best-err)
                 (multiple-value-bind (successp fit err)
                     (ransac-iteration xs ys k d err)
                   (if (and successp (< err best-err))
                       (%go fit err (1- n))
                       (%go best-fit best-err (1- n)))))))
    (%go nil ff:single-float-positive-infinity n)))

(sera:-> affine-transform (list &key
                                (:max-iter    alex:positive-fixnum)
                                (:seed-points alex:positive-fixnum)
                                (:well-fit    alex:positive-fixnum)
                                (:err         (single-float 0f0)))
         (values (or null (sift/core:mat 3)) single-float &optional))
(defun affine-transform (matches &key (max-iter 10) (seed-points 10) (well-fit 50) (err 1f0))
  "Find an affine transform matrix which transform the first keypoint
in each pair of matches to the second keypoint. Keypoint parameters
are related to the RANSAC algorithm: @c(MAX-ITER) is the maximal
number of iterations, @c(SEED-POINTS) is an initial number of points
to make a fit, @c(WELL-FIT) is a number of well fit points needed to
treat a fit as successful. A point is well-fit if \\(\\| y - Ax \\|\\)
is less than @c(ERR), (\\(A\\) is a candidate for the found fit)."
  (multiple-value-bind (fit error)
      (multiple-value-bind (xs ys)
          (matches->matrices matches)
        (ransac-fit xs ys max-iter seed-points well-fit err))
    (values (if fit (em:transpose fit)) error)))
