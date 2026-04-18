(in-package :sift/transform)

(deftype fitfn () '(function ((simple-array single-float (* 3))
                              (simple-array single-float (* 3)))
                     (values (sift/core:mat 3) &optional)))

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
(sera:-> least-squares ((simple-array single-float (* 3))
                        (simple-array single-float (* 3)))
         (values (sift/core:mat 3) &optional))
(defun least-squares (xs ys)
  "Perform linear least squares fit which maps points in the set
@c(xs) to points in the set @c(ys)."
  (let ((fit (em:solve
              (em:mult xs xs :ta t)
              (em:mult xs ys :ta t))))
    (if fit fit sift/core:+mat3-identity+)))

(sera:-> fit-error ((sift/core:mat 3)
                    (simple-array single-float (* 3))
                    (simple-array single-float (* 3)))
         (values single-float &optional))
(defun fit-error (βs xs ys)
  (declare (optimize (speed 3)))
  (let ((diff (em:sub ys (em:mult xs βs))))
    (sqrt
     (loop for i below (array-total-size diff)
           sum (expt (row-major-aref diff i) 2)
           single-float))))

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
(sera:-> ransac-iteration (fitfn
                           (simple-array single-float (* 3))
                           (simple-array single-float (* 3))
                           alex:positive-fixnum
                           alex:non-negative-fixnum
                           (single-float 0f0)
                           (single-float 0f0))
         (values boolean &optional
                 (sift/core:mat 3)
                 (single-float 0f0)
                 alex:non-negative-fixnum))
(defun ransac-iteration (f xs ys k prev-inliers ε prev-error)
  (let* ((length (array-dimension xs 0))
         (is (random-integers k length))
         (%xs (select-rows xs is))
         (%ys (select-rows ys is))
         (βs (funcall f %xs %ys)))
    (multiple-value-bind (n xs ys)
        (loop for i below length
              for xrow = (em:vector->row (em:row xs i))
              for yrow = (em:vector->row (em:row ys i))
              for yfit = (em:mult xrow βs)
              for pair-ε = (em:norm (em:row (em:sub yrow yfit) 0))
              when (< pair-ε ε)
              collect xrow into fit-x-rows and
              collect yrow into fit-y-rows and
              sum 1 into n
              finally (when (not (zerop n))
                        (return
                          (values n
                                  (em:vstack fit-x-rows 'single-float)
                                  (em:vstack fit-y-rows 'single-float)))))
      (when (and n (>= n prev-inliers))
        (let* ((βs (funcall f xs ys))
               (fit-error (fit-error βs xs ys)))
          (if (or (> n prev-inliers)
                  (< fit-error prev-error))
              (values t βs (fit-error βs xs ys) n)))))))

;; TODO: Remove this later
(sera:-> transpose ((sift/core:mat 3))
         (values (sift/core:mat 3) &optional))
(defun transpose (m)
  (declare (optimize (speed 3)))
  (let ((result (make-array '(3 3) :element-type 'single-float)))
    (loop for i below (array-dimension result 0) do
      (loop for j below (array-dimension result 1) do
        (setf (aref result i j) (aref m j i))))
    result))

(sera:-> ransac (fitfn
                 (simple-array single-float (* 3))
                 (simple-array single-float (* 3))
                 alex:positive-fixnum
                 alex:positive-fixnum
                 (single-float 0f0))
         (values (or (sift/core:mat 3) null)
                 single-float alex:non-negative-fixnum &optional))
(defun ransac (f xs ys n k err)
  (assert (= (array-dimension xs 0)
             (array-dimension ys 0)))
  (labels ((%go (best-fit best-err best-inliers n)
             (if (zerop n)
                 (values (transpose best-fit) best-err best-inliers)
                 (multiple-value-bind (successp fit err inliers)
                     (ransac-iteration f xs ys k best-inliers err best-err)
                   (let ((n (1- n)))
                     (if successp
                         (%go fit err inliers n)
                         (%go best-fit best-err best-inliers n)))))))
    (let ((initial-error ff:single-float-positive-infinity))
      (if (< (array-dimension xs 0) k)
          (values nil initial-error 0)
          (%go    nil initial-error 0 n)))))

(sera:-> fit-model (fitfn list &key
                    (:iterations  alex:positive-fixnum)
                    (:seed-points alex:positive-fixnum)
                    (:err         (single-float 0f0)))
         (values (or (sift/core:mat 3) null)
                 single-float alex:non-negative-fixnum &optional))
(defun fit-model (f matches &key (iterations 2000) (seed-points 10) (err 10f0))
  "Find a linear fit which maps the first keypoint in each pair of
matches to the second keypoint. The function @c(F) determines the type
of fit (e.g. unconstrained, rigid, rigid + uniform scale). Keypoint
parameters are related to the RANSAC algorithm: @c(MAX-ITER) is the
maximal number of iterations, @c(SEED-POINTS) is an initial number of
points to make a fit. A point is well-fit if \\(\\| y - Ax \\|\\) is
less than @c(ERR), (\\(A\\) is a candidate for the found fit)."
  (multiple-value-bind (xs ys)
      (matches->matrices matches)
    (ransac f xs ys iterations seed-points err)))
