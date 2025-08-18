(in-package :sift/registration)

;; Convert a list of keypoint pairs (matches) into 2 Nx3 matrices
;; where the first matrix corresponds to the first keypoint in a pair
;; and the second matrix corresponds to the second keypoint in a pair.
(sera:-> matches->matrices (list)
         (values magicl:matrix/double-float magicl:matrix/double-float &optional))
(defun matches->matrices (matches)
  (flet ((coord-list (kp)
           (multiple-value-bind (x y)
               (sift:image-coordinate kp)
             (list x y 1d0))))
    (multiple-value-bind (xs ys n)
        (loop for (kp1 . kp2) in matches
              append (coord-list kp1) into xs
              append (coord-list kp2) into ys
              sum 1 into n
              finally (return (values xs ys n)))
      (values
       (magicl:from-list xs (list n 3))
       (magicl:from-list ys (list n 3))))))

;; Return a matrix βs so that ys ≈ xs * βs using least squares.
(sera:-> least-squares-fit (magicl:matrix/double-float magicl:matrix/double-float)
         (values magicl:matrix/double-float &optional))
(defun least-squares-fit (xs ys)
  (magicl:mult
   (magicl:mult
    (magicl:inv (magicl:mult xs xs :transa :t))
    xs :transb :t)
   ys))

(sera:-> fit-error (magicl:matrix/double-float
                    magicl:matrix/double-float
                    magicl:matrix/double-float)
         (values double-float &optional))
(defun fit-error (βs xs ys)
  (let ((diff (magicl:.- ys (magicl:@ xs βs))))
    (flet ((norm (column)
             (magicl:norm (magicl:column diff column))))
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

(sera:-> select-rows (magicl:matrix/double-float list)
         (values magicl:matrix/double-float &optional))
(defun select-rows (m is)
  (magicl:vstack
   (mapcar
    (lambda (idx)
      (magicl:vector->row-matrix
       (magicl:row m idx)))
    is)))

;; One iteration of RANSAC fit
;; https://en.wikipedia.org/wiki/Random_sample_consensus
;; K — number of points for initial fit
;; D — number of points needed to be fit with the model to treat the model as good.
(sera:-> ransac-iteration (magicl:matrix/double-float
                           magicl:matrix/double-float
                           alex:positive-fixnum
                           alex:positive-fixnum
                           (double-float 0d0))
         (values boolean &optional magicl:matrix/double-float double-float))
(defun ransac-iteration (xs ys k d err)
  (let* ((length (first (magicl:shape xs)))
         (is (random-integers k length))
         (%xs (select-rows xs is))
         (%ys (select-rows ys is))
         (βs (least-squares-fit %xs %ys)))
    (multiple-value-bind (n xs ys)
        (loop for i below length
              for xrow = (magicl:vector->row-matrix (magicl:row xs i))
              for yrow = (magicl:vector->row-matrix (magicl:row ys i))
              for yfit = (magicl:@ xrow βs)
              for pair-err = (magicl:norm (magicl:.- yrow yfit))
              when (< pair-err err)
              collect xrow into fit-x-rows and
              collect yrow into fit-y-rows and
              sum 1 into n
              finally (when (not (zerop n))
                        (return
                          (values
                           n (magicl:vstack fit-x-rows) (magicl:vstack fit-y-rows)))))
      (when (and n (>= n d))
        (let ((βs (least-squares-fit xs ys)))
          (values t βs (fit-error βs xs ys)))))))

(sera:-> ransac-fit (magicl:matrix/double-float
                     magicl:matrix/double-float
                     alex:positive-fixnum
                     alex:positive-fixnum
                     alex:positive-fixnum
                     (double-float 0d0))
         (values (or magicl:matrix/double-float null) &optional))
(defun ransac-fit (xs ys n k d err)
  (labels ((%go (best-fit best-err n)
             (if (zerop n) best-fit
                 (multiple-value-bind (successp fit err)
                     (ransac-iteration xs ys k d err)
                   (if (and successp (< err best-err))
                       (%go fit err (1- n))
                       (%go best-fit best-err (1- n)))))))
    (%go nil ff:double-float-positive-infinity n)))

(sera:-> matrix->array (magicl:matrix/double-float)
         (values (sift:mat 3) &optional))
(defun matrix->array (m)
  (let ((res (make-array '(3 3) :element-type 'double-float)))
    (loop for i below 3 do
          (loop for j below 3 do
                (setf (aref res i j)
                      (magicl:tref m i j))))
    res))

(sera:-> affine-transform (list &key
                                (:max-iter    alex:positive-fixnum)
                                (:seed-points alex:positive-fixnum)
                                (:well-fit    alex:positive-fixnum)
                                (:err         (double-float 0d0)))
         (values (or (sift:mat 3) null) &optional))
(defun affine-transform (matches &key (max-iter 10) (seed-points 10) (well-fit 50) (err 1d0))
  "Find an affine transform matrix which transform the first keypoint
in each pair of matches to the second keypoint. Keypoint parameters
are related to the RANSAC algorithm: @c(MAX-ITER) is the maximal
number of iterations, @c(SEED-POINTS) is an initial number of points
to make a fit, @C(WELL-FIT) is a number of well fit points needed to
treat a fit as successful. A point is well-fit if \\(\\| y - \\beta x
\\|\\) is less than @c(ERR)."
  (let ((m (multiple-value-bind (xs ys)
               (matches->matrices matches)
             (ransac-fit xs ys max-iter seed-points well-fit err))))
    (if m (matrix->array (magicl:transpose m)))))
