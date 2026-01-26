(in-package :sift/tests)

(defparameter *number-of-runs* 50)
(defconstant +pi+ (float pi 0.0))

(defun run-tests ()
  (every #'identity
         (mapcar (lambda (suite)
                   (let ((status (run suite)))
                     (explain! status)
                     (results-status status)))
                 '(linalg descr regis))))

(defun mat3-rand ()
  (sift/core:mat3
   (random 1f0)
   (random 1f0)
   (random 1f0)
   (random 1f0)
   (random 1f0)
   (random 1f0)
   (random 1f0)
   (random 1f0)
   (random 1f0)))

(defun mat2-rand ()
  (sift/core:mat2
   (random 1f0)
   (random 1f0)
   (random 1f0)
   (random 1f0)))

(defun unitary3-rand ()
  (let* ((ϕ (random (* 2 +pi+)))
         (ψ (random (* 2 +pi+)))
         (m1 (sift/core:mat3
              (cos ϕ) (- (sin ϕ)) 0f0 (sin ϕ) (cos ϕ) 0f0 0f0 0f0 1f0))
         (m2 (sift/core:mat3
              (cos ψ) 0f0 (- (sin ψ)) 0f0 1f0 0f0 (sin ψ) 0f0 (cos ψ))))
    (sift/core:mul m1 m2)))

(defun unitary2-rand ()
  (let ((ϕ (random (* 2 +pi+))))
    (sift/core:mat2 (cos ϕ) (- (sin ϕ)) (sin ϕ) (cos ϕ))))

;; Neumann, Rodrigo, ANDREETA, MARIANE, Lucas-Oliveira, Everton. "11
;; Sandstones: raw, filtered and segmented data." Digital Rocks
;; Portal, Digital Rocks Portal, 21 Apr 2025,
;; https://www.doi.org/10.17612/f4h1-w124 Accessed 8 Aug 2025.
(defparameter *slices*
  (numpy-npy:load-array
     (asdf:system-relative-pathname
      :sift/tests (make-pathname :name "slices"
                                 :type "npy"
                                 :directory '(:relative "tests")))))

(def-suite linalg :description "Linear algebra tests")
(def-suite descr  :description "SIFT keypoint descriptors")
(def-suite regis  :description "Image registration with SIFT descriptors")

(in-suite linalg)

(test mul-identity3
  (loop repeat 3000
        for m = (mat3-rand) do
        (is (approx:array-approx-p m (sift/core:mul m sift/core:+mat3-identity+)))
        (is (approx:array-approx-p m (sift/core:mul sift/core:+mat3-identity+ m)))))

(test mul-identity2
  (loop repeat 3000
        for m = (mat2-rand) do
        (is (approx:array-approx-p m (sift/core:mul m sift/core:+mat2-identity+)))
        (is (approx:array-approx-p m (sift/core:mul sift/core:+mat2-identity+ m)))))

(test determinant2
  (loop repeat 3000
        for m1 = (mat2-rand)
        for m2 = (mat2-rand)
        when (> (sift/core:det2 (sift/core:mul m2 m1)) 1f-3) do
        (is (approx:approxp
             (* (sift/core:det2 m1) (sift/core:det2 m2))
             (sift/core:det2 (sift/core:mul m1 m2))))
        (is (approx:approxp
             (* (sift/core:det2 m1) (sift/core:det2 m2))
             (sift/core:det2 (sift/core:mul m2 m1))))))

(in-suite descr)

(defun test-matches (a1 a2 m)
  (let* ((rates (sift/util:success-rates a1 a2 m))
         (nsucc (car rates))
         (rsucc (/ (car rates) (cdr rates))))
    (is (> nsucc 500))
    (is (> rsucc 9d-1))
    rsucc))

(test descriptor-matching/scale
  (loop repeat *number-of-runs*
        for slice = (select:select *slices* (random (array-dimension *slices* 0))
                                   (select:range 0 1000) (select:range 0 1000))
        for s = (1+ (random 2f0))
        for slice2 = (sift/util:scale-array slice s s)
        for m = (sift/util:scale-transform s) sum
        (test-matches slice slice2 m)))

(test descriptor-matching/rotation
  (loop repeat *number-of-runs*
        for slice = (select:select *slices* (random (array-dimension *slices* 0))
                                   (select:range 0 1000) (select:range 0 1000))
        for ϕ = (random (/ +pi+ 2))
        for slice2 = (sift/util:rotate-array slice ϕ)
        for m = (sift/util:rotation-transform 1000 ϕ) sum
        (test-matches slice slice2 m)))

(test descriptor-matching/combined
  (loop repeat *number-of-runs*
        for slice = (select:select *slices* (random (array-dimension *slices* 0))
                                   (select:range 0 1000) (select:range 0 1000))
        for s = (1+ (random 2f0))
        for ϕ = (random (/ +pi+ 2))
        for slice2 = (sift/util:rotate-array (sift/util:scale-array slice s s) ϕ)
        for m1 = (sift/util:scale-transform s)
        for m2 = (sift/util:rotation-transform (* s 1000) ϕ)
        for m = (sift/core:mul m2 m1) sum
        (test-matches slice slice2 m)))

(in-suite regis)

(declaim (inline random-matrix))
(defun random-matrix (m n)
  (let ((result (make-array (list m n) :element-type 'single-float)))
    (loop for i below (array-total-size result) do
      (setf (row-major-aref result i) (random 1.0)))
    result))

(test ransac
  (loop repeat 500
        for xs1 = (em:scale (random-matrix 1000 3) 100.0)
        for m   = (random-matrix 3 3)
        for ys1 = (em:mult xs1 m)
        for xs2 = (em:scale (random-matrix 20 3) 10000.0)
        for ys2 = (em:scale (random-matrix 20 3) 10000.0)
        for xs = (em:vstack (list xs1 xs2) 'single-float)
        for ys = (em:vstack (list ys1 ys2) 'single-float)
        for fit = (sift/transform:ransac xs ys 30 10 50 1f0)
        when fit do
        (is (approx:array-approx-p m fit))))
