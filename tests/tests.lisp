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
  (let* ((rates (sift/debug:success-rates a1 a2 m))
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
        for slice2 = (sift/debug:scale-array slice s s)
        for m = (sift/debug:scale-transform s) sum
        (test-matches slice slice2 m)))

(test descriptor-matching/rotation
  (loop repeat *number-of-runs*
        for slice = (select:select *slices* (random (array-dimension *slices* 0))
                                   (select:range 0 1000) (select:range 0 1000))
        for ϕ = (random (/ +pi+ 2))
        for slice2 = (sift/debug:rotate-array slice ϕ)
        for m = (sift/debug:rotation-transform 1000 ϕ) sum
        (test-matches slice slice2 m)))

(test descriptor-matching/combined
  (loop repeat *number-of-runs*
        for slice = (select:select *slices* (random (array-dimension *slices* 0))
                                   (select:range 0 1000) (select:range 0 1000))
        for s = (1+ (random 2f0))
        for ϕ = (random (/ +pi+ 2))
        for slice2 = (sift/debug:rotate-array (sift/debug:scale-array slice s s) ϕ)
        for m1 = (sift/debug:scale-transform s)
        for m2 = (sift/debug:rotation-transform (* s 1000) ϕ)
        for m = (sift/core:mul m2 m1) sum
        (test-matches slice slice2 m)))

(in-suite regis)

(test ransac
  (loop repeat 500
        for xs1 = (magicl:scale (magicl:rand '(1000 3) :type 'single-float) 100)
        for m  = (magicl:rand '(3 3) :type 'single-float)
        for ys1 = (magicl:@ xs1 m)
        for xs2 = (magicl:scale (magicl:rand '(20 3) :type 'single-float) 10000)
        for ys2 = (magicl:scale (magicl:rand '(20 3) :type 'single-float) 10000)
        for xs = (magicl:vstack (list xs1 xs2))
        for ys = (magicl:vstack (list ys1 ys2))
        for fit = (sift/registration:ransac-fit xs ys 30 10 50 1f0)
        when fit do
        (is (approx:array-approx-p (magicl::storage m) (magicl::storage fit)))))
