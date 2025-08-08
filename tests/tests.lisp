(in-package :sift/tests)

(defun run-tests ()
  (every #'identity
         (mapcar (lambda (suite)
                   (let ((status (run suite)))
                     (explain! status)
                     (results-status status)))
                 '(linalg3 linalg2 interp descr))))

(defun approx-= (x y)
  (< (abs (- x y)) 1d-8))

(defun mat-approx-= (m1 m2)
  (every
   #'approx-=
   (aops:flatten m1)
   (aops:flatten m2)))

(defun mat3-rand ()
  (sift:make-mat3
   (random 1d0)
   (random 1d0)
   (random 1d0)
   (random 1d0)
   (random 1d0)
   (random 1d0)
   (random 1d0)
   (random 1d0)
   (random 1d0)))

(defun mat2-rand ()
  (sift:make-mat2
   (random 1d0)
   (random 1d0)
   (random 1d0)
   (random 1d0)))

(defun unitary3-rand ()
  (let* ((ϕ (random (* 2 pi)))
         (ψ (random (* 2 pi)))
         (m1 (sift:make-mat3 (cos ϕ) (- (sin ϕ)) 0d0 (sin ϕ) (cos ϕ) 0d0 0d0 0d0 1d0))
         (m2 (sift:make-mat3 (cos ψ) 0d0 (- (sin ψ)) 0d0 1d0 0d0 (sin ψ) 0d0 (cos ψ))))
    (sift:mul3 m1 m2)))

(defun unitary2-rand ()
  (let ((ϕ (random (* 2 pi))))
    (sift:make-mat2 (cos ϕ) (- (sin ϕ)) (sin ϕ) (cos ϕ))))

(defun linear-function (x y z)
  (+ (* 10 x) (* 4 y) (* 6 z) 34))

(defun %linear-function (index)
  (linear-function
   (float (sift:index3-i index) 0d0)
   (float (sift:index3-j index) 0d0)
   (float (sift:index3-k index) 0d0)))

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

(def-suite linalg3 :description "Linear algebra tests (3x3 matrices)")
(def-suite linalg2 :description "Linear algebra tests (2x2 matrices)")
(def-suite interp  :description "Linear interpolation")
(def-suite descr   :description "SIFT keypoint descriptors")

(in-suite linalg3)

(test mul-identity3
  (loop repeat 1000
        for m = (mat3-rand) do
        (is (mat-approx-= m (sift:mul3 m sift:+mat3-identity+)))
        (is (mat-approx-= m (sift:mul3 sift:+mat3-identity+ m)))))

(test determinant3
  (loop repeat 1000
        for m1 = (mat3-rand)
        for m2 = (mat3-rand)
        do
        (is (approx-= (* (sift:det3 m1) (sift:det3 m2))
                      (sift:det3 (sift:mul3 m1 m2))))
        (is (approx-= (* (sift:det3 m1) (sift:det3 m2))
                      (sift:det3 (sift:mul3 m2 m1))))))

(test trace3
  (loop repeat 1000
        for m1 = (mat3-rand)
        for m2 = (unitary3-rand)
        do
        (is (approx-= (sift:trace3 m1)
                      (sift:trace3 (sift:mul3 m2 (sift:mul3 m1 (sift:inv3 m2))))))))

(test inversion3
  (loop repeat 1000
        for m1 = (mat3-rand)
        for m2 = (sift:inv3 m1) do
        (is (mat-approx-= sift:+mat3-identity+ (sift:mul3 m1 m2)))
        (is (mat-approx-= sift:+mat3-identity+ (sift:mul3 m2 m1)))))

(in-suite linalg2)

(test mul-identity2
  (loop repeat 1000
        for m = (mat2-rand) do
        (is (mat-approx-= m (sift:mul2 m sift:+mat2-identity+)))
        (is (mat-approx-= m (sift:mul2 sift:+mat2-identity+ m)))))

(test determinant2
  (loop repeat 1000
        for m1 = (mat2-rand)
        for m2 = (mat2-rand)
        do
        (is (approx-= (* (sift:det2 m1) (sift:det2 m2))
                      (sift:det2 (sift:mul2 m1 m2))))
        (is (approx-= (* (sift:det2 m1) (sift:det2 m2))
                      (sift:det2 (sift:mul2 m2 m1))))))

(test trace2
  (loop repeat 1000
        for m1 = (mat2-rand)
        for m2 = (unitary2-rand)
        do
        (is (approx-= (sift:trace2 m1)
                      (sift:trace2 (sift:mul2 m2 (sift:mul2 m1 (sift:inv2 m2))))))))

(test inversion2
  (loop repeat 1000
        for m1 = (mat2-rand)
        for m2 = (sift:inv2 m1) do
        (is (mat-approx-= sift:+mat2-identity+ (sift:mul2 m1 m2)))
        (is (mat-approx-= sift:+mat2-identity+ (sift:mul2 m2 m1)))))

(in-suite interp)

(test interp
  (loop repeat 100
        for x = (random 10d0)
        for y = (random 10d0)
        for z = (random 10d0) do
        (is (approx-= (linear-function x y z)
                      (sift:interpolate #'%linear-function x y z)))))

(in-suite descr)

;; TODO: Try to improve successful rate in each separate test
(defun test-matches (a1 a2 m)
  (let* ((rates (sift/debug:success-rates a1 a2 m))
         (nsucc (car rates))
         (rsucc (/ (car rates) (cdr rates))))
    (is (> nsucc 150))
    (is (> rsucc 6.5d-1))
    rsucc))

(test descriptor-matching/scale
  (let ((result
         (loop repeat 50
               for slice = (select:select *slices* (random (array-dimension *slices* 0))
                                          (select:range 0 1000) (select:range 0 1000))
               for s = (1+ (random 2d0))
               for slice2 = (sift/debug:scale-array slice s s)
               for m = (sift/debug:scale-transform s) sum
               (test-matches slice slice2 m))))
    (is (> (/ result 50) 9d-1))))

(test descriptor-matching/rotation
  (let ((result
         (loop repeat 50
               for slice = (select:select *slices* (random (array-dimension *slices* 0))
                                          (select:range 0 1000) (select:range 0 1000))
               for ϕ = (random (/ pi 2))
               for slice2 = (sift/debug:rotate-array slice ϕ)
               for m = (sift/debug:rotation-transform 1000 ϕ) sum
               (test-matches slice slice2 m))))
    (is (> (/ result 50) 9d-1))))

(test descriptor-matching/combined
  (let ((result
         (loop repeat 50
               for slice = (select:select *slices* (random (array-dimension *slices* 0))
                                          (select:range 0 1000) (select:range 0 1000))
               for s = (1+ (random 2d0))
               for ϕ = (random (/ pi 2))
               for slice2 = (sift/debug:rotate-array (sift/debug:scale-array slice s s) ϕ)
               for m1 = (sift/debug:scale-transform s)
               for m2 = (sift/debug:rotation-transform (* s 1000) ϕ)
               for m = (sift:mul3 m2 m1) sum
               (test-matches slice slice2 m))))
    (is (> (/ result 50) 9d-1))))
