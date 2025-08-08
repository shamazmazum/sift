(in-package :sift)

(sera:-> descriptor-metric (%descriptor %descriptor)
         (values (double-float 0d0) &optional))
(defun descriptor-metric (d1 d2)
  (declare (optimize (speed 3)))
  (sqrt
   (loop for x across d1
         for y across d2
         sum (expt (- x y) 2)
         double-float)))

(sera:-> find-matches (list list)
         (values list &optional))
(defun find-matches (set1 set2)
  (declare (optimize (speed 3)))
  (reduce
   (lambda (acc dk1)
     (let ((d1 (descriptor-array    dk1))
           (k1 (descriptor-keypoint dk1)))
       (multiple-value-bind (closest dist1 dist2)
           (loop with dist1 = ff:double-float-positive-infinity
                 with dist2 = ff:double-float-positive-infinity
                 with closest1 = nil
                 with closest2 = nil
                 for dk2 in set2
                 for d2 = (descriptor-array    dk2)
                 for k2 = (descriptor-keypoint dk2) do
                 (let ((d (descriptor-metric d1 d2)))
                   (cond
                     ((< d dist1)
                      (setq dist2 dist1
                            closest2 closest1
                            dist1 d
                            closest1 k2))
                     ((< d dist2)
                      (setq dist2 d
                            closest2 k2))))
                 finally (return (values closest1 dist1 dist2)))
         (if (< dist1 (* 0.75 dist2))
             (cons (cons k1 closest) acc)
             acc))))
   set1 :initial-value nil))
