(in-package :sift)

(sera:-> descriptor-metric (descriptor descriptor)
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
  (let ((tree (vp-trees:make-vp-tree set2 #'descriptor-metric :key #'car)))
    (reduce
     (lambda (acc keypoint)
       (multiple-value-bind (closest dist)
           (vp-trees:nearest-neighbor
            tree (car keypoint)
            #'descriptor-metric :key #'car)
         (let ((matches (vp-trees:find tree (car keypoint) (* 1.3d0 dist)
                                       #'descriptor-metric
                                       :key #'car
                                       :max 2)))
           (if (cdr matches) acc
               (cons (cons (cdr keypoint) (cdr closest)) acc)))))
     set1 :initial-value nil)))
