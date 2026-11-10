(in-package :sift/core)

;; Here extract keypoints to a simple vector for random access
(sera:-> extract-keypoints (list)
         (values simple-vector &optional))
(defun extract-keypoints (descs)
  (declare (optimize (speed 3)))
  (map 'vector #'descriptor-keypoint descs))

;; Extract descriptors for faiss
(sera:-> extract-descriptors (list)
         (values (simple-array single-float (* 128)) &optional))
(defun extract-descriptors (descs)
  (declare (optimize (speed 3)))
  (let ((array (make-array (list (length descs) 128) :element-type 'single-float)))
    (loop for i below (array-dimension array 0)
          for point in descs
          for desc = (descriptor-array point)
          for base = (array-row-major-index array i 0)
          do (loop for j below (array-dimension array 1)
                   do (setf (row-major-aref array (+ base j))
                            (aref desc j))))
    array))

(sera:-> find-matches (list list)
         (values list &optional))
(defun find-matches (set1 set2)
  "Find matching keypoints in @c(SET1) and @c(SET2). A point from the
first set can have at most one corresponding point from the second
set. Algoritmic complexity: \\(O(n \\cdot m\\)), \\(n\\) and \\(m\\)
being numbers of elements in each set."
  (declare (optimize (speed 3)))
  (let ((desc1 (extract-descriptors set1))
        (desc2 (extract-descriptors set2))
        (kps2  (extract-keypoints   set2)))
    (faiss:with-index (index 128 "Flat" :l2)
      (faiss:index-add index desc2)
      (multiple-value-bind (dists labels)
          (faiss:index-search index desc1 2)
        (loop for point in set1
              for i from 0 by 1
              for d1 = (aref dists i 0)
              for d2 = (aref dists i 1)
              when (< d1 (* 0.75 d2))
                collect (cons (descriptor-keypoint point)
                              (aref kps2 (aref labels i 0))))))))
