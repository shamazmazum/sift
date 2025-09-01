(in-package :sift/registration)

;; Faster replacements for magick functions

(sera:-> select-rows (magicl:matrix/single-float list)
         (values magicl:matrix/single-float &optional))
(defun select-rows (m is)
  (declare (optimize (speed 3)))
  (let* ((length (length is))
         (n (second (magicl:shape m)))
         (result (magicl:make-tensor
                  'magicl:matrix/single-float
                  (list length n))))
    (loop for i below length
          for ridx in is do
          (loop for j fixnum below n do
                (setf (magicl:tref result i j)
                      (magicl:tref m ridx j))))
    result))

;; MAGICL:VSTACK is extremely slow
(sera:-> vstack (list)
         (values magicl:matrix/single-float &optional))
(defun vstack (list)
  (declare (optimize (speed 3)))
  (let* ((length (length list))
         (n (second (magicl:shape (first list))))
         (result (magicl:make-tensor
                  'magicl:matrix/single-float
                  (list length n))))
    (loop for i below length
          for row in list do
          (loop for j fixnum below n do
                (setf (magicl:tref result i j)
                      (magicl:tref row 0 j))))
    result))

;; And so is MAGICL:ROW
(sera:-> row (magicl:matrix/single-float alex:non-negative-fixnum)
         (values magicl:matrix/single-float &optional))
(defun row (m idx)
  (declare (optimize (speed 3)))
  (let* ((n (second (magicl:shape m)))
         (result (magicl:make-tensor
                  'magicl:matrix/single-float
                  (list 1 n))))
    (loop for i fixnum below n do
          (setf (magicl:tref result 0 i)
                (magicl:tref m idx i)))
    result))

;; And MAGICL:COLUMN
(sera:-> column (magicl:matrix/single-float alex:non-negative-fixnum)
         (values magicl:vector/single-float &optional))
(defun column (m idx)
  (declare (optimize (speed 3)))
  (let* ((n (first (magicl:shape m)))
         (result (magicl:make-tensor
                  'magicl:vector/single-float
                  (list n))))
    (loop for i fixnum below n do
          (setf (magicl:tref result i)
                (magicl:tref m i idx)))
    result))
