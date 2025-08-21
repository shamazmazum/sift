(in-package :sift/debug)

(sera:-> draw-matches ((simple-array single-float (* *))
                       (simple-array single-float (* *))
                       list)
         (values imago:rgb-image &optional))
(defun draw-matches (array1 array2 matches)
  "Make an imago image of two arrays shown side by side + matched
keypoints + lines connecting keypoints in a match."
  (let* ((image1 (draw-keypoints array1 (mapcar #'car matches)))
         (image2 (draw-keypoints array2 (mapcar #'cdr matches)))
         (width (+ (imago:image-width image1)
                   (imago:image-width image2)))
         (height (max (imago:image-height image1)
                      (imago:image-height image2)))
         (result (imago:make-rgb-image
                  width height (imago:make-color 0 0 0))))
    (imago:copy result image1 :dest-x 0)
    (imago:copy result image2 :dest-x (imago:image-width image1))
    (loop for (kp1 . kp2) in matches do
          (let ((coord1 (adjust-coordinates kp1))
                (coord2 (adjust-coordinates kp2)))
            (destructuring-bind (y1 . x1) coord1
              (destructuring-bind (y2 . x2) coord2
                (imago:draw-line result x1 y1 (+ x2 (imago:image-width image1)) y2
                                 (imago:make-color
                                  (random 255)
                                  (random 255)
                                  (random 255)))))))
    result))
