(in-package :sift/debug)

(defun adjust-coordinates (keypoint)
  (multiple-value-bind (x y)
      (sift/core:image-coordinate keypoint)
    (cons (floor x)
          (floor y))))

(declaim (inline clamp))
(defun clamp (v min max)
  (max (min v max) min))

(sera:-> draw-keypoints ((simple-array single-float (* *)) list)
         (values imago:rgb-image &optional))
(defun draw-keypoints (array keypoints)
  "Make an imago image of an array with keypoints drawn as circles
with an orientation mark. Size of a circle corresponds to a keypoint's
octave number."
  (let ((image
         (imago:make-rgb-image-from-pixels
          (aops:vectorize* 'imago:rgb-pixel
              (array)
            (let ((v (clamp (floor (* 255 array)) 0 255)))
              (imago:make-color v v v))))))
    (loop for color = (imago:make-color
                       (random 255)
                       (random 255)
                       (random 255))
          for kp in keypoints
          for octave = (sift/core:keypoint-octave kp)
          for (y . x) = (adjust-coordinates kp)
          for angle = (sift/core:keypoint-angle kp)
          for r = (* 4 (expt 2 octave))
          do
          (imago:draw-circle
           image x y r color)
          (imago:draw-line
           image x y
           (floor (+ x (* r (sin angle))))
           (floor (+ y (* r (cos angle))))
           color))
    image))
