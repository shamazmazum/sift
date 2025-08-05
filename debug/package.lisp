(defpackage sift/debug
  (:use #:cl)
  (:local-nicknames (#:sera #:serapeum))
  (:export #:load-image
           #:write-image
           #:draw-keypoints
           #:draw-matches

           #:scale-array
           #:rotate-array

           #:success-rates-scaling
           #:success-rates-rotation))
