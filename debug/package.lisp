(defpackage sift/debug
  (:use #:cl)
  (:local-nicknames (#:sera #:serapeum))
  (:export #:load-image
           #:write-image
           #:draw-keypoints
           #:draw-matches

           #:success-rates-scaling
           #:success-rates-rotation))
