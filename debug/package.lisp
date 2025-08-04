(defpackage sift/debug
  (:use #:cl)
  (:local-nicknames (#:sera #:serapeum))
  (:export #:draw-keypoints
           #:draw-matches

           #:success-rates-scaling
           #:success-rates-rotation))
