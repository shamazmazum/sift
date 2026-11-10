(defpackage sift/util
  (:use #:cl)
  (:local-nicknames (#:sera #:serapeum)
                    (#:alex #:alexandria))
  (:export #:image->intensities
           #:intensities->image
           #:enhance-contrast

           #:draw-keypoints
           #:draw-matches

           #:scale-array
           #:rotate-array
           #:scale-transform
           #:rotation-transform
           #:interpolate/linear

           #:success-rates
           #:success-rates-scaling
           #:success-rates-rotation))
