(defpackage sift
  (:use #:cl)
  (:local-nicknames (#:alex #:alexandria)
                    (#:sera #:serapeum)
                    (#:ff   #:float-features))
  (:export #:gaussian-scale-space
           #:keypoints
           #:global-coordinate
           #:find-matches

           #:keypoint
           #:keypoint-angle
           #:keypoint-octave
           #:keypoint-index
           #:keypoint-coord

           #:space-attachment
           #:space-attachment-point

           #:index3 #:index3-i #:index3-j #:index3-k
           #:vec #:mat
           #:make-vec3 #:make-mat3 #:make-mat2 #:shrink3
           #:mul3 #:det3 #:inv3 #:trace3 #:scale3 #:+mat3-identity+
           #:mul2 #:det2 #:inv2 #:trace2 #:scale2 #:+mat2-identity+

           #:interpolate))
