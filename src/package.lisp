(defpackage sift
  (:use #:cl)
  (:local-nicknames (#:alex #:alexandria)
                    (#:sera #:serapeum)
                    (#:ff   #:float-features))
  (:export #:gaussian-scale-space
           #:keypoints
           #:image-coordinate #:image-coordinate-vector
           #:describe-point
           #:descriptors
           #:descriptor
           #:descriptor-keypoint
           #:descriptor-array
           #:find-matches

           #:keypoint
           #:keypoint-angle
           #:keypoint-octave
           #:keypoint-index
           #:keypoint-coord

           #:index3 #:index3-i #:index3-j #:index3-k
           #:aref-index3 #:aref-index3/p
           #:vec #:mat
           #:make-vec3 #:make-mat3 #:make-mat2 #:shrink3 #:dist3 #:mul-m3v3
           #:mul3 #:det3 #:inv3 #:trace3 #:scale3 #:+mat3-identity+
           #:mul2 #:det2 #:inv2 #:trace2 #:scale2 #:+mat2-identity+

           #:loop-array
           #:loop-ranges

           #:interpolate))
