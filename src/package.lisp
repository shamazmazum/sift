(defpackage sift/core
  (:use #:cl)
  (:local-nicknames (#:alex #:alexandria)
                    (#:sera #:serapeum)
                    (#:em   #:entzauberte-matrices)
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

           #:index3 #:index3-i #:index3-j #:index3-k #:aref-index3
           #:vec #:mat
           #:vec3 #:mat3 #:mat2 #:dist #:mul-mv
           #:mul #:mtrace #:scale
           #:det2 #:shrink3
           #:+mat3-identity+ #:+mat2-identity+

           #:loop-array
           #:loop-ranges

           #:interpolate))
