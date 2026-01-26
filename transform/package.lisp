(defpackage sift/transform
  (:use #:cl)
  (:local-nicknames (#:sera #:serapeum)
                    (#:alex #:alexandria)
                    (#:ff   #:float-features)
                    (#:em   #:entzauberte-matrices))
  (:export #:affine-transform
           #:ransac))
