(defpackage sift/registration
  (:use #:cl)
  (:local-nicknames (#:sera #:serapeum)
                    (#:alex #:alexandria)
                    (#:ff   #:float-features))  
  (:export #:affine-transform
           #:ransac-fit))
