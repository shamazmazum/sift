(defpackage sift/transform
  (:use #:cl)
  (:local-nicknames (#:sera #:serapeum)
                    (#:alex #:alexandria)
                    (#:ff   #:float-features)
                    (#:em   #:entzauberte-matrices))
  (:export #:least-squares
           #:fit-model
           ;; For tests
           #:ransac))
