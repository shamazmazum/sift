(defsystem :sift/core
  :name :sift/core
  :version "0.2"
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :description "Scale-invariant image keypoints with descriptors"
  :licence "2-clause BSD"
  :pathname "src"
  :serial t
  :components ((:file "package")
               (:file "util")
               (:file "linalg")
               (:file "gaussian-blur")
               (:file "downsample")
               (:file "scale-space")
               (:file "keypoint-def")
               (:file "finite-differences")
               (:file "interpolation")
               (:file "orientation")
               (:file "descriptor")
               (:file "keypoints")
               (:file "matching"))
  :depends-on (:cl-fftw/single
               :serapeum
               :alexandria
               :float-features
               :picolens))

(defsystem :sift/debug
  :name :sift/debug
  :version "0.2"
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :licence "2-clause BSD"
  :pathname "debug"
  :serial t
  :components ((:file "package")
               (:file "draw-keypoints")
               (:file "draw-matches")
               (:file "image-io")
               (:file "success-rates"))
  :depends-on (:sift/core :imago :array-operations))

(defsystem :sift/registration
  :name :sift/registration
  :version "0.2"
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :licence "2-clause BSD"
  :pathname "registration"
  :serial t
  :components ((:file "package")
               (:file "affine-transform"))
  :depends-on (:sift/core :magicl))

(defsystem :sift
  :name :sift
  :version "0.2"
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :licence "2-clause BSD"
  :depends-on (:sift/core
               :sift/debug
               :sift/registration)
  
  :in-order-to ((test-op (load-op "sift/tests")))
  :perform (test-op (op system)
                    (declare (ignore op system))
                    (uiop:symbol-call :sift/tests '#:run-tests)))

(defsystem :sift/tests
  :name :sift/tests
  :version "0.2"
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :licence "2-clause BSD"
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "tests"))
  :depends-on (:sift
               :select
               :fiveam
               :numpy-npy
               :approx))
