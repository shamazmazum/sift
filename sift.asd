(defsystem :sift
  :name :sift
  :version "0.1"
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
  :depends-on (:cl-fftw
               :serapeum
               :alexandria
               :float-features
               :picolens)
  :in-order-to ((test-op (load-op "sift/tests")))
  :perform (test-op (op system)
                    (declare (ignore op system))
                    (uiop:symbol-call :sift/tests '#:run-tests)))

(defsystem :sift/debug
  :name :sift/debug
  :version "0.1"
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :licence "2-clause BSD"
  :pathname "debug"
  :serial t
  :components ((:file "package")
               (:file "draw-keypoints")
               (:file "draw-matches")
               (:file "image-io")
               (:file "success-rates"))
  :depends-on (:sift :imago :array-operations))

(defsystem :sift/registration
  :name :sift/registration
  :version "0.1"
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :licence "2-clause BSD"
  :pathname "registration"
  :serial t
  :components ((:file "package")
               (:file "affine-transform"))
  :depends-on (:sift :magicl))

(defsystem :sift/all
  :name :sift/all
  :version "0.1"
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :licence "2-clause BSD"
  :depends-on (:sift
               :sift/debug
               :sift/registration))

(defsystem :sift/tests
  :name :sift/tests
  :version "0.1"
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :licence "2-clause BSD"
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "tests"))
  :depends-on (:sift/all
               :select
               :fiveam
               :array-operations
               :numpy-npy))
