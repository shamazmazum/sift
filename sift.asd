(defsystem :sift/core
  :name :sift/core
  :version "0.5.1"
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
               (:file "orientation")
               (:file "descriptor")
               (:file "keypoints")
               (:file "matching"))
  :depends-on (:cl-fftw/single
               :cffi
               :serapeum
               :alexandria
               :float-features
               :picolens
               :entzauberte-matrices))

(defsystem :sift/util
  :name :sift/util
  :version "0.5.1"
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :licence "2-clause BSD"
  :pathname "util"
  :serial t
  :components ((:file "package")
               (:file "draw-keypoints")
               (:file "draw-matches")
               (:file "image-io")
               (:file "linear-interpolation")
               (:file "contrast")
               (:file "success-rates"))
  :depends-on (:sift/core :imago :array-operations))

(defsystem :sift/transform
  :name :sift/transform
  :version "0.5.1"
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :licence "2-clause BSD"
  :pathname "transform"
  :serial t
  :components ((:file "package")
               (:file "application")
               (:file "registration"))
  :depends-on (:sift/core :sift/util))

(defsystem :sift
  :name :sift
  :version "0.5.1"
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :licence "2-clause BSD"
  :depends-on (:sift/core
               :sift/util
               :sift/transform)
  :in-order-to ((test-op (load-op "sift/tests")))
  :perform (test-op (op system)
                    (declare (ignore op system))
                    (uiop:symbol-call :sift/tests '#:run-tests)))

(defsystem :sift/tests
  :name :sift/tests
  :version "0.5.1"
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
