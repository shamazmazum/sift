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
               :vp-trees
               :picolens)
  :in-order-to ((test-op (load-op "sift/tests")))
  :perform (test-op (op system)
                    (declare (ignore op system))
                    (uiop:symbol-call :sift/tests '#:run-tests)))

(defsystem :sift/tests
  :name :sift/tests
  :version "0.1"
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :licence "2-clause BSD"
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "tests"))
  :depends-on (:sift :fiveam :array-operations))

(defsystem :sift/debug
  :name :sift/debug
  :version "0.1"
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :licence "2-clause BSD"
  :pathname "debug"
  :serial t
  :components ((:file "package")
               (:file "draw-keypoints")
               (:file "draw-matches"))
  :depends-on (:sift :imago))
