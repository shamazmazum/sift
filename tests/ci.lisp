(defun do-all()
  (ql:quickload :sift/tests)
  ;; Short version
  (setf
   (symbol-value
    (find-symbol "*NUMBER-OF-RUNS*"
                 (find-package :sift/tests)))
   5)
  (uiop:quit
   (if (uiop:call-function "sift/tests:run-tests")
       0 1)))

(do-all)
