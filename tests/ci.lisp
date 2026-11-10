(defun do-all()
  (handler-case
      (ql:quickload :sift/tests)
    (error ()
      (uiop:quit 1)))
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
