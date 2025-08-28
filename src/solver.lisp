(in-package :sift/core)

(cffi:define-foreign-library lapack
  (:unix (:or "liblapack.so"))
  (t (:default "liblapack")))
(cffi:use-foreign-library lapack)

(cffi:defcfun ("sgesv_" %sgesv) :void
  (n    (:pointer :int32))
  (nrhs (:pointer :int32))
  (a    :pointer)
  (lda  (:pointer :int32))
  (ipiv :pointer)
  (b    :pointer)
  (ldb  (:pointer :int32))
  (info (:pointer :int32)))

#+sbcl
(sb-c:defknown solve ((mat *) (vec *)) (or (vec *) null)
    (sb-c:movable sb-c:flushable)
  :overwrite-fndb-silently t)

#+sbcl
(sb-c:defoptimizer (solve sb-c:derive-type) ((m v))
  (let* ((vtype (sb-c::lvar-type v))
         (dim (sb-kernel:array-type-dimensions vtype)))
    (sb-kernel:type-union
     (sb-kernel:make-array-type dim :element-type (sb-kernel:array-type-element-type vtype))
     (sb-kernel:specifier-type 'null))))

#+sbcl
(sb-c:defoptimizer (solve sb-c::ir2-hook) ((m v) node)
  (let ((mtype (sb-c::lvar-type m))
        (vtype (sb-c::lvar-type v)))
    (let ((d1 (second (sb-kernel:array-type-dimensions mtype)))
          (d2 (first  (sb-kernel:array-type-dimensions vtype))))
      (when (and (not (eq d1 '*))
                 (not (eq d2 '*))
                 (/= d1 d2))
        (sb-c:compiler-warn
         "Dimensions mismatch: ~d vs ~d" d1 d2)))))

#-sbcl
(serapeum:-> solve ((mat *) (vec *))
             (values (or (vec *) null) &optional))
(defun solve (a b)
  (assert (= (array-dimension a 0)
             (array-dimension a 1)
             (length b)))
  (let ((n (length b)))
    (cffi:with-foreign-objects
        ((nref     :int32) (nrhs-ref :int32)
         (lda-ref  :int32) (ldb-ref  :int32)
         (info-ref :int32) (ipiv-ref :int32 n)
         (aref :float (expt n 2))
         (bref :float (expt n 2)))
      ;; Fill A
      (loop for i below n do
            (loop for j below n do
                  (setf (cffi:mem-aref aref :float (+ (* n j) i))
                        (aref a i j))))
      ;; Fill B
      (loop for i below n do
            (setf (cffi:mem-aref bref :float i)
                  (aref b i)))
      ;; Set everything else
      (setf (cffi:mem-ref nref     :int32) n
            (cffi:mem-ref nrhs-ref :int32) 1
            (cffi:mem-ref lda-ref  :int32) n
            (cffi:mem-ref ldb-ref  :int32) n)
      (%sgesv nref nrhs-ref aref lda-ref ipiv-ref bref ldb-ref info-ref)

      (when (zerop (cffi:mem-ref info-ref :int32))
        (let ((result (make-array n :element-type 'single-float)))
          ;; Fill the result
          (loop for i below n do
                (setf (aref result i)
                      (cffi:mem-aref bref :float i)))
          result)))))
