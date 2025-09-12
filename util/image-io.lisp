(in-package :sift/util)

(sera:-> pixel-intensity (imago:image)
         (values (sera:-> ((unsigned-byte 32))
                          (values (unsigned-byte 8) &optional))
                 &optional))
(defun pixel-intensity (image)
  (etypecase image
    (imago:rgb-image       #'imago:color-intensity)
    (imago:grayscale-image #'imago:gray-intensity)))

(sera:-> write-image ((simple-array single-float (* *)) (or string pathname))
         (values &optional))
(defun write-image (data name)
  "Write an array of single floats in the range [0, 1] to a grayscale
image."
  (let ((pixels (aops:vectorize* 'imago:grayscale-pixel
                    (data)
                  (imago:make-gray
                   (clamp (floor (* data 255)) 0 255)))))
    (imago:write-image
     (imago:make-grayscale-image-from-pixels pixels)
     name)
    (values)))

#+sbcl
(sb-c:defknown load-image ((or string pathname) &optional boolean)
    (or (simple-array single-float (* *))
        (simple-array sera:octet   (* *)))
    (sb-c:any)
  :overwrite-fndb-silently t)

#-sbcl
(sera:-> load-image ((or string pathname) &optional boolean)
         (values (or (simple-array single-float (* *))
                     (simple-array sera:octet   (* *)))
                 &optional))
(defun load-image (name &optional normalize)
  "Load an image as an array of intensities of type @c((unsigned-byte
8)) (if @c(normalize) is @c(nil)) or @c(single-float) values in the
range \\([0, 1]\\) (if @c(normalize) is @c(t))."
  (let* ((image (imago:read-image name))
         (intensity (pixel-intensity image))
         (pixels (imago:image-pixels image)))
    (if normalize
        (aops:vectorize* 'single-float
            (pixels)
          (/ (funcall intensity pixels) 255f0))
        (aops:vectorize* 'sera:octet
            (pixels)
          (funcall intensity pixels)))))

#+sbcl
(sb-c:defoptimizer (load-image sb-c:derive-type) ((name &optional normalize))
  (let ((normalized   (sb-kernel:specifier-type '(simple-array single-float (* *))))
        (unnormalized (sb-kernel:specifier-type '(simple-array sera:octet   (* *))))
        (constantp (and normalize (sb-c:constant-lvar-p normalize))))
    (cond
      ((and constantp
            (eq (sb-c::constant-value (sb-c::lvar-constant normalize)) t))
       normalized)
      ((and constantp
            (eq (sb-c::constant-value (sb-c::lvar-constant normalize)) nil))
       unnormalized)
      ((not normalize)  ; Optional argument not given
       unnormalized)))) ; Fall through: give up
