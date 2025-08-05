(in-package :sift/debug)

(sera:-> load-image ((or string pathname))
         (values (simple-array double-float (* *)) &optional))
(defun load-image (name)
  (let ((pixels (imago:image-pixels (imago:read-image name))))
    (aops:vectorize* 'double-float
        (pixels)
      (/ (imago:color-intensity pixels) 255d0))))

(sera:-> write-image ((simple-array double-float (* *)) (or string pathname))
         (values &optional))
(defun write-image (data name)
  (let ((pixels (aops:vectorize* 'imago:grayscale-pixel
                    (data)
                  (imago:make-gray
                   (clamp (floor (* data 255)) 0 255)))))
    (imago:write-image
     (imago:make-grayscale-image-from-pixels pixels)
     name)
    (values)))
