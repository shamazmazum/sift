(in-package :sift/debug)

(sera:-> pixel-intensity (imago:image)
         (values (sera:-> ((unsigned-byte 32))
                          (values (unsigned-byte 8) &optional))
                 &optional))
(defun pixel-intensity (image)
  (etypecase image
    (imago:rgb-image       #'imago:color-intensity)
    (imago:grayscale-image #'imago:gray-intensity)))

(sera:-> load-image ((or string pathname))
         (values (simple-array single-float (* *)) &optional))
(defun load-image (name)
  "Load an image as an array of single float values in the range [0,
1]. If the image is in color, only pixel intensity matters."
  (let* ((image (imago:read-image name))
         (intensity (pixel-intensity image))
         (pixels (imago:image-pixels image)))
    (aops:vectorize* 'single-float
        (pixels)
      (/ (funcall intensity pixels) 255f0))))

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
