(in-package :sift/util)

(sera:-> pixel-intensity (imago:image)
         (values (sera:-> ((unsigned-byte 32))
                          (values (unsigned-byte 8) &optional))
                 &optional))
(declaim (inline pixel-intensity))
(defun pixel-intensity (image)
  (etypecase image
    (imago:rgb-image       #'imago:color-intensity)
    (imago:grayscale-image #'imago:gray-intensity)))

(sera:-> image->intensities (imago:image)
         (values (image (unsigned-byte 8)) &optional))
(defun image->intensities (image)
  "Extract array of pixels intensities from an imago image."
  (let* ((f (pixel-intensity image))
         (pixels (imago:image-pixels image))
         (result (make-array (array-dimensions pixels)
                             :element-type '(unsigned-byte 8))))
    (loop for i below (array-total-size result) do
      (setf (row-major-aref result i)
            (funcall f (row-major-aref pixels i))))
    result))

(sera:-> intensities->image ((image (unsigned-byte 8)))
         (values imago:grayscale-image &optional))
(defun intensities->image (array)
  "Convert an array of pixel intensities to a grayscale image."
  (declare (optimize (speed 3)))
  (let ((pixels (make-array (array-dimensions array)
                            :element-type 'imago:grayscale-pixel)))
    (loop for i below (array-total-size pixels) do
      (setf (row-major-aref pixels i)
            (imago:make-gray
             (row-major-aref array i))))
    (imago:make-grayscale-image-from-pixels pixels)))
