(in-package :cl-chan)

(defclass image-upload ()
  ((name :reader name :initarg :name)
   (file-path :reader file-path :initarg :file-path)))

;;;;;;;;;; utility
(defun file-tuple->image-upload (hunchentoot-file-tuple)
  (destructuring-bind (file-path original-file-name mimetype) hunchentoot-file-tuple
    (make-instance (intern (string-upcase mimetype) :cl-chan)
		   :name (file-namestring file-path)
		   :file-path file-path)))

(defun store! (hunchentoot-file-tuple)
  (when hunchentoot-file-tuple
    (let ((img (file-tuple->image-upload hunchentoot-file-tuple)))
      (store-images! img))))

(defun new-dimensions (size width height)
  "Given a target size and width/height, returns a new width/height preserving aspect ratio. 
If both dimensions are smaller than or equal to [size], they are merely returned."
  (let ((ratio (float (/ (max width height) size))))
    (if (>= 1 ratio)
	(values width height)
	(values (round (/ width ratio)) (round (/ height ratio))))))

;;;;;;;;;; PNGs
(defclass png (image-upload) ())
(defclass image/x-png (png) ())
(defclass image/png (png) ())

(defmethod store-images! ((img png))
  "Takes an image and a pathname. 
Creates a preview of the given image in the folder specified by pathname."
  (let* ((pic (read-png (file-path img)))
	 (w (imago:image-width pic))
	 (h (imago:image-height pic))
	 (pic-name (make-pathname :name (name img) :type "png")))
    (copy-file (file-path img) (merge-pathnames pic-name *big-dir*))
    (multiple-value-bind (new-width new-height) (new-dimensions 250 w h)
      (write-png (imago:resize pic new-width new-height) 
		 (merge-pathnames pic-name *preview-dir*)))
    (namestring pic-name)))

;;;;;;;;;; JPGs
(defclass jpg (image-upload) ())
(defclass image/jpeg (jpg) ())
(defclass image/pjpeg (jpg) ())

(defmethod store-images! ((img jpg))
  "Takes an image and a pathname. 
Creates a preview of the given image in the folder specified by pathname."
  (let ((pic-name (make-pathname :name (name img) :type "jpg")))
    (copy-file (file-path img) (merge-pathnames pic-name *big-dir*))
    (let* ((pic (read-image-file (merge-pathnames pic-name *big-dir*)))
	   (w (ch-image:image-width pic))
	   (h (ch-image:image-height pic)))
      (multiple-value-bind (new-width new-height) (new-dimensions 250 w h)
	(write-image-file (merge-pathnames pic-name *preview-dir*)
			  (ch-image:resize-image pic new-height new-width))))
    (namestring pic-name)))