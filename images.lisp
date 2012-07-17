(in-package :cl-chan)

(defclass image-upload ()
  ((name :reader name :initarg :name)
   (file-path :reader file-path :initarg :file-path)))

;;;;;;;;;; utility
(defun file-tuple->image-upload (hunchentoot-file-tuple)
  (destructuring-bind (file-path original-file-name mimetype) hunchentoot-file-tuple
    (make-instance (intern (string-upcase mimetype) :cl-chan)
		   :name (format nil "~a-~a" (file-namestring file-path) (get-universal-time)) 
		   :file-path file-path)))

(defun store! (hunchentoot-file-tuple)
  (when hunchentoot-file-tuple
    (let ((img (file-tuple->image-upload hunchentoot-file-tuple)))
      (store-images! img))))

(defun new-dimensions (size width height)
  "Given a target size and width/height, returns a new width/height preserving aspect ratio. 
Does not scale images smaller than 250x250."
  (let ((ratio (max 1 (float (/ (max width height) size)))))
    (values (round (/ width ratio)) (round (/ height ratio)) ratio)))

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
  "Handles JPG images."
  (let ((pic-name (make-pathname :name (name img) :type "jpg")))
    (copy-file (file-path img) (merge-pathnames pic-name *big-dir*))
    (let* ((pic (read-image-file (merge-pathnames pic-name *big-dir*)))
	   (w (ch-image:image-width pic))
	   (h (ch-image:image-height pic)))
      (multiple-value-bind (new-width new-height) (new-dimensions 250 w h)
	(write-image-file (merge-pathnames pic-name *preview-dir*)
			  (ch-image:resize-image pic new-height new-width))))
    (namestring pic-name)))

;;;;;;;;;; GIFs
(defclass image/gif (image-upload) ())

(defmethod store-images! ((img image/gif))
  "Takes an image and a pathname. 
Creates a preview of the given image in the folder specified by pathname."
  (let* ((pic (load-data-stream (file-path img)))
	 (first-frame (aref (skippy:images pic) 0))
	 (width (skippy:width pic))
	 (height (skippy:height pic))
	 (pic-name (make-pathname :name (name img) :type "gif")))
    (copy-file (file-path img) (merge-pathnames pic-name *big-dir*))
    (multiple-value-bind (new-w new-h) (new-dimensions 250 width height)
      (let ((new-pic (skippy:make-data-stream 
		      :width new-w :height new-h
		      :color-table (skippy:color-table pic))))
	(skippy:add-image 
	 (skippy:composite first-frame
			   (skippy:make-image :width new-w :height new-h) 
			   :width new-w :height new-h)
	 new-pic)
	(output-data-stream new-pic (merge-pathnames pic-name *preview-dir*))))
    (namestring pic-name)))