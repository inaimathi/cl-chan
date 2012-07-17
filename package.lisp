(defpackage 
    :cl-chan (:use :cl :cl-who :hunchentoot :formlets :clsql :cl-fad)
    (:import-from :imago :read-png :write-png) ;; resize
    (:import-from :ch-image :read-image-file :write-image-file) ;; resize-image
    (:import-from :skippy :load-data-stream :output-data-stream) ;; scale
    (:shadowing-import-from :clsql :select))
(in-package :cl-chan)

;;;;;;;;;;;;;;;;;;;; config variables

(defparameter *image-storage-directory* "img"
  "Specifies where cl-chan should store images. 
'big/' and 'preview/' subdirs will be created there.")

(defparameter *allowed-image-fn*
  (file-type? "image/x-png" "image/png" "image/jpeg" "image/pjpeg" "image/gif"))

(defparameter *image-message*
  "You need a PNG, JPG or GIF smaller than 3MB")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *big-dir* (merge-pathnames (make-pathname :directory `(:relative ,*image-storage-directory* "big"))))
(defparameter *preview-dir* (merge-pathnames (make-pathname :directory `(:relative ,*image-storage-directory* "preview"))))

(ensure-directories-exist *big-dir*)
(ensure-directories-exist *preview-dir*)

(connect '("localhost" "cl_chan" "me" "my password") :database-type :mysql)

(setf *default-caching* nil)

(setf formlets:*public-key* "my-public-key" 
      formlets:*private-key* "my-private-key")

(defvar *web-server* (start (make-instance 'hunchentoot:easy-acceptor :port 4242)))
(push (create-static-file-dispatcher-and-handler "/cl-chan.css" (merge-pathnames "cl-chan.css")) *dispatch-table*)
(push (create-folder-dispatcher-and-handler 
       "/img/" 
       (merge-pathnames (make-pathname :directory `(:relative ,*image-storage-directory*)))) 
      *dispatch-table*)