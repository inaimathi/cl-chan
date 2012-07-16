(defpackage :cl-chan (:use :cl :cl-who :hunchentoot :formlets :clsql)
  (:shadowing-import-from :clsql :select))
(in-package :cl-chan)

(connect '("localhost" "cl_chan" "me" "my password") :database-type :mysql)

(setf *default-caching* nil)

(setf formlets:*public-key* "my-public-key" 
      formlets:*private-key* "my-private-key")

(defvar *web-server* (start (make-instance 'hunchentoot:easy-acceptor :port 4242)))
(push (create-static-file-dispatcher-and-handler "/cl-chan.css" (merge-pathnames "cl-chan.css")) *dispatch-table*)