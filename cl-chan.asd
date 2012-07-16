;;; -*- Mode: Lisp -*-
(defpackage :cl-chan-system (:use :cl :asdf))
(in-package :cl-chan-system)

(asdf:defsystem cl-chan
  :version "0.001"
  :author "Inaimathi"
  :maintainer "you"
  :licence "AGPL"
  :description "A simple message board server"
  :depends-on (:hunchentoot :cl-who :clsql :formlets)
  :serial t  
  :components ((:file "package") (:file "model") (:file "cl-chan") (:file "testing-data")))