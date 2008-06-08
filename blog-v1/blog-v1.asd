;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(defpackage #:blog-asd
  (:use :cl :asdf))

(in-package :blog-asd)

(defsystem blog-v1
    :name "blog-v1"
    :version "0.0.1"
    :maintainer ""
    :author ""
    :licence ""
    :description "blog-v1"
    :depends-on (:weblocks)
    :components ((:file "blog")
		 (:module conf
		  :components ((:file "stores"))
		  :depends-on ("blog"))
		 (:module src
		  :components ((:file "init-session" :depends-on ("layout"))
			       (:file "layout" :depends-on ("models" "views"))
			       (:file "models")
			       (:file "views" :depends-on ("models")))
		  :depends-on ("blog" conf))))

