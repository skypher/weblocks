;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(defpackage #:blog-asd
  (:use :cl :asdf))

(in-package :blog-asd)

(defsystem blog-v0
    :name "blog-v0"
    :version "0.0.1"
    :maintainer ""
    :author ""
    :licence ""
    :description "blog-v0"
    :depends-on (:weblocks)
    :components ((:file "blog")
		 (:module conf
		  :components ((:file "stores"))
		  :depends-on ("blog"))
		 (:module src
		  :components ((:file "init-session"))
		  :depends-on ("blog" conf))))

