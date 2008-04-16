;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(defpackage #:blog-asd
  (:use :cl :asdf))

(in-package :blog-asd)

(defsystem blog
    :name "blog"
    :version "0.0.1"
    :maintainer ""
    :author ""
    :licence ""
    :description "blog"
    :depends-on (:weblocks)
    :components ((:file "blog")
		 (:module conf
		  :components ((:file "stores"))
		  :depends-on ("blog"))
		 (:module src
		  :components ((:file "init-session" :depends-on ("layout"))
			       (:file "layout" :depends-on ("models" "views"))
			       (:file "models")
			       (:file "views" :depends-on ("models"))
			       (:file "specials")
			       (:module widgets
				:components
				((:file "post")
				 (:file "blog" :depends-on ("post")))
				:depends-on ("models" "views" "specials")))
		  :depends-on ("blog" conf))))

