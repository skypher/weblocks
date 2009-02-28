;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(defpackage #:simple-blog-asd
  (:use :cl :asdf))

(in-package :simple-blog-asd)

(defsystem simple-blog
  :name "simple-blog"
  :version "0.0.2"
  :maintainer ""
  :author ""
  :licence ""
  :description "simple blog"
  :depends-on (:weblocks)
  :components ((:file "simple-blog")
	       (:module conf
			:components ((:file "stores"))
			:depends-on ("simple-blog"))
	       (:module src
			:components 
			((:file "init-session" 
				:depends-on ("layout"))
			 (:file "layout" 
				:depends-on ("model" "views"))
			 (:module model
				  :components
				  ((:file "post"
					  :depends-on ("user"))
				   (:file "user")))
			 (:module views
				  :components
				  ((:file "post-views"
					  :depends-on ("presentations"))
				   (:module presentations
					    :components
					    ((:file "action-link"))))
				  :depends-on ("model"))
			 (:module widgets
				  :components
				  ((:file "post")
				   (:file "blog" 
					  :depends-on ("post")))
				  :depends-on ("model" "views")))
			:depends-on ("simple-blog" conf))))
