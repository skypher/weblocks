;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(defpackage #:weblocks-elephant-demo-asd
  (:use :cl :asdf))

(in-package :weblocks-elephant-demo-asd)

(defsystem weblocks-elephant-demo
    :name "weblocks-elephant-demo"
    :version "0.1"
    :author "Slava Akhmechet"
    :licence "Public Domain"
    :description "weblocks-elephant-demo"
    :depends-on (:weblocks-elephant :metatilities)
    :components ((:file "weblocks-elephant-demo")
		 (:module conf
		  :components ((:file "stores"))
		  :depends-on ("weblocks-elephant-demo"))
		 (:module src
		  :components ((:file "layout"
				      :depends-on (model))
			       (:file "snippets")
			       (:file "init-session"
				      :depends-on ("layout" "snippets"))
			       (:module model
					:components ((:file "company")
						     (:file "address")
						     (:file "employee"
							    :depends-on ("company" "address")))))
		  :depends-on (conf "weblocks-elephant-demo"))))

