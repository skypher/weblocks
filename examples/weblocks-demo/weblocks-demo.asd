;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(defpackage #:weblocks-demo-asd
  (:use :cl :asdf))

(in-package :weblocks-demo-asd)

(defsystem weblocks-demo
    :name "weblocks-demo"
    :version "0.1"
    :author "Slava Akhmechet"
    :licence "Public Domain"
    :description "weblocks-demo"
    :depends-on (:weblocks :metatilities)
    :components ((:file "package")
                 (:module conf
		  :components ((:file "stores"))
                  :depends-on ("package"))
                 (:file "weblocks-demo"
		  :depends-on ("conf" "package"))
		 (:module src
		  :components ((:file "layout"
				      :depends-on (model))
			       (:file "snippets")
			       (:file "sandbox"
				      :depends-on (model))
			       (:file "init-session"
				      :depends-on ("layout" "snippets" "sandbox"))
			       (:module model
					:components ((:file "company")
						     (:file "address")
						     (:file "person"
							    :depends-on ("address"))
						     (:file "employee"
							    :depends-on ("person" "company")))))
		  :depends-on ("weblocks-demo" conf package))))

