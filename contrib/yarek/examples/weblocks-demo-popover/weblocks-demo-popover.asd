;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(defpackage #:weblocks-demo-popover-asd
  (:use :cl :asdf))

(in-package :weblocks-demo-popover-asd)

(defsystem weblocks-demo-popover
    :name "weblocks-demo-popover"
    :version "0.1"
    :author "Yarek Kowalik, Slava Akhmechet"
    :licence "Public Domain"
    :description "weblocks-demo"
    :depends-on (:weblocks :metatilities :weblocks-yarek)
    :components ((:file "weblocks-demo-popover")
		 (:module conf
		  :components ((:file "stores"))
		  :depends-on ("weblocks-demo-popover"))
		 (:module src
		  :components ((:file "layout"
				      :depends-on (model))
			       (:file "snippets"
                                      :depends-on ("init-session"))
			       (:file "sandbox"
				      :depends-on (model))
			       (:file "init-session"
				      :depends-on ("layout" "sandbox"))
			       (:module model
					:components ((:file "company")
						     (:file "address")
						     (:file "person"
							    :depends-on ("address"))
						     (:file "employee"
							    :depends-on ("person" "company")))))
		  :depends-on ("weblocks-demo-popover" conf))))

