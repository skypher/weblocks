;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(defpackage #:weblocks-clsql-demo-asd
  (:use :cl :asdf))

(in-package :weblocks-clsql-demo-asd)

(defsystem weblocks-clsql-demo
    :name "weblocks-clsql-demo"
    :version "0.1"
    :author "Slava Akhmechet"
    :licence "Public Domain"
    :description "weblocks-clsql-demo"
    :depends-on (:weblocks :metatilities :clsql)
    :components ((:file "weblocks-clsql-demo")
		 (:module conf
		  :components ((:file "stores"))
		  :depends-on ("weblocks-clsql-demo"))
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
		  :depends-on (conf "weblocks-clsql-demo"))))

