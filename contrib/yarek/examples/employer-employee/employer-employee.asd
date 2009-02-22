;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(defpackage #:employer-employee-asd
  (:use :cl :asdf))

(in-package :employer-employee-asd)

(defsystem employer-employee
    :name "employer-employee"
    :version "0.1"
    :author "Yarek Kowalik"
    :licence "Public Domain"
    :description "weblocks-demo"
    :depends-on (:weblocks :metatilities :weblocks-yarek)
    :components ((:file "employer-employee")
		 (:module conf
		  :components ((:file "stores"))
		  :depends-on ("employer-employee"))
		 (:module src
		  :components ((:file "layout"
				      :depends-on (model widgets))
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
							    :depends-on ("person" "company"))))
                               (:module widgets
					:components ((:file "company-gridedit"
                                                            :depends-on ("company-presenter"))
						     (:file "company-presenter"))))
		  :depends-on ("employer-employee" conf))))

