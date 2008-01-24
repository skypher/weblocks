;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(defpackage #:{APPNAME}-asd
  (:use :cl :asdf))

(in-package :{APPNAME}-asd)

(defsystem {APPNAME}
    :name "{APPNAME}"
    :version "0.0.1"
    :maintainer ""
    :author ""
    :licence ""
    :description "{APPNAME}"
    :depends-on (:weblocks)
    :components ((:module conf
		  :components ((:file "stores"))
		  :depends-on (src))
		 (:module src
		  :components ((:file "{APPNAME}")
			       (:file "init-session"
				      :depends-on ("{APPNAME}"))))))

