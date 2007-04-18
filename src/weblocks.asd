;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(defpackage #:weblocks-asd
  (:use :cl :asdf))

(in-package :weblocks-asd)

(defsystem weblocks
  :name "weblocks"
  :version "0.0.1"
  :maintainer "Slava Akhmechet"
  :author "Slava Akhmechet"
  :licence "GPL"
  :description "A Common Lisp web framework."
  :depends-on ("closer-mop" "metatilities" "hunchentoot" "cl-who")
  :components ((:file "weblocks")
	       (:file "utils"
		      :depends-on ("weblocks"))
	       (:defile "page-template"
		   :depends-on ("weblocks"))
	       (:file "actions"
		      :depends-on ("weblocks" "utils" "page-template"))
	       (:module renderers
			:components ((:file "renderer-output-utils")
				     (:file "data-renderer"
				      :depends-on ("renderer-output-utils"))
				     (:file "form-renderer"
				      :depends-on ("renderer-output-utils"))
				     (:file "table-renderer"
				      :depends-on ("renderer-output-utils")))
			:depends-on ("weblocks" "utils"))
	       (:file "request-object-mapping"
		      :depends-on (renderers "utils"))
	       (:module widgets
			:components ((:file "widget")
				     (:file "dataform"
				      :depends-on ("widget"))
				     (:file "composite"
				      :depends-on ("widget")))
			:depends-on (renderers "request-object-mapping" "utils" "actions"))
	       (:file "server"
		      :depends-on (widgets))))


