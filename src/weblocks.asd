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
  :depends-on ("closer-mop" "metatilities" "hunchentoot" "cl-who" "cl-ppcre" "cl-json" "puri")
  :components ((:file "weblocks")
	       (:file "utils"
		      :depends-on ("weblocks"))
	       (:file "page-template"
		      :depends-on ("weblocks" "utils"))
	       (:file "actions"
		      :depends-on ("weblocks" "utils"))
	       (:file "debug-mode"
		      :depends-on ("weblocks" "actions"))
	       (:file "request-handler"
		      :depends-on ("weblocks" "utils" "page-template" "debug-mode" "actions"))
	       (:module renderers
			:components ((:file "renderer-output-utils")
				     (:file "data-renderer"
				      :depends-on ("renderer-output-utils"))
				     (:file "form-renderer"
				      :depends-on ("renderer-output-utils"))
				     (:file "table-renderer"
				      :depends-on ("renderer-output-utils")))
			:depends-on ("weblocks" "utils"))
	       (:module form-management
			:components ((:file "validation")
				     (:file "form-parsers"
				      :depends-on ("validation"))
				     (:file "request-object-mapping"
				      :depends-on ("validation" "form-parsers")))
			:depends-on ("utils"))
	       (:module widgets
			:components ((:file "widget")
				     (:file "dataform"
				      :depends-on ("widget"))
				     (:file "datagrid"
				      :depends-on ("widget"))
				     (:file "paging"
				      :depends-on ("widget"))
				     (:file "composite"
				      :depends-on ("widget"))
				     (:file "navigation"
				      :depends-on ("composite" "widget")))
			:depends-on (renderers form-management "utils" "actions"))
	       (:file "server"
		      :depends-on (widgets))))


