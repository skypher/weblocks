;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(defpackage #:weblocks-asd
  (:use :cl :asdf))

(in-package :weblocks-asd)

(defsystem weblocks
  :name "weblocks"
  :version "0.0.1"
  :maintainer "Slava Akhmechet"
  :author "Slava Akhmechet"
  :licence "LLGPL"
  :description "A Common Lisp web framework."
  :depends-on ("closer-mop" "metatilities" "hunchentoot" "cl-who" "cl-ppcre" "cl-json" "puri" "md5"
			    "fare-matcher")
  :components ((:file "weblocks")
	       (:module utils
			:components ((:file "misc")
				     (:file "typespec"
					    :depends-on ("misc"))
				     #+(or openmcl mcl) (:file "dfun-mcl"
							       :depends-on ("typespec")))
			:depends-on ("weblocks"))
	       (:file "page-template"
		      :depends-on ("weblocks" utils "application"))
	       (:file "actions"
		      :depends-on ("weblocks" utils))
	       (:file "debug-mode"
		      :depends-on ("weblocks" "actions"))
	       (:file "request-hooks"
		      :depends-on ("weblocks"))
	       (:file "request-handler"
		      :depends-on ("weblocks" utils "page-template" "debug-mode" "actions" "request-hooks"
				   "application" "request"))
	       (:module snippets
			:components ((:file "suggest")
				     (:file "isearch"
				      :depends-on ("html-utils"))
				     (:file "html-utils"))
			:depends-on ("weblocks" "request" "server" "actions"))
	       (:module linguistic
			:components ((:file "grammar")
				     (:file "typespecs"
				      :depends-on ("grammar")))
			:depends-on ("weblocks" utils))
	       (:module renderers
			:components ((:file "renderer-output-utils")
				     (:file "data-renderer"
				      :depends-on ("renderer-output-utils"))
				     (:file "form-renderer"
				      :depends-on ("renderer-output-utils" "data-renderer"))
				     (:file "table-renderer"
				      :depends-on ("renderer-output-utils")))
			:depends-on ("weblocks" utils snippets))
	       (:module form-management
			:components ((:file "validation")
				     (:file "form-parsers"
				      :depends-on ("validation"))
				     (:file "request-object-mapping"
				      :depends-on ("validation" "form-parsers")))
			:depends-on (utils linguistic))
	       (:module widgets
			:components ((:module widget
				      :components ((:file "widget"
						    :depends-on ("widget-mop"))
						   (:file "widget-mop")))
				     (:file "flash"
				      :depends-on (widget))
				     (:file "dataform"
				      :depends-on (widget))
				     (:module datagrid
				      :components ((:file "datagrid"
						    :depends-on ("filter" "sort" "select"
									  "drilldown"))
						   (:file "filter")
						   (:file "sort")
						   (:file "select")
						   (:file "drilldown"))
				      :depends-on (widget))
				     (:file "gridedit"
				      :depends-on ("datagrid" "dataform"))
				     (:file "paging"
				      :depends-on (widget))
				     (:file "composite"
				      :depends-on (widget))
				     (:file "navigation"
				      :depends-on ("composite" widget)))
			:depends-on (snippets renderers
					      form-management utils "actions" "server" "request"
					      "request-hooks" linguistic))
	       (:module types
			:components ((:file "us-states")
				     (:file "boolean")
				     (:file "member")
				     (:file "symbol")
				     (:file "keyword"
				      :depends-on ("symbol")))
			:depends-on (renderers snippets widgets))
	       (:file "server"
		      :depends-on ("weblocks" utils))
	       (:file "request"
		      :depends-on ("weblocks" "actions"))
	       (:file "application"
		      :depends-on ("weblocks"))
	       (:file "default-application"
		      :depends-on ("server" "weblocks" utils "request-handler"))))


