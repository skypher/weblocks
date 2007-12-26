;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(defpackage #:weblocks-test-asd
  (:use :cl :asdf))

(in-package :weblocks-test-asd)

(defsystem weblocks-test
  :name "weblocks-test"
  :version "0.0.1"
  :maintainer "Slava Akhmechet"
  :author "Slava Akhmechet"
  :licence "LLGPL"
  :description "A test harness for weblocks framework."
  :depends-on (:weblocks :weblocks-store-test :rt :closer-mop :metatilities)
  :components ((:file "weblocks-test")
	       (:file "weblocks"
		      :depends-on ("weblocks-test"))
	       (:module utils-test
			:components ((:file "misc")
				     (:file "typespec"
				      :depends-on ("misc"))
				     #+(or openmcl mcl) (:file "dfun-mcl"
							       :depends-on ("typespec")))
			:depends-on ("weblocks-test"))
	       (:file "actions"
		      :depends-on ("weblocks-test"))
	       (:file "request-hooks"
		      :depends-on ("weblocks-test"))
	       (:file "request-handler-utils"
		      :depends-on ("weblocks-test"))
	       (:file "request-handler"
		      :depends-on ("weblocks-test" "request-handler-utils" fixtures))
	       (:file "debug-mode"
		      :depends-on ("weblocks-test"))
	       (:file "server"
		      :depends-on ("weblocks-test"))
	       (:file "request"
		      :depends-on ("weblocks-test"))
	       (:file "application"
		      :depends-on ("weblocks-test"))
	       (:file "page-template"
		      :depends-on ("weblocks-test"))
	       (:module linguistic
			:components ((:file "grammar")
				     (:file "typespecs"))
			:depends-on ("weblocks-test"))
	       (:module fixtures
			:components ((:file "shared"))
			:depends-on ("weblocks-test"))
	       (:module store
			:components ((:file "store-utils"))
			:depends-on ("weblocks-test"))
	       (:module types
			:components ((:file "boolean")
				     (:file "member")
				     (:file "us-states")
                                     (:file "text")
				     (:file "password")
				     (:file "symbol")
				     (:file "keyword"))
			:depends-on ("weblocks-test" renderers))
	       (:module snippets
			:components ((:file "suggest")
				     (:file "isearch")
				     (:file "html-utils-helper")
				     (:file "html-utils"
				      :depends-on ("html-utils-helper")))
			:depends-on ("weblocks-test" fixtures))
	       (:module renderers
			:components ((:file "renderer-output-utils")
				     (:file "data-renderer-utils")
				     (:file "table-renderer-utils")
				     (:file "data-renderer"
				      :depends-on ("data-renderer-utils"))
				     (:file "form-renderer-utils")
				     (:file "form-renderer"
				      :depends-on ("form-renderer-utils"))
				     (:file "table-renderer"
				      :depends-on ("table-renderer-utils")))
			:depends-on ("weblocks-test" "utils-test" snippets fixtures))
	       (:module form-management
			:components ((:file "validation")
				     (:file "form-parsers")
				     (:file "request-object-mapping"))
			:depends-on ("weblocks-test" fixtures))
	       (:module widgets
			:components ((:file "widget-test-utils")
				     (:module widget
				      :components ((:file "widget")
						   (:file "widget-mop"))
				      :depends-on ("widget-test-utils"))
				     (:file "dataform")
				     (:file "flash")
				     (:module datagrid
				      :components ((:file "datagrid")
						   (:file "filter")
						   (:file "sort")
						   (:file "select")
						   (:file "drilldown"))
				      :depends-on ("widget-test-utils" "pagination-utils"))
				     (:file "gridedit"
					    :depends-on ("widget-test-utils" "pagination-utils"))
				     (:file "navigation")
				     (:file "pagination-utils")
				     (:file "pagination"
					    :depends-on ("pagination-utils"))
				     (:file "composite"))
			:depends-on ("weblocks-test" fixtures renderers))
	       (:module control-flow
			:components ((:file "call-answer")
				     (:file "dialog")
				     (:file "workflow"))
			:depends-on ("weblocks-test" snippets))))


