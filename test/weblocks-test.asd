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
  :depends-on ("weblocks" "rt" "closer-mop" "metatilities")
  :components ((:file "weblocks-test")
	       (:file "utils-test"
		      :depends-on ("weblocks-test"))
	       (:file "linguistic"
		      :depends-on ("weblocks-test"))
	       (:file "action-utils"
		      :depends-on ("weblocks-test"))
	       (:file "actions"
		      :depends-on ("weblocks-test" "action-utils"))
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
	       (:module fixtures
			:components ((:file "shared"))
			:depends-on ("weblocks-test"))
	       (:module blocks
			:components ((:file "suggest")
				     (:file "isearch")
				     (:file "form"))
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
			:depends-on ("weblocks-test" "utils-test" "action-utils" fixtures))
	       (:module form-management
			:components ((:file "validation")
				     (:file "form-parsers")
				     (:file "request-object-mapping"))
			:depends-on ("weblocks-test" fixtures))
	       (:module widgets
			:components ((:file "widget-test-utils")
				     (:file "dataform")
				     (:file "flash")
				     (:module datagrid
				      :components ((:file "datagrid")
						   (:file "filter")
						   (:file "sort")
						   (:file "select"))
				      :depends-on ("widget-test-utils"))
				     (:file "gridedit"
					    :depends-on ("widget-test-utils"))
				     (:file "navigation")
				     (:file "composite")
				     (:file "widget"))
			:depends-on ("weblocks-test" fixtures renderers))))


