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
  :depends-on (:weblocks :weblocks-store-test :lift :closer-mop :metatilities
	       :anaphora :f-underscore)
  :components ((:module test
		:components (
		 (:file "weblocks-test")
		 (:module "test-code"
			  :components ((:file "query-actions")
				       (:file "weblocks-suite")
				       (:file "ensure-diff")
				       (:file "utils"))
			  :depends-on ("weblocks-test"))
		 (:file "weblocks"
			:depends-on ("test-code"))
		 (:module utils-test
			  :components ((:file "misc")
				       (:file "runtime-class"))
			  :depends-on ("test-code"))
		 (:file "versioning"
			:depends-on ("test-code" utils-test))
		 (:file "bundling"
			:depends-on ("test-code" "versioning" utils-test))
		 (:file "dependencies"
			:depends-on ("test-code" "versioning" utils-test))
		 (:file "actions"
			:depends-on ("test-code"))
                 (:file "uri-tokens"
			:depends-on ("test-code"))
		 (:file "request-hooks"
			:depends-on ("test-code"))
		 (:file "request-handler-utils"
			:depends-on ("test-code"))
		 (:file "request-handler"
			:depends-on ("test-code" "request-handler-utils" fixtures))
		 (:file "debug-mode"
			:depends-on ("test-code"))
		 (:file "server"
			:depends-on ("test-code"))
		 (:file "request"
			:depends-on ("test-code"))
		 (:file "application"
			:depends-on ("test-code"))
		 (:file "page-template"
			:depends-on ("test-code"))
		 (:module linguistic
			  :components ((:file "grammar"))
			  :depends-on ("test-code"))
		 (:module fixtures
			  :components ((:file "shared"))
			  :depends-on ("test-code"))
		 (:module store
			  :components ((:file "store-utils"))
			  :depends-on ("test-code"))
		 (:module snippets
			  :components ((:file "suggest")
				       (:file "isearch")
				       (:file "html-utils-helper")
				       (:file "html-utils"
					      :depends-on ("html-utils-helper")))
			  :depends-on ("test-code" fixtures))
		 (:module views
			  :components ((:module view
						:components ((:file "view")
							     (:file "compiler")
							     (:file "scaffold")
							     (:file "utils")))
				       (:module dataview
						:components ((:file "test-template")
							     (:file "dataview"
								    :depends-on ("test-template"))))
				       (:module formview
						:components ((:file "test-template")
							     (:file "formview"
								    :depends-on ("test-template"))
							     (:file "helpers")
							     (:file "parser")
							     (:file "scaffold")
							     (:file "validation")
							     (:file "request-deserialization"))
						:depends-on (dataview))
				       (:module tableview
						:components ((:file "test-template")
							     (:file "tableview"
								    :depends-on ("test-template")))
						:depends-on (dataview))
				       (:file "sequence-view"
					      :depends-on (view))
				       (:module types
						:components ((:file "boolean")
							     (:file "member")
							     (:file "password")
							     (:file "us-states")
							     (:module parsers
								      :components ((:file "common")))
							     (:module presentations
								      :components ((:file "choices")
										   (:file "dropdown")
										   (:file "radio")
										   (:file "excerpt")
										   (:file "paragraph")
										   (:file "textarea")
										   (:file "image")
										   (:file "date")
										   (:file "url"))))))
			  :depends-on ("test-code" fixtures snippets))
		 (:module widgets
			  :components ((:file "widget-test-utils")
				       (:module widget
						:components ((:file "widget")
							     (:file "widget-mop"))
						:depends-on ("widget-test-utils"))
				       (:file "dataform")
				       (:file "data-editor")
				       (:file "quickform")
				       (:file "login")
				       (:file "flash")
				       (:file "datalist")
				       (:file "listedit")
				       (:module dataseq
						:components ((:file "dataseq")))
				       (:module dataedit
						:components ((:file "dataedit")
							     (:file "delete-action")))
				       (:module datagrid
						:components ((:file "datagrid")
							     (:file "sort")
							     (:file "select")
							     (:file "drilldown"))
						:depends-on ("widget-test-utils" "pagination-utils"))
				       (:file "gridedit"
					      :depends-on ("widget-test-utils" "pagination-utils"))
				       (:file "selector")
				       (:file "navigation")
				       (:file "pagination-utils")
				       (:file "pagination"
					      :depends-on ("pagination-utils"))
				       (:file "composite"))
			  :depends-on ("versioning" "test-code" fixtures views))
		 (:module control-flow
			  :components ((:file "call-answer")
				       (:file "dialog")
				       (:file "workflow"))
			  :depends-on ("test-code" snippets))))))


