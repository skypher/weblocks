;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(defpackage #:weblocks-test-asd
  (:use :cl :asdf))

(in-package :weblocks-test-asd)

(defsystem weblocks-test
  :name "weblocks-test"
  :version "0.0.1"
  :maintainer "Slava Akhmechet"
  :author "Slava Akhmechet"
  :licence "GPL"
  :description "A test harness for weblocks framework."
  :depends-on ("weblocks" "rt" "closer-mop" "metatilities")
  :components ((:file "weblocks-test")
	       (:file "utils-test"
		      :depends-on ("weblocks-test"))
	       (:file "actions"
		      :depends-on ("weblocks-test"))
	       (:file "server"
		      :depends-on ("weblocks-test"))
	       (:file "page-template"
		      :depends-on ("weblocks-test"))
	       (:module fixtures
			:components ((:file "shared"))
			:depends-on ("weblocks-test"))
	       (:module renderers
			:components ((:file "renderer-output-utils")
				     (:file "data-renderer-utils")
				     (:file "data-renderer"
				      :depends-on ("data-renderer-utils"))
				     (:file "form-renderer-utils")
				     (:file "form-renderer"
				      :depends-on ("form-renderer-utils"))
				     (:file "table-renderer"))
			:depends-on ("weblocks-test" fixtures))
	       (:module form-management
			:components ((:file "validation")
				     (:file "form-parsers")
				     (:file "request-object-mapping"))
			:depends-on ("weblocks-test" fixtures))
	       (:module widgets
			:components ((:file "dataform")
				     (:file "composite"))
			:depends-on ("weblocks-test" fixtures renderers))))


