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
  :depends-on ("weblocks" "rt" "closer-mop")
  :components ((:file "weblocks-test")
	       (:module fixtures
		:components ((:file "shared"))
		:depends-on ("weblocks-test"))
	       (:module renderers
		:components ((:file "renderer-output-utils")
			     (:file "data-renderer"))
		:depends-on ("weblocks-test" fixtures))))


