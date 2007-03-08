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
	       (:module renderers
		:components ((:file "renderer-output-utils")
			     (:module data
			      :components ((:file "data-renderer"))))
		:depends-on ("weblocks"))
	       (:file "server"
		      :depends-on (renderers))))


