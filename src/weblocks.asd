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
  :depends-on ("closer-mop" "metatilities" "hunchentoot" "tinaa")
  :components ((:file "weblocks")
	       (:file "renderer-output-utils"
		      :depends-on ("weblocks"))
	       (:file "data-renderer"
		      :depends-on ("weblocks" "renderer-output-utils"))
	       (:file "renderer-test")
	       (:file "server"
		      :depends-on ("data-renderer" "renderer-test"))
	       (:file "scripts"
		      :depends-on ("weblocks"))))


