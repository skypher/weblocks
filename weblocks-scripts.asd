;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(defpackage #:weblocks-scripts-asd
  (:use :cl :asdf))

(in-package :weblocks-scripts-asd)

(defsystem weblocks-scripts
  :name "weblocks-scripts"
  :version "0.0.1"
  :maintainer "Slava Akhmechet"
  :author "Slava Akhmechet"
  :licence "LLGPL"
  :description "A set of scripts for weblocks framework."
  :depends-on ("cl-fad" "cl-ppcre")
  :components ((:module scripts
		:components (
	         (:file "weblocks-scripts")
		 (:file "gen-doc"
			:depends-on ("weblocks-scripts"))
		 (:file "make-new-app"
			:depends-on ("weblocks-scripts"))))))

