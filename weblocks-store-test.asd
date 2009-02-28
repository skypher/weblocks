
;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(defpackage #:weblocks-store-test-asd
  (:use :cl :asdf))

(in-package :weblocks-store-test-asd)

(defsystem weblocks-store-test
  :name "weblocks-store-test"
  :maintainer "Slava Akhmechet"
  :author "Slava Akhmechet"
  :licence "LLGPL"
  :description "A test suite for weblocks backend stores."
  :depends-on (:rt :weblocks :lift :f-underscore)
  :components ((:module test
                :components (
	         (:file "weblocks-store-test")))))

