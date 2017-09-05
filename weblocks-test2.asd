;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(defpackage #:weblocks-test2-asd
  (:use :cl :asdf))

(in-package :weblocks-test2-asd)

(defsystem weblocks-test2
  :name "weblocks-test"
  :version "0.2.0"
  :maintainer "Olexiy Zamkoviy, Scott L. Burson"
  :author "Slava Akhmechet"
  :licence "LLGPL"
  :description "A test harness for weblocks framework."
  :defsystem-depends-on (:prove-asdf)
  :depends-on (:weblocks
               :prove   ;; will change lift with prove soon
               :hamcrest-prove
               :cl-mock ;; to mock some functions in tests
               :lack-test
               :closer-mop
               :metatilities
               :anaphora
               :f-underscore)
  :serial t
  :components ((:module t
                :components
                ((:file "utils")
                 (:test-file "dependencies")
                 (:test-file "request-hooks")
                 (:test-file "weblocks")
                 (:test-file "actions")
                 (:test-file "request")))))


