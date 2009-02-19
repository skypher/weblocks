
;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(defpackage #:weblocks-clsql-asd
  (:use :cl :asdf))

(in-package :weblocks-clsql-asd)

(defsystem weblocks-clsql
  :name "weblocks-clsql"
  :maintainer "Slava Akhmechet"
  :author "Slava Akhmechet"
  :licence "LLGPL"
  :description "A weblocks backend for clsql."
  :depends-on (:closer-mop :metatilities :clsql :clsql-fluid :weblocks)
  :components ((:file "clsql")))

