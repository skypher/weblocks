;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(defpackage #:weblocks-yui-asd
  (:use :cl :asdf))

(in-package :weblocks-yui-asd)

(defsystem weblocks-yui
    :name "weblocks-yui"
    :version "0.0.1"
    :maintainer ""
    :author "Jan Rychter"
    :licence ""
    :description "YUI integration for weblocks"
    :depends-on (:weblocks)
    :components ((:file "yui")))

