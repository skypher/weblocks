
;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(defpackage #:weblocks-prevalence-asd
  (:use :cl :asdf))

(in-package :weblocks-prevalence-asd)

(defsystem weblocks-prevalence
  :name "weblocks-prevalence"
  :maintainer "Slava Akhmechet"
  :author "Slava Akhmechet"
  :licence "LLGPL"
  :description "A weblocks backend for cl-prevalence."
  :depends-on (:metatilities :cl-ppcre :cl-prevalence :bordeaux-threads :weblocks-memory)
  :components ((:file "prevalence")))
