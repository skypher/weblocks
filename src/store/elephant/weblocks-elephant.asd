
;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(defpackage #:weblocks-elephant-asd
  (:use :cl :asdf))

(in-package :weblocks-elephant-asd)

(defsystem weblocks-elephant
  :name "weblocks-elephant"
  :maintainer "Ian Eslick"
  :author "Ian Eslick"
  :licence "LLGPL"
  :description "A weblocks backend for elephant."
  :depends-on (:moptilities :metatilities :elephant :weblocks :weblocks-memory)
  :components ((:file "elephant")
	       (:file "proxy"))
  :serial t)


