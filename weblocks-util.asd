;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(defpackage #:weblocks-util-asd
  (:use :cl :asdf))

(in-package :weblocks-util-asd)

(defsystem weblocks-util
  :name "weblocks-util"
  :version "0.1.1"
  :maintainer "Olexiy Zamkoviy, Scott L. Burson"
  :author "Slava Akhmechet"
  :licence "LLGPL"
  :description "Utilities for weblocks"
  :depends-on (:closer-mop
               :hunchentoot
               :puri
               :cl-json
               :cl-who
               :parenscript
               :cl-fad
               :optima
               :cl-cont
               :metatilities
               :cl-ppcre
               :anaphora
               :f-underscore
               :bordeaux-threads
               :salza2
               :html-template
               :trivial-timeout
               :trivial-backtrace 
               :parse-number 
               :pretty-function 
               :ironclad)
  :components 
    ((:module src
      :components (
          (:file "util")
          (:module utils
           :components 
           ((:file "misc")
            (:file "clos")
            ;(:file "runtime-class")
            ;(:file "string")
            (:file "list")
            ;(:file "uri")
            ;(:file "html")
            ;(:file "javascript")
            ;(:file "isearch" :depends-on ("html"))
            ;(:file "menu" :depends-on ("html"))
            ;(:file "suggest")
            ;(:file "timing")
            ;(:file "repl")
            (:file "i18n")
            (:file "templates" :depends-on ("html-parts"))
            (:file "html-parts"))
           :depends-on ("util"))))))
