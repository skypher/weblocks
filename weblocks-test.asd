;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(defpackage #:weblocks-test-asd
  (:use :cl :asdf))

(in-package :weblocks-test-asd)

(defsystem "weblocks-test"
  :class :package-inferred-system
  :pathname "t"
  :depends-on ("weblocks-test/dependencies"
               "weblocks-test/hooks"
               "weblocks-test/weblocks"
               "weblocks-test/request"
               "weblocks-test/response"
               "weblocks-test/request-handler"
               "weblocks-test/actions"
               "weblocks-test/commands")
  :perform (test-op (o c) (uiop:symbol-call :rove '#:run c)))


(asdf:register-system-packages "lack" '(#:lack.request))
(asdf:register-system-packages "lack-test" '(#:lack.test))

;; (asdf:register-system-packages "weblocks" '(#:weblocks.dependencies
;;                                             #:weblocks.session
;;                                             #:weblocks.request
;;                                             #:weblocks.app
;;                                             #:weblocks.html
;;                                             #:weblocks.hooks
;;                                             #:weblocks.widgets.string-widget
;;                                             #:weblocks.request-handler
;;                                             #:weblocks.response))

