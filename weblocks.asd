;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(defpackage #:weblocks-asd
  (:use :cl :asdf)
  (:nicknames :wop)
  (:export #:test #:test-op #:doc #:doc-op #:make-app))

(in-package :weblocks-asd)

(defsystem weblocks
  :name "weblocks"
  :version (:read-file-form "version.lisp-expr")
  :maintainer "Alexander Artemenko, Olexiy Zamkoviy, Scott L. Burson"
  :author "Slava Akhmechet"
  :licence "LLGPL"
  :description "A Common Lisp web framework."
  :depends-on 
  (:log4cl
   :clack
   :lack-request
   :routes
   :function-cache
   :local-time
   :local-time-duration
   :dexador ;; to retrive remote dependencies and put them into the cache
   :closer-mop
   :puri
   :cl-json
   :spinneret
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
   :babel
   :metacopy
   :split-sequence
   :cl-strings)
  :serial t
  :components ((:module src
                :components (
                             (:file "package")
                             (:module utils
                              :components ((:file "clos")
                                           (:file "misc")
                                           (:file "runtime-class")
                                           (:file "string")
                                           (:file "list")
                                           (:file "uri")
                                           (:file "html-parts")
                                           (:file "timing")
                                           )
                                      )
                             (:file "variables")
                             (:file "html")
                             (:file "session")
                             (:file "hooks")
                             (:file "debug")
                             (:file "weblocks")
                             (:file "request2")

                             (:file "versioning")
                             (:file "bundling")
                             (:file "actions")
                             (:file "actions2")
                             (:file "response")
                             (:file "dependencies")
                             (:file "routes")
                             (:file "dependencies2")
                             (:file "dom-object")
                             (:file "application-mop")
                             (:module "js-backend"
                              :components ((:file "base")
                                           (:file "jquery")))
                             (:file "application")
                             (:file "page")
                             (:file "log-actions")
                             (:file "debug-mode")
                             (:file "uri-tokens")
                             (:file "session-lock")
                             (:module widgets
                              :components ((:module widget
                                            :components ((:file "widget-mop")
                                                         (:file "uri-parameters-mixin")
                                                         (:file "widget")
                                                         (:file "widget2")
                                                         (:file "string-widget")
                                                         (:file "funcall-widget")))))
                             (:file "error-handler2")
                             (:file "request-handler2")
                             (:module linguistic
                              :components ((:file "grammar")))
                             (:module control-flow
                              :components ((:file "call-answer")
                                           (:file "dialog")
                                           (:file "workflow")))
                             (:file "server")
                             (:file "server2")
                             (:file "request")
                             (:file "default-application")
                             (:file "widget-translation"))))
  ;;  :in-order-to ;; ((test-op (load-op "weblocks-test"))
  ;;  (doc-op (load-op "weblocks-scripts"))
  ;;  (make-app-op (load-op "weblocks-scripts")))
  )

;;; test-op
(defmethod perform ((o asdf:test-op) (c (eql (find-system :weblocks))))
  "A method specializer to run the weblocks test suite through ASDF."
  (funcall (intern (symbol-name :test-weblocks) (find-package :weblocks-test))))

(defmethod operation-done-p ((o asdf:test-op) (c (eql (find-system :weblocks))))
  nil)

;;;; test operation (same functionality as asdf:test-op, but defined for consistency)
(defclass wop::test-op (operation)
  ()
  (:documentation "Allows to specialize built-in ASDF methods to run
   the Weblocks test suite."))

(defmethod perform ((o wop::test-op) (c (eql (find-system :weblocks))))
  "A method specializer to run the weblocks test suite through ASDF."
  (funcall (intern (symbol-name :test-weblocks) (find-package :weblocks-test))))

(defmethod operation-done-p ((o wop::test-op) (c (eql (find-system :weblocks))))
  nil)

;;;; doc-op operation
(defclass doc-op (operation)
  ()
  (:documentation "Allows to specialize built-in ASDF methods to run
   the Weblocks documentation generation."))

(defmethod perform ((o doc-op) (c component))
  "Runs the documentation generating function."
  nil)

(defmethod perform ((o doc-op) (c (eql (find-system :weblocks))))
  "Runs the documentation generating function."
  (funcall (intern (symbol-name :document-weblocks) (find-package :weblocks-scripts))))

(defmethod operation-done-p ((o doc-op) (c (eql (find-system :weblocks))))
  nil)

;;;; helper functions that hide away the unnecessary arguments to
;;;; (asdf:operate)
(defun test ()
  "Runs the Weblocks test suite together with loading the necessary packages."
  (asdf:operate 'test-op :weblocks))

(defun doc ()
  "Generates Weblocks documentation together with loading the necessary packages."
  (asdf:operate 'doc-op :weblocks))

(defun make-app (name &optional target)
   "Creates a new Weblocks app named <name> into directory <target> 
    based on the new-app-template."
   (or (find-package :weblocks-scripts) (asdf:load-system :weblocks-scripts))
   (uiop:symbol-call :weblocks-scripts :make-application name target))


