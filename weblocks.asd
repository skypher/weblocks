;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(defpackage #:weblocks-asd
  (:use :cl :asdf)
  (:nicknames :wop)
  (:export #:test #:test-op #:doc #:doc-op #:make-app))

(in-package :weblocks-asd)

(defsystem weblocks
  :name "weblocks"
  :version (:read-file-form "version.lisp-expr")
  :maintainer "Olexiy Zamkoviy, Scott L. Burson"
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
   ;;   :weblocks-util

   ;; TODO: remove this dependency and reverse it
   ;;:weblocks-stores
   
   :closer-mop
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
   :babel 
   :split-sequence)
  :serial t
  :components ((:module src
                :components (
                             (:file "package")
                             (:file "variables")
                             
                             (:file "session")
                             (:file "weblocks"
                              ;; :depends-on ("session"
                              ;;              "package")
                              )
                             (:file "request2")
                             (:module utils
                              :components ((:file "clos")
                                           (:file "misc"
;;                                            :depends-on ;; (
                                                        ;;  ;; utils/misc stores unique ids in a session
                                                        ;;  "repl"
                                                        ;;  ;;  "session"
                                                        ;;  )
                                                  )
                                           (:file "runtime-class")
                                           (:file "string")
                                           (:file "list")
                                           (:file "uri")
                                           (:file "html-parts")
                                           (:file "templates")
                                           (:file "html")
                                           (:file "menu"
;;                                            :depends-on ;; ("html")
                                                  )
                                           (:file "suggest")
                                           (:file "timing")
                                           (:file "repl"))
;;                              :depends-on ;; ("session"
                                          ;;  "weblocks"
                                          ;;  )
                              )
                             (:file "versioning"
;;                              :depends-on ;; ("weblocks" "session" utils)
                              )
                             (:file "bundling"
;;                              :depends-on ;; ("weblocks" "session" utils)
                              )
                             (:file "dependencies"
;;                              :depends-on ;; ("weblocks" "server" "bundling" "versioning" "session" utils)
                                    )
                             (:file "routes")
                             (:file "dependencies2"
;;                              :depends-on ;; ("weblocks"
                                          ;;  "bundling"
                                          ;;  "versioning"
                                          ;;  "session"
                                          ;;  utils
                                          ;;  "routes")
                              )
                             (:file "dom-object"
;;                              :depends-on ;; ("weblocks" "session" utils)
                              )
                             (:file "application-mop"
                              ;;:depends-on ;; ("weblocks" "server")
                                    )
                             (:module "js-backend"
                              :components ((:file "base")
                                           (:file "jquery")))
                             (:file "application"
                              ;;:depends-on ;; ("weblocks"
                                          ;;  "application-mop")
                              )
                             (:file "page-template"
;;                              :depends-on ;; ("weblocks" "session" utils "application")
                              )
                             (:file "actions"
;;                              :depends-on ;; ("weblocks"
                                          ;;  "session"
                                          ;;  utils)
                              )
                             (:file "log-actions"
;;                              :depends-on ;; ("weblocks")
                              )
                             (:file "debug-mode"
;;                              :depends-on ;; ("weblocks" "actions")
                              )
                             (:file "uri-tokens"
;;                              :depends-on ;; ("weblocks")
                              )
                             (:file "hooks"
;;                              :depends-on ;; ("weblocks" "debug-mode")
                                    )
;;                             (:file "response2")
                             (:file "response")
                             (:file "error-handler"
;;                              :depends-on ;; ("weblocks"
                                          ;;  "application"
                                          ;;  "request2")
                                    )
                             (:file "request-handler"
;;                              :depends-on ;; ("session"
                                          ;;  utils "weblocks" "page-template" "debug-mode"
                                          ;;  "actions" "request-hooks" "application"
                                          ;;  "request" "dependencies" "uri-tokens"
                                          ;;  "error-handler")
                              )
                             (:file "session-lock"
;;                              :depends-on ;; ("session")
                              )
                             (:file "request-handler2"
;;                              :depends-on ;; ("session"
                                          ;;  utils
                                          ;;  "weblocks"
                                          ;;  "page-template"
                                          ;;  "debug-mode"
                                          ;;  "actions"
                                          ;;  "request-hooks"
                                          ;;  "application"
                                          ;;  "request"
                                          ;;  "dependencies"
                                          ;;  "uri-tokens"
                                          ;;  "error-handler"
                                          ;;  "routes"
                                          ;;  "request2"
                                          ;;  "variables"
                                          ;;  "session-lock")
                              )
                             (:module linguistic
                              :components ((:file "grammar"))
;;                              :depends-on ;; ("weblocks" "session" utils)
                              )
                             ;; (:module views
;;                               :components ((:module view
;;                                             :components ((:file "view")
;;                                                          (:file "utils"
;; ;;                                                          :depends-on ;; ("view")
;;                                                                 )
;;                                                          (:file "compiler"
;; ;;                                                          :depends-on ;; ("view")
;;                                                                 )
;;                                                          (:file "scaffold"
;; ;;                                                          :depends-on ;; ("view" "utils")
;;                                                                 )
;;                                                          (:file "presentation"
;; ;;                                                          :depends-on ;; ("view" "compiler")
;;                                                                 )))
;;                                            (:file "dataview"
;; ;;                                            :depends-on ;; (view)
;;                                                   )
;;                                            (:module formview
;;                                             :components ((:file "formview")
;;                                                          (:file "helpers")
;;                                                          (:file "parser"
;; ;;                                                          :depends-on ;; ("formview")
;;                                                                 )
;;                                                          (:file "scaffold"
;; ;;                                                          :depends-on ;; ("formview" "parser")
;;                                                                 )
;;                                                          (:file "validation"
;; ;;                                                          :depends-on ;; ("formview")
;;                                                                 )
;;                                                          (:file "request-deserialization"
;; ;;                                                          :depends-on ;; ("formview" "parser"
;;                                                                       ;;             "validation")
;;                                                                 )
;;                                                          (:file "template-form-view"
;;                                                           ;;:depends-on ;; ("formview")
;;                                                                 ))
;; ;;                                            :depends-on ;; (view)
;;                                                     )
;;                                            (:file "sequence-view"
;; ;;                                            :depends-on ;; (view)
;;                                                   )
;;                                            (:file "tableview"
;; ;;                                            :depends-on ;; (view dataview "sequence-view")
;;                                                   )
;;                                            (:module
;;                                             types
;;                                             :components ((:file "file-upload")
;;                                                          (:file "us-states")
;;                                                          (:file "boolean")
;;                                                          (:file "member"
;; ;;                                                          :depends-on ;; (presentations parsers)
;;                                                                 )
;;                                                          (:file "password")
;;                                                          (:module
;;                                                           presentations
;;                                                           :components ((:file "hidden")
;;                                                                        (:file "choices")
;;                                                                        (:file "date")
;;                                                                        (:file "radio"
;; ;;                                                                        :depends-on ;; ("choices")
;;                                                                               )
;;                                                                        (:file "dropdown"
;; ;;                                                                        :depends-on ;; ("choices")
;;                                                                               )
;;                                                                        (:file "textarea")
;;                                                                        (:file "paragraph")
;;                                                                        (:file "excerpt")
;;                                                                        (:file "image")
;;                                                                        (:file "checkboxes")
;;                                                                        (:file "url")
;;                                                                        (:file "html")
;;                                                                        (:file "widget")))
;;                                                          (:module
;;                                                           parsers
;;                                                           :components ((:file "common"))))
;; ;;                                            :depends-on ;; (view formview dataview)
;;                                             ))
;; ;;                              :depends-on ;; ("weblocks"
;;                                           ;;  "dependencies"
;;                                           ;;  "dependencies2"
;;                                           ;;  "session"
;;                                           ;;  utils
;;                                           ;;  "widget-translation")
;;                               )
                             (:module widgets
                              :components ((:module widget
                                            :components ((:file "widget-mop")
                                                         (:file "uri-parameters-mixin")
                                                         (:file "widget"
;;                                                          :depends-on ;; ("widget-mop"
                                                                      ;;  "uri-parameters-mixin")
                                                                )
                                                         (:file "string-widget"
;;                                                          :depends-on ;; ("widget")
                                                                )
                                                         (:file "funcall-widget"
;;                                                          :depends-on ;; ("widget")
                                                                )))
                                           (:file "composite"
;;                                            :depends-on ;; (widget)
                                                  )
                                           (:file "flash"
;;                                            :depends-on ;; (widget)
                                                  )
                                           (:file "data-editor"
;;                                            :depends-on ;; (widget)
                                                  )
                                           (:file "dataform"
                                            ;;depends-on ;; (widget "data-editor")
                                                  )
                                           (:file "quickform"
                                            ;;:depends-on ;; (widget "dataform")
                                                  )
                                           ;; depends on form view
                                           ;; (:file "simpleform"
                                           ;;  ;;:depends-on ;; (widget "quickform")
                                           ;;        )
                                           ;; depends on form view
                                           ;; (:file "wizard"
                                           ;;  ;;:depends-on ;; (widget "dataform")
                                           ;;        )
                                           (:file "template-block"
                                            ;;:depends-on ;; (widget)
                                                  )
                                           ;; defined in 
                                           ;; (:file "login"
                                           ;;  ;;:depends-on ;; (widget "quickform")
                                           ;;        )
                                           (:module dataseq
                                            :components ((:file "dataseq")
                                                         #-cmu (:file "operations-action"))
                                            ;;:depends-on ;; (widget "flash")
                                                    )
                                           ;; depends on WEBLOCKS::TABLE-VIEW-FIELD
                                           ;; (:module datagrid
                                           ;;  :components ((:file "datagrid")
                                           ;;               (:file "sort"
                                           ;;                ;;:depends-on ;; ("datagrid")
                                           ;;                      )
                                           ;;               (:file "select"
                                           ;;                ;;:depends-on ;; ("datagrid")
                                           ;;                      )
                                           ;;               (:file "drilldown"
                                           ;;                ;;:depends-on ;; ("datagrid")
                                           ;;                      ))
                                           ;;  ;;:depends-on ;; (widget "dataseq")
                                           ;;          )
                                           (:module dataedit
                                            :components ((:file "dataedit"
                                                          ;;:depends-on ;; (#-cmu "delete-action")
                                                                )
                                                         #-cmu (:file "delete-action"))
                                            ;;:depends-on ;; (widget dataseq)
                                                    )
                                           (:file "gridedit"
                                            ;;:depends-on ;; (datagrid dataedit "dataform")
                                                  )
                                           (:file "listedit"
                                            ;;:depends-on ;; (datalist dataedit "dataform" "quickform")
                                                  )
                                           (:file "datalist"
                                            ;;:depends-on ;; (widget "dataseq")
                                                  )
                                           (:file "pagination"
                                            ;;:depends-on ;; (widget "flash")
                                                  )
                                           (:file "selector"
                                            ;;:depends-on ;; (widget)
                                                  )
                                           (:file "on-demand-selector"
                                            ;;:depends-on ;; ("selector")
                                                  )
                                           (:file "navigation"
                                            ;;:depends-on ;; ("selector" widget)
                                                  )
                                           (:file "breadcrumbs"
                                            ;;:depends-on ;; ("navigation")
                                                  ))
                              ;;:depends-on ;; (views "session" utils "dependencies" "actions" "server" "request"
                                          ;;        "request-hooks" "dom-object" linguistic "widget-translation")
                              )
                             (:module control-flow
                              :components ((:file "call-answer")
                                           (:file "dialog"
                                            ;;:depends-on ;; ("call-answer")
                                                  )
                                           (:file "workflow"
                                            ;;:depends-on ;; ("call-answer")
                                                  ))
                              ;;:depends-on ;; ("weblocks" "widgets" "request-handler")
                              )
                             ;; Removed because it was used specially for Hunchentoot
                             ;; and dont needed anymore after migration to Woo.
                             ;; (:file "acceptor"
                             ;;  ;;:depends-on ;; ("weblocks")
                             ;;  )
                             (:file "server"
                              ;;:depends-on ;; ("weblocks" "acceptor" "debug-mode" "session" utils)
                              )
                             (:file "server2"
                              ;;:depends-on ;; ("weblocks"
                                          ;;  "acceptor"
                                          ;;  "session"
                                          ;;  utils
                                          ;;  "routes"
                                          ;;  "request-handler2"
                                          ;;  "dependencies2")
                              )
                             (:file "request"
                              ;;:depends-on ;; ("weblocks" "request-hooks" "actions")
                              )
                             (:file "default-application"
                              ;;:depends-on ;; ("server"
                                          ;;  "weblocks"
                                          ;;  "session"
                                          ;;  utils
                                          ;;  "request-handler"
                                          ;;  "request-handler2")
                              )
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


