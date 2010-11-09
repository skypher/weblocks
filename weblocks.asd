;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(defpackage #:weblocks-asd
  (:use :cl :asdf)
  (:nicknames :wop)
  (:export #:test #:test-op #:doc #:doc-op #:make-app #:make-app-op))

(in-package :weblocks-asd)

(defsystem weblocks
  :name "weblocks"
  :version "0.8.3"
  :maintainer "Slava Akhmechet"
  :author "Slava Akhmechet"
  :licence "LLGPL"
  :description "A Common Lisp web framework."
  :depends-on (:closer-mop
               :hunchentoot
               :puri
               :cl-json
               :cl-who
               :parenscript
               :cl-fad
               :fare-matcher
               :cl-cont
               :metatilities
               :cl-ppcre
               :md5
               :anaphora
               :f-underscore
               :bordeaux-threads
               :salza2
               :html-template
               :trivial-timeout
               :trivial-backtrace)
  :components ((:module src
		:components (
		 (:file "package")
		 (:file "weblocks" :depends-on ("package"))
		 (:module utils
			  :components ((:file "misc")
                                       (:file "clos")
                                       (:file "runtime-class")
                                       (:file "string")
                                       (:file "list")
                                       (:file "uri")
                                       (:file "html")
                                       (:file "javascript")
                                       (:file "isearch"
                                              :depends-on ("html"))
                                       (:file "menu"
                                              :depends-on ("html"))
                                       (:file "suggest")
                                       (:file "timing")
                                       (:file "repl"))
			  :depends-on ("weblocks"))
		 (:file "versioning"
			:depends-on ("weblocks" utils))
		 (:file "bundling"
			:depends-on ("weblocks" utils))
		 (:file "dependencies"
			:depends-on ("weblocks" "server" "bundling" "versioning" utils))
		 (:file "dom-object"
			:depends-on ("weblocks" utils))
		 (:file "page-template"
			:depends-on ("weblocks" utils "application"))
		 (:file "actions"
			:depends-on ("weblocks" utils))
		 (:file "log-actions"
			:depends-on ("weblocks"))
		 (:file "debug-mode"
			:depends-on ("weblocks" "actions"))
		 (:file "uri-tokens"
			:depends-on ("weblocks"))
		 (:file "request-hooks"
			:depends-on ("weblocks"))
                 (:file "error-handler"
                        :depends-on ("weblocks" "application"))
		 (:file "request-handler"
			:depends-on (utils "weblocks" "page-template" "debug-mode"
					   "actions" "request-hooks" "application"
					   "request" "dependencies" "uri-tokens"
                                           "error-handler" store))
		 (:module linguistic
			  :components ((:file "grammar"))
			  :depends-on ("weblocks" utils))
		 (:module views
			  :components ((:module view
						:components ((:file "view")
							     (:file "utils"
								    :depends-on ("view"))
							     (:file "compiler"
								    :depends-on ("view"))
							     (:file "scaffold"
								    :depends-on ("view" "utils"))
							     (:file "presentation"
								    :depends-on ("view" "compiler"))))
				       (:file "dataview"
					      :depends-on (view))
				       (:module formview
						:components ((:file "formview")
							     (:file "helpers")
							     (:file "parser"
								    :depends-on ("formview"))
							     (:file "scaffold"
								    :depends-on ("formview" "parser"))
							     (:file "validation"
								    :depends-on ("formview"))
							     (:file "request-deserialization"
								    :depends-on ("formview" "parser"
											    "validation")))
						:depends-on (view))
				       (:file "sequence-view"
					      :depends-on (view))
				       (:file "tableview"
					      :depends-on (view dataview "sequence-view"))
				       (:module
					types
					:components ((:file "file-upload")
                                                     (:file "us-states")
						     (:file "boolean")
						     (:file "member"
							    :depends-on (presentations parsers))
						     (:file "password")
						     (:module
						      presentations
						      :components ((:file "hidden")
                                                                   (:file "choices")
								   (:file "date")
								   (:file "radio"
									  :depends-on ("choices"))
								   (:file "dropdown"
									  :depends-on ("choices"))
								   (:file "textarea")
								   (:file "paragraph")
								   (:file "excerpt")
								   (:file "image")
								   (:file "checkboxes")
								   (:file "url")
								   (:file "html")))
						     (:module
						      parsers
						      :components ((:file "common"))))
					:depends-on (view formview dataview)))
			  :depends-on ("weblocks" "dependencies" utils))
		 (:module store
			  :components ((:file "store-api")
				       (:file "store-utils"))
			  :depends-on (weblocks utils))
		 (:module widgets
			  :components ((:module widget
						:components ((:file "widget-mop")
							     (:file "uri-parameters-mixin")
							     (:file "widget"
								    :depends-on ("widget-mop"
										 "uri-parameters-mixin"))
                                                             (:file "string-widget"
                                                                    :depends-on ("widget"))
                                                             (:file "funcall-widget"
                                                                    :depends-on ("widget"))))
                                       (:file "composite"
					      :depends-on (widget))
				       (:file "flash"
					      :depends-on (widget))
				       (:file "data-editor"
					      :depends-on (widget))
				       (:file "dataform"
					      :depends-on (widget "data-editor"))
				       (:file "quickform"
					      :depends-on (widget "dataform"))
				       (:file "simpleform"
					      :depends-on (widget "quickform"))
				       (:file "wizard"
					      :depends-on (widget "dataform"))
                                       (:file "template-block"
                                              :depends-on (widget))
				       (:file "login"
					      :depends-on (widget "quickform"))
				       (:module dataseq
						:components ((:file "dataseq")
							     #-cmu (:file "operations-action"))
						:depends-on (widget "flash"))
				       (:module datagrid
						:components ((:file "datagrid")
							     (:file "sort"
								    :depends-on ("datagrid"))
							     (:file "select"
								    :depends-on ("datagrid"))
							     (:file "drilldown"
								    :depends-on ("datagrid")))
						:depends-on (widget "dataseq"))
				       (:module dataedit
						:components ((:file "dataedit"
								    :depends-on (#-cmu "delete-action"))
							     #-cmu (:file "delete-action"))
						:depends-on (widget dataseq))
				       (:file "gridedit"
					      :depends-on (datagrid dataedit "dataform"))
				       (:file "listedit"
					      :depends-on (datalist dataedit "dataform" "quickform"))
				       (:file "datalist"
					      :depends-on (widget "dataseq"))
				       (:file "pagination"
					      :depends-on (widget "flash"))
                                       #+(or)(:file "table-composite"
                                                    :depends-on (composite))
				       (:file "selector"
					      :depends-on (widget))
				       (:file "on-demand-selector"
					      :depends-on ("selector"))
				       (:file "navigation"
					      :depends-on ("selector" widget))
				       (:file "breadcrumbs"
					      :depends-on ("navigation")))
			  :depends-on (views utils "dependencies" "actions" "server" "request"
						"request-hooks" "dom-object" linguistic store))
		 (:module control-flow
			  :components ((:file "call-answer")
				       (:file "dialog"
					      :depends-on ("call-answer"))
				       (:file "workflow"
					      :depends-on ("call-answer")))
			  :depends-on ("weblocks" "widgets" "request-handler"))
                 (:file "acceptor"
                        :depends-on ("weblocks"))
		 (:file "server"
			:depends-on ("weblocks" "acceptor" "debug-mode" utils store))
		 (:file "request"
			:depends-on ("weblocks" "request-hooks" "actions"))
		 (:file "application-mop"
			:depends-on ("weblocks" "server"))
		 (:file "application"
			:depends-on ("weblocks" "application-mop" store))
		 (:file "default-application"
			:depends-on ("server" "weblocks" utils "request-handler")))))
  :in-order-to ((asdf:test-op (load-op "weblocks-test"))
		(test-op (load-op "weblocks-test"))
		(doc-op (load-op "weblocks-scripts"))
		(make-app-op (load-op "weblocks-scripts"))))

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

;;;; make-app-op operation
(defclass make-app-op (operation)
  ()
  (:documentation "Allows to specialize built-in ASDF methods to create
   a new Weblocks app."))

(defmethod perform ((o make-app-op) (c component))
  "Creates a new Weblocks application"
  nil)

(defmethod perform ((o make-app-op) (c (eql (find-system :weblocks))))
  "Creates a new Weblocks application when (wop:make-app 'name \"/path/to/target/\")
   is called."
  (let ((app-name (cadr (member :name (asdf::operation-original-initargs o))))
	(app-target (cadr (member :target (asdf::operation-original-initargs o)))))
    (funcall (intern (symbol-name :make-application) (find-package :weblocks-scripts))
	     app-name app-target)))

(defmethod operation-done-p ((o make-app-op) (c (eql (find-system :weblocks))))
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
  (asdf:operate 'make-app-op :weblocks :name name :target target))

