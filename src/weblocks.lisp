;;; Code shared accross the entire weblocks framework
(defpackage #:weblocks
  (:use :cl :c2mop :metabang.utilities :hunchentoot :cl-who)
  (:export #:humanize-name #:attributize-name #:list->assoc
	   #:class-visible-slots #:object-visible-slots #:object-class-name
	   #:object-name #:render-slot-inline-p #:get-slot-value
	   #:*weblocks-output-stream* #:render-extra-tags
	   #:with-data-header #:render-data-slot #:render-data)
  (:documentation
   "Weblocks is a Common Lisp framework that eases the pain of
web application development. It achieves its goals by
standardizing on various libraries, providing flexible and
extensible generic renderers, and exposing a unique widget-based
approach to maintaining UI state.

You can use the following starting points to dive into the
documentation:

Generic function 'render-data'."))

(in-package :weblocks)

(defparameter *weblocks-output-stream* (make-string-output-stream)
  "Output stream for Weblocks framework. All html should be
  rendered to this stream.")

(defmacro with-html (&body body)
  "A wrapper around cl-who with-html-output macro."
  `(with-html-output (*weblocks-output-stream* nil :indent nil)
     ,@body))
