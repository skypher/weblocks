;;; Code shared accross the entire weblocks framework
(defpackage #:weblocks
  (:use :cl :c2mop :metabang.utilities)
  (:export #:humanize-name #:object-slot-names #:render-data-header #:render-data-slot
	   #:render-data-footer #:render-data))

(in-package :weblocks)

;;; Output stream for our framework
;;; All HTML should be rendered to this stream
(defparameter *weblocks-output-stream* (make-string-output-stream)
  "Output stream for Weblocks framework. All html should be
  rendered to this stream.")
