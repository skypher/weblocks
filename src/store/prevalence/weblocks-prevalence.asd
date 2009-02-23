
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
  :depends-on (:metatilities :cl-ppcre :cl-prevalence :bordeaux-threads)
  :components ((:file "prevalence"))
  :in-order-to ((compile-op (prepare-prevalence-op :weblocks-prevalence))
		(load-op (prepare-prevalence-op :weblocks-prevalence))))

;;; This is necessary for specifying weblocks-memory as a dynamic dependency
(defclass prepare-prevalence-op (operation)
  ()
  (:documentation "Used to run special code before prevalence system
  is loaded."))

(defmethod perform ((o prepare-prevalence-op) (c (eql (find-system :weblocks-prevalence))))
  (unless (find-package :weblocks-memory)
    ; load weblocks if necessary
    (unless (find-package :weblocks)
      (asdf:oos 'asdf:load-op :weblocks))
    ; load weblocks-memory.asd
    (load (merge-pathnames
	   (make-pathname :directory '(:relative "src" "store" "memory")
			  :name "weblocks-memory" :type "asd")
	   (funcall (symbol-function (find-symbol (symbol-name '#:asdf-system-directory)
						  (find-package :weblocks)))
		    :weblocks)))
    ; load weblocks-memory
    (asdf:oos 'asdf:load-op :weblocks-memory)))

(defmethod perform ((o prepare-prevalence-op) c)
  nil)

