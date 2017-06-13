(defpackage #:weblocks.js
  (:use #:cl)
  (:export
   #:js-backend
   #:make-js-backend))
(in-package weblocks.js)


(defclass js-backend ()
  ())


(defgeneric make-js-backend (name)
  (:documentation "Creates a js backend instance of a given name.

Name should be a keyword like a :jquery or a :prototype."))
