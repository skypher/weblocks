(defpackage #:weblocks.routes
  (:use #:cl)
  (:export
   #:route
   #:get-dependency
   #:make-route))
(in-package weblocks.routes)


(defclass route (routes:route)
  ((dependency :initform nil
               :initarg :dependency
               :reader get-dependency)))


(defun make-route (uri dependency)
  "Makes a route for dependency.

Automatically adds a prefix depending on current webapp and widget."

  (make-instance 'route
                 :template (routes:parse-template uri)
                 :dependency dependency))
