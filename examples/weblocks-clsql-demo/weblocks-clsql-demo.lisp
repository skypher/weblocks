
(defpackage #:weblocks-clsql-demo
  (:use :cl :weblocks :metatilities)
  (:documentation
   "A web application based on Weblocks."))

(in-package :weblocks-clsql-demo)

(export '(start-weblocks-clsql-demo stop-weblocks-clsql-demo))

(defun start-weblocks-clsql-demo (&rest args)
  "Starts the application by calling 'start-weblocks' with appropriate
arguments."
  (apply #'start-weblocks args))

(defun stop-weblocks-clsql-demo ()
  "Stops the application by calling 'stop-weblocks'."
  (stop-weblocks))

