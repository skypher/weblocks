
(defpackage #:{APPNAME}
  (:use :cl :weblocks)
  (:documentation
   "A web application based on Weblocks."))

(in-package :{APPNAME})

(export '(start-{APPNAME} stop-{APPNAME}))

(defun start-{APPNAME} (&rest args)
  "Starts the application by calling 'start-weblocks' with appropriate
arguments."
  (apply #'start-weblocks args))

(defun stop-{APPNAME} ()
  "Stops the application by calling 'stop-weblocks'."
  (stop-weblocks))

