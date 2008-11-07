
(defpackage #:employer-employee
  (:use :cl :weblocks :metatilities :weblocks-yarek)
  (:documentation
    "A web application based on Weblocks."))

(in-package :employer-employee)

(export '(start-employer-employee stop-employer-employee))

(defun start-employer-employee (&rest args)
  "Starts the application by calling 'start-weblocks' with appropriate
  arguments."
  (apply #'start-weblocks args))

(defun stop-employer-employee ()
  "Stops the application by calling 'stop-weblocks'." 
  (stop-weblocks))

;;; A sandbox store macro
(defmacro sandbox-store ()
  "Access to a sandbox store in the session."
  `(hunchentoot:session-value 'sandbox-store))

