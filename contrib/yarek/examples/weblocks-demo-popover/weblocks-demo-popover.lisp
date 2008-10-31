
(defpackage #:weblocks-demo-popover
  (:use :cl :weblocks :metatilities :weblocks-yarek)
  (:documentation
    "A web application based on Weblocks."))

(in-package :weblocks-demo-popover)

(export '(start-weblocks-demo-popover stop-weblocks-demo-popover))

(defun start-weblocks-demo-popover (&rest args)
  "Starts the application by calling 'start-weblocks' with appropriate
  arguments."
  (apply #'start-weblocks args))

(defun stop-weblocks-demo-popover ()
  "Stops the application by calling 'stop-weblocks'." 
  (stop-weblocks))

;;; A sandbox store macro
(defmacro sandbox-store ()
  "Access to a sandbox store in the session."
  `(hunchentoot:session-value 'sandbox-store))

