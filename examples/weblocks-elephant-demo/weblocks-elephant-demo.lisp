
(defpackage #:weblocks-elephant-demo
  (:use :cl :weblocks :metatilities :weblocks-elephant)
  (:documentation
   "A web application based on Weblocks and Elephant."))

(in-package :weblocks-elephant-demo)

(export '(start-weblocks-elephant-demo stop-weblocks-elephant-demo))

(defun start-weblocks-elephant-demo (&rest args)
  "Starts the application by calling 'start-weblocks' with appropriate
arguments."
  (apply #'start-weblocks args))

(defun stop-weblocks-elephant-demo ()
  "Stops the application by calling 'stop-weblocks'."
  (stop-weblocks))

