
(defpackage #:blog
  (:use :cl :weblocks)
  (:documentation
   "A web application based on Weblocks."))

(in-package :blog)

(export '(start-blog stop-blog))

(defun start-blog (&rest args)
  "Starts the application by calling 'start-weblocks' with appropriate
arguments."
  (apply #'start-weblocks args))

(defun stop-blog ()
  "Stops the application by calling 'stop-weblocks'."
  (stop-weblocks))

