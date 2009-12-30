
(in-package :weblocks-demo)

(export '(start-weblocks-demo stop-weblocks-demo))

(defun start-weblocks-demo (&rest args)
  "Starts the application by calling 'start-weblocks' with appropriate
  arguments."
  (apply #'start-weblocks args))

(defun stop-weblocks-demo ()
  "Stops the application by calling 'stop-weblocks'."
  (stop-webapp 'weblocks-demo)
  (stop-weblocks))

;;; A sandbox store macro
(defmacro sandbox-store ()
  "Access to a sandbox store in the session."
  `(hunchentoot:session-value 'sandbox-store))

;; Define our application
(defwebapp weblocks-demo :prefix ""
           :autostart t
           :public-files-cache-time 100000
	   :description "A web application based on Weblocks"
	   :dependencies '((:stylesheet "suggest")))

