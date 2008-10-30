
(defpackage #:{APPNAME}
  (:use :cl :weblocks
        :f-underscore)
  (:documentation
   "A web application based on Weblocks."))

(in-package :{APPNAME})

(export '(start-{APPNAME} stop-{APPNAME}))

;; A macro that generates a class or this webapp

(defwebapp {APPNAME}
    :prefix ""                       ;; default application prefix
    :description "{APPNAME}: A new application"
    :init-user-session '{APPNAME}::init-user-session
    :autostart nil                   ;; have to start the app manually
    :ignore-default-dependencies nil ;; accept the defaults
    )

;; Ensure that your public files directory is setup appropriately

(eval-when (:load-toplevel :execute)
  (set-weblocks-default-public-files-path 
   (compute-public-files-path :{APPNAME})))


;; Top level start & stop scripts

(defun start-{APPNAME} (&rest args)
  "Starts the application by calling 'start-weblocks' with appropriate
arguments."
  (apply #'start-weblocks args))

(defun stop-{APPNAME} ()
  "Stops the application by calling 'stop-weblocks'."
  (stop-weblocks))

