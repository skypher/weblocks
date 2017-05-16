
(defpackage #:{APPNAME}
  (:use :cl :weblocks
        :f-underscore :anaphora)
  (:import-from :hunchentoot #:header-in
                #:set-cookie #:set-cookie* #:cookie-in
                #:user-agent #:referer)
  (:documentation
   "A web application based on Weblocks."))

(in-package :{APPNAME})

(export '(start-{APPNAME} stop-{APPNAME}))

;; A macro that generates a class or this webapp

(defwebapp {APPNAME}
    :prefix "/"
    :description "{APPNAME}: A new application"
    :init-user-session '{APPNAME}::init-user-session
    :autostart nil                   ;; have to start the app manually
    :ignore-default-dependencies nil ;; accept the defaults
    :js-backend :jquery
    :debug t
    )

;; Top level start & stop scripts

(defun start-{APPNAME} (&rest args)
  "Starts the application by calling 'start-weblocks' with appropriate
arguments."
  (apply #'start-weblocks args)
  (start-webapp '{APPNAME}))

(defun stop-{APPNAME} ()
  "Stops the application by calling 'stop-weblocks'."
  (stop-webapp '{APPNAME})
  (stop-weblocks))

