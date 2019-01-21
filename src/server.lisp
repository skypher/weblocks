(defpackage #:weblocks/server
  (:use #:cl
        #:f-underscore)
  ;; to load js dependencies after app was started
  (:import-from #:weblocks/app-dependencies)
  ;; we need to depend on this package, because
  ;; lack:builder will try to find `LACK.MIDDLEWARE.SESSION`
  ;; package
  (:import-from #:lack.middleware.session)
  ;; We need this import because this module defines important method
  ;; make-js-backend
  (:import-from #:weblocks/js/jquery)
 
  (:import-from #:weblocks/session
                #:*session*)
  (:import-from #:weblocks/hooks
                #:prepare-hooks)
  (:import-from #:weblocks/routes
                #:route
                #:get-route
                #:add-route
                #:reset-routes)
  (:import-from #:weblocks/app
                #:app-active-p
                #:get-active-apps
                #:get-prefix
                #:app-serves-hostname-p
                #:weblocks-webapp-name
                #:get-autostarting-apps)
  (:import-from #:weblocks/request
                #:with-request)
  (:import-from #:weblocks/response
                #:get-code)
  (:import-from #:weblocks/request-handler
                #:handle-request)
    
  (:import-from #:lack.request
                #:make-request)
  (:import-from #:lack
                #:builder)
  (:import-from #:clack
                #:clackup)
  (:import-from #:cl-strings
                #:starts-with)
  ;; Just dependencies
  (:import-from #:weblocks/debug)
  (:import-from #:log)
  
  (:export ;; #:get-server-type
           ;; #:get-port
           ;; #:make-server
           ;; #:handle-http-request
           #:stop
           #:start
           #:serve-static-file))
(in-package weblocks/server)


(defvar *server* nil
  "If the server is started, bound to a server
  object. Otherwise, nil.")


(defclass server ()
  ((port :type integer
         :initarg :port
         :reader get-port)
   (interface :type string
              :initarg :interface
              :reader get-interface)
   (server-type :initarg :server-type
                :reader get-server-type)
   (handler :initform nil
            :accessor get-handler)))


(defgeneric handle-http-request (server env)
  (:documentation "Handles HTTP request, passed by Clack"))


(defun make-server (&key
                      (port 8080)
                      (interface "localhost")
                      (server-type :hunchentoot))
  "Makes a webserver instance.
Make instance, then start it with ``start`` method."
  (make-instance 'server
                 :port port
                 :interface interface
                 :server-type server-type))


(defun search-app-for-request-handling (path-info hostname)
  (dolist (app (get-active-apps))
    (let ((app-prefix (get-prefix app)))
      (log:debug "Searching handler in" app app-prefix)
      
      (when (and (app-serves-hostname-p app hostname)
                 (starts-with path-info
                              app-prefix))
        (return-from search-app-for-request-handling
          app)))))


(defmethod handle-http-request ((server server) env)
  "Weblocks HTTP dispatcher.
This function serves all started applications and their static files."

  (let* ((*session* (getf env :lack.session))
         ;; This "hack" is needed to allow widgets to change *random-state*
         ;; and don't interfere with other threads and requests
         (*random-state* *random-state*))
    (with-request ((make-request env))
      ;; Dynamic hook :handle-http-request makes possible to write
      ;; some sort of middlewares, which change *request* and *session*
      ;; variables.
      (prepare-hooks
        (weblocks/hooks:with-handle-http-request-hook ()

          (let* ((path-info (getf env :path-info))
                 (hostname (getf env :server-name))
                 (route (get-route path-info))
                 (app (search-app-for-request-handling path-info hostname)))

            (log:debug "Processing request to" path-info)

            ;; If dependency found, then return it's content along with content-type
            (cond
              (route
               (log:debug "Route was found" route)
               (weblocks/routes:serve route env))
              (app
               (log:debug "App was found" route)
               (let* ((response (handle-request app)))
                 (list (get-code response)
                       (get-headers response)
                       ;; Here we use catch to allow to abort usual response
                       ;; processing and to return data immediately
                       (list (get-content response)))))
              (t
               (log:error "Application dispatch failed for" path-info)

               (list 404
                     (list :content-type "text/html")
                     (list (format nil "File \"~A\" was not found.~%"
                                   path-info)))))))))))


(defun start-server (server &key debug)
  "Starts a Clack webserver, returns this server as result.

If server is already started, then logs a warning and does nothing."
  
  (if (get-handler server)
      (log:warn "Webserver already started")
      
      ;; Otherwise, starting a server
      (let* ((port (get-port server))
             (interface (get-interface server))
             (app (builder
                   :session
                   (lambda (env)
                     (handle-http-request server env)
                     ;; (handler-case ()
                     ;;   (t (condition)
                     ;;     (let* ((traceback (with-output-to-string (stream)
                     ;;                         (trivial-backtrace:print-condition condition stream)))
                     ;;            (condition (describe condition))
                     ;;            (just-traceback (trivial-backtrace:backtrace-string)))
                     ;;       (log:error "Unhandled exception" condition traceback just-traceback))
                     ;;     '(500
                     ;;       ("Content-Type" "text/html")
                     ;;       ("Something went wrong!"))))
                     ))))
        (log:info "Starting webserver on" interface port debug)
        
        ;; Suppressing output to stdout, because Clack writes message
        ;; about started server and we want to write into a log instead.
        (with-output-to-string (*standard-output*)
          (setf (get-handler server)
                (clackup app
                         :address interface
                         :server (get-server-type server)
                         :port port
                         :debug debug)))))
  server)


(defun stop-server (server)
  "Stops a Clack server, but does not deactivates active applications,
   use `stop' function for that."

  (if (get-handler server)
      (progn (log:info "Stopping server" server)
             (clack:stop (get-handler server))
             (setf (get-handler server)
                   nil))
      (log:warn "Server wasn't started"))

  server)


(defmethod print-object ((server server) stream)
  (format stream "#<SERVER port=~S ~A>"
          (get-port server)
          (if (get-handler server)
              "running"
              "stopped")))


(defun start (&key (debug t)
                (port 8080)
                (interface "localhost")
                (server-type :hunchentoot))
  "Starts weblocks framework hooked into Clack server.

Set DEBUG to true in order for error messages and stack traces to be shown
to the client (note: stack traces are temporarily not available due to changes
in Hunchentoot 1.0.0).

All other keywords will be passed as initargs to the acceptor;
the initargs :PORT and :SESSION-COOKIE-NAME default to
8080 and `weblocks-GENSYM'.

Also opens all stores declared via DEFSTORE and starts webapps
declared AUTOSTART."

  (weblocks/hooks:with-start-weblocks-hook ()
    (when *server*
      (restart-case
          (error "Server already running on port ~A"
                 (get-port *server*))
        (continue ()
          :report "Stop the old server and start a new one."
          (stop))))



    (log:info "Starting weblocks" port server-type debug)

    (reset-routes)
  
    (unless (member :bordeaux-threads *features*)
      (cerror "I know what I'm doing and will stubbornly continue."
              "You're trying to start Weblocks without threading ~
            support. Recompile your Lisp with threads enabled."))
    (if debug
        (weblocks/debug:on)
        (weblocks/debug:off))

    (setf *server*
          (make-server :port port
                       :interface interface
                       :server-type server-type))
    (values
     (start-server *server*
                   :debug debug)
     (mapcar (lambda (class)
               (unless (app-active-p class)
                 (weblocks/app:start class :debug debug)))
             (get-autostarting-apps)))))


(defun stop ()
  "Stops weblocks, by deactivating all active applications and stopping Clack server"

  (when *server*
    (weblocks/hooks:with-stop-weblocks-hook ()
      (dolist (app (get-active-apps))
        (weblocks/app:stop (weblocks-webapp-name app)))

      (when *server*
        (stop-server *server*))
      (setf *server* nil))))


;;;; Static files

(defclass static-route-from-file (route)
  ((path :initarg :path
         :reader get-path)
   (content-type :initarg :content-type
                 :reader get-content-type)))


(defmethod weblocks/routes:serve ((route static-route-from-file) env)
  "Returns a file's content"
  (declare (ignorable env))
  (list 200
        (list :content-type (get-content-type route))
        (get-path route)))


(defgeneric serve-static-file (uri object &key content-type)
  (:documentation "Adds a route to serve given object by static URI."))


(defmethod serve-static-file (uri (path pathname) &key (content-type "text/plain"))
  (let* ((route (make-instance 'static-route-from-file
                               :template (routes:parse-template uri)
                               :path path
                               :content-type content-type)))
    (add-route route)))
