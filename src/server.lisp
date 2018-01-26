(defpackage #:weblocks/server
  (:use #:cl
        #:f-underscore)
  ;; to load js dependencies after app was started
  (:import-from #:weblocks/app-dependencies)
  
  (:import-from #:weblocks/session
                #:*session*)
  (:import-from #:weblocks/hooks
                #:prepare-hooks
                #:with-hook)
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
                #:start-webapp
                #:stop-webapp
                #:weblocks-webapp-name
                #:get-autostarting-apps)
  (:import-from #:weblocks/request
                #:ajax-request-p
                #:with-request)
  (:import-from #:weblocks/response
                #:*code*
                #:*content-type*
                #:*headers*
                #:catch-possible-abort)
  (:import-from #:weblocks/request-handler
                #:handle-client-request)
    
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
  
  (:export #:start
           #:stop
           #:get-server-type
           #:get-port
           #:make-server
           #:handle-request
           #:*server*
           #:stop-weblocks
           #:start-weblocks
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


(defgeneric handle-request (server env)
  (:documentation "Handles HTTP request, passed by Clack"))


(defgeneric start (server &key debug)
  (:documentation "Starts a webserver, returns this server as result.
If server is already started, then logs a warning and does nothing."))


(defgeneric stop (server)
  (:documentation "Stops a webserver if it if running. If it's not - does nothing.
Returns a webserver's instance.")
  )


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


(defmethod handle-request ((server server) env)
  "Weblocks HTTP dispatcher.
This function serves all started applications and their static files."

  (let* ((*session* (getf env :lack.session))
         ;; This "hack" is needed to allow widgets to change *random-state*
         ;; and don't interfere with other threads and requests
         (*random-state* *random-state*))
    (with-request ((make-request env))
      ;; Dynamic hook :handle-request makes possible to write
      ;; some sort of middlewares, which change *request* and *session*
      ;; variables.
      (prepare-hooks
        (with-hook (:handle-request)

          (let* ((path-info (getf env :path-info))
                 (hostname (getf env :server-name))
                 (route (get-route path-info)))

            (log:debug "Processing request to" path-info)

            ;; If dependency found, then return it's content along with content-type
            (when route
              (log:debug "Route was found" route)
              (return-from handle-request
                (weblocks/routes:serve route env)))

            (dolist (app (get-active-apps))
              (let ((app-prefix (get-prefix app)))

                (log:debug "Searching handler in" app app-prefix)

                (when (and (app-serves-hostname-p app hostname)
                           (starts-with path-info
                                        app-prefix))

                  ;; TODO это внутри использует hunchentoot
                  ;;      но при запуске на Woo вызывает ошибку
                  ;;      The variable HUNCHENTOOT:*REPLY* is unbound.
                  ;; (weblocks::no-cache)    ; disable caching for dynamic pages

                  (log:debug "Staringdsadasd BOOO HIT" path-info)
                  (return-from handle-request
                    ;; TODO: replace veariable binding to some macro from weblocks/response
                    (let* ((*code* 200)
                           (*content-type*
                             (if (ajax-request-p)
                                 "application/json"
                                 "text/html"))
                           (*headers* nil)
                           ;; TODO: make a macro to catch aborting
                           (content (catch-possible-abort
                                      (handle-client-request app))))

                      (list *code* ;; this value can be changed somewhere in
                            ;; handle-client-request
                            (append (list :content-type *content-type*)
                                    *headers*)
                            ;; Here we use catch to allow to abort usual response
                            ;; processing and to return data immediately
                            (list content)))))))
                                  
            (log:error "Application dispatch failed for" path-info)

            (list 404
                  (append (list :content-type "text/html")
                          *headers*)
                  (list (format nil "File \"~A\" was not found.~%"
                                path-info)))))))))


(defmethod start ((server server) &key debug)
  (if (get-handler server)
      (log:warn "Webserver already started")
      
      ;; Otherwise, starting a server
      (let* ((port (get-port server))
             (interface (get-interface server))
             (app (builder
                   :session
                   (lambda (env)
                     (handle-request server env)
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


(defmethod stop ((server server))
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


(defun start-weblocks (&key (debug t)
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

  (log:info "Starting weblocks" port server-type debug)

  (reset-routes)
  
  (unless (member :bordeaux-threads *features*)
    (cerror "I know what I'm doing and will stubbornly continue."
            "You're trying to start Weblocks without threading ~
            support. Recompile your Lisp with threads enabled."))
  (if debug
      (weblocks/debug:on)
      (weblocks/debug:off))
  
  (when (null *server*)
    (values
     (start (setf *server*
                  (make-server :port port
                               :interface interface
                               :server-type server-type))
            :debug debug)
     
     (log:info "Starting webapps flagged as ``autostarted``")
     (mapcar (lambda (class)
               (unless (app-active-p class)
                 (start-webapp class :debug debug)))
             (get-autostarting-apps)))))


(defun stop-weblocks ()
  "Stops weblocks."

  ;; TODO: Investigate if it closes all stores declared via 'defstore'.
  
  (when (not (null *server*))
    (with-hook
        (:stop-weblocks)
        
        (dolist (app (get-active-apps))
          (stop-webapp (weblocks-webapp-name app)))

        ;; Was commented because *last-session* is unknown
        ;; (setf weblocks.session::*last-session* nil)

        ;; TODO: Replace with CLACK's sessions
        ;; (weblocks::reset-sessions)
        (when *server*
          (stop *server*))
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
