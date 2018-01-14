(defpackage #:weblocks.server
  (:use #:cl
        #:f-underscore)
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
(in-package weblocks.server)


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

  (let* ((weblocks.request:*request* (lack.request:make-request env))
         (weblocks.session::*session* (getf env :lack.session))
         ;; This "hack" is needed to allow widgets to change *random-state*
         ;; and don't interfere with other threads and requests
         (*random-state* *random-state*))

    ;; Dynamic hook :handle-request makes possible to write
    ;; some sort of middlewares, which change *request* and *session*
    ;; variables or make some sort
    (weblocks.hooks:prepare-hooks
      (weblocks.hooks:with-hook (:handle-request)
          (setf weblocks.request::*latest-request*
                weblocks.request:*request*)

          (let* ((path-info (getf env :path-info))
                 (hostname (getf env :server-name))
                 (route (weblocks.routes:get-route path-info)))

            ;; If dependency found, then return it's content along with content-type
            (when route
              (return-from handle-request
                (weblocks.routes:serve route env)))

            (dolist (app (weblocks.app:get-active-apps))
              (let ((app-prefix (weblocks.app:get-prefix app)))

                (log:debug "Searching handler in" app app-prefix)

                (cond
                  ((and (weblocks.app:app-serves-hostname-p app hostname)
                        (weblocks::list-starts-with (weblocks::tokenize-uri path-info nil)
                                                    (weblocks::tokenize-uri app-prefix nil)
                                                    :test #'string=))

                   ;; TODO это внутри использует hunchentoot
                   ;;      но при запуске на Woo вызывает ошибку
                   ;;      The variable HUNCHENTOOT:*REPLY* is unbound.
                   ;; (weblocks::no-cache)    ; disable caching for dynamic pages

                   (return-from handle-request
                     (let* ((weblocks.response:*code* 200)
                            (weblocks.response:*content-type*
                              (if (weblocks.request:ajax-request-p)
                                  "application/json"
                                  "text/html"))
                            (weblocks.response:*headers* nil)
                            (content (catch 'weblocks.response::abort-processing
                                       (weblocks.request-handler:handle-client-request app))))

                       (list weblocks.response:*code* ;; this value can be changed somewhere in
                             ;; handle-client-request
                             (append (list :content-type weblocks.response:*content-type*)
                                     weblocks.response:*headers*)
                             ;; Here we use catch to allow to abort usual response
                             ;; processing and to return data immediately
                             (list content))))))))
                                  
            (log:debug "Application dispatch failed for" path-info)

            (list 404
                  (list :content-type "text/html")
                  (list (format nil "File \"~A\" was not found"
                                path-info))))))))


(defmethod start ((server server) &key debug)
  (if (get-handler server)
      (log:warn "Webserver already started")
      
      ;; Otherwise, starting a server
      (let* ((port (get-port server))
             (interface (get-interface server))
             (app (lack:builder
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
                (clack:clackup app
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

  (weblocks.routes:reset-routes)
  
  (unless (member :bordeaux-threads *features*)
    (cerror "I know what I'm doing and will stubbornly continue."
            "You're trying to start Weblocks without threading ~
            support. Recompile your Lisp with threads enabled."))
  (if debug
      (weblocks::enable-global-debugging)
      (weblocks::disable-global-debugging))
  (when (null *server*)
    (values
     (start (setf *server*
                  (make-server :port port
                               :interface interface
                               :server-type server-type))
            :debug debug)
     
     (log:info "Starting webapps flagged as ``autostarted``")
     (mapcar (lambda (class)
               (unless (weblocks.app:app-active-p class)
                 (weblocks::start-webapp class :debug debug)))
             (weblocks.app:get-autostarting-apps)))))


(defun stop-weblocks ()
  "Stops weblocks."

  ;; TODO: Investigate if it closes all stores declared via 'defstore'.
  
  (when (not (null *server*))
    (weblocks.hooks:with-hook
        (:stop-weblocks)
        
        (dolist (app (weblocks.app:get-active-apps))
          (weblocks::stop-webapp (weblocks::weblocks-webapp-name app)))

        ;; Was commented because *last-session* is unknown
        ;; (setf weblocks.session::*last-session* nil)

        ;; TODO: Replace with CLACK's sessions
        ;; (weblocks::reset-sessions)
        (when *server*
          (stop *server*))
        (setf *server* nil))))


;;;; Static files

(defclass static-route-from-file (weblocks.routes:route)
  ((path :initarg :path
         :reader get-path)
   (content-type :initarg :content-type
                 :reader get-content-type)))


(defmethod weblocks.routes:serve ((route static-route-from-file) env)
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
    (weblocks.routes:add-route route)))
