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
           #:start-weblocks))
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


;; (let ((dependency (weblocks.dependencies:make-static-css-dependency "/tmp/bar.css")))
;;   (routes:connect *routes* (weblocks.dependencies:get-route dependency)))


;; (alexandria:read-file-into-string
;;  "/Users/art/common-lisp/weblocks-twitter-bootstrap-application/twitter-bootstrap.css")



(defmethod handle-request ((server server) env)
  "Weblocks HTTP dispatcher.
This function serves all started applications and their static files."

  (let* ((weblocks.request:*request* (lack.request:make-request env))
         (weblocks.session::*session* (getf env :lack.session)))

    ;; Dynamic hook :handle-request makes possible to write
    ;; some sort of middlewares, which change *request* and *session*
    ;; variables or make some sort
    (weblocks.hooks:with-dynamic-hooks (:handle-request)
      (setf weblocks.request::*latest-request*
            weblocks.request:*request*)
      (setf weblocks.session::*latest-session*
            weblocks.session::*session*)


      (let* ((path-info (getf env :path-info))
             (hostname (getf env :server-name))
             (route (routes:match weblocks.routes:*routes* path-info)))

        ;; If dependency found, then return it's content along with content-type
        (when route
          (let ((dependency (weblocks.routes:get-dependency route)))
            (multiple-value-bind (content content-type)
                (weblocks.dependencies:serve dependency)
              
              (let ((content (typecase content
                               (string (list content))
                               (t content))))
                (return-from handle-request
                  (list 200
                        (list :content-type content-type)
                        content))))))

        (dolist (app weblocks::*active-webapps*)
          (let ((app-prefix (weblocks::webapp-prefix app))
                (app-pub-prefix (weblocks::compute-webapp-public-files-uri-prefix app)))

            (log:debug "Searching handler in" app app-prefix app-pub-prefix)

            (cond
              ((or 
                (find path-info weblocks::*force-files-to-serve* :test #'string=)
                (and (weblocks::webapp-serves-hostname hostname app)
                     (weblocks::list-starts-with (weblocks::tokenize-uri path-info nil)
                                                 (weblocks::tokenize-uri app-pub-prefix nil)
                                                 :test #'string=)))
               (let* ((virtual-folder (weblocks::maybe-add-trailing-slash app-pub-prefix))
                      (physical-folder (weblocks::compute-webapp-public-files-path app))
                      ;; TODO send-gzip-rules move to this file
                      (content-type (weblocks::send-gzip-rules (weblocks::gzip-dependency-types* app)
                                                               path-info env virtual-folder physical-folder)))
                 ;; TODO send-cache-rules
                 (weblocks::send-cache-rules (weblocks::weblocks-webapp-public-files-cache-time app))

                 ;; This is not optimal, because a new dispatcher created for each request
                 ;; TODO: find out how to serve directory in Clack
                 ;;       and move route creation into app initialization code
                 ;; (return-from handle-request
                 ;;   (funcall (weblocks::create-folder-dispatcher-and-handler virtual-folder physical-folder content-type)
                 ;;            env))
                 ))
              ((and (weblocks::webapp-serves-hostname hostname app)
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
                            path-info)))))))


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
                               :debug debug)))

        (log:info "Starting webapps flagged as ``autostarted``")
        
        (mapcar (lambda (class)
                  (unless (weblocks:get-webapps-for-class class)
                    (weblocks:start-webapp class :debug debug)))
                weblocks::*autostarting-webapps*)))
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
     (mapcar (lambda (class)
               (unless (weblocks::get-webapps-for-class class)
                 (weblocks::start-webapp class :debug debug)))
             weblocks::*autostarting-webapps*))))


(defun stop-weblocks ()
  "Stops weblocks."

  ;; TODO: Investigate if it closes all stores declared via 'defstore'.
  
  (when (not (null *server*))
    (dolist (app weblocks::*active-webapps*)
      (weblocks::stop-webapp (weblocks::weblocks-webapp-name app)))
    (setf weblocks.session::*last-session* nil)

    ;; TODO: Replace with CLACK's sessions
    ;; (weblocks::reset-sessions)
    (when *server*
      (stop *server*))
    (setf *server* nil)))
