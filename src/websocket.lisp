;; TODO remove, this was moved to separate file
(defpackage #:weblocks.websocket
  (:use #:cl)
  (:export
   #:websocket-widget))
(in-package weblocks.websocket)


(defvar *uri* "/websocket")


(defvar *route-created* nil
  "This variable will be set to true when first WebSocket widget will be initialized.")


(defun process-websocket (env)
  (log:info "Processing websocket env")
  
  (handler-case (let ((ws (wsd:make-server env))
                      ;; Remember session to use it later in message processing
                      (session weblocks.session::*session*))

                  ;; Bind websocket server to user's session.
                  ;; This way we'll be able to send him commands
                  ;; from other pieces of the server-side code.
                  (setf (weblocks.session:get-value :websocket)
                        ws)

                  (wsd:on :message ws
                          (lambda (message)
                            (let ((weblocks.session::*session* session))
                              (log:info "Received websocket message" message))
                            
                            ;; (wsd:send ws (concatenate 'string
                            ;;                           "pong "
                            ;;                           message))
                            ))
    
                  (lambda (responder)
                    (declare (ignore responder))
                    (log:info "Websocket responder was called" ws)
                    (wsd:start-connection ws)))
    (error ()
      (log:error "Unable to handle websocket.")
      (list 500
            (list :content-type "plain/text")
            (list "Unable to handle websocket")))))



(defclass websocket-route (routes:route)
  ())



(defun make-websocket-route (uri)
  "Makes a route for websocket handle.

Automatically adds a prefix depending on current webapp and widget."

  (let ((route (make-instance 'websocket-route
                              :template (routes:parse-template uri))))
    (weblocks.routes:add-route route)))


(weblocks:defwidget websocket-widget ()
  ())


(defmethod initialize-instance ((widget websocket-widget) &rest initargs)
  (declare (ignorable initargs))

  (call-next-method)

  (unless *route-created*
    (make-websocket-route *uri*)
    (setf *route-created* t)))


(defun make-websocket-client-code ()
  (weblocks.parenscript:make-dependency
    (flet ((on-open ()
             (console.log "Connection was opened")
             (setf (ps:@ window websocket_connected)
                   t)
             ((ps:@ this send) "connected"))
           (on-close (event)
             (if (ps:@ event was-clean)
                 (console.log "Connection closed cleanly")
                 (console.log "Connection was interrupted"))
             (console.log (+ "Code: " (ps:@ event code)
                             " reason: " (ps:@ event reason))))
           (on-message (message)
             (console.log "Message received: " message)
             (let* ((data ((ps:@ -J-S-O-N parse)
                           (ps:@ message data)))
                    (dirty-widgets (ps:@ data widgets)))
               (update-element-for-tree (widgets-json-to-tree dirty-widgets))))
           (on-error (error)
             (console.log (+ "Error: " error)))
           (connect (url)
             (let* ((full-url (+ (if (equal window.location.protocol "http:")
                                     "ws://"
                                     "wss://")
                                 window.location.host
                                 url))
                    (socket (ps:new (-web-socket full-url))))
               (setf (ps:@ socket onopen)
                     on-open
                     (ps:@ socket onclose)
                     on-close
                     (ps:@ socket onmessage)
                     on-message
                     (ps:@ socket onerror)
                     on-error)
               socket)))
      (unless (ps:@ window websocket_connected)
       (connect *uri*)))))



(defmethod weblocks.dependencies:get-dependencies ((widget websocket-widget))
  (append (list (make-websocket-client-code))
          (call-next-method)))


(defvar *background* nil
  "This variable becomes t during background processing.")



(defmethod weblocks:mark-dirty ((widget websocket-widget) &key propagate
                                                            putp)
  (declare (ignorable putp propagate))
  (log:info "Websocket widget marked as dirty")
  (if *background*
      (let* ((rendered-widget (weblocks:render-widget widget))
             (dom-id (alexandria:make-keyword
                      (weblocks:dom-id widget)))
             (payload (list :|widgets| (list dom-id
                                             rendered-widget)))
             (json-payload (jonathan:to-json payload)))
        (log:info "Created" payload json-payload)
        (wsd:send (weblocks.session:get-value :websocket)
                  json-payload))
      (call-next-method)))
