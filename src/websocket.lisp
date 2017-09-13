(defpackage #:weblocks.websocket
  (:use #:cl))
(in-package weblocks.websocket)


(defun process-websocket (env)
  (log:info "Processing websocket env")
  
  (handler-case (let ((ws (wsd:make-server env)))
                  (wsd:on :message ws
                          (lambda (message)
                            (log:info "Processing websocket message" message)
                            (wsd:send ws message)))
    
                  (lambda (responder)
                    (declare (ignore responder))
                    (log:info "Websocket responder was called")
                    (wsd:start-connection ws)))
    (error ()
      (log:error "Unable to handle websocket.")
      (list 500
            (list :content-type "plain/text")
            (list "Unable to handle websocket")))))
