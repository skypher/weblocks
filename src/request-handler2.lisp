(defpackage #:weblocks.request-handler
  (:use #:cl
        #:f-underscore)
  (:export
   #:handle-client-request))
(in-package weblocks.request-handler)


(defgeneric handle-client-request (app)
  (:documentation
   "This method handles each request as it comes in from the
server. It is a hunchentoot handler and has access to all hunchentoot
dynamic variables. The default implementation executes a user
action (if any) and renders the root widget wrapped in HTML
provided by 'render-page'. If the request is an AJAX request, only the
dirty widgets are rendered into a JSON data structure. It also invokes
user supplied 'init-user-session' on the first request that has no
session setup.

'handle-client-request' immediately returns '+http-not-found+' if it
sees a mime type on the script name (it doesn't handle what could be
files because these mess with callback functions and break some
widgets that depend on them).

Additionally, on the first request a session is created and a client
is forced to redirect. At this point if the cookie is sent, session
information is removed from the URL, otherwise the URL is left in
tact. This is done so that session information appears on the URL for
clients that don't support cookies (this way AJAX requests followed by
a refresh will work).

This function also manages lists of callback functions and calls them
at different points before and after request. See 'request-hook'.

Override this method (along with :before and :after specifiers) to
customize behavior."))


(defmethod handle-client-request :around ((app weblocks::weblocks-webapp))
  "This wrapper sets current application and suppresses error output from Hunchentoot."
  (handler-bind ((error (lambda (c)
                          (if weblocks::*catch-errors-p*
                            (return-from handle-client-request
                                         (weblocks::handle-error-condition app c))
                            (invoke-debugger c)))))
    (let ((*print-pretty* t)
          ; Hunchentoot already displays warnings into log file, we just suppress output
          (*error-output* (make-string-output-stream)))
      (weblocks::with-webapp app (call-next-method)))))


(defmethod handle-client-request :around (app)
  "This wrapper sets a timeout on the request and reports response timings."

  (log:debug "Handling client request for" app)
  

  ;; TODO: understand how to use it and write a doc.
  
  (handler-bind ((trivial-timeout::timeout-error
                   (lambda (c)
                     (declare (ignorable c))
                     ;; TODO: let the user customize this
                     (error "Your request timed out."))))
    ;; TRIVIAL-TIMEOUT seems to be broken on CCL and in yet another way on
    ;; Lispworks. For now let's only enable it on SBCL.
    (#-sbcl progn 
     #+sbcl trivial-timeout:with-timeout #+sbcl (weblocks::*request-timeout*)
     (weblocks::webapp-update-thread-status "Request prelude")
     (unwind-protect
          (let* ((timings nil)
                 (weblocks::*timing-level* 0)
                 (weblocks::*timing-report-fn*
                   (lambda (name real cpu)
                     (setf timings (acons name
                                          (list real cpu
                                                weblocks::*timing-level*)
                                          timings))))
                 (result (weblocks::timing "handle-client-request"
                           (call-next-method))))
            (dolist (timing timings)
              (dotimes (i (cadddr timing))
                (write "  " :escape nil))
              (finish-output)
              (format t "~A time (real/cpu): ~F/~F~%" (car timing)
                      (cadr timing) (caddr timing)))
            result)
       (weblocks::webapp-update-thread-status "Request complete/idle")))))


(defmethod handle-client-request ((app weblocks::weblocks-webapp))
  (progn                                ;save it for splitting this up
    (when (null weblocks::*session*)
      (when (weblocks::get-request-action-name)
        (weblocks::expired-action-handler app))
      (weblocks::start-session)
      (setf (weblocks::webapp-session-value 'last-request-uri)
            :none)
      (when weblocks::*rewrite-for-session-urls*
        (weblocks::redirect (weblocks::request-uri*))))
    (when weblocks::*maintain-last-session*
      (bordeaux-threads:with-lock-held (weblocks::*maintain-last-session*)
        (setf weblocks::*last-session*
              weblocks::*session*)))
    (let ((weblocks::*request-hook* ;; Make an instance to store Request's hooks
            (make-instance 'weblocks::request-hooks))
          weblocks::*dirty-widgets*)
      (when (null (weblocks::root-widget))
        (let ((root-widget (make-instance 'weblocks::widget
                                          :name "root")))
          (setf (weblocks::root-widget)
                root-widget)
          (let (finished?
                (init-user-session-func (weblocks::webapp-init-user-session)))
            (unwind-protect
                 (progn
                   (handler-bind ((error (lambda (c) 
                                           (warn "Error initializing user session: ~A" c)
                                           (when weblocks::*backtrace-on-session-init-error*
                                             (format t "~%~A~%" (weblocks::print-trivial-backtrace c)))
                                           (signal c))))
                     (funcall init-user-session-func
                              root-widget))
                   (setf finished? t))
              (unless finished?
                (setf (weblocks::root-widget)
                      nil)
                (weblocks::reset-webapp-session))))
          
          ;; TODO: understand why there is coupling with Dialog here and
          ;;       how to move it into the Dialog's code.
          (push 'weblocks::update-dialog-on-request
                (weblocks::request-hook :session :post-action)))
        
        (when (and weblocks::*rewrite-for-session-urls*
                   (weblocks::cookie-in (weblocks::session-cookie-name
                                         weblocks::*weblocks-server*)))
          (weblocks::redirect (weblocks::remove-session-from-uri (weblocks::request-uri*)))))

      (let ((weblocks::*weblocks-output-stream*
              (make-string-output-stream))
            (weblocks::*uri-tokens*
              (make-instance 'weblocks::uri-tokens
                             :tokens (weblocks::tokenize-uri (weblocks::request-uri*))))
            weblocks::*before-ajax-complete-scripts*
            weblocks::*on-ajax-complete-scripts*
            weblocks::*page-dependencies*
            
            ;; New-style dependencies
            (weblocks.dependencies:*page-dependencies*
              (weblocks.dependencies:get-dependencies app))
            
            weblocks::*current-page-title*
            weblocks::*current-page-description*
            weblocks::*current-page-keywords*
            weblocks::*current-page-headers*
            (cl-who::*indent* (weblocks::weblocks-webapp-html-indent-p app)))
        
        (when (weblocks::pure-request-p)
          (weblocks::abort-request-handler (weblocks::eval-action))) ; FIXME: what about the txn hook?

        (weblocks::webapp-update-thread-status "Processing action")
        (weblocks::timing "action processing (w/ hooks)"
          (weblocks::eval-hook :pre-action)
          (weblocks::with-dynamic-hooks (:dynamic-action)
                                        (weblocks::eval-action))
          (weblocks::eval-hook :post-action))

        (when (and (not (weblocks.server:ajax-request-p))
                   (find weblocks::*action-string* (weblocks::get-parameters*)
                         :key #'car :test #'string-equal))
          (weblocks::redirect (weblocks::remove-action-from-uri
                               (weblocks::request-uri*))))

        (weblocks::timing "rendering (w/ hooks)"
          (weblocks::eval-hook :pre-render)
          (weblocks::with-dynamic-hooks (:dynamic-render)
                                        (if (weblocks.server:ajax-request-p)
                                            (weblocks::handle-ajax-request app)
                                            (weblocks::handle-normal-request app)))
          (weblocks::log-hooks :post-render)
          (weblocks::eval-hook :post-render))

        

        (if (member (weblocks::return-code*)
                    weblocks::*approved-return-codes*)
            (progn 
              (unless (weblocks.server:ajax-request-p)
                (setf (weblocks::webapp-session-value 'last-request-uri)
                      (weblocks::all-tokens weblocks::*uri-tokens*)))
              (get-output-stream-string weblocks::*weblocks-output-stream*))
            (weblocks::handle-http-error app (weblocks::return-code*)))))))
