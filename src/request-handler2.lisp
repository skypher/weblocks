(defpackage #:weblocks.request-handler
  (:use #:cl
        #:f-underscore)
  (:export
   #:handle-client-request
   #:abort-request-handler
   #:page-not-found-handler))
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
                          (if weblocks.variables:*catch-errors-p*
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


(defgeneric page-not-found-handler (app)
  (:documentation "This function is called when the current widget 
   heirarchy fails to parse a URL.  The default behavior simply sets the 
   404 return code")
  (:method ((app t))
    (declare (ignore app))
    
    (setf weblocks.response:*code* 404
          weblocks.response:*content-type* "plain/text")

    (weblocks.response:abort-processing "Not found")))


(defmethod handle-normal-request ((app weblocks:weblocks-webapp))
  ;; we need to render widgets before the boilerplate HTML
  ;; that wraps them in order to collect a list of script and
  ;; stylesheet dependencies.
  (log:debug "Handling normal request")
  
  (weblocks::webapp-update-thread-status "Handling normal request [tree shakedown]")
  (bordeaux-threads:with-lock-held ((weblocks.session-lock:get-lock))
    (handler-case (weblocks::timing "tree shakedown"
                    (weblocks::update-widget-tree))
      (weblocks::http-not-found ()
        (return-from handle-normal-request
          (page-not-found-handler app))))

    (weblocks::webapp-update-thread-status "Handling normal request [rendering widgets]")
    (weblocks::timing "widget tree rendering"
      (weblocks::render-widget (weblocks::root-widget))))

  (log:debug "Page's new-style dependencies"
             weblocks.dependencies:*page-dependencies*)

  ;; set page title if it isn't already set
  (when (and (null weblocks::*current-page-description*)
             (last (weblocks::all-tokens weblocks::*uri-tokens*)))
    (setf weblocks::*current-page-description* 
          (weblocks::humanize-name (weblocks::last-item
                                    (weblocks::all-tokens weblocks::*uri-tokens*)))))
  ;; render page will wrap the HTML already rendered to
  ;; *weblocks-output-stream* with necessary boilerplate HTML
  (weblocks::webapp-update-thread-status "Handling normal request [rendering page]")
  (weblocks::timing "page render"
    (weblocks::render-page app))
  ;; make sure all tokens were consumed (FIXME: still necessary?)
  (unless (or (weblocks::tokens-fully-consumed-p weblocks::*uri-tokens*)
              (null (weblocks::all-tokens weblocks::*uri-tokens*)))
    (page-not-found-handler app)))


(defmethod handle-client-request ((app weblocks:weblocks-webapp))
  (restart-case
      (progn                            ;save it for splitting this up
        ;; TODO: replace with lack.session
        ;; (when (null weblocks::*session*)
        ;;   (when (get-action-name-from-request)
        ;;     (weblocks::expired-action-handler app))
        ;;   (weblocks::start-session)
        ;;   (setf (weblocks::weblocks.session:get-value 'last-request-uri)
        ;;         :none)
        ;;   (when weblocks::*rewrite-for-session-urls*
        ;;     (weblocks::redirect (weblocks::request-uri*))))
        ;;
        ;; (when weblocks::*maintain-last-session*
        ;;   (bordeaux-threads:with-lock-held (weblocks::*maintain-last-session*)
        ;;     (setf weblocks::*last-session*
        ;;           weblocks::*session*)))

        (let ((uri (weblocks.request:request-uri)))
          (log:debug "Handling client request" uri))
    
        (weblocks.hooks:with-hooks
          (let (weblocks::*dirty-widgets*)
            (when (null (weblocks::root-widget))
              (let ((root-widget (weblocks::make-root-widget app)))
                (weblocks.session:set-value 'weblocks::root-widget
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
                      (weblocks.session:set-value 'weblocks::root-widget
                                                  nil)
                      (weblocks::reset-webapp-session))))
          
                ;; TODO: understand why there is coupling with Dialog here and
                ;;       how to move it into the Dialog's code.
                (weblocks.hooks:add-session-hook :post-action
                                                 'weblocks::update-dialog-on-request))
        
              ;; (when (and weblocks::*rewrite-for-session-urls*
              ;;            (weblocks::cookie-in (weblocks::session-cookie-name
              ;;                                  weblocks::*weblocks-server*)))
              ;;   (weblocks::redirect (weblocks::remove-session-from-uri (weblocks::request-uri*))))
              )

            (let ((weblocks::*weblocks-output-stream*
                    (make-string-output-stream))
                  (weblocks::*uri-tokens*
                    (make-instance 'weblocks::uri-tokens
                                   :tokens (weblocks::tokenize-uri (weblocks.request:request-uri))))
                  weblocks::*before-ajax-complete-scripts*
                  weblocks::*on-ajax-complete-scripts*
                  ;;            weblocks::*page-dependencies*
            
                  ;; New-style dependencies
                  (weblocks.dependencies:*page-dependencies*
                    (weblocks.dependencies:get-dependencies app))
            
                  weblocks::*current-page-title*
                  weblocks::*current-page-description*
                  weblocks::*current-page-keywords*
                  weblocks::*current-page-headers*
                  (cl-who::*indent* (weblocks::weblocks-webapp-html-indent-p app)))
        
              (let ((action-name (weblocks.request::get-action-name-from-request))
                    (action-arguments
                      (weblocks::alist->plist (weblocks.request:request-parameters))))
          
                (when (weblocks::pure-request-p)
                  (weblocks.response:abort-processing
                   (weblocks::eval-action action-name
                                          action-arguments))) ; FIXME: what about the txn hook?

                (weblocks::webapp-update-thread-status "Processing action")
                (weblocks::timing "action processing (w/ hooks)"
                  (weblocks.hooks:eval-hooks :pre-action)
                  (weblocks.hooks:with-dynamic-hooks (:dynamic-action)
                    (weblocks::eval-action action-name
                                           action-arguments))
                  (weblocks.hooks:eval-hooks :post-action)))

              ;; Remove "action" parameter for the GET parameters
              ;; it it is not an AJAX request
              (when (and (not (weblocks.request:ajax-request-p))
                         (weblocks.request:request-parameter weblocks::*action-string*))
                (weblocks::redirect (weblocks::remove-action-from-uri
                                     (weblocks.request:request-uri))))

              (weblocks::timing "rendering (w/ hooks)"
                (weblocks.hooks:eval-hooks :pre-render)
                (weblocks.hooks:with-dynamic-hooks (:dynamic-render)
                  (if (weblocks.request:ajax-request-p)
                      (weblocks::handle-ajax-request app)
                      (handle-normal-request app))

                  ;; Now we'll add routes for each page dependency.
                  ;; This way, a dependency for widgets, created by action
                  ;; can be served when browser will follow up with next request.
                  ;;
                  ;; TODO: only add new routes
                  (weblocks.routes:register-dependencies
                   weblocks.dependencies:*page-dependencies*))
                
                (weblocks.hooks:log-hooks :post-render)
                (weblocks.hooks:eval-hooks :post-render))

        

              ;; TODO: replace return-code with something else
              (if (member weblocks.response:*code*
                          weblocks::*approved-return-codes*)
                  (progn 
                    (unless (weblocks.request:ajax-request-p)
                      (weblocks.session::set-value 'last-request-uri
                                                   (weblocks::all-tokens weblocks::*uri-tokens*)))
                    (get-output-stream-string weblocks::*weblocks-output-stream*))
                  (weblocks::handle-http-error app weblocks.response:*code*))))))

    ;; Restart
    (abort (&optional v)
      :report "abort request processing and return 500"
      (log:error "Aborting request processing")
      (weblocks.response:abort-processing "" :code 500))
    ))



;; (defun abort-request-handler (response)
;;   "Aborts execution of the current request and returns a response as is."

;;   ;; TODO: signal a condition and handle it somewhere.
;;   nil)
