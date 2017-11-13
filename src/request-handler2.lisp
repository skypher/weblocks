(defpackage #:weblocks.request-handler
  (:use #:cl
        #:f-underscore)
  (:export
   #:handle-client-request
   #:abort-request-handler
   #:page-not-found-handler
   *request-timeout*
   #:handle-ajax-request))
(in-package weblocks.request-handler)


(defvar *request-timeout* 180
  "Seconds until we abort a request because it took too long.
  This prevents threads from hogging the CPU indefinitely.

  You can set this to NIL to disable timeouts (not recommended).")


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
      (weblocks::with-webapp app
        ;;(log4cl-json:with-log-unhandled ())
        (call-next-method)))))


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
     #+sbcl trivial-timeout:with-timeout #+sbcl (*request-timeout*)
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


(defvar weblocks::*current-page-description* nil)


;; NOTE (svetlyak40wt): not sure if we need all this complexity
(defun update-widget-tree ()
  (let ((weblocks::*tree-update-pending* t)
        (depth 0)
        page-title
        page-description
        page-keywords)

    (weblocks:walk-widget-tree
     (weblocks:root-widget)
     (lambda (widget d)
       (weblocks::update-widget-parameters widget
                                           (weblocks.request:request-method)
                                           (weblocks.request:request-parameters))
       (weblocks:update-children widget)
       (let ((title (weblocks:page-title widget))
             (description (weblocks:page-description widget))
             (keywords (weblocks:page-keywords widget))
             (headers (weblocks:page-headers widget)))
         (when (and (> d depth) title)
           (setf page-title title))
         (when (and (> d depth) description)
           (setf page-description description))
         (when headers
           (setf weblocks:*current-page-headers*
                 (append (weblocks:page-headers widget)
                         weblocks:*current-page-headers*)))
         (cond
           ((and keywords weblocks:*accumulate-page-keywords*)
            (setf page-keywords
                  (append keywords page-keywords)))
           ((and keywords (> d depth))
            (setf page-keywords keywords)))
         (when (> d depth)
           (setf depth d)))))
    (when page-title
      (setf weblocks:*current-page-title* page-title))
    (when page-description
      (setf weblocks::*current-page-description* page-description))
    (when page-keywords
      (setf weblocks:*current-page-keywords* (remove-duplicates page-keywords :test #'equalp)))))


(defun update-location-hash-dependents ()
  ;; NOTE (svetlyak40wt): Interesting place.
  ;;                      Looks like it supposed to store some widget's
  ;;                      state in #hash-part-of-the URL.
  ;;                      Don't know if somebody uses it.
  ;;                      Probably should be excluded from new version
  ;;                      of the Weblocks.
  (let ((hash (weblocks::parse-location-hash)))
    (when hash
      (mapc (lambda (w)
              (weblocks::update-state-from-location-hash w hash))
           (weblocks::get-widgets-by-type 'location-hash-dependent)))))

(defun remove-duplicate-dirty-widgets ()
  "Removes all widgets that should be rendered through rendering their parent"
  ;; Obvously, this algorithm have not the best performance, and
  ;; may be will require some optimizations.
  (dolist (widget weblocks::*dirty-widgets*)
    (dolist (widget2 weblocks::*dirty-widgets*)
      (when (and (not (equal widget widget2))
                 (weblocks::child-of-p widget widget2))
        (setf weblocks::*dirty-widgets*
              (remove widget2 weblocks::*dirty-widgets*))))))


(defun render-dirty-widgets ()
  "Renders widgets that have been marked as dirty into a JSON
association list. This function is normally called by
'handle-client-request' to service AJAX requests."

  (log:debug "Rendering dirty widgets")

  (remove-duplicate-dirty-widgets)

  (setf weblocks.response:*content-type*
        weblocks::*json-content-type*)
  
  (let ((render-state (make-hash-table :test 'eq)))
    (labels ((circularity-warn (w)
               (when weblocks.variables:*style-warn-on-circular-dirtying*
                 (weblocks::style-warn 'non-idempotent-rendering
                  :change-made
                  (format nil "~A was marked dirty and skipped after ~
                               already being rendered" w))))
             (render-enqueued (dirty)
               (loop for w in dirty
                     if (gethash w render-state)
                       do (circularity-warn w)
                     else
                       do (weblocks::render-widget w)
                          (setf (gethash w render-state) t)
                       and collect (cons (weblocks::dom-id w)
                                         (get-output-stream-string
                                             weblocks::*weblocks-output-stream*))))
             (late-propagation-warn (ws)
               (when weblocks.variables:*style-warn-on-late-propagation*
                 (weblocks::style-warn 'non-idempotent-rendering
                  :change-made
                  (format nil "~A widgets were marked dirty: ~S" (length ws) ws))))
             (absorb-dirty-widgets ()
               (loop for dirty = weblocks::*dirty-widgets*
                     while dirty
                     count t into runs
                     when (= 2 runs)
                       do (late-propagation-warn dirty)
                     do (setf weblocks::*dirty-widgets* '())
                     nconc (render-enqueued dirty))))
      (let ((rendered-widgets (absorb-dirty-widgets)))
        (write 
         (jonathan:to-json
          ;; For now, we are mixing old-style payload and newstyle
          (list :|widgets| rendered-widgets
                :|before-load| weblocks.variables:*before-ajax-complete-scripts*
                :|on-load| weblocks.variables:*on-ajax-complete-scripts*
                :|commands| weblocks.actions::*commands*))
          :stream weblocks:*weblocks-output-stream*
          :escape nil)))))



(defmethod handle-ajax-request ((app weblocks:weblocks-webapp))
  (log:debug "Handling AJAX request")
  
  (weblocks:webapp-update-thread-status "Handling AJAX request")
  (weblocks::timing "handle-ajax-request"
    (update-location-hash-dependents)
    (render-dirty-widgets)

    ;; TODO: only add new routes
    (weblocks.dependencies:register-dependencies
     (weblocks.dependencies:get-collected-dependencies))))


(defmethod handle-normal-request ((app weblocks:weblocks-webapp))
  ;; we need to render widgets before the boilerplate HTML
  ;; that wraps them in order to collect a list of script and
  ;; stylesheet dependencies.
  (log:debug "Handling normal request")
  
  (weblocks::webapp-update-thread-status "Handling normal request [tree shakedown]")
  (bordeaux-threads:with-lock-held ((weblocks.session-lock:get-lock))
    ;; TODO: Probably it is good idea to remove this widget tree protocol
    ;;       from Weblocks and leave only rendering. Because update-widget-tree
    ;;       only collects page's title, description and keywords.
    ;;       And they can be set during root widget rendering phase
    (handler-case (weblocks::timing "tree shakedown"
                    (update-widget-tree))
      (weblocks::http-not-found ()
        (return-from handle-normal-request
          (page-not-found-handler app))))

    (weblocks::webapp-update-thread-status "Handling normal request [rendering widgets]")
    (weblocks::timing "widget tree rendering"
      (weblocks::render-widget (weblocks::root-widget))))

  (log:debug "Page's new-style dependencies"
             (weblocks.dependencies:get-collected-dependencies))

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
    (weblocks::render-page app)))


(defun remove-action-from-uri (uri)
  "Removes the action info from a URI."
  (weblocks::remove-parameter-from-uri uri weblocks.variables:*action-string*))


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
                                               (when weblocks.variables:*backtrace-on-session-init-error*
                                                 (format t "~%~A~%" (trivial-backtrace:print-backtrace c)))
                                               (signal c))))
                         (funcall init-user-session-func
                                  root-widget))
                       (setf finished? t))
                  (unless finished?
                    (weblocks.session:set-value 'weblocks::root-widget
                                                nil))))
              
              ;; TODO: understand why there is coupling with Dialog here and
              ;;       how to move it into the Dialog's code.
              (weblocks.hooks:add-session-hook :action
                  update-dialog ()
                (weblocks::update-dialog-on-request)))
            
            ;; (when (and weblocks::*rewrite-for-session-urls*
            ;;            (weblocks::cookie-in (weblocks::session-cookie-name
            ;;                                  weblocks::*weblocks-server*)))
            ;;   (weblocks::redirect (weblocks::remove-session-from-uri (weblocks::request-uri*))))
            )

          (weblocks.dependencies:with-collected-dependencies
            (let ((weblocks::*weblocks-output-stream*
                    (make-string-output-stream))
                  (weblocks::*uri-tokens*
                    (make-instance 'weblocks::uri-tokens
                                   :tokens (weblocks::tokenize-uri (weblocks.request:request-uri))))
                  weblocks.variables:*before-ajax-complete-scripts*
                  weblocks.variables:*on-ajax-complete-scripts*
                  weblocks::*current-page-title*
                  weblocks::*current-page-description*
                  weblocks::*current-page-keywords*
                  weblocks::*current-page-headers*
                  (cl-who::*indent* (weblocks::weblocks-webapp-html-indent-p app)))

              
              (weblocks.dependencies:push-dependencies
               (weblocks.dependencies:get-dependencies app))
              
              (let ((action-name (weblocks.request::get-action-name-from-request))
                    (action-arguments
                      (weblocks::alist->plist (weblocks.request:request-parameters))))

                (when action-name
                  (when (weblocks::pure-request-p)
                    (weblocks.response:abort-processing
                     (weblocks.actions:eval-action
                      app
                      action-name
                      action-arguments)))

                  (weblocks::webapp-update-thread-status "Processing action")
                  (weblocks::timing "action processing (w/ hooks)"
                    (weblocks.hooks:with-hook (:action)
                                              (weblocks.actions:eval-action
                                               app
                                               action-name
                                               action-arguments)))))

              ;; Remove "action" parameter for the GET parameters
              ;; it it is not an AJAX request
              (when (and (not (weblocks.request:ajax-request-p))
                         (weblocks.request:request-parameter weblocks.variables:*action-string*))
                (weblocks::redirect (remove-action-from-uri
                                     (weblocks.request:request-uri))))

              (weblocks::timing "rendering (w/ hooks)"
                (weblocks.hooks:with-hook (:render)
                                          (if (weblocks.request:ajax-request-p)
                                              (handle-ajax-request app)
                                              (handle-normal-request app))

                                          ;; Now we'll add routes for each page dependency.
                                          ;; This way, a dependency for widgets, created by action
                                          ;; can be served when browser will follow up with next request.
                                          ;;
                                          ;; TODO: only add new routes
                                          (weblocks.dependencies:register-dependencies
                                           (weblocks.dependencies:get-collected-dependencies))))

              

              ;; TODO: replace return-code with something else
              (if (eql weblocks.response:*code* 200)
                  (let ((content (get-output-stream-string weblocks::*weblocks-output-stream*)))
                    (unless (weblocks.request:ajax-request-p)
                      (weblocks.session:set-value 'last-request-uri
                                                  (weblocks::all-tokens weblocks::*uri-tokens*)))
                    ;; Return rendered content as a response on request.
                    content)
                  ;; TODO: use weblocks.error-handler here
                  (weblocks::handle-http-error app weblocks.response:*code*))))))

    ;; Restart
    (abort ()
      :report "abort request processing and return 500"
      (log:error "Aborting request processing")
      (weblocks.error-handler:on-error app))))



;; (defun abort-request-handler (response)
;;   "Aborts execution of the current request and returns a response as is."

;;   ;; TODO: signal a condition and handle it somewhere.
;;   nil)
