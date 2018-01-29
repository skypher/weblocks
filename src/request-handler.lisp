(defpackage #:weblocks/request-handler
  (:use #:cl
        #:f-underscore)
  (:import-from #:weblocks/request
                #:get-path
                #:get-action-name-from-request
                #:get-parameter
                #:get-parameters
                #:pure-request-p
                #:ajax-request-p)
  (:import-from #:weblocks/utils/list
                #:alist->plist)
  (:import-from #:weblocks/utils/uri
                #:remove-parameter-from-uri)
  (:import-from #:weblocks/page
                #:render-page-with-widgets)
  (:import-from #:weblocks/session-lock
                #:get-lock)
  (:import-from #:weblocks/widget
                #:dom-id
                #:render-widget)
  (:import-from #:weblocks/html
                #:with-html-string
                #:*stream*)
  (:import-from #:weblocks/utils/warn
                #:style-warn
                #:non-idempotent-rendering)
  (:import-from #:weblocks/dependencies
                #:with-collected-dependencies
                #:push-dependencies
                #:get-dependencies
                #:get-collected-dependencies
                #:register-dependencies)
  (:import-from #:weblocks/app
                #:app
                #:with-app)
  (:import-from #:weblocks/actions
                #:eval-action)
  (:import-from #:weblocks/commands
                #:get-collected-commands)
  (:import-from #:weblocks/hooks
                #:with-hook)
  (:import-from #:weblocks/error-handler
                #:on-error)
  (:import-from #:weblocks/variables
                #:*backtrace-on-session-init-error*
                #:*action-string*
                ;; #:*style-warn-on-circular-dirtying*
                ;; #:*style-warn-on-late-propagation*
                ;; #:*before-ajax-complete-scripts*
                ;; #:*on-ajax-complete-scripts*
                #:*catch-errors-p*)
  (:import-from #:weblocks/utils/timing
                #:*timing-level*
                #:*timing-report-fn*
                #:timing)
  (:import-from #:bordeaux-threads
                #:with-lock-held)
  (:import-from #:jonathan
                #:to-json)
  (:import-from #:trivial-backtrace
                #:print-backtrace)
  (:import-from #:trivial-timeout
                #:timeout-error
                #:with-timeout)
  (:import-from #:weblocks/response
                #:*code*
                #:*content-type*
                #:abort-processing
                #:redirect)
  ;; Just dependencies
  (:import-from #:log)
  (:import-from #:weblocks/widgets/root)
  (:import-from #:weblocks/session)
  (:import-from #:alexandria
                #:make-keyword)

  (:export
   #:handle-client-request
   #:abort-request-handler
   #:page-not-found-handler
   *request-timeout*
   #:handle-ajax-request))
(in-package weblocks/request-handler)


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
user supplied 'weblocks/session:init' method on the first request that has no
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


(defmethod handle-client-request :around ((app app))
  "This wrapper sets current application and suppresses error output from Hunchentoot."
  (handler-bind ((error (lambda (c)
                          (if *catch-errors-p*
                            (return-from handle-client-request
                                         (on-error app c))
                            (invoke-debugger c)))))
    (let ((*print-pretty* t)
          ; Hunchentoot already displays warnings into log file, we just suppress output
          (*error-output* (make-string-output-stream)))
      (with-app app
        ;;(log4cl-json:with-log-unhandled ())
        (call-next-method)))))


(defmethod handle-client-request :around (app)
  "This wrapper sets a timeout on the request and reports response timings."

  (log:debug "Handling client request for" app)


  ;; TODO: understand how to use it and write a doc.

  (handler-bind ((timeout-error
                   (lambda (c)
                     (declare (ignorable c))
                     ;; TODO: let the user customize this
                     (error "Your request timed out."))))
    ;; TRIVIAL-TIMEOUT seems to be broken on CCL and in yet another way on
    ;; Lispworks. For now let's only enable it on SBCL.
    (#-sbcl progn
     #+sbcl with-timeout #+sbcl (*request-timeout*)
     (unwind-protect
          (let* ((timings nil)
                 (*timing-level* 0)
                 (*timing-report-fn*
                   (lambda (name real cpu)
                     (setf timings (acons name
                                          (list real cpu
                                                *timing-level*)
                                          timings))))
                 (result (timing "handle-client-request"
                           (call-next-method))))
            (dolist (timing timings)
              (dotimes (i (cadddr timing))
                (write "  " :escape nil))
              (finish-output)
              (format t "~A time (real/cpu): ~F/~F~%" (car timing)
                      (cadr timing) (caddr timing)))
            result)))))


(defgeneric page-not-found-handler (app)
  (:documentation "This function is called when the current widget 
   heirarchy fails to parse a URL.  The default behavior simply sets the 
   404 return code")
  (:method ((app t))
    (declare (ignore app))
    
    (setf *code* 404
          *content-type* "plain/text")

    (abort-processing "Not found")))


;; Removed because we use `update' method now and it adds a command
;; (defun render-dirty-widgets ()
;;   "Renders widgets that have been marked as dirty into a JSON
;; association list. This function is normally called by
;; 'handle-client-request' to service AJAX requests."

;;   (log:debug "Rendering dirty widgets")

;;   (setf *content-type*
;;         "application/json; charset=utf-8")
  
;;   (let ((render-state (make-hash-table :test 'eq)))
;;     (labels ((circularity-warn (w)
;;                (when *style-warn-on-circular-dirtying*
;;                  (style-warn 'non-idempotent-rendering
;;                              :change-made
;;                              (format nil "~A was marked dirty and skipped after ~
;;                                already being rendered" w))))
;;              (render-enqueued (dirty)
;;                "Returns a plist of dirty widgets where keys are their
;;                 dom ids."
;;                (loop with widget-html = nil
;;                      for w in dirty
;;                      if (gethash w render-state)
;;                        do (circularity-warn w)
;;                      else
;;                        do (setf widget-html
;;                                 (with-html-string
;;                                   (render-widget w)))
;;                           (setf (gethash w render-state) t)
;;                        and appending (list (alexandria:make-keyword (dom-id w))
;;                                            widget-html)))
;;              (late-propagation-warn (ws)
;;                (when *style-warn-on-late-propagation*
;;                  (style-warn 'non-idempotent-rendering
;;                              :change-made
;;                              (format nil "~A widgets were marked dirty: ~S" (length ws) ws))))
;;              (absorb-dirty-widgets ()
;;                (loop for dirty = weblocks::*dirty-widgets*
;;                      while dirty
;;                      count t into runs
;;                      when (= 2 runs)
;;                        do (late-propagation-warn dirty)
;;                      do (setf weblocks::*dirty-widgets* '())
;;                      nconc (render-enqueued dirty))))
;;       (let ((rendered-widgets (absorb-dirty-widgets)))
;;         (write 
;;          (jonathan:to-json
;;           ;; For now, we are mixing old-style payload and newstyle
;;           (list :|widgets| rendered-widgets
;;                 :|before-load| *before-ajax-complete-scripts*
;;                 :|on-load| *on-ajax-complete-scripts*
;;                 :|commands| (get-collected-commands)))
;;          ;; Seems like a hack, because we have to know implementation
;;          ;; details of weblocks/html here.
;;          ;; TODO: hide implementation details.
;;          :stream *stream*
;;          :escape nil)))))



(defmethod handle-ajax-request ((app app))
  (log:debug "Handling AJAX request")
  
  ;; (timing "handle-ajax-request"
  ;;   (render-dirty-widgets))

  (write
   (to-json
    (list ;; :|widgets| rendered-widgets
          ;; :|before-load| *before-ajax-complete-scripts*
          ;; :|on-load| *on-ajax-complete-scripts*
          :|commands| (get-collected-commands)))
   ;; Seems like a hack, because we have to know implementation details of weblocks/html here.
   ;; TODO: hide implementation details.
   :stream *stream*
   :escape nil))


(defmethod handle-normal-request ((app app))
  ;; we need to render widgets before the boilerplate HTML
  ;; that wraps them in order to collect a list of script and
  ;; stylesheet dependencies.
  (log:debug "Handling normal request")

  ;; TODO: make a macro weblocks/session-lock:with-lock
  (with-lock-held ((get-lock))
    ;; TODO: Probably it is good idea to remove this widget tree protocol
    ;;       from Weblocks and leave only rendering. Because update-widget-tree
    ;;       only collects page's title, description and keywords.
    ;;       And they can be set during root widget rendering phase
    ;; (handler-case (timing "tree shakedown"
    ;;                 (update-widget-tree))
    ;;   (weblocks::http-not-found ()
    ;;     (return-from handle-normal-request
    ;;       (page-not-found-handler app))))

    (timing "widget tree rendering"
      (render-widget (weblocks/widgets/root:get))))

  ;; render page will wrap the HTML already rendered to
  ;; weblocks.html::*stream* with necessary boilerplate HTML
  (timing "page render"
    ;; Here we are using internal symbol, because we don't want to expose
    ;; this method for usage outside of the weblocks.
    (render-page-with-widgets app)))


(defun remove-action-from-uri (uri)
  "Removes the action info from a URI."
  (remove-parameter-from-uri uri *action-string*))


(defun handle-action-if-needed (app)
  (let ((action-name (get-action-name-from-request))
        (action-arguments
          (alist->plist (get-parameters))))

    (when action-name
      (log:debug "Processing action" action-name)

      ;; Remove :action key from action arguments
      (remf action-arguments (make-keyword (string-upcase *action-string*)))
      
      (when (pure-request-p)
        (log:debug "Request is pure, processing will be aborted.")
        ;; TODO: add with-hook (:action)
        (abort-processing
         (eval-action
          app
          action-name
          action-arguments)))

      (timing "action processing (w/ hooks)"
        (with-hook (:action)
          (eval-action
           app
           action-name
           action-arguments)))))


  ;; Remove "action" parameter for the GET parameters
  ;; it it is not an AJAX request
  (when (and (not (ajax-request-p))
             (get-parameter *action-string*))
    
    (let ((url (remove-action-from-uri
                (get-path :with-params t))))
      (log:debug "Redirecting to an URL without action parameter" url)
      (redirect url))))


(defmethod handle-client-request ((app app))
  (restart-case
      (progn
        ;; save it for splitting this up
        ;; TODO: replace with lack.session
        ;; (when (null weblocks::*session*)
        ;;   (when (get-action-name-from-request)
        ;;     (weblocks::expired-action-handler app))
        ;;   (weblocks::start-session)
        ;;   (when weblocks::*rewrite-for-session-urls*
        ;;     (weblocks::redirect (weblocks::request-uri*))))
        ;;
        ;; (when weblocks::*maintain-last-session*
        ;;   (bordeaux-threads:with-lock-held (weblocks::*maintain-last-session*)
        ;;     (setf weblocks::*last-session*
        ;;           weblocks::*session*)))

        (let ((path (get-path)))
          (log:debug "Handling client request" path)

          ;; TODO: write a test
          (when (null (weblocks/widgets/root:get))
            (log:debug "Initializing session")
            (handler-bind ((error (lambda (c) 
                                    (when *backtrace-on-session-init-error*
                                      (let ((traceback))
                                        (log:error "Error during session initialization" traceback)))
                                    (signal c))))
              (setf (weblocks/widgets/root:get)
                    (weblocks/session:init app))))
          
          ;; TODO: understand why there is coupling with Dialog here and
          ;;       how to move it into the Dialog's code.
          
          ;; (weblocks/hooks:add-session-hook :action
          ;;     update-dialog ()
          ;;   (weblocks::update-dialog-on-request)))

          (with-collected-dependencies
            (let ((content nil) ;; this variable will be set to HTML string after rendering
                  ;; TODO: may be remove uri-tokens
                  ;; (weblocks::*uri-tokens*
                  ;;   (make-instance 'weblocks::uri-tokens
                  ;;                  :tokens (weblocks::tokenize-uri (get-path))))
                  ;; weblocks.variables:*before-ajax-complete-scripts*
                  ;; weblocks.variables:*on-ajax-complete-scripts*
                  ;; weblocks::*current-page-title*
                  ;; weblocks::*current-page-description*
                  ;; weblocks::*current-page-keywords*
                  ;; weblocks::*current-page-headers*
                  )

              
              (push-dependencies
               (get-dependencies app))
              
              (handle-action-if-needed app)

              (setf content
                    (with-html-string
                      (timing "rendering (w/ hooks)"
                        (with-hook (:render)
                          (if (ajax-request-p)
                              (handle-ajax-request app)
                              (handle-normal-request app))

                          ;; Now we'll add routes for each page dependency.
                          ;; This way, a dependency for widgets, created by action
                          ;; can be served when browser will follow up with next request.
                          (let ((dependencies (get-collected-dependencies)))
                            (log:debug "Collected dependencies"
                                       dependencies)

                            (register-dependencies
                             dependencies))))))

              content))))

    ;; Restart
    (abort ()
      :report "abort request processing and return 500"
      (log:error "Aborting request processing")
      (abort-processing "Request was aborted"
                        :code 500))))



;; (defun abort-request-handler (response)
;;   "Aborts execution of the current request and returns a response as is."

;;   ;; TODO: signal a condition and handle it somewhere.
;;   nil)
