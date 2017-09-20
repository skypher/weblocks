(in-package :weblocks)

(export '(handle-client-request
          *before-ajax-complete-scripts*
          *on-ajax-complete-scripts*
          *request-timeout*
          *backtrace-on-session-init-error*
          *style-warn-on-circular-dirtying*
          *style-warn-on-late-propagation*
          *approved-return-codes*))


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

(defmethod handle-client-request :around ((app weblocks-webapp))
  (handler-bind ((error (lambda (c)
                          (if weblocks.variables:*catch-errors-p*
                            (return-from handle-client-request
                                         (handle-error-condition app c))
                            (invoke-debugger c)))))
    (let ((*print-pretty* t)
          ; Hunchentoot already displays warnings into log file, we just suppress output
          (*error-output* (make-string-output-stream)))
      (with-webapp app (call-next-method)))))

(defmethod handle-client-request :around (app)
  (handler-bind ((timeout-error (lambda (c)
                                  ;; TODO: let the user customize this
                                  (error "Your request timed out."))))
    ;; TRIVIAL-TIMEOUT seems to be broken on CCL and in yet another way on
    ;; Lispworks. For now let's only enable it on SBCL.
    (#-sbcl progn 
     #+sbcl with-timeout #+sbcl (*request-timeout*)
      (webapp-update-thread-status "Request prelude")
      (unwind-protect
        (let* ((timings nil)
               (*timing-level* 0)
               (*timing-report-fn* (lambda (name real cpu)
                                     (setf timings (acons name (list real cpu *timing-level*) timings))))
               (result (timing "handle-client-request"
                         (call-next-method))))
          (dolist (timing timings)
            (dotimes (i (cadddr timing))
              (write "  " :escape nil))
            (finish-output)
            (format t "~A time (real/cpu): ~F/~F~%" (car timing)
                    (cadr timing) (caddr timing)))
          result)
        (webapp-update-thread-status "Request complete/idle")))))

(defmethod handle-client-request ((app weblocks-webapp))

  (progn                                ;save it for splitting this up
    (when (null *session*)
      (when (get-request-action-name)
        (expired-action-handler app))
      (start-session)
      (setf (weblocks.session:get-value 'last-request-uri) :none)
      (when *rewrite-for-session-urls*
        (redirect (request-uri*))))
    (when *maintain-last-session*
      (bordeaux-threads:with-lock-held (*maintain-last-session*)
        (setf *last-session* *session*)))
    (let ((*request-hook* (make-instance 'request-hooks))
          *dirty-widgets*)
      (when (null (root-widget))
        (let ((root-widget (make-instance 'widget :name "root")))
          (setf (root-widget) root-widget)
          (let (finished?)
            (unwind-protect
                 (progn
                   (handler-bind ((error (lambda (c) 
                                           (warn "Error initializing user session: ~A" c)
                                           (when *backtrace-on-session-init-error*
                                             (format t "~%~A~%" (print-trivial-backtrace c)))
                                           (signal c))))
                       (funcall (webapp-init-user-session) root-widget))
                   (setf finished? t))
              (unless finished?
                (setf (root-widget) nil)
                (reset-webapp-session))))
          (weblocks.hooks:add-session-hook :post-action 'update-dialog-on-request))
        (when (and *rewrite-for-session-urls*
                   (cookie-in (session-cookie-name *weblocks-server*)))
          (redirect (remove-session-from-uri (request-uri*)))))

      (let ((*weblocks-output-stream* (make-string-output-stream))
            (*uri-tokens* (make-instance 'uri-tokens :tokens (tokenize-uri (request-uri*))))
            *before-ajax-complete-scripts*
            *on-ajax-complete-scripts*
            *current-page-title*
            *current-page-description*
            *current-page-keywords*
            *current-page-headers*
            (cl-who::*indent* (weblocks-webapp-html-indent-p app)))

        (when (pure-request-p)
          (abort-request-handler (eval-action))) ; FIXME: what about the txn hook?

        (webapp-update-thread-status "Processing action")
        (timing "action processing (w/ hooks)"
          (eval-hook :pre-action)
          (with-dynamic-hooks (:dynamic-action)
            (eval-action))
          (eval-hook :post-action))

        (when (and (not (weblocks.request:ajax-request-p))
                   (find *action-string* (get-parameters*)
                         :key #'car :test #'string-equal))
          (redirect (remove-action-from-uri (request-uri*))))

        (timing "rendering (w/ hooks)"
          (eval-hook :pre-render)
          (with-dynamic-hooks (:dynamic-render)
            (if (weblocks.request:ajax-request-p)
              (handle-ajax-request app)
              (handle-normal-request app)))
          (eval-hook :post-render))

        

        (if (member (return-code*) *approved-return-codes*)
          (progn 
            (unless (weblocks.request:ajax-request-p)
              (setf (weblocks.session:get-value 'last-request-uri) (all-tokens *uri-tokens*)))
            (get-output-stream-string *weblocks-output-stream*))
          (handle-http-error app (return-code*)))))))


(defun remove-session-from-uri (uri)
  "Removes the session info from a URI."
  (remove-parameter-from-uri uri (session-cookie-name *weblocks-server*)))


;; TODO: move this code to weblocks.stores
;; a default dynamic-action hook function wraps actions in a transaction
;; (weblocks.hooks:add-application-hook
;;     :action
;;     action-txn-hook ()
;;   "This is a dynamic action hook that wraps POST actions using the 
;;    weblocks transaction functions over all stores"

;;   ;; Added this temporarily to fix errors without stores
;;   (weblocks.hooks:call-next-hook)
  
;;   ;; (if (eq (weblocks.request:request-method) :post)
;;   ;;     (let (tx-error-occurred-p)
;;   ;;       (multiple-value-bind (dynamic-stores non-dynamic-stores)
;;   ;;           (loop for store-name in *store-names*
;;   ;;                 for store = (symbol-value store-name)
;;   ;;                 when store
;;   ;;                   if (use-dynamic-transaction-p store)
;;   ;;                     collect store into dynamic-stores
;;   ;;                   else collect store into non-dynamic-stores
;;   ;;                 finally (return (values dynamic-stores non-dynamic-stores)))
;;   ;;         (labels ((dynamic-transactions (stores)
;;   ;;                    (if (null stores)
;;   ;;                        (eval-dynamic-hooks hooks)
;;   ;;                        (dynamic-transaction
;;   ;;                         (car stores)
;;   ;;                         (f0 (dynamic-transactions (cdr stores))))))
;;   ;;                  (handle-error (error)
;;   ;;                    (declare (ignore error))
;;   ;;                    (mapc #'rollback-transaction non-dynamic-stores)
;;   ;;                    (setf tx-error-occurred-p t)))
;;   ;;           (unwind-protect
;;   ;;                (handler-bind ((error #'handle-error))
;;   ;;                  (mapc #'begin-transaction non-dynamic-stores)
;;   ;;                  (dynamic-transactions dynamic-stores))
;;   ;;             (unless tx-error-occurred-p
;;   ;;               (mapc #'commit-transaction non-dynamic-stores))))))
;;   ;;     (eval-dynamic-hooks hooks))
;;   )

