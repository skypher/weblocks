(in-package :weblocks)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (find-symbol "*APPROVED-RETURN-CODES*" 'hunchentoot)
    (pushnew 'hunchentoot-approved-return-codes *features*)))

(export '(handle-client-request
          *before-ajax-complete-scripts*
          *on-ajax-complete-scripts*
	  *catch-errors-p*
          *request-timeout*
          *backtrace-on-session-init-error*
          *style-warn-on-circular-dirtying*
          *style-warn-on-late-propagation*
          #-hunchentoot-approved-return-codes
            *approved-return-codes*))

#-hunchentoot-approved-return-codes
  (defvar *approved-return-codes* (list +http-ok+))

(defvar *before-ajax-complete-scripts*)
(setf (documentation '*before-ajax-complete-scripts* 'variable)
      "A list of client-side scripts to be sent over to the browser at
      the end of ajax request execution.  TODO when executed?")

(defvar *on-ajax-complete-scripts*)
(setf (documentation '*on-ajax-complete-scripts* 'variable)
      "A list of client-side scripts to be sent over to the browser at
      the end of ajax request execution.")

;; remove this when Hunchentoot reintroduces *catch-errors-p*
(defvar *catch-errors-p* t)

(defvar *request-timeout* 180
  "Seconds until we abort a request because it took too long.
  This prevents threads from hogging the CPU indefinitely.
  
  You can set this to NIL to disable timeouts (not recommended).")

(defvar *backtrace-on-session-init-error* t)

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
                          (if *catch-errors-p*
                            (return-from handle-client-request
                                         (handle-error-condition app c))
                            (invoke-debugger c)))))
    (let ((*print-pretty* t))
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
  (progn				;save it for splitting this up
    (when (null *session*)
      (when (get-request-action-name)
	(expired-action-handler app))
      (start-session)
      (setf (webapp-session-value 'last-request-uri) :none)
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
	  (push 'update-dialog-on-request (request-hook :session :post-action)))
	(when (and *rewrite-for-session-urls*
                   (cookie-in (session-cookie-name *weblocks-server*)))
	  (redirect (remove-session-from-uri (request-uri*)))))

      (let ((*weblocks-output-stream* (make-string-output-stream))
	    (*uri-tokens* (make-instance 'uri-tokens :tokens (tokenize-uri (request-uri*))))
	    *before-ajax-complete-scripts*
            *on-ajax-complete-scripts*
	    *page-dependencies*
            *current-page-title*
            *current-page-description*
            *current-page-keywords*
            *current-page-headers*
	    (cl-who::*indent* (weblocks-webapp-html-indent-p app)))
	(declare (special *weblocks-output-stream*
                          *dirty-widgets*
			  *on-ajax-complete-scripts*
                          *uri-tokens*
                          *page-dependencies*
                          *current-page-title*
                          *current-page-description*
                          *current-page-keywords*
                          *current-page-headers*))
	(when (pure-request-p)
	  (abort-request-handler (eval-action))) ; FIXME: what about the txn hook?

        (webapp-update-thread-status "Processing action")
        (timing "action processing (w/ hooks)"
          (eval-hook :pre-action)
          (with-dynamic-hooks (:dynamic-action)
            (eval-action))
          (eval-hook :post-action))

	(when (and (not (ajax-request-p))
		   (find *action-string* (get-parameters*)
			 :key #'car :test #'string-equal))
	  (redirect (remove-action-from-uri (request-uri*))))

        (timing "rendering (w/ hooks)"
          (eval-hook :pre-render)
          (with-dynamic-hooks (:dynamic-render)
            (if (ajax-request-p)
              (handle-ajax-request app)
              (handle-normal-request app)))
          (eval-hook :post-render))

	(unless (ajax-request-p)
	  (setf (webapp-session-value 'last-request-uri) (all-tokens *uri-tokens*)))

        (if (member (return-code*) *approved-return-codes*)
          (get-output-stream-string *weblocks-output-stream*)
          (handle-http-error app (return-code*)))))))

(defmethod handle-ajax-request ((app weblocks-webapp))
  (declare (special *weblocks-output-stream* *dirty-widgets*
                    *on-ajax-complete-scripts* *uri-tokens* *page-dependencies*))
  (webapp-update-thread-status "Handling AJAX request")
  (timing "handle-ajax-request"
    (update-location-hash-dependents)
    (render-dirty-widgets)))

(defun update-location-hash-dependents ()
  (let ((hash (parse-location-hash)))
    (when hash
      (mapc (lambda (w)
              (update-state-from-location-hash w hash))
           (get-widgets-by-type 'location-hash-dependent)))))

(defun update-widget-tree ()
  (let ((*tree-update-pending* t)
        (depth 0)
        page-title
        page-description
        page-keywords)
    (declare (special *tree-update-pending*
                      *current-page-title*
                      *current-page-description*
                      *current-page-keywords*
                      *current-page-headers*))
    (walk-widget-tree (root-widget)
                      (lambda (widget d)
                        (update-children widget)
                        (let ((title (page-title widget))
                              (description (page-description widget))
                              (keywords (page-keywords widget))
                              (headers (page-headers widget)))
                          (when (and (> d depth) title)
                            (setf page-title title))
                          (when (and (> d depth) description)
                            (setf page-description description))
                          (when headers
                            (setf *current-page-headers*
                                  (append (page-headers widget)
                                          *current-page-headers*)))
                          (cond
                            ((and keywords *accumulate-page-keywords*)
                             (setf page-keywords
                                   (append keywords page-keywords)))
                            ((and keywords (> d depth))
                             (setf page-keywords keywords)))
                          (when (> d depth)
                            (setf depth d)))))
    (when page-title
      (setf *current-page-title* page-title))
    (when page-description
      (setf *current-page-description* page-description))
    (when page-keywords
      (setf *current-page-keywords* (remove-duplicates page-keywords :test #'equalp)))))

(defvar *session-locks* (make-hash-table :test #'eq
                                         #+sbcl :weakness #+sbcl :key
					 #+ccl :weak #+ccl :key)
  "Per-session locks to avoid having unrelated threads
  waiting.")
#-(or sbcl ccl) (warn "No GC mechanism for *SESSION-LOCKS* on your Lisp. ~
            Expect a tiny memory leak until fixed.")

(defvar *session-lock-table-lock* (bordeaux-threads:make-lock
                                    "*session-lock-table-lock*"))

(defun session-lock ()
  (bordeaux-threads:with-lock-held (*session-lock-table-lock*)
    (unless (gethash *session* *session-locks*)
      (setf (gethash *session* *session-locks*) 
            (bordeaux-threads:make-lock (format nil "session lock for session ~S" *session*))))
    (gethash *session* *session-locks*)))

(defmethod handle-normal-request ((app weblocks-webapp))
  (declare (special *weblocks-output-stream*
                    *uri-tokens*))
  ; we need to render widgets before the boilerplate HTML
  ; that wraps them in order to collect a list of script and
  ; stylesheet dependencies.
  (webapp-update-thread-status "Handling normal request [tree shakedown]")
  (bordeaux-threads:with-lock-held ((session-lock))
    (handler-case (timing "tree shakedown"
                    (update-widget-tree))
      (http-not-found () (return-from handle-normal-request
                                      (page-not-found-handler app))))

    (webapp-update-thread-status "Handling normal request [rendering widgets]")
    (timing "widget tree rendering"
      (render-widget (root-widget))))
  ; set page title if it isn't already set
  (when (and (null *current-page-description*)
             (last (all-tokens *uri-tokens*)))
    (setf *current-page-description* 
          (humanize-name (last-item (all-tokens *uri-tokens*)))))
  ; render page will wrap the HTML already rendered to
  ; *weblocks-output-stream* with necessary boilerplate HTML
  (webapp-update-thread-status "Handling normal request [rendering page]")
  (timing "page render"
    (render-page app))
  ;; make sure all tokens were consumed (FIXME: still necessary?)
  (unless (or (tokens-fully-consumed-p *uri-tokens*)
              (null (all-tokens *uri-tokens*)))
    (page-not-found-handler app)))

(defun remove-session-from-uri (uri)
  "Removes the session info from a URI."
  (remove-parameter-from-uri uri (session-cookie-name *weblocks-server*)))

(defun remove-action-from-uri (uri)
  "Removes the action info from a URI."
  (remove-parameter-from-uri uri *action-string*))

(defparameter *style-warn-on-circular-dirtying* nil
  "Whether to emit a style-warning when widgets are
marked dirty after the rendering phase.")

(defparameter *style-warn-on-late-propagation* nil
  "Whether to emit a style-warning when widgets are
marked dirty in the rendering phase.")

(defun render-dirty-widgets ()
  "Renders widgets that have been marked as dirty into a JSON
association list. This function is normally called by
'handle-client-request' to service AJAX requests."
  (declare (special *dirty-widgets* *weblocks-output-stream*
		    *before-ajax-complete-scripts* *on-ajax-complete-scripts*))
  (setf (content-type*) *json-content-type*)
  (let ((render-state (make-hash-table :test 'eq)))
    (labels ((circularity-warn (w)
               (when *style-warn-on-circular-dirtying*
                 (style-warn 'non-idempotent-rendering
                  :change-made
                  (format nil "~A was marked dirty and skipped after ~
                               already being rendered" w))))
	     (render-enqueued (dirty)
	       (loop for w in dirty
		     if (gethash w render-state)
		       do (circularity-warn w)
		     else
		       do (render-widget w)
			  (setf (gethash w render-state) t)
		       and collect (cons (dom-id w)
					 (get-output-stream-string
					     *weblocks-output-stream*))))
	     (late-propagation-warn (ws)
               (when *style-warn-on-late-propagation*
                 (style-warn 'non-idempotent-rendering
                  :change-made
                  (format nil "~A widgets were marked dirty: ~S" (length ws) ws))))
	     (absorb-dirty-widgets ()
	       (loop for dirty = *dirty-widgets*
		     while dirty
		     count t into runs
		     when (= 2 runs)
		       do (late-propagation-warn dirty)
		     do (setf *dirty-widgets* '())
		     nconc (render-enqueued dirty))))
      (let ((rendered-widgets (absorb-dirty-widgets)))
        (write 
          (encode-json-alist-to-string
            `(("widgets" . ,rendered-widgets)
              ("before-load" . ,*before-ajax-complete-scripts*)
              ("on-load" . ,*on-ajax-complete-scripts*)))
          :stream *weblocks-output-stream*
          :escape nil)))))

(defun action-txn-hook (hooks)
  "This is a dynamic action hook that wraps POST actions using the 
   weblocks transaction functions over all stores"
  (if (eq (request-method*) :post)
      (let (tx-error-occurred-p)
	(multiple-value-bind (dynamic-stores non-dynamic-stores)
	    (loop for store-name in *store-names*
		  for store = (symbol-value store-name)
		  when store
		    if (use-dynamic-transaction-p store)
		      collect store into dynamic-stores
		    else collect store into non-dynamic-stores
		  finally (return (values dynamic-stores non-dynamic-stores)))
	  (labels ((dynamic-transactions (stores)
		     (if (null stores)
			 (eval-dynamic-hooks hooks)
			 (dynamic-transaction
			  (car stores)
			  (f0 (dynamic-transactions (cdr stores))))))
		   (handle-error (error)
		     (declare (ignore error))
		     (mapc #'rollback-transaction non-dynamic-stores)
		     (setf tx-error-occurred-p t)))
	    (unwind-protect
		 (handler-bind ((error #'handle-error))
		   (mapc #'begin-transaction non-dynamic-stores)
		   (dynamic-transactions dynamic-stores))
	      (unless tx-error-occurred-p
		(mapc #'commit-transaction non-dynamic-stores))))))
      (eval-dynamic-hooks hooks)))
  
;; a default dynamic-action hook function wraps actions in a transaction
(eval-when (:load-toplevel)
  (pushnew 'action-txn-hook
	   (request-hook :application :dynamic-action)))

