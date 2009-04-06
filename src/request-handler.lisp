(in-package :weblocks)


(export '(handle-client-request
          *before-ajax-complete-scripts* *on-ajax-complete-scripts*
	  *current-page-description* *catch-errors-p*))

(defvar *before-ajax-complete-scripts*)
(setf (documentation '*before-ajax-complete-scripts* 'variable)
      "A list of client-side scripts to be sent over to the browser at
      the end of ajax request execution.  TODO when executed?")

(defvar *on-ajax-complete-scripts*)
(setf (documentation '*on-ajax-complete-scripts* 'variable)
      "A list of client-side scripts to be sent over to the browser at
      the end of ajax request execution.")

(defvar *dispatch/render-lock* (bordeaux-threads:make-lock
                                 "*dispatch-render-lock*"))

;; remove this when Hunchentoot reintroduces *catch-errors-p*
(defvar *catch-errors-p* t)

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

(defmethod handle-client-request :around (app)
  (handler-bind ((error (lambda (c)
                          (if *catch-errors-p*
                            (return-from handle-client-request
                                         (handle-error-condition app c))
                            (signal c)))))
    (call-next-method)))

(defmethod handle-client-request ((app weblocks-webapp))
  (let ((*current-webapp* app))
    (declare (special *current-webapp*))
    (when (null *session*)
      (when (get-request-action-name)
	(expired-action-handler app))
      (start-session)
      (setf (webapp-session-value 'last-request-uri) :none)
      (redirect (request-uri*)))
    (when *maintain-last-session*
      (bordeaux-threads:with-lock-held (*maintain-last-session*)
	(setf *last-session* *session*)))
    (let ((*request-hook* (make-instance 'request-hooks)))
      (when (null (root-widget))
	(let ((root-widget (make-instance 'widget :name "root")))
	  (when (weblocks-webapp-debug app)
	    (initialize-debug-actions))
	  (setf (root-widget) root-widget)
	  (let (finished?)
	    (unwind-protect
		 (progn
		   (funcall (webapp-init-user-session) root-widget)
		   (setf finished? t))
	      (unless finished?
		(setf (root-widget) nil)
		(reset-webapp-session))))
	  (push 'update-dialog-on-request (request-hook :session :post-action)))
	(when (cookie-in (session-cookie-name *weblocks-server*))
	  (redirect (remove-session-from-uri (request-uri*)))))

      (let ((*weblocks-output-stream* (make-string-output-stream))
	    (*uri-tokens* (make-instance 'uri-tokens :tokens (tokenize-uri (request-uri*))))
	     *dirty-widgets*
	    *before-ajax-complete-scripts* *on-ajax-complete-scripts*
	    *page-dependencies* *current-page-description*
	    (cl-who::*indent* (weblocks-webapp-html-indent-p app)))
	(declare (special *weblocks-output-stream* *dirty-widgets*
			  *on-ajax-complete-scripts* *uri-tokens* *page-dependencies*
			  *current-page-description*))
	(when (pure-request-p)
	  (throw 'hunchentoot::handler-done (eval-action)))
	;; a default dynamic-action hook function wraps get operations in a transaction
	(eval-hook :pre-action)
	(with-dynamic-hooks (:dynamic-action)
	  (eval-action))
	(eval-hook :post-action)
	(when (and (not (ajax-request-p))
		   (find *action-string* (get-parameters*)
			 :key #'car :test #'string-equal))
	  (redirect (remove-action-from-uri (request-uri*))))
	(eval-hook :pre-render)
	(with-dynamic-hooks (:dynamic-render)
	  (if (ajax-request-p)
            (handle-ajax-request app)
            (handle-normal-request app)))
        (eval-hook :post-render)
	(unless (ajax-request-p)
	  (setf (webapp-session-value 'last-request-uri) (all-tokens *uri-tokens*)))
        (if (member (return-code*) *approved-return-codes*)
          (get-output-stream-string *weblocks-output-stream*)
          (handle-http-error app (return-code*)))))))

(defmethod handle-ajax-request ((app weblocks-webapp))
  (declare (special *weblocks-output-stream* *dirty-widgets*
                    *on-ajax-complete-scripts* *uri-tokens* *page-dependencies*
                    *current-page-description*))
  (render-dirty-widgets))

(defun update-widget-tree ()
  (declare (special *current-page-description*))
  (let ((depth 0) page-title)
    (walk-widget-tree (root-widget)
		      (lambda (widget d)
			(update-children widget)
			(let ((title (page-title widget)))
			  (when (and title (> d depth))
			    (setf page-title title
				  depth d)))))
    (when page-title (setf *current-page-description* page-title))))

(defmethod handle-normal-request ((app weblocks-webapp))
  (declare (special *weblocks-output-stream*
                    *uri-tokens*
                    *current-page-description*))
  ; we need to render widgets before the boilerplate HTML
  ; that wraps them in order to collect a list of script and
  ; stylesheet dependencies.
  (bordeaux-threads:with-lock-held (*dispatch/render-lock*)
    (handler-case (update-widget-tree)
      (http-not-found () (return-from handle-normal-request
                                      (page-not-found-handler app))))
    (render-widget (root-widget)))
  ; set page title if it isn't already set
  (when (and (null *current-page-description*)
             (last (all-tokens *uri-tokens*)))
    (setf *current-page-description* 
          (humanize-name (last-item (all-tokens *uri-tokens*)))))
  ; render page will wrap the HTML already rendered to
  ; *weblocks-output-stream* with necessary boilerplate HTML
  (render-page app)
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

(defun render-dirty-widgets ()
  "Renders widgets that have been marked as dirty into a JSON
association list. This function is normally called by
'handle-client-request' to service AJAX requests."
  (declare (special *dirty-widgets* *weblocks-output-stream*
		    *before-ajax-complete-scripts* *on-ajax-complete-scripts*))
  (setf (content-type*) *json-content-type*)
  (let ((render-state (make-hash-table :test 'eq)))
    (labels ((circularity-warn (w)
	       (style-warn 'non-idempotent-rendering
		:change-made
		(format nil "~A was marked dirty and skipped after ~
			     already being rendered" w)))
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
	       (style-warn 'non-idempotent-rendering
		:change-made
		(format nil "~A widgets were marked dirty" (length ws))))
	     (absorb-dirty-widgets ()
	       (loop for dirty = *dirty-widgets*
		     while dirty
		     count t into runs
		     when (= 2 runs)
		       do (late-propagation-warn dirty)
		     do (setf *dirty-widgets* '())
		     nconc (render-enqueued dirty))))
      (format *weblocks-output-stream*
	      "{\"widgets\":~A,\"before-load\":~A,\"on-load\":~A}"
	      (encode-json-to-string (absorb-dirty-widgets))
	      (encode-json-to-string *before-ajax-complete-scripts*)
	      (encode-json-to-string *on-ajax-complete-scripts*)))))

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
  
(eval-when (:load-toplevel)
  (pushnew 'action-txn-hook
	   (request-hook :application :dynamic-action)))

