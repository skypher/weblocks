
(in-package :weblocks)

(export '(handle-client-request
          *before-ajax-complete-scripts* *on-ajax-complete-scripts*
          *uri-tokens* *current-navigation-url* *current-page-description*))

(defvar *uri-tokens*)
(setf (documentation '*uri-tokens* 'variable)
      "URL-decoded list of the path elements in the current request
URI, minus elements in `*current-navigation-url*.  See `dispatcher'
for detailed semantics.")

(defvar *current-navigation-url*)
(setf (documentation '*current-navigation-url* 'variable)
      "Bound while rendering widgets to the URL so far consumed by
`dispatcher's that are parents of the widget being rendered.  See
`dispatcher' for detailed semantics.")

(defvar *before-ajax-complete-scripts*)
(setf (documentation '*before-ajax-complete-scripts* 'variable)
      "A list of client-side scripts to be sent over to the browser at
      the end of ajax request execution.  TODO when executed?")

(defvar *on-ajax-complete-scripts*)
(setf (documentation '*on-ajax-complete-scripts* 'variable)
      "A list of client-side scripts to be sent over to the browser at
      the end of ajax request execution.")

(defgeneric handle-client-request (app)
  (:documentation
   "This method handles each request as it comes in from the
server. It is a hunchentoot handler and has access to all hunchentoot
dynamic variables. The default implementation executes a user
action (if any) and renders the main composite wrapped in HTML
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

Override this method (along with :before and :after specifiers to
customize behavior)."))


(defmethod handle-client-request (app)
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
      (when (null (root-composite))
	(let ((root-composite (make-instance 'composite :name "root")))
	  (when (weblocks-webapp-debug app)
	    (initialize-debug-actions))
	  (setf (root-composite) root-composite)
	  (let (finished?)
	    (unwind-protect
		 (progn
		   (funcall (webapp-init-user-session) root-composite)
		   (setf finished? t))
	      (unless finished?
		(setf (root-composite) nil)
		(reset-webapp-session))))
	  (push 'update-dialog-on-request (request-hook :session :post-action)))
	(when (cookie-in (session-cookie-name *weblocks-server*))
	  (redirect (remove-session-from-uri (request-uri*)))))

      (let ((*weblocks-output-stream* (make-string-output-stream))
	    (*uri-tokens* (tokenize-uri (request-uri*)))
	    (*current-navigation-url* "/") *dirty-widgets*
	    *before-ajax-complete-scripts* *on-ajax-complete-scripts*
	    *page-dependencies* *current-page-description*
	    *uri-tokens-fully-consumed*
	    (cl-who::*indent* (weblocks-webapp-html-indent-p app)))
	(declare (special *weblocks-output-stream* *current-navigation-url* *dirty-widgets*
			  *on-ajax-complete-scripts* *uri-tokens* *page-dependencies*
			  *current-page-description* *uri-tokens-fully-consumed*))
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
	      (render-dirty-widgets)
	      (progn
		; we need to render widgets before the boilerplate HTML
		; that wraps them in order to collect a list of script and
		; stylesheet dependencies.
		(render-widget (root-composite))
		; set page title if it isn't already set
		(when (and (null *current-page-description*)
			   (last *uri-tokens*))
		  (setf *current-page-description* 
			(humanize-name (last-item *uri-tokens*))))
		; render page will wrap the HTML already rendered to
		; *weblocks-output-stream* with necessary boilerplate HTML
		(render-page app)
		;; make sure all tokens were consumed
		(unless (or *uri-tokens-fully-consumed*
                            (null *uri-tokens*))
		  (page-not-found-handler app)))))
	(eval-hook :post-render)
	(unless (ajax-request-p)
	  (setf (webapp-session-value 'last-request-uri) *uri-tokens*))
	(get-output-stream-string *weblocks-output-stream*)))))

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
