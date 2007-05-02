
(in-package :weblocks)

(defgeneric handle-client-request ()
  (:documentation
   "This method handles each request as it comes in from the
   server. It is a hunchentoot handler and has access to all
   hunchentoot dynamic variables. The default implementation
   executes a user action (if any), and renders the main
   composite wrapped in HTML provided by 'with-page'. It also
   invokes user supplied 'init-user-session' on the first request
   that has no session setup.  Override this method (along
   with :before and :after specifiers to customize behavior)."))

(defmethod handle-client-request ()
  (when (null (session-value 'root-composite))
    (let ((root-composite (make-instance 'composite)))
      (when *render-debug-toolbar*
	(initialize-debug-actions))
      (funcall (symbol-function (find-symbol "INIT-USER-SESSION" (symbol-package *webapp-name*)))
	       root-composite)
      (setf (session-value 'root-composite) root-composite)))

  (let ((action-fn (get-request-action))
	(*weblocks-output-stream* (make-string-output-stream)))
    (declare (special *weblocks-output-stream*))
    (safe-funcall action-fn)
    (with-page (lambda ()
		 (render-widget (session-value 'root-composite))
		 (when *render-debug-toolbar*
		   (render-debug-toolbar))))
    (get-output-stream-string *weblocks-output-stream*)))
