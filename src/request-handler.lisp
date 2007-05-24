
(in-package :weblocks)

(defgeneric handle-client-request ()
  (:documentation
   "This method handles each request as it comes in from the
   server. It is a hunchentoot handler and has access to all
   hunchentoot dynamic variables. The default implementation executes
   a user action (if any), prepares the navigation controls using
   'apply-uri-to-navigation', and renders the main composite wrapped
   in HTML provided by 'with-page'. It also invokes user supplied
   'init-user-session' on the first request that has no session setup.
   Override this method (along with :before and :after specifiers to
   customize behavior)."))

(defmethod handle-client-request ()
  (when (null (session-value 'root-composite))
    (let ((root-composite (make-instance 'composite :name "root")))
      (when *render-debug-toolbar*
	(initialize-debug-actions))
      (funcall (symbol-function (find-symbol (symbol-name '#:init-user-session)
					     (symbol-package *webapp-name*)))
	       root-composite)
      (setf (session-value 'root-composite) root-composite)))

  (let ((*weblocks-output-stream* (make-string-output-stream))
	(*current-navigation-url* "/")
	*dirty-widgets*)
    (declare (special *weblocks-output-stream* *current-navigation-url* *dirty-widgets*))
    (safe-funcall (get-request-action))
    (if (ajax-request-p)
	(render-dirty-widgets)
	(progn
	  (apply-uri-to-navigation (tokenize-uri (request-uri))
				   (find-navigation-widget (session-value 'root-composite)))
	  (with-page (lambda ()
		       (render-widget (session-value 'root-composite))))))
    (get-output-stream-string *weblocks-output-stream*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Below is code that implements friendly URLs ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun apply-uri-to-navigation (tokens navigation-widget)
  "Takes URI tokens and applies them one by one to navigation widgets
in order to allow for friendly URLs. The URLs are basically intimately
linked to navigation controls to simulate document resources on the
server."
  (when (null tokens)
    (reset-navigation-widgets navigation-widget)
    (return-from apply-uri-to-navigation))
  (if (and navigation-widget (pane-exists-p navigation-widget (car tokens)))
      (progn
	(setf (slot-value navigation-widget 'current-pane) (car tokens))
	(apply-uri-to-navigation (cdr tokens)
				 (find-navigation-widget (current-pane-widget navigation-widget))))
      (setf (return-code) +http-not-found+)))

(defun find-navigation-widget (comp)
  "Given a composite 'comp', returns the first navigation widget
contained in 'comp' or its children."
  (when (null comp)
    (return-from find-navigation-widget))
  (when (typep comp 'navigation)
    (return-from find-navigation-widget comp))
  (car (flatten (remove-if #'null
			   (mapcar (lambda (w)
				     (typecase w
				       (navigation w)
				       (composite (find-navigation-widget w))
				       (otherwise nil)))
				   (composite-widgets comp))))))

(defun reset-navigation-widgets (nav)
  "Resets all navigation widgets from 'nav' down, using
'reset-current-pane'."
  (unless (null nav)
    (reset-current-pane nav)
    (reset-navigation-widgets (find-navigation-widget (current-pane-widget nav)))))

(defun tokenize-uri (uri)
  "Tokenizes a URI into a list of elements.

ex:
\(tokenize-uri \"/hello/world/blah\\test\\hala/world?hello=5;blah=7\"
=> (\"hello\" \"world\" \"blah\" \"test\" \"hala\" \"world\")"
  (remove-if (curry #'string-equal "")
	     (cl-ppcre:split "[/\\\\]" (cl-ppcre:regex-replace "\\?.*" uri ""))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of friendly URL code ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun render-dirty-widgets ()
  "Renders widgets that have been marked as dirty into a JSON
association list. This function is normally called by
'handle-client-request' to service AJAX requests."
  (declare (special *dirty-widgets* *weblocks-output-stream*))
  (setf (header-out "X-JSON")
	(encode-json-to-string
	 (mapcar (lambda (w)
		   (cons
		    (widget-name w)
		    (progn
		      (render-widget w :inlinep t)
		      (get-output-stream-string *weblocks-output-stream*))))
		 *dirty-widgets*))))

