
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
      (funcall (symbol-function (find-symbol "INIT-USER-SESSION" (symbol-package *webapp-name*)))
	       root-composite)
      (setf (session-value 'root-composite) root-composite)))

  (let ((action-fn (get-request-action))
	(*weblocks-output-stream* (make-string-output-stream))
	(*current-navigation-url* "/"))
    (declare (special *weblocks-output-stream* *current-navigation-url*))
    (safe-funcall action-fn)
    (apply-uri-to-navigation (tokenize-uri (request-uri))
			     (find-navigation-widget (session-value 'root-composite)))
    (with-page (lambda ()
		 (render-widget (session-value 'root-composite))
		 (when *render-debug-toolbar*
		   (render-debug-toolbar))))
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
