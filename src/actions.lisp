
(in-package :weblocks)

(export '(make-action-url make-action render-link handle-client-request))

(defparameter *action-string* "action"
  "A string used to pass actions from a client to the server. See
  'get-request-action'.")

(defun make-action (action-fn &optional (action-code (gensym)))
  "Coverts a function into an action that can be rendered into
HTML. A unique string is generated for the function, and a
function is added to the session hashtable under this string. The
string is then returned. When later requests come in,
'get-request-action' machinery determines if the action string
that came with the request is stored in the hashtable, and if so,
invokes the stored function.

'action-fn' - A function of zero arguments that will be called if
the user initiates appropriate control (link, form, etc.)

'action-code' - The code to use for an action (if not specified
make-action generates a unique value for each action)"
  (setf (session-value action-code) action-fn)
  action-code)

(defun make-action-url (action-code)
  "Accepts action code and returns a URL that can be used to render
the action. Used, among others, by 'render-link'.

Ex:

\(make-action-url \"test-action\") => \"?action=test-action\""
  (concatenate 'string "?" *action-string* "=" (princ-to-string action-code)))

(defun render-link (action-code name)
  "Renders an action into an href link. The link will be rendered in
such a way that the action will be invoked via AJAX, or will fall back
to regular request if JavaScript is not available. When the user
clicks on the link, the action will be called on the server.

'action-code' - The action created with 'make-action'.
'name' - A string that will be presented to the user in the
link."
  (let ((url (make-action-url action-code)))
    (with-html
      (:a :href url :onclick (format nil "initiateAction(\"~A\", \"~A\"); return false;"
				     action-code (session-name-string-pair))
	  (str name)))))

(defun get-request-action-name ()
  "Gets the name of the action from the request."
  (let* ((request-action-name (request-parameter *action-string*))
	 (get/post-action-name (parameter *action-string*))
	 (action-name (if request-action-name
			  request-action-name
			  get/post-action-name)))
    action-name))

(defun get-request-action ()
  "Gets an action from the request. If the request contains
*action-string* parameter, the action is looked up in the session and
appropriate function is returned. If no action is in the parameter,
returns nil. If the action isn't in the session (somehow invalid),
raises an assertion."
  (let ((action-name (get-request-action-name))
	request-action)
    (when action-name
      (setf request-action (session-value action-name))
      (assert request-action (request-action)
	      (concatenate 'string "Cannot find action: " action-name))
      request-action)))

(defun eval-action ()
  "Evaluates the action that came with the request."
  (safe-apply (get-request-action) (alist->plist (request-parameters))))

