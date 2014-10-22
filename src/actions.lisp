
(in-package :weblocks)

(export '(*expired-action-handler* expired-action-handler page-not-found-handler
          make-action-url make-action function-or-action->action
          *ignore-missing-actions*))

(defvar *expired-action-handler* 'default-expired-action-handler
  "Must be bound to a designator of a function with a single optional
argument - the application. The function gets called when the user
tries to invoke an expired action (due to a session timeout). The
function should determine the behavior in this
situation (e.g. redirect, signal an error, etc.)  Default function
redirects to the root of the application.")

(defgeneric expired-action-handler (app)
  (:documentation "Webapp specific protocol now used in action 
   handler.  This method provides backwards compatibility.")
  (:method ((app t))
    (funcall *expired-action-handler* app)))

(defun default-expired-action-handler (&optional (app (current-webapp)))
  "Default value of *expired-action-handler*. Redirects to application
root and sets a query parameter 'timeout' to true, so that the home
page may display a relevant message, if necessary."
  (redirect
    (concatenate 'string (make-webapp-uri "/") "?timeout=t")
    :defer nil))


(defgeneric page-not-found-handler (app)
  (:documentation "This function is called when the current widget 
   heirarchy fails to parse a URL.  The default behavior simply sets the 
   +http-not-found+ return code")
  (:method ((app t))
    (declare (ignore app))
    (setf (return-code*) +http-not-found+)))


(defparameter *action-string* "action"
  "A string used to pass actions from a client to the server. See
  'get-request-action'.")

(defun generate-action-code ()
  "Generates unique, hard to guess action codes."
  (let ((new-action-id (gensym "")))
    (format nil "~A:~A"
            new-action-id
            (md5
             (hunchentoot::create-random-string 10 36)))))

(defun make-action (action-fn &optional (action-code (generate-action-code)))
  "Converts a function into an action that can be rendered into HTML. A
unique, hard to guess string is generated for the function, and a
function is added to the session hashtable under this string. The
string is then returned. When later requests come in,
'get-request-action' machinery determines if the action string that
came with the request is stored in the hashtable, and if so, invokes
the stored function.

'action-fn' - A function that will be called if the user initiates
appropriate control (link, form, etc.) GET and POST parameters will be
passed to this function as keyword arguments by the framework.

'action-code' - The code to use for an action (if not specified
make-action generates a unique value for each action). Note, if you
don't provide a hard to guess code ('generate-action-code' is used by
default), the user will be vulnerable to an attack where a malicious
attacker can attempt to guess a dangerour action id and send the user
a link to it. Only use guessable action codes for GET actions."
  (setf (webapp-session-value action-code) action-fn)
  action-code)

(defun function-or-action->action (function-or-action)
  "Accepts a function or an existing action. If the value is a
function, calls 'make-action' and returns its result. Otherwise,
checks if the action already exists. If it does, returns the value. If
it does not, signals an error."
  (if (functionp function-or-action)
      (make-action function-or-action)
      (multiple-value-bind (res presentp)
          (webapp-permanent-action function-or-action)
        (declare (ignore res))
        (if presentp function-or-action
            (multiple-value-bind (res presentp)
                (webapp-session-value function-or-action)
              (declare (ignore res))
              (if presentp
                  function-or-action
                  (error "The value '~A' is not an existing action." function-or-action)))))))

(defun make-action-url (action-code &optional (include-question-mark-p t))
  "Accepts action code and returns a URL that can be used to render
the action. Used, among others, by 'render-link'.

Ex:

\(make-action-url \"test-action\") => \"?action=test-action\""
  (concatenate 'string
               (request-uri-path) ; we need this for w3m
               (if include-question-mark-p "?" "")
               *action-string* "="
               (url-encode (princ-to-string action-code))))

(defun get-request-action-name ()
  "Gets the name of the action from the request."
  (let* ((request-action-name (request-parameter *action-string*))
         (get/post-action-name (parameter *action-string*))
         (action-name (or request-action-name get/post-action-name)))
    action-name))

(defvar *ignore-missing-actions* t)

(defun get-request-action ()
  "Gets an action from the request. If the request contains
*action-string* parameter, the action is looked up in the session and
appropriate function is returned. If no action is in the parameter,
returns nil. If the action isn't in the session (somehow invalid),
raises an assertion."
  (let ((action-name (get-request-action-name))
        request-action)
    (when action-name
      (let ((permanent-action (webapp-permanent-action action-name))
            (session-action (webapp-session-value action-name)))
        (setf request-action (or permanent-action session-action))
        (unless *ignore-missing-actions*
          (assert request-action (request-action)
                  (concatenate 'string "Cannot find action: " action-name)))
        request-action))))

(defun eval-action ()
  "Evaluates the action that came with the request."
  (safe-apply (get-request-action) (alist->plist (request-parameters))))

