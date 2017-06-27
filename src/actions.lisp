
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

(defun default-expired-action-handler (&optional (app *current-webapp*))
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


(defun generate-action-code ()
  "Generates unique, hard to guess action codes."
  (let ((new-action-id (gensym "")))
    (format nil "~A:~A"
            new-action-id
            (lack.util:generate-random-id))))

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

  ;; Here we put into the session two maps:
  ;; code->action which maps from string code to a function
  ;; and
  ;; action->code which maps backward from a function to a code.
  (let ((code->action 
          (weblocks.session:get-value 'code->action
                                      (make-hash-table :test #'equal))))

    (setf (gethash action-code code->action) action-fn))

  ;; Now, get or create a table for function->code mapping
  (let ((action->code
          (weblocks.session:get-value 'action->code
                                      (make-hash-table))))

    (setf (gethash action-fn action->code) action-code))
  
  action-code)


(defun function-or-action->action (function-or-action)
  "Accepts a function or an existing action. If the value is a
function, calls 'make-action' and returns its result. Otherwise,
checks if the action already exists. If it does, returns the value. If
it does not, signals an error."
  (if (functionp function-or-action)
      ;; If it is a function, first we'll try to find
      ;; a code for it in the session.
      (multiple-value-bind (code code-p)
          (gethash function-or-action
                   (weblocks.session:get-value 'action->code
                                               (make-hash-table)))
        (if code-p
            code
            (make-action function-or-action)))
      
      ;; if it is an action code
      (multiple-value-bind (res presentp)
          (webapp-permanent-action function-or-action)
        (declare (ignore res))
        (if presentp
            function-or-action
            (multiple-value-bind (res presentp)
                (gethash function-or-action
                         (weblocks.session:get-value 'code->action
                                                     (make-hash-table)))
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
               (weblocks.request:request-path-info) ; we need this for w3m
               (if include-question-mark-p "?" "")
               *action-string* "="
               (quri:url-encode (princ-to-string action-code))))


(defvar *ignore-missing-actions* t)

(defun get-request-action (action-name)
  "Gets an action from the request. If the request contains
*action-string* parameter, the action is looked up in the session and
appropriate function is returned. If no action is in the parameter,
returns nil. If the action isn't in the session (somehow invalid),
raises an assertion."
  (when action-name
    (let* ((permanent-action (webapp-permanent-action action-name))
           (code->action 
             (weblocks.session:get-value 'code->action
                                         (make-hash-table :test #'equal)))
           (session-action (gethash action-name code->action))
           (request-action (or permanent-action session-action)))
      (unless *ignore-missing-actions*
        (assert request-action (request-action)
                (concatenate 'string "Cannot find action: " action-name)))
      request-action)))


(defun eval-action (action-name arguments)
  "Evaluates the action that came with the request."
  (let ((action (get-request-action action-name)))
    
    (log:debug "Calling" action "with" arguments "and" action-name)
    (safe-apply action
                arguments)))


;; TODO add to documentation
(defun make-js-action (action)
  "Returns a code which can be inserted into onclick attribute and will
execute given Lisp function on click.

It accepts any function as input and produces a string with JavaScript code.
"
  
  (let* ((action-code (function-or-action->action action)))
    (format nil "initiateAction(\"~A\", \"~A\"); return false;"
            action-code (session-name-string-pair))))
