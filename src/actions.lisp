(defpackage #:weblocks/actions
  (:use #:cl)
  (:import-from #:weblocks/utils/misc
                #:safe-apply)
  (:import-from #:weblocks/variables
                #:*action-string*
                #:*ignore-missing-actions*)
  (:import-from #:lack.util
                #:generate-random-id)
  ;; Just dependencies
  (:import-from #:weblocks/session)
  (:import-from #:weblocks/request
                #:get-path)
  (:import-from #:quri
                #:url-encode)
  
  (:export
   #:on-missing-action
   #:eval-action
   #:get-session-action
   #:get-request-action
   #:make-js-action))
(in-package weblocks/actions)


(defgeneric on-missing-action (app action-name)
  (:documentation "Must be overridden by application to prevent default
behaviour - redirect to a root of the application.
The new method should determine the behavior in this
situation (e.g. redirect, signal an error, etc.)."))


(defgeneric eval-action (app action-name arguments)
  (:documentation "Evaluates the action that came with the request."))


(defmethod eval-action (app action-name arguments)
  "Evaluates the action that came with the request."
  (let ((action (get-request-action action-name)))

    (unless action
      (on-missing-action app action-name))
    
    (log:debug "Calling" action "with" arguments "and" action-name)
    (safe-apply action arguments)))


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
          (weblocks/session:get-value 'code->action
                                      (make-hash-table :test #'equal))))

    (setf (gethash action-code code->action) action-fn))

  ;; Now, get or create a table for function->code mapping
  (let ((action->code
          (weblocks/session:get-value 'action->code
                                      (make-hash-table))))

    (setf (gethash action-fn action->code) action-code))
  
  action-code)


;; TODO: make this public and may be to rename it to just make-action
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
                   (weblocks/session:get-value 'action->code
                                               (make-hash-table)))
        (if code-p
            code
            (make-action function-or-action)))
      
      ;; if it is an action code
      (multiple-value-bind (res presentp)
          (weblocks/app-actions:get-action function-or-action)
        (declare (ignore res))
        (if presentp
            function-or-action
            (multiple-value-bind (res presentp)
                (gethash function-or-action
                         (weblocks/session:get-value 'code->action
                                                     (make-hash-table)))
              (declare (ignore res))
              (if presentp
                  function-or-action
                  (error "The value '~A' is not an existing action." function-or-action)))))))


(defun make-action-url (action-code &optional (include-question-mark-p t))
  "Accepts action code and returns a URL that can be used to render
the action.

Ex:

\(make-action-url \"test-action\") => \"?action=test-action\""
  (concatenate 'string
               (get-path) ;; we need this for w3m
               (if include-question-mark-p "?" "")
               *action-string*
               "="
               (url-encode (princ-to-string action-code))))


;; TODO add to documentation #easy
(defun make-js-action (action)
  "Returns a code which can be inserted into onclick attribute and will
execute given Lisp function on click.

It accepts any function as input and produces a string with JavaScript code.
"
  
  (let* ((action-code (function-or-action->action action)))
    (format nil "initiateAction(\"~A\"); return false;"
            action-code)))


(defun get-session-action (name)
  "Returns an action bound to the current session."
  (let ((code->action 
          (weblocks/session:get-value 'code->action
                              (make-hash-table :test #'equal))))
    (gethash name code->action)))


(defun get-request-action (action-name)
  "Gets an action from the request. If the request contains
*action-string* parameter, the action is looked up in the session and
appropriate function is returned. If no action is in the parameter,
returns nil. If the action isn't in the session (somehow invalid),
raises an assertion."
  (when action-name
    (let* ((app-wide-action (weblocks/app-actions:get-action action-name))
           (session-action (get-session-action action-name))
           (request-action (or app-wide-action session-action)))
      ;; TODO: rethink this form. May be throw a special condition instead of string
      (unless *ignore-missing-actions*
        (assert request-action (request-action)
                (concatenate 'string "Cannot find action: " action-name)))
      request-action)))
