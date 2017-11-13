(defpackage #:weblocks.actions
  (:use #:cl)
  (:export
   #:command
   #:render-command
   #:add-command
   #:on-missing-action
   #:eval-action))
(in-package weblocks.actions)


(defvar *commands* nil
  "A list of commands to execute on a client as a result of action call.

These commands are collected during action processing and rendered to resulting JSON
as some sort of JSON-rpc calls to be esecuted on a client-side.")


(defun cl-symbol-to-js-symbol (symbol)
  "A little helper to transform symbols like :foo-baz-bar into keywords :|fooBazBar|."
  (alexandria:make-keyword
   (ps:symbol-to-js-string symbol)))


(defun create-command (name &rest args)
  "Prepares command for rendering into JSON-rpc style dict."
  (check-type name symbol)
  (list :|jsonrpc| "2.0"
        :|method| (cl-symbol-to-js-symbol name)
        :|params| (loop for (key value) on args by #'cddr
                        append (list (cl-symbol-to-js-symbol key)
                                     value))))


(defun add-command (name &rest args)
  "Pushes a new command into the stack.

After action processing these commands will be sent for execution on the client."
  (push
   (apply #'create-command name args)
   *commands*))



(weblocks.hooks:add-application-hook :handle-request
    reset-commands-list ()
  (let (*commands*)
    (weblocks.hooks:call-next-hook)))


(defgeneric on-missing-action (app action-name)
  (:documentation "Must be overridden by application to prevent default
behaviour - redirect to a root of the application.
The new method should determine the behavior in this
situation (e.g. redirect, signal an error, etc.)."))


(defmethod on-missing-action (app action-name)
  (declare (ignorable app action-name))
  (weblocks::redirect
   (weblocks::make-webapp-uri "/" app)))


(defgeneric eval-action (app action-name arguments)
  (:documentation "Evaluates the action that came with the request."))


(defmethod eval-action (app action-name arguments)
  "Evaluates the action that came with the request."
  (let ((action (weblocks::get-request-action action-name)))

    (unless action
      (on-missing-action app action-name))
    
    (log:debug "Calling" action "with" arguments "and" action-name)
    (weblocks::safe-apply action
                          arguments)))
