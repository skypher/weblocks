(defpackage #:weblocks/commands
  (:use #:cl)
  (:import-from #:weblocks/hooks
                #:on-application-hook-handle-request
                #:call-next-hook)
  (:export #:add-command
           #:get-collected-commands))
(in-package weblocks/commands)


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


(defun get-collected-commands ()
  *commands*)


(on-application-hook-handle-request
  reset-commands-list ()
  (let (*commands*)
    (call-next-hook)))
