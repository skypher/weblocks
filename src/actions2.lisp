(defpackage #:weblocks.actions
  (:use #:cl)
  (:export
   #:on-missing-action
   #:eval-action))
(in-package weblocks.actions)



(defgeneric on-missing-action (app action-name)
  (:documentation "Must be overridden by application to prevent default
behaviour - redirect to a root of the application.
The new method should determine the behavior in this
situation (e.g. redirect, signal an error, etc.)."))


(defmethod on-missing-action (app action-name)
  (declare (ignorable app action-name))
  (weblocks.response:redirect
   (weblocks.app:make-uri app "/")))


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
