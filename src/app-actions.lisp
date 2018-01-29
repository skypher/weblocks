(defpackage #:weblocks/app-actions
  (:use #:cl)
  (:import-from #:weblocks/variables
                #:*current-app*)
  (:export
   #:get-app-actions
   #:get-action
   #:add-action
   #:remove-action
   #:define-action
   #:define-action/cc))
(in-package weblocks/app-actions)

;;
;; Permanent actions
;;

;; TODO: lock-protect this table since users may add actions at runtime
(defvar *apps-actions*
  (make-hash-table)
  "This hash maps app classes to hashes which store application's actions.")

(defun get-app-actions (app)
  (gethash (if (symbolp app)
               app
               (type-of app))
           *apps-actions*))

(defun get-action (action)
  "Returns the action function associated with this symbol in the current app"
  (when (boundp '*current-app*)
    (let ((action-table (get-app-actions *current-app*)))
      (when action-table
        (gethash (if (symbolp action)
                     (symbol-name action)
                     action)
                 action-table)))))

(defun add-action (app-class action-name function-or-name)
  "Remove an action from a webapp.  action-name should be a string, or it
   will be converted to one (to work with the macro).  function-or-name is
   a symbol or a function object (valid object for funcall)"
  (check-type app-class symbol)
  (check-type action-name (or symbol string))
  (macrolet ((action-table (appname)
               `(gethash ,appname *apps-actions*)))
    (unless (action-table app-class)
      (setf (action-table app-class)
            (make-hash-table :test 'equal)))
    (setf (gethash (string-downcase
                    (if (symbolp action-name)
                        (symbol-name action-name)
                        action-name))
                   (action-table app-class)) 
          function-or-name)))
      
(defun remove-action (app-class action-name)
  "Removes a permanent action from a webapp"
  (check-type app-class symbol)
  (check-type action-name (or symbol string))
  
  (let ((table (gethash app-class *apps-actions*)))
    (when table
      (remhash action-name table))))


(defmacro define-action (name app-class action-params &body body)
  "Adds a permanent action to the class's set of permanent actions"
  (assert (find-class app-class))
  `(add-action ',app-class ',name
               (lambda ,action-params
                 ,@body)))

(defmacro define-action/cc (name app-class action-params &body body)
  "Adds a permanent action to the class's set of permanent actions"
  (assert (find-class app-class))
  `(add-action ',app-class ',name
               (lambda/cc ,action-params
                 ,@body)))

