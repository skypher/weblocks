
(in-package :weblocks)

(export '(request-hook *request-hook* eval-dynamic-hooks))

(defclass request-hooks ()
  ((dynamic-action :accessor dynamic-action-hook
		   :initform nil
		   :documentation "A set of functions that establish
                dynamic state around a body function in the action context")
   (pre-action :accessor pre-action-hook
	       :initform nil
	       :documentation "A list of callback functions of no
	       arguments called before user action is evaluated.")
   (post-action :accessor post-action-hook
		:initform nil
		:documentation "A list of callback functions of no
	        arguments called after user action is evaluated.")
   (dynamic-render :accessor dynamic-render-hook
		   :initform nil
		   :documentation "A set of functions that establish
                dynamic state around a body function in the render context")
   (pre-render :accessor pre-render-hook
	       :initform nil
	       :documentation "A list of callback functions of no
	       arguments called before widgets are rendered.")
   (post-render :accessor post-render-hook
		:initform nil
		:documentation "A list of callback functions of no
	        arguments called after widgets are rendered."))
  (:documentation "A data structure that maintains appropriate
  callback functions used to hook into request evaluation."))

(defparameter *application-request-hooks* (make-instance 'request-hooks)
  "A request hook object used in the application scope.")

(defun session-request-hooks ()
  "A request hook object used in the session scope."
  (if (webapp-session-value 'request-hooks)
      (webapp-session-value 'request-hooks)
      (setf (webapp-session-value 'request-hooks)
	    (make-instance 'request-hooks))))

(defvar *request-hook*)
(setf (documentation '*request-hook* 'variable)
      "A request hook object used in the request scope.")

(defmacro hook-by-scope (scope)
  "Returns a place which contains the hook object for the specified
scope."
  (ecase scope
    (:application '*application-request-hooks*)
    (:session '(session-request-hooks))
    (:request '*request-hook*)))

(defmacro request-hook (scope location)
  "Allows access to a series of hooks exposed by 'handle-client-request'.

scope - the scope of the hook. Can be set to :application, :session,
or :request. An :application hook is maintained throughout the
lifetime of the entire application. A :session hook is destroyed along
with the session. A :request hook is only valid for the request.

location - the location of the hook. Can be set
to :dynamic-action :pre-action, :post-action, 
   :dynamic-render :pre-render, and :post-render.

The macro returns a place that can be used to push a callback function
of no arguments."
  (ecase location
    (:dynamic-action `(dynamic-action-hook (hook-by-scope ,scope)))
    (:pre-action `(pre-action-hook (hook-by-scope ,scope)))
    (:post-action `(post-action-hook (hook-by-scope ,scope)))
    (:dynamic-render `(dynamic-render-hook (hook-by-scope ,scope)))
    (:pre-render `(pre-render-hook (hook-by-scope ,scope)))
    (:post-render `(post-render-hook (hook-by-scope ,scope)))))

(defmacro eval-hook (location)
  "Evaluates the appropriate hook. See 'request-hook'."
  `(progn
     (mapc #'funcall (request-hook :application ,location))
     (mapc #'funcall (request-hook :session ,location))
     (mapc #'funcall (request-hook :request ,location))))

(defmacro with-dynamic-hooks ((type) &rest body)
  "Performs nested calls of all the hooks of type, the innermost call is
   a closure over the body expression.  Dynamic action hooks take one
   argument, which is a list of dynamic hooks.  In the inner context, they
   apply the first element of the list to the rest

   An example of a dynamic hook:
  
   (defun transaction-hook (inner-fns)
     (with-transaction ()
       (unless (null inner-fns)
         (funcall (first inner-fns) (rest inner-fns)))))"
  (with-gensyms (null-list)
    `(eval-dynamic-hooks 
      (append (request-hook :application ,type)
	      (request-hook :session ,type)
	      (request-hook :request ,type)
	      (list (lambda (,null-list) 
		      (assert (null ,null-list))
		      ,@body))))))

(defun eval-dynamic-hooks (var)
  "A helper function that makes it easier to write dynamic hooks.

   (defun my-hook (hooks)
     (with-my-context ()
        (eval-dynamic-hooks hooks)))
  "
  (let ((list (etypecase var 
		(symbol (symbol-value var))
		(list var))))
    (unless (null list)
      (funcall (first list) (rest list)))))

