
(in-package :weblocks)

(export '(request-hook *request-hook*))

(defclass request-hooks ()
  ((pre-action :accessor pre-action-hook
	       :initform nil
	       :documentation "A list of callback functions of no
	       arguments called before user action is evaluated.")
   (post-action :accessor post-action-hook
		:initform nil
		:documentation "A list of callback functions of no
	        arguments called after user action is evaluated.")
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
  (if (session-value 'request-hooks)
      (session-value 'request-hooks)
      (setf (session-value 'request-hooks)
	    (make-instance 'request-hooks))))

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
to :pre-action, :post-action, :pre-render, and :post-render.

The macro returns a place that can be used to push a callback function
of no arguments."
  (ecase location
    (:pre-action `(pre-action-hook (hook-by-scope ,scope)))
    (:post-action `(post-action-hook (hook-by-scope ,scope)))
    (:pre-render `(pre-render-hook (hook-by-scope ,scope)))
    (:post-render `(post-render-hook (hook-by-scope ,scope)))))

(defmacro eval-hook (location)
  "Evaluates the appropriate hook. See 'request-hook'."
  `(progn
     (mapc #'funcall (request-hook :application ,location))
     (mapc #'funcall (request-hook :session ,location))
     (mapc #'funcall (request-hook :request ,location))))

