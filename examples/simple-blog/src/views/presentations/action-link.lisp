;;;; mode: common-lisp; mode: paredit; mode: slime
(in-package :simple-blog)

(defclass action-link-presentation (text-presentation)
  ((action :initform nil
	   :initarg :action-fn
	   :accessor action-link-presentation-body
	   :documentation "This needs to be a function that accepts at
	   least the first six arguments passed to
	   render-view-field-value in order to get access to the
	   object being rendered.")))

(defmethod render-view-field-value (value (presentation action-link-presentation)
				    field view widget obj &rest args
				    &key highlight &allow-other-keys)
  (declare (ignore highlight args))
  (if (null value)
      (call-next-method) ;; just render as text
      (render-link (lambda (&rest args)
		     (funcall (action-link-presentation-body presentation)
			      widget))
		   value)))