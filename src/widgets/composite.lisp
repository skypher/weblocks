
(in-package :weblocks)

(export '(composite composite-widgets))

(defwidget composite (widget)
  ((widgets :accessor composite-widgets
	    :initform nil
	    :initarg :widgets
	    :documentation "A list of widgets that are contained in
            this composite widget. For convinience, one can assign a
            single widget to this slot via 'composite-widgets', and
            the value will be converted to a list."))
  (:documentation "A composite widget is simply a container for other
  widgets. The 'widgets' slot accessible with 'composite-widgets'
  accessor contains a list of widgets. When 'render-widget' is invoked
  on the composite, it invokes 'render-widget' on each widget in the
  list."))

(defmethod (setf composite-widgets) (new-value (widget composite))
  (setf (slot-value widget 'widgets)
	(if (or (consp new-value)
		(null new-value))
	    new-value
	    (list new-value))))

(defmethod render-widget-body ((obj composite) &rest args)
  (mapc (lambda (w)
	  (render-widget w))
	(composite-widgets obj)))

(defmethod find-widget-by-path* (path (root composite))
  (find-widget-by-path* (cdr path)
			(car (member (car path)
				     (composite-widgets root)
				     :key #'widget-name
				     :test #'string-equal))))

