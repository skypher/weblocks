
(in-package :weblocks)

(export '(composite composite-widgets))

(defwidget composite (widget)
  ((widgets :accessor composite-widgets
	    :initform nil
	    :initarg :widgets
	    :documentation "A list of widgets that are contained in
            this composite widget."))
  (:documentation "A composite widget is simply a container for other
  widgets. The 'widgets' slot accessible with 'composite-widgets'
  accessor contains a list of widgets. When 'render-widget' is invoked
  on the composite, it invokes 'render-widget' on each widget in the
  list."))

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

