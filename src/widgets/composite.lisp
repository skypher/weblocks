
(in-package :weblocks)

(export '(composite composite-widgets))

(defwidget composite (container)
  ()
  (:documentation "A composite widget is simply a container for other
  widgets. The 'widgets' slot accessible with 'composite-widgets'
  accessor contains a list of widgets. When 'render-widget' is invoked
  on the composite, it invokes 'render-widget' on each widget in the
  list."))

(defmethod initialize-instance :around
    ((obj composite) &rest initargs &key widgets children &allow-other-keys)
  ;; FIXME: remove :widgets from initargs and pass as :children
  (if widgets
      (progn
        (warn "The :WIDGETS initarg to composites is deprecated; ~
               use :CHILDREN instead. This time I will do it for you.")
        (remf initargs :widgets)
        (apply #'call-next-method obj :children (append children widgets) initargs))
      (call-next-method)))

;;; backwards compatibility
(defmethod composite-widgets ((comp composite))
  (widget-children comp))

(defmethod (setf composite-widgets) (value (comp composite))
  (setf (widget-children comp) value))

(defmethod render-widget-body ((obj composite) &rest args)
  (declare (ignore args))
  (mapc (lambda (w)
	  (render-widget w))
	(widget-children obj)))

(defmethod find-widget-by-path* (path (root composite))
  (find-widget-by-path* (cdr path)
			(car (member (car path)
				     (composite-widgets root)
				     :key #'widget-name
				     :test #'string-equal))))

