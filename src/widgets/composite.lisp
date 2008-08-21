
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

(defmethod initialize-instance :after ((obj composite) &rest initargs &key widgets &allow-other-keys)
  (declare (ignore initargs))
  ;; We need this to properly initialize values
  (setf (slot-value obj 'widgets) nil)
  (setf (composite-widgets obj) widgets))

(defmethod make-widget-place-writer ((composite composite) (child widget))
  "Return a function encapsulating the list where the child is in the car
   of the first element."
  (let ((place (member child (composite-widgets composite))))
    (unless place
      (error "Widget ~S cannot be found in parent ~S."
	     child composite))
    (lambda (&optional callee)
      (format t "Placing callee ~A into ~A" callee place)
      (if callee
	  (progn (rplaca place callee)
		 (setf (widget-parent callee) composite)
		 (mark-dirty composite))
	  (car place)))))

(defmethod (setf composite-widgets) (new-value (composite composite))
  ;; We're no longer a parent of widgets we hold
  (loop for widget in (ensure-list (slot-value composite 'widgets))
     do (setf (widget-parent widget) nil))
  (let ((new-widgets (if (or (consp new-value)
			     (null new-value))
			 new-value
			 (list new-value))))
    ;; But we're a part of new widgets we're passed
    (loop for widget in new-widgets
       do (if (widget-parent widget)
	      (error "Widget ~a already has a parent." widget)
	      (setf (widget-parent widget) composite)))
    (setf (slot-value composite 'widgets)
	  new-widgets)))

(defmethod render-widget-body ((obj composite) &rest args)
  (declare (ignore args))
  (mapc (lambda (w)
	  (render-widget w))
	(composite-widgets obj)))

(defmethod find-widget-by-path* (path (root composite))
  (find-widget-by-path* (cdr path)
			(car (member (car path)
				     (composite-widgets root)
				     :key #'widget-name
				     :test #'string-equal))))

