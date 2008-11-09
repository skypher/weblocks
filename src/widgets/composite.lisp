
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

(defmethod make-widget-place-writer ((composite composite) child)
  "Return a function encapsulating the list where the child is in the car
   of the first element."
  (let ((place (member child (composite-widgets composite))))
    (unless place
      (error "Widget ~S cannot be found in parent ~S."
	     child composite))
    (lambda (&optional (callee nil callee-supplied-p))
      (assert (cons-in-list-p place (composite-widgets composite)))
      (cond (callee-supplied-p
	     (check-type callee valid-widget
			 "a potential member of a composite")
	     (rplaca place callee)
	     (setf (widget-parent callee) composite)
	     (mark-dirty composite))
	    (t (car place))))))

(defun cons-in-list-p (cell list)
  "Simple test that the cell is still part of list to validate the place
   stored in the closure"
  (cond ((null list) nil)
	((eq list cell) t)
	(t (cons-in-list-p cell (cdr list)))))

(defmethod (setf composite-widgets) (new-value (comp composite))
  "Assign new children to the composite and update their parents.
Signals an error if one of the children already has a parent
unless *OVERRIDE-PARENT-P* is set."
  ;; we're no longer a parent of widgets we hold
  (symbol-macrolet ((children (slot-value comp 'widgets)))
    (mapcar
      (lambda (child)
        (setf (widget-parent child) nil))
      (ensure-list children))
    ;; but we're a parent of new widgets we're passed
    (let ((new-widgets (ensure-list new-value)))
      (mapcar (lambda (child)
                (setf (widget-parent child) comp))
              new-widgets)
      (setf children new-widgets))))

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

