
(in-package :weblocks)

(export '(composite composite-widgets))

(defclass composite ()
  ((widgets :accessor composite-widgets
	    :initform nil
	    :initarg :widgets
	    :documentation "An association list of names and
	      widgets that are contained in this composite
	      widget."))
  (:documentation "A composite widget is simply a container for
  other widgets. The 'widgets' slot accessible with
  'composite-widgets' accessor contains an association list of
  widget names and widgets. When 'render' is invoked on the
  composite, it invokes 'render' on each widget in the list."))

(defmethod render ((obj composite) &rest args)
  (mapc (lambda (w)
	  (render (cdr w)))
	(composite-widgets obj)))
