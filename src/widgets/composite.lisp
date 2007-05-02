
(in-package :weblocks)

(export '(composite composite-widgets init-composite make-composite))

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

(defun init-composite (comp &rest args)
  "A helper function to initialize a composite widget.

ex:

\(init-composite comp
   \"test1\" (make-instance ...)
   \"test2\" (make-instance ...)"
  (loop for count from 1
        for x in args
        for y in (cdr args)
     when (oddp count)
     do (push-end `(,x . ,y) (composite-widgets comp))))

(defun make-composite (&rest args)
  "Instantiates 'composite' widget via 'make-instance' and forwards it
along with 'args' to 'init-composite'."
  (let ((comp (make-instance 'composite)))
    (apply #'init-composite comp args)
    comp))
