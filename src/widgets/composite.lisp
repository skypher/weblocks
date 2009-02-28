
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

(defun cons-in-list-p (cell list)
  "Simple test that the cell is still part of list to validate the place
   stored in the closure"
  (cond ((null list) nil)
        ((eq list cell) t)
        (t (cons-in-list-p cell (cdr list)))))

(defmethod make-widget-place-writer ((composite composite) child)
  "Return a function encapsulating the list where the child is in the car
   of the first element."
  (let ((place (member child (composite-widgets composite))))
    (if place
      (lambda (&optional (callee nil callee-supplied-p))
        (assert (cons-in-list-p place (composite-widgets composite)))
        (cond
          (callee-supplied-p
            (check-type callee widget-designator
                        "a potential member of a composite")
            (rplaca place callee)
            (setf (widget-parent callee) composite)
            (mark-dirty composite))
          (t (car place))))
      (lambda (&rest args)
        (style-warn 'widget-not-in-parent :widget child :parent composite)
        (mark-dirty composite)))))

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



