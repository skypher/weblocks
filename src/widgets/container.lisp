
(in-package :weblocks)

(export '(container))

(defwidget container (widget)
  ((children :accessor widget-children
             :initform nil
             :initarg :children
             :documentation "A list of renderable children of this container."))
  (:documentation "A widget containing other widgets. Acts as a non-leaf
  tree node. Manages parent-child relation between itself and its child
  widgets."))

(defmethod initialize-instance :after ((obj container) &rest initargs &key children &allow-other-keys)
  (declare (ignore initargs))
  ;; We need this in order to properly initialize the parent slot in all
  ;; the children, which requires calling the (setf widget-children)
  ;; method.
  (setf (slot-value obj 'children) nil)
  (setf (widget-children obj) children))

(defmethod (setf widget-children) (new-value (cont container))
  "Assign new children to the container and update their parents."
  (symbol-macrolet ((children (slot-value cont 'children)))
    ;; we're a parent of new widgets we're passed
    (let ((new-widgets (ensure-list new-value)))
      (mapc (lambda (child)
	      (setf (widget-parent child) cont))
	    new-widgets)
      (setf children new-widgets))))

(defmethod render-widget-body ((widget container) &rest args)
  (error "Can't render the children of a pure container."))

