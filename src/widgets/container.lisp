
(in-package :weblocks)

(export '(container container-children))

(defwidget container (widget)
  ((children :accessor container-children
             :initform nil
             :initarg :children
             :documentation "The children of this container. An alist where
             the CAR is a widget and the CDR contains book-keeping information."))
  (:documentation "A widget containing other widgets.
             Acts as a non-leaf tree node."))

(defmethod initialize-instance :after ((obj container) &rest initargs &key children &allow-other-keys)
  (declare (ignore initargs))
  ;; We need this to properly initialize values
  (setf (slot-value obj 'children) nil)
  (setf (container-children obj) children))

(defparameter *override-parent-p* nil
  "If set, allow parent overriding in (SETF CONTAINER-CHILDREN).")

(defmethod (setf container-children) (new-value (cont container))
  "Assign new children to the container and update their parents.
Signals an error if one of the children already has a parent
unless *OVERRIDE-PARENT-P* is set."
  ;; we're no longer a parent of widgets we hold
  (symbol-macrolet ((children (slot-value cont 'children)))
    (mapcar
      (lambda (child)
        (setf (widget-parent child) nil))
      (ensure-list children))
    ;; but we're a parent of new widgets we're passed
    (let ((new-widgets (ensure-list new-value)))
      (mapcar (lambda (child)
                (if (and (widget-parent child) (not *override-parent-p*))
                  (error "Widget ~A already has a parent." child)
                  (setf (widget-parent child) cont)))
              new-widgets)
      (setf children new-widgets))))

(defgeneric container-update-children (cont)
  (:documentation "Update the children of the container.")
  (:method ((obj null))
    nil) ;; bug?
  (:method ((cont container))
     ;; first let's get our own children straight
     (container-update-direct-children cont)
     ;; now update the indirect children
     (dolist (child (container-children cont))
       (when (typep child 'container)
         (container-update-children child)))))

(defgeneric container-update-direct-children (cont)
  (:method ((cont container))
    nil))

(defmethod render-widget-body ((widget container) &rest args)
  (error "Can't render the children of a pure container."))

