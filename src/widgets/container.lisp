
(in-package :weblocks)

(export '(container widget-children container-update-direct-children
	  update-widget-tree))

(defwidget container (widget)
  ((children :accessor widget-children
             :initform nil
             :initarg :children
             :documentation "A list of renderable children of this container."))
  (:documentation "A widget containing other widgets. Acts as a non-leaf tree node."))

(defmethod initialize-instance :after ((obj container) &rest initargs &key children &allow-other-keys)
  (declare (ignore initargs))
  ;; We need this in order to properly initialize the parent slot in all
  ;; the children, which requires calling the (setf widget-children)
  ;; method.
  (setf (slot-value obj 'children) nil)
  (setf (widget-children obj) children))

(defparameter *override-parent-p* nil
  "If set, allow parent overriding in (SETF WIDGET-CHILDREN).")

(defmethod (setf widget-children) (new-value (cont container))
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

(defgeneric container-update-direct-children (cont)
  (:documentation "Update the list of our direct renderable children.")
  (:method ((cont container)) nil))

(defgeneric update-widget-tree (cont)
  (:documentation "Update widget the tree so that it represents the renderable part of the widget tree.")
  (:method ((obj null)) (assert nil))	; bug?
  (:method ((obj widget)) nil)		; widgets are leaves by default
  (:method ((obj function)) nil)	; functions have no children
  (:method ((obj string)) nil)		; and neither do strings
  (:method ((cont container))
     ;; first let's get our own children straight
     (container-update-direct-children cont)
     ;; now go down the tree and update the indirect children
     (mapc #'update-widget-tree (widget-children cont))))

(defmethod render-widget-body ((widget container) &rest args)
  (error "Can't render the children of a pure container."))

