
(in-package :weblocks)

(export '(widget widget-name widget-args with-widget-header
	  render-widget-body render-widget))

(defun generate-widget-id ()
  "Generates a unique ID that can be used to identify a widget."
  (gensym))

(defclass widget ()
  ((name :accessor widget-name
	 :initform (generate-widget-id)
	 :initarg :name
	 :documentation "A name of the widget used in rendering CSS
	 classes. If the name is not provided it will be generated
	 automatically with 'generate-widget-id'.")
   (widget-args :accessor widget-args
		:initform nil
		:initarg :widget-args
		:documentation "A list of arguments that will be
		passed by 'render-widget' to all functions involved in
		rendering the widget."))
  #+lispworks (:optimize-slot-access nil)
  (:documentation "Base class for all widget objects."))

(defgeneric with-widget-header (obj body-fn &rest args)
  (:documentation
   "Renders a header and footer for the widget and calls 'body-fn'
within it. Specialize this function to provide customized headers for
different widgets."))

(defmethod with-widget-header (obj body-fn &rest args)
  (let* ((obj-name (attributize-name (widget-name obj)))
	 (widget-id (when (and obj-name (not (string-equal obj-name "")))
		      (attributize-name obj-name))))
    (with-html
      (:div :class (concatenate 'string
				"widget "
				(attributize-name (class-name (class-of obj))))
	    :id widget-id
	    (apply body-fn obj args)))))

(defgeneric render-widget-body (obj &rest args)
  (:documentation
   "A generic function that renders a widget in its current state. In
order to actually render the widget, call 'render-widget' instead.

'obj' - widget object to render.

One of the implementations allows \"rendering\" functions. When
'render-widget' is called on a function, the function is simply
called. This allows to easily add functions to composite widgets that
can do custom rendering without much boilerplate."))

(defmethod render-widget-body ((obj function) &rest args)
  (apply obj args))

(defmethod widget-name ((obj function))
  nil)

(defmethod widget-args ((obj function))
  nil)

(defun render-widget (obj &key inlinep)
  "Renders a widget ('render-widget-body') wrapped in a
header ('with-widget-header'). If 'inlinep' is true, renders the
widget without a header."
  (if inlinep
      (apply #'render-widget-body obj (widget-args obj))
      (apply #'with-widget-header obj #'render-widget-body (widget-args obj))))

;;; Make all widgets act as composites to simplify development
(defmethod composite-widgets ((obj widget))
  nil)

(defmethod composite-widgets ((obj function))
  nil)

(defun make-dirty (w)
  "Adds a widget to a list of dirty widgets. Normally used during an
AJAX request."
  (declare (special *dirty-widgets*))
  (setf *dirty-widgets* (cons w *dirty-widgets*)))

;;; When slots of a widget are modified, the widget should be marked
;;; as dirty to service AJAX calls.
(defmethod (setf slot-value-using-class) :around (new-value class (object widget) slot-name)
  (make-dirty object)
  (call-next-method new-value class object slot-name))
