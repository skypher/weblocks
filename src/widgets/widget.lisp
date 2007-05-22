
(in-package :weblocks)

(export '(widget widget-name with-widget-header render-widget-body
	  render-widget))

(defclass widget ()
  ((name :accessor widget-name
	 :initform nil
	 :initarg :name
	 :documentation "A name of the widget used in rendering CSS
	 classes.")
   (widget-args :accessor widget-args
		:initform nil
		:initarg :widget-args
		:documentation "A list of arguments that will be
		passed by 'render-widget' to all functions involved in
		rendering the widget."))
  (:documentation "Base class for all widget objects."))

(defgeneric with-widget-header (obj body-fn &rest args)
  (:documentation
   "Renders a header and footer for the widget and calls 'body-fn'
within it. Specialize this function to provide customized headers for
different widgets."))

(defmethod with-widget-header (obj body-fn &rest args)
  (let* ((obj-name (attributize-name (widget-name obj)))
	 (widget-id (when (and obj-name (not (string-equal obj-name "")))
		      obj-name)))
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

(defun render-widget (obj)
  "Renders a widget ('render-widget-body') wrapped in a
header ('with-widget-header')."
  (apply #'with-widget-header obj #'render-widget-body (widget-args obj)))

;;; Make all widgets act as composites to simplify development
(defmethod composite-widgets ((obj widget))
  nil)

(defmethod composite-widgets ((obj function))
  nil)
