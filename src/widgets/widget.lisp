
(in-package :weblocks)

(export '(widget-class defwidget widget widget-name widget-args
	  with-widget-header render-widget-body render-widget
	  make-dirty find-widget-by-path* find-widget-by-path))

(defclass widget-class (standard-class)
  ()
  (:documentation "A metaclass used for all widget classes. A
  custom metaclass is necessary to specialize
  'slot-value-using-class'."))

;;; Necessary to allow deriving
(defmethod validate-superclass ((class widget-class)
				(superclass standard-class))
  t)

(defun generate-widget-id ()
  "Generates a unique ID that can be used to identify a widget."
  (gensym))

(defmacro defwidget (&body body)
  "A macro used to define new widget classes. Behaves exactly as
defclass, except adds 'widget-class' metaclass specification."
  `(defclass ,@body
       (:metaclass widget-class)))

(defwidget widget ()
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
		rendering the widget.")
   (propagate-dirty :accessor widget-propagate-dirty
		    :initform nil
		    :initarg :propagate-dirty
		    :documentation "A list of widget paths (see
                    'find-widget-by-path') or widgets each of which
                    will be made dirty when this widget is made dirty
                    via a POST request. This slot allows setting up
                    dependencies between widgets that will make
                    multiple widgets update automatically during AJAX
                    requests."))
  #+lispworks (:optimize-slot-access nil)
  (:documentation "Base class for all widget objects."))

(defgeneric with-widget-header (obj body-fn &rest args)
  (:documentation
   "Renders a header and footer for the widget and calls 'body-fn'
within it. Specialize this function to provide customized headers for
different widgets."))

(defmethod with-widget-header (obj body-fn &rest args)
  (let* ((obj-name (attributize-name (widget-name obj))) ; obj-name may be null in functions
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
can do custom rendering without much boilerplate.

Another implementation allows rendering strings."))

(defmethod render-widget-body ((obj function) &rest args)
  (apply obj args))

(defmethod render-widget-body ((obj string) &rest args &key id class &allow-other-keys)
  (with-html
    (:p :id id :class class (str obj))))

(defmethod widget-name ((obj function))
  nil)

(defmethod widget-args ((obj function))
  nil)

(defmethod widget-name ((obj string))
  nil)

(defmethod widget-args ((obj string))
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

(defun make-dirty (w &key putp)
  "Adds a widget to a list of dirty widgets. Normally used during an
AJAX request. If there are any widgets in the 'propagate-dirty' slot
of 'w' and 'putp' is true, these widgets are added to the dirty list
as well."
  (declare (special *dirty-widgets*))
  (when (functionp w)
    (error "AJAX is not supported for functions. Convert the function
    into a CLOS object derived from 'widget'."))
  (setf *dirty-widgets* (adjoin w *dirty-widgets*))
  (when putp
    (mapc
     (lambda (i)
       (setf *dirty-widgets* (adjoin i *dirty-widgets*)))
     (remove nil (loop for i in (widget-propagate-dirty w)
		    collect (find-widget-by-path i))))))

;;; When slots of a widget are modified, the widget should be marked
;;; as dirty to service AJAX calls.
(defmethod (setf slot-value-using-class) (new-value (class widget-class) (object widget) slot-name)
  (make-dirty object)
  (call-next-method new-value class object slot-name))

(defgeneric find-widget-by-path* (path root)
  (:documentation
   "Returns a widget object located at 'path', where 'path' is a list
of widget names starting from root. For convinience, 'path' can be a
widget object, in which case it is simply returned.

'root' - a widget to start the search from."))

(defmethod find-widget-by-path* ((path (eql nil)) root)
  root)

(defmethod find-widget-by-path* ((path widget) root)
  path)

(defmethod find-widget-by-path* (path (root (eql nil)))
  nil)

(defun find-widget-by-path (path &optional (root (session-value 'root-composite)))
  (find-widget-by-path* path root))
