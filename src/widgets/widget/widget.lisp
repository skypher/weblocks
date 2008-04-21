
(in-package :weblocks)

(export '(defwidget widget widget-name widget-propagate-dirty
	  widget-rendered-p widget-continuation widget-parent
	  widget-prefix-fn widget-suffix-fn
	  widget-public-dependencies with-widget-header
	  render-widget-body widget-css-classes render-widget
	  mark-dirty widget-dirty-p find-widget-by-path*
	  find-widget-by-path))

(defun generate-widget-id ()
  "Generates a unique ID that can be used to identify a widget."
  (let ((new-widget-id (1+ (or (session-value 'last-widget-id) -1))))
    (setf (session-value 'last-widget-id) new-widget-id)
    (format nil "~A" new-widget-id)))

(defmacro defwidget (name direct-superclasses &body body)
  "A macro used to define new widget classes. Behaves exactly as
defclass, except adds 'widget-class' metaclass specification and
inherits from 'widget' if no direct superclasses are provided."
  `(defclass ,name ,(or direct-superclasses '(widget))
       ,@body
       (:metaclass widget-class)))

(defclass widget ()
  ((name :accessor widget-name
	 :initform (generate-widget-id)
	 :initarg :name
	 :documentation "A name of the widget used in rendering CSS
	 classes. If the name is not provided it will be generated
	 automatically with 'generate-widget-id'.")
   (propagate-dirty :accessor widget-propagate-dirty
		    :initform nil
		    :initarg :propagate-dirty
		    :documentation "A list of widget paths (see
                    'find-widget-by-path') or widgets each of which
                    will be made dirty when this widget is made dirty
                    via a POST request. This slot allows setting up
                    dependencies between widgets that will make
                    multiple widgets update automatically during AJAX
                    requests.")
   (renderedp :accessor widget-rendered-p
	      :initform nil
	      :affects-dirty-status-p nil
	      :documentation "This slot holds a boolean flag
              indicating whether the widget has been rendered at least
              once. Because marking unrendered widgets as dirty may
              cause JS problems, 'mark-dirty' will use this flag to
              determine the status of a widget.")
   (continuation :accessor widget-continuation
		 :initform nil
		 :documentation "Stores the continuation object for
                 widgets that were invoked via one of the do-*
                 functions ('do-page', etc.). When 'answer' is called
                 on a widget, this value is used to resume the
                 computation.")
   (parent :accessor widget-parent
	   :initform nil
	   :documentation "Stores the 'parent' of a widget,
	   i.e. the composite widget in which this widget is
	   located, if any. This value is automatically set by
	   composites with the slot 'composite-widgets' is
	   set. Note, a widget can only have one parent at any
	   given time.")
   (widget-prefix-fn :initform nil
	             :initarg :widget-prefix-fn
	             :accessor widget-prefix-fn
		     :documentation "A function called prior to
	             rendering the widget body. The function should
	             expect the widget as well as any additional
	             arguments passed to the widget.")
   (widget-suffix-fn :initform nil
	             :initarg :widget-suffix-fn
		     :accessor widget-suffix-fn
		     :documentation "A function called after rendering
	             the widget body. The function should expect the
	             widget as well as any additional arguments passed
	             to the widget."))
  #+lispworks (:optimize-slot-access nil)
  (:metaclass widget-class)
  (:documentation "Base class for all widget objects."))

;;; Define widget-rendered-p for objects that don't derive from
;;; 'widget'
(defmethod widget-rendered-p (obj)
  nil)

(defmethod (setf widget-rendered-p) (obj val)
  nil)

;;; Define widget-parent for objects that don't derive from 'widget'
(defmethod widget-parent (obj)
  (declare (ignore obj))
  nil)

(defmethod (setf widget-parent) (obj val)
  (declare (ignore obj val))
  nil)

(defgeneric widget-public-dependencies (obj)
  (:documentation
   "Whenever a widget is rendered by weblocks in a non-AJAX request,
this function is called to determine which stylesheets and javascript
files the widget depends on. These dependencies are then included into
the header of the containing page.

The function must return a list of dependencies. Each member of the
list must be the virtual path to the file (see convinience functions
'public-file-relative-path' and 'public-files-relative-paths'). The
extension (currently only \".css\" and \".js\") will be used by
weblocks to determine how to include the dependency into the page
header.

The default implementation uses the following protocol to determine if
a widget has dependencies. It looks under
*public-files-path*/scripts/[attributized-widget-class-name].js and
*public-files-path*/stylesheets/[attributized-widget-class-name].css. If
it finds the aforementioned files, it returns them as
dependencies. This way the developer can simply place relevant files
in the appropriate location and not worry about specializing this
function most of the time.

'widget-public-dependencies' also returns widgets for the superclasses
of 'obj', because it's intuitive to assume that a widget will use the
stylesheets of its superclasses."))

(defmethod widget-public-dependencies (obj)
  (reverse
   (remove nil
	   (flatten
	    (loop for i in (superclasses obj :proper? nil)
	       until (string-equal (class-name i) 'standard-object)
	       collect (widget-public-dependencies-aux (class-name i)))))))

(defmethod widget-public-dependencies ((obj symbol))
  (widget-public-dependencies-aux obj))

(defun widget-public-dependencies-aux (widget-class-name)
  "A utility function used to help 'widget-public-dependencies'
implement its functionality. Determines widget dependencies for a
particular class name."
  (let (dependencies
	(attributized-widget-class-name (attributize-name widget-class-name)))
    (loop for type in '(:stylesheet :script)
       do (when (probe-file (merge-pathnames
			     (public-file-relative-path type attributized-widget-class-name)
			     *public-files-path*))
	    (push (public-file-relative-path type attributized-widget-class-name) dependencies)))
    (remove nil dependencies)))

(defgeneric with-widget-header (obj body-fn &rest args &key
				    widget-prefix-fn widget-suffix-fn
				    &allow-other-keys)
  (:documentation
   "Renders a header and footer for the widget and calls 'body-fn'
within it. Specialize this function to provide customized headers for
different widgets.

'widget-prefix-fn' and 'widget-suffix-fn' allow specifying functions
that will be applied before and after the body is rendered.")
  (:method (obj body-fn &rest args
	    &key widget-prefix-fn widget-suffix-fn
	    &allow-other-keys)
    (let* ((obj-name (attributize-name (widget-name obj))) ; obj-name may be null in functions
	   (widget-id (when (and obj-name (not (string-equal obj-name "")))
			(attributize-name obj-name))))
      (with-html
	(:div :class (widget-css-classes obj)
	      :id widget-id
	      (safe-apply widget-prefix-fn obj args)
	      (apply body-fn obj args)
	      (safe-apply widget-suffix-fn obj args))))))

(defgeneric render-widget-body (obj &rest args &key &allow-other-keys)
  (:documentation
   "A generic function that renders a widget in its current state. In
order to actually render the widget, call 'render-widget' instead.

'obj' - widget object to render.

One of the implementations allows \"rendering\" functions. When
'render-widget' is called on a function, the function is simply
called. This allows to easily add functions to composite widgets that
can do custom rendering without much boilerplate. Similarly symbols
that are fbound to functions can be treated as widgets.

Another implementation allows rendering strings."))

(defmethod render-widget-body ((obj symbol) &rest args)
  (if (fboundp obj)
      (apply obj args)
      (error "Cannot render ~A as widget. Symbol not bound to a
      function." obj)))

(defmethod render-widget-body ((obj function) &rest args)
  (apply obj args))

(defmethod render-widget-body ((obj string) &rest args &key id class &allow-other-keys)
  (with-html
    (:p :id id :class class (str obj))))

(defgeneric widget-css-classes (widget)
  (:documentation "Returns a string that represents applicable CSS
classes for 'widget'. Normally includes the class name and the names
of its subclasses. It is safe to assume that the class 'widget' will
be present for all widgets."))

(defmethod widget-css-classes ((obj widget))
  (apply #'concatenate 'string
	 (intersperse
	  (mapcar (compose #'attributize-name #'class-name)
		  (reverse
		   (loop for i in (superclasses obj :proper? nil)
		      until (string-equal (class-name i) 'standard-object)
		      collect i))) " ")))

(defmethod widget-css-classes ((obj symbol))
  (format nil "widget function ~A" (attributize-name obj)))

(defmethod widget-css-classes ((obj function))
  "widget function")

(defmethod widget-css-classes ((obj string))
  "widget string")

(defmethod widget-name ((obj symbol))
  obj)

(defmethod widget-name ((obj function))
  nil)

(defmethod widget-name ((obj string))
  nil)

(defmethod widget-prefix-fn (obj)
  nil)

(defmethod widget-suffix-fn (obj)
  nil)

(defun render-widget (obj &key inlinep)
  "Renders a widget ('render-widget-body') wrapped in a
header ('with-widget-header'). If 'inlinep' is true, renders the
widget without a header.

Additionally, calls 'widget-public-dependencies' and adds the returned
items to *page-public-dependencies*. This is later used by Weblocks to
declare stylesheets and javascript links in the page header."
  (declare (special *page-public-dependencies*))
  (setf *page-public-dependencies*
	(append *page-public-dependencies*
		(widget-public-dependencies obj)))
  (if inlinep
      (funcall #'render-widget-body obj)
      (apply #'with-widget-header obj #'render-widget-body
	     (append
	      (when (widget-prefix-fn obj)
		(list :widget-prefix-fn (widget-prefix-fn obj)))
	      (when (widget-suffix-fn obj)
		(list :widget-suffix-fn (widget-suffix-fn obj))))))
  (setf (widget-rendered-p obj) t))

;;; Make all widgets act as composites to simplify development
(defmethod composite-widgets ((obj widget))
  nil)

(defmethod composite-widgets ((obj function))
  nil)

(defmethod composite-widgets ((obj symbol))
  nil)

(defmethod composite-widgets ((obj string))
  nil)

(defgeneric mark-dirty (w &key putp)
  (:documentation
   "Default implementation adds a widget to a list of dirty
widgets. Normally used during an AJAX request. If there are any
widgets in the 'propagate-dirty' slot of 'w' and 'putp' is true, these
widgets are added to the dirty list as well.

Note, this function is automatically called when widget slots are
modified, unless slots are marked with affects-dirty-status-p."))

(defmethod mark-dirty ((w widget) &key putp)
  (declare (special *dirty-widgets*))
  (when (functionp w)
    (error "AJAX is not supported for functions. Convert the function
    into a CLOS object derived from 'widget'."))
  (ignore-errors
    (when (widget-rendered-p w)
      (setf *dirty-widgets* (adjoin w *dirty-widgets*)))
    (when putp
      (mapc #'mark-dirty
	    (remove nil (loop for i in (widget-propagate-dirty w)
			   collect (find-widget-by-path i)))))))

(defun widget-dirty-p (w)
  "Returns true if the widget 'w' has been marked dirty."
  (member w *dirty-widgets*))

;;; When slots of a widget are modified, the widget should be marked
;;; as dirty to service AJAX calls.
(defmethod (setf slot-value-using-class) (new-value (class widget-class) (object widget)
					  (slot-name widget-effective-slot-definition))
  (when (widget-slot-affects-dirty-status-p slot-name)
    (mark-dirty object))
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

(defmethod print-object ((obj widget) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~s" (slot-value obj 'name))))

