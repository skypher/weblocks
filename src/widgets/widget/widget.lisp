
(in-package :weblocks)

(export '(defwidget widget widget-name
          widget-propagate-dirty widget-rendered-p widget-continuation
          widget-parent widget-prefix-fn widget-suffix-fn
          with-widget-header
          render-widget-body widget-css-classes render-widget mark-dirty
          widget-dirty-p find-widget-by-path* find-widget-by-path
          *current-widget*))

(defmacro defwidget (name direct-superclasses &body body)
  "A macro used to define new widget classes. Behaves exactly as
defclass, except adds 'widget-class' metaclass specification and
inherits from 'widget' if no direct superclasses are provided."
  `(progn
     (defclass ,name ,(or direct-superclasses '(widget))
       ,@body
       (:metaclass widget-class))
     (defmethod per-class-dependencies append ((obj ,name))
       (declare (ignore obj))
       (dependencies-by-symbol (quote ,name)))))

(defclass widget (dom-object-mixin)
  ((propagate-dirty :accessor widget-propagate-dirty
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

;; Process the :name initarg and set the dom-id accordingly. Note that
;; it is possible to pass :name nil, which simply means that objects
;; will render without id in generated HTML.
(defmethod initialize-instance :after ((obj widget) &key name &allow-other-keys)
  (when name (setf (dom-id obj) name)))

(defgeneric widget-name (obj)
  (:documentation "An interface to the DOM id of a widget. Provides
  access to the underlying implementation, can return either a symbol, a
  string, or nil.")
  (:method ((obj widget)) (ensure-dom-id obj))
  (:method ((obj symbol)) obj)
  (:method ((obj function)) nil)
  (:method ((obj string)) nil))

(defmethod (setf widget-name) (name (obj widget))
  (setf (dom-id obj) name))


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
    (with-html
      (:div :class (dom-classes obj)
	    :id (dom-id obj)
	    (safe-apply widget-prefix-fn obj args)
	    (apply body-fn obj args)
	    (safe-apply widget-suffix-fn obj args)))))

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

(defmethod widget-prefix-fn (obj)
  nil)

(defmethod widget-suffix-fn (obj)
  nil)

(defun render-widget (obj &key inlinep &allow-other-keys)
  "Renders a widget ('render-widget-body') wrapped in a
header ('with-widget-header'). If 'inlinep' is true, renders the
widget without a header.

Additionally, calls 'dependencies' and adds the returned items to
*page-dependencies*. This is later used by Weblocks to declare
stylesheets and javascript links in the page header."
  (declare (special *page-dependencies*))
  (if (ajax-request-p)
    (dolist (dep (dependencies obj))
      (send-script
	(ps* `(,(typecase dep
                  (stylesheet-dependency 'include_css)
                  (script-dependency 'include_dom))
               ,(make-webapp-uri (puri:render-uri (dependency-url dep) nil))))))
    (setf *page-dependencies*
	  (append *page-dependencies* (dependencies obj))))
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

(defun find-widget-by-path (path &optional (root (root-composite)))
  (find-widget-by-path* path root))

(defmethod print-object ((obj widget) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~s" (ensure-dom-id obj))))

