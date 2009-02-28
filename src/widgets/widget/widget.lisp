
(in-package :weblocks)

(export '(defwidget widget widget-name ensure-widget-methods
          widget-propagate-dirty widget-rendered-p widget-continuation
          widget-parent widget-children widget-prefix-fn widget-suffix-fn
          with-widget-header
          render-widget-body widget-css-classes render-widget mark-dirty
          widget-dirty-p find-widget-by-path* find-widget-by-path
          *current-widget*
          *override-parent-p*))

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

(defgeneric widget-current-navigation-url (widget)
  (:documentation "Return the current navigation URL for WIDGET
                  as computed at the dispatching stage.")
  (:method ((widget string))
           nil)
  (:method ((widget function))
    "FIXME: should find out somehow..."
    nil))

(defclass widget (dom-object-mixin)
  ((current-navigation-url :accessor widget-current-navigation-url
                           :type (or string null) ;; XXX unbound instead of null
                           :initform nil)
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

(defmethod widget-current-navigation-url ((widget widget))
  (log-message :debug "~%WIDGET-CNURL: for widget ~A~%" widget)
  (acond
    ((eql widget (root-composite))
     (log-message :debug "WIDGET-CNURL: root-composite~%")
     "/")
    ((slot-value widget 'current-navigation-url)
     (log-message :debug "WIDGET-CNURL: has own: ~S~%" it)
     it)
    ((widget-parent widget)
     (log-message :debug "WIDGET-CNURL: has parent (~S), following...~%" it)
     (widget-current-navigation-url it))
    (t ;; XXX huh?
     (log-message :debug "WIDGET-CNURL: err, don't know.~%" it)
      "/"
      )))

(defmethod widget-current-navigation-url ((widget function))
  "/")

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

(defparameter *override-parent-p* nil
  "Allow parent overriding in (SETF COMPOSITE-WIDGETS).")

;;; Don't allow setting a parent for widget that already has one
;;; (unless it's setting parent to nil)
(defmethod (setf widget-parent) (val (obj widget))
  (if (and val (not (member (widget-parent obj) `(,val nil)))
	   (not *override-parent-p*))
      (error "Widget ~a already has a parent." obj)
      (setf (slot-value obj 'parent) val)))

(deftype widget-designator ()
  "The supertype of all widgets.  Check against this type instead of
`widget' unless you know what you're doing."
  '(or widget (and symbol (not null)) string function))

(defun widget-designator-p (widget)
  "Returns t when widget is a valid, renderable widget; this includes
strings, function, etc."
  (identity (typep widget 'widget-designator)))

;;; Define widget-rendered-p for objects that don't derive from
;;; 'widget'
(defmethod widget-rendered-p (obj)
  nil)

(defmethod (setf widget-rendered-p) (val obj)
  nil)

(defgeneric widget-children (wij)
  (:documentation "Answer an ordered list, settable when appropriate,
of widgets for whom WIJ is formally a parent.")
  (:method (wij)
    "NIL unless defined otherwise."
    (check-type wij valid-widget)
    nil))

(defgeneric (setf widget-children) (new-value wij)
  (:documentation "Set the value of `widget-children', when
appropriate for WIJ's class."))

;;; Define widget-parent for objects that don't derive from 'widget'
(defmethod widget-parent (obj)
  (declare (ignore obj))
  nil)

(defmethod (setf widget-parent) (obj val)
  (declare (ignore obj val))
  nil)

(defgeneric make-widget-place-writer (container widget)
  (:documentation "Returns a function accepting (&optional ARG) that
   encapsulates the place where widget is stored, behaving like this:

      When ARG not given, return the current contained widget.
      Otherwise, put ARG in the place, set ARG's parent to CONTAINER,
      and dirty CONTAINER.  Signal an error if this place is no longer
      valid or ARG is null.

   Any widget that supports flows must implement this function.  Part
   of the contract is that the fn sets the parent slot of the callee
   to the container.  The other part is that the widget is dirty after
   the write via a direct call to make-dirty, or to a write to a
   widget slot."))

(defparameter *widget-classes*
  (mapcar #'find-class '(widget symbol string function))
  "The classes that widgets can be.")

(defun ensure-widget-methods (gf args function &optional (replace? t))
  "Define a set of methods on GF for each of the 4 standard classes
that represent widgets, in locations indicated by ARGS (a designator
for a list of 0-indices into the lambda list), where the specializers
for the remaining args are all T, calling FUNCTION with the arguments
given to the GF.  This means that for 3 widget args, 64 methods will
be defined, and so on.

When REPLACE?, the default, always replace whatever methods with equal
signatures are already defined."
  (unless args				;avoid (*)=1 case
    (return-from ensure-widget-methods nil))
  (when (typep gf '(or symbol list))
    (setf gf (fdefinition gf)))
  (setf args (sort (copy-list (ensure-list args)) #'>))
  (let* ((old-methods (generic-function-methods gf))
	 (gfll (generic-function-lambda-list gf))
	 (function-lambda (congruent-lambda-expression gfll function)))
    (labels ((descend-combination (proc specializers pos args)
	       (let ((next-arg (or (first args) -1)))
		 (cond ((= -1 pos)
			(funcall proc specializers))
		       ((/= pos next-arg)
			(descend-combination
			 proc (nconc (make-list (- pos next-arg)
						:initial-element (find-class 't))
				     specializers)
			 next-arg args))
		       (t
			(dolist (wclass *widget-classes*)
			  (descend-combination proc (cons wclass specializers)
					       (1- pos) (rest args)))))))
	     (maybe-ensure-method (specializers)
	       (when (or replace?
			 (not (position specializers old-methods
					:key #'method-specializers :test #'equal)))
		 (ensure-method gf function-lambda :specializers specializers))))
      (descend-combination
       #'maybe-ensure-method '()
       (1- (or (position-if (f_ (member _ lambda-list-keywords)) gfll)
	       (length gfll)))
       args))))

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
  (declare (ignore args))
  (with-html
    (:p :id id :class class (str obj))))

(defmethod widget-prefix-fn (obj)
  nil)

(defmethod widget-suffix-fn (obj)
  nil)

(defgeneric render-widget (obj &key inlinep &allow-other-keys)
  (:documentation
  "Renders a widget ('render-widget-body') wrapped in a
header ('with-widget-header'). If 'inlinep' is true, renders the
widget without a header.

Additionally, calls 'dependencies' and adds the returned items to
*page-dependencies*. This is later used by Weblocks to declare
stylesheets and javascript links in the page header."))

(defmethod render-widget (obj &rest args &key inlinep &allow-other-keys)
  (declare (special *page-dependencies*))
  (if (ajax-request-p)
    (mapc #'render-dependency-in-ajax-response (dependencies obj))
    (setf *page-dependencies*
	  (append *page-dependencies* (dependencies obj))))
  (let ((*current-navigation-url* (widget-current-navigation-url obj))
        (*current-widget* obj))
    (declare (special *current-navigation-url* *current-widget*))
    (if inlinep
      (apply #'render-widget-body obj args)
      (apply #'with-widget-header obj #'render-widget-body
             (append
               (when (widget-prefix-fn obj)
                 (list :widget-prefix-fn (widget-prefix-fn obj)))
               (when (widget-suffix-fn obj)
                 (list :widget-suffix-fn (widget-suffix-fn obj)))
               args)))
    (setf (widget-rendered-p obj) t)))

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

(defmethod container-update-children ((widget widget))
  nil) ; only containers have children

