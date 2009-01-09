
(in-package :weblocks)

(export '(defwidget widget widget-name ensure-widget-methods
          widget-propagate-dirty widget-rendered-p widget-continuation
          widget-parent widget-children widget-prefix-fn widget-suffix-fn
          with-widget-header
	  widget-children update-children update-parent-for-children
	  set-children-of-type get-children-of-type
	  walk-widget-tree
          render-widget render-widget-body render-widget-children
	  widget-css-classes
	  mark-dirty widget-dirty-p
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
		    :documentation "A list of widgets which will be made
                    dirty when this widget is made dirty via a POST
                    request. This slot allows setting up dependencies
                    between widgets that will make multiple widgets
                    update automatically during AJAX requests.")
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
	   :documentation "Stores the 'parent' of a widget, i.e. the
	   widget in which this widget is located, if any. This value is
	   automatically set when you set the widget's children
	   slot. Note, a widget can only have one parent at any given
	   time.")
   (children :initform nil
	     :documentation "This widget's children.")
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
(defmethod initialize-instance :after ((obj widget) &key name children &allow-other-keys)
  (when name (setf (dom-id obj) name))
  (when children (set-children-of-type obj children :widget)))

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

(defmethod (setf widget-parent) (val (obj widget))
  (setf (slot-value obj 'parent) val))

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

(defgeneric widget-children (w)
  (:documentation "Return a list of all widgets who are children of
w (e.g. may be rendered when w is rendered).")
  (:method (w) "NIL unless defined otherwise." nil)
  (:method ((w widget)) (reduce #'append (slot-value w 'children) :key #'cdr)))

(defgeneric (setf widget-children) (new-widgets w)
  (:documentation "This function is obsolete and should never be used.")
  (:method (new-widgets (w widget))
    (error "(setf widget-children) should never be called, use set-children-of-type")))

(defgeneric set-children-of-type (obj widgets type)
  (:method ((obj widget) widgets type)
    (let* ((children (slot-value obj 'children))
	   (cell (assoc type children)))
      (if cell
	  (if widgets
	      (rplacd cell (ensure-list widgets))
	      (setf (slot-value obj 'children) (remove type children :key #'car)))
	  (when widgets (push (cons type (ensure-list widgets)) (slot-value obj 'children)))))
    (update-parent-for-children obj)))

(defgeneric get-children-of-type (obj type)
  (:method ((obj widget) type)
    (cdr (assoc type (slot-value obj 'children)))))

(defgeneric update-children (widget)
  (:documentation "Called during the tree shakedown phase (before
  rendering) while walking the widget tree. Implement this method for
  widgets whose children might depend on external factors.")
  (:method (w) "NIL unless defined otherwise" nil))

(defgeneric update-parent-for-children (widget)
  (:documentation "Called during the tree shakedown phase and when
  creating the tree (before rendering). Updates the parent of WIDGET to
  PARENT-WIDGET.")
  (:method (w) "NIL unless defined otherwise" nil)
  (:method ((w widget))
    (mapc (lambda (child) (setf (widget-parent child) w))
	  (widget-children w))))


(defgeneric walk-widget-tree (obj fn)
  (:documentation "Walk the widget tree starting at obj and calling fn
  at every node. Fn is a function that takes one argument (a widget).")
  (:method ((obj function) fn) (funcall fn obj))
  (:method ((obj string) fn) (funcall fn obj))
  (:method ((obj symbol) fn) (funcall fn obj))
  (:method ((obj widget) fn)
    (funcall fn obj)
    (mapc (curry-after #'walk-widget-tree fn) (widget-children obj))))

;;; Define widget-parent for objects that don't derive from 'widget'
(defmethod widget-parent (obj)
  (declare (ignore obj))
  nil)

(defmethod (setf widget-parent) (obj val)
  (declare (ignore obj val))
  nil)


(defun place-in-children-p (cell children)
  "Simple test that the cell is still part of one of the children lists
   to validate the place stored in the closure"
  (notevery #'null
	    (mapcar (lambda (row)
		      (notevery #'null
				(maplist (lambda (e) (eq cell e)) (cdr row)))) children)))


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
   widget slot.")
  (:method ((obj widget) child)
    (let ((place (find-if-not #'null
			      (mapcar (lambda (row) (member child (cdr row)))
				      (slot-value obj 'children)))))
      (if place
	  (lambda (&optional (callee nil callee-supplied-p))
	    (assert (place-in-children-p place (slot-value obj 'children)))
	    (cond
	      (callee-supplied-p
	       (check-type callee widget-designator
			   "a potential child of a widget")
	       (rplaca place callee)
	       (setf (widget-parent callee) obj)
	       (mark-dirty obj))
	      (t (car place))))
	  (lambda (&rest args)
	    (declare (ignore args))
	    (style-warn 'widget-not-in-parent :widget child :parent obj)
	    (mark-dirty obj))))))


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

(defgeneric render-widget-children (obj &rest args)
  (:documentation "Renders the widget's children")
  (:method ((obj symbol) &rest args) nil)
  (:method ((obj function) &rest args) nil)
  (:method ((obj string) &rest args) nil)
  (:method ((obj widget) &rest args)
    (mapc (lambda (child) (apply #'render-widget child args)) (get-children-of-type obj :widget))))

(defgeneric render-widget-body (obj &rest args &key &allow-other-keys)
  (:documentation
   "A generic function that renders a widget in its current state. In
order to actually render the widget, call 'render-widget' instead.

'obj' - widget object to render.

One of the implementations allows \"rendering\" functions. When
'render-widget' is called on a function, the function is simply
called. This allows to easily add functions to widgets that
can do custom rendering without much boilerplate. Similarly symbols
that are fbound to functions can be treated as widgets.

Another implementation allows rendering strings.")
  (:method ((obj widget) &rest args)
    (declare (ignore args))
    "By default this method does nothing."))

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
  (let ((*current-widget* obj))
    (declare (special *current-widget*))
    (if inlinep
      (progn (apply #'render-widget-body obj args)
	     (apply #'render-widget-children obj (remove-keyword-parameter args :inlinep)))
      (apply #'with-widget-header
	     obj
	     (lambda (obj &rest args)
	       (apply #'render-widget-body obj args)
	       (apply #'render-widget-children obj (remove-keyword-parameter args :inlinep)))
             (append
               (when (widget-prefix-fn obj)
                 (list :widget-prefix-fn (widget-prefix-fn obj)))
               (when (widget-suffix-fn obj)
                 (list :widget-suffix-fn (widget-suffix-fn obj)))
               args)))
    (setf (widget-rendered-p obj) t)))

(defgeneric mark-dirty (w &key propagate)
  (:documentation
   "Default implementation adds a widget to a list of dirty
widgets. Normally used during an AJAX request. If there are any
widgets in the 'propagate-dirty' slot of 'w' and 'propagate' is true, these
widgets are added to the dirty list as well.

Note, this function is automatically called when widget slots are
modified, unless slots are marked with affects-dirty-status-p."))

(defmethod mark-dirty ((w widget) &key propagate)
  (declare (special *dirty-widgets*))
  (when (functionp w)
    (error "AJAX is not supported for functions. Convert the function
    into a CLOS object derived from 'widget'."))
  (ignore-errors
    (when (widget-rendered-p w)
      (setf *dirty-widgets* (adjoin w *dirty-widgets*)))
    (when propagate
      (mapc #'mark-dirty (remove nil (widget-propagate-dirty w))))))

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


(defmethod print-object ((obj widget) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~s" (ensure-dom-id obj))))


