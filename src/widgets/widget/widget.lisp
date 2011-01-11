
(in-package :weblocks)

(export '(defwidget
          widget
          make-widget
          widget-name
          widget-propagate-dirty
          widget-continuation
          widget-parent
          widget-parents
          widgets-roots
          widget-children
          widget-prefix-fn
          widget-suffix-fn
          with-widget-header
	  update-children
          update-parent-for-children
	  map-subwidgets
          walk-widget-tree
          page-title
          page-description
          page-keywords
          page-headers
          render-widget
          render-widget-body
          render-widget-children
	  update-widget-slots-with-map
          get-widgets-by-type
          get-widget-by-id
	  widget-css-classes
	  mark-dirty
          widget-dirty-p
          widget-equal
          widget-tree-equal
          copy-widget-tree
          *current-widget*))

(defvar *tree-update-pending* nil
  "T if we're currently updating the widget tree.
  Used as recursion guard in (SETF WIDGET-CHILDREN).")

(defmacro defwidget (name direct-superclasses &body body)
  "A macro used to define new widget classes. Behaves exactly as
defclass, except adds 'widget-class' metaclass specification and
inherits from 'widget' if no direct superclasses are provided."
  `(progn
     (defclass ,name ,(remove-duplicates
		       (append (or direct-superclasses '(widget))
			       (when (uri-parameter-def-p (car body))
				 (list 'uri-parameters-mixin))))
       ,@body
       (:metaclass widget-class))
     (defmethod per-class-dependencies append ((obj ,name))
       (declare (ignore obj))
       (dependencies-by-symbol (quote ,name)))
     ,@(awhen (maybe-generate-parameter-slot-map-fn name (car body))
	 (list it))))

(defclass widget (dom-object-mixin)
  ((propagate-dirty :accessor widget-propagate-dirty
		    :initform nil
		    :initarg :propagate-dirty
		    :documentation "A list of widgets which will be made
                    dirty when this widget is made dirty via a POST
                    request. This slot allows setting up dependencies
                    between widgets that will make multiple widgets
                    update automatically during AJAX requests.")
   (continuation :accessor widget-continuation
		 :initform nil
		 :documentation "Stores the continuation object for
                 widgets that were invoked via one of the do-*
                 functions ('do-page', etc.). When 'answer' is called
                 on a widget, this value is used to resume the
                 computation.")
   (parent :accessor widget-parent
           :initarg :parent 
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
  (when children (setf (widget-children obj :widget) children)))

(defgeneric make-widget (obj)
  (:documentation "Create a widget from OBJ.")
  (:method (obj)
    "Create a widget from the printable (PRINC-TO-STRING) representation
     of OBJ."
    (warn "Fallback: Creating widget from printable representation of ~S (type ~S)" obj (type-of obj))
    (make-instance 'string-widget :content (princ-to-string obj)))
  (:method ((obj widget))
    "MAKE-WIDGET is idempotent."
    obj))

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
  ;; TODO: we might want to ensure that the old parent doesn't
  ;;       keep a reference to the widget.
  ;;
  ;;       Enable test COMPOSITE-ADD-WIDGET-PARENT-SWITCHING
  ;;       when this is implemented.
  (setf (slot-value obj 'parent) val))

(defgeneric widget-children (w &optional type)
  (:documentation "Return a list of all widgets (all types) who are
children of w (e.g. may be rendered when w is rendered).")
  (:method (w &optional type) "NIL unless defined otherwise." nil)
  (:method ((w widget) &optional type)
    (if type
      (cdr (assoc type (slot-value w 'children)))
      (reduce #'append (slot-value w 'children) :from-end t :key #'cdr))))

;; Rationale: yes, I realize it would be nicer to use methods (and
;; possibly generators) to functionally enumerate the children of
;; widgets that inherit from multiple parents. However, since a) this
;; would be quite hard to understand for newcomers, b) it would be more
;; difficult to control and override the order of rendering, c) weblocks
;; is written in a mutable-data style anyway, I figured we might as well
;; store these children somewhere. Now, if everyone uses (setf
;; widget-children), then in the case of multiple inheritance we'll get
;; conflicts: some widgets will clobber the list, replacing the children
;; of other widgets. A solution is to use an alist keyed by a symbol
;; (keyword), with a list corresponding to each symbol. That way widgets
;; can store as many lists of children as they like.

;; This solution has the disadvantage of looking less clean and
;; functional, but has many advantages. In particular, it is very easy
;; to debug, as the current list of children is simply stored in a slot
;; and can easily be looked at. The same cannot be said for methods with
;; custom method combinations. Also, it is very flexible: a widget can
;; easily control the way _all_ of its children are rendered, including
;; children inherited from its superclasses. Another advantage is that
;; we get one make-widget-place-writer function which works for nearly
;; all cases, so you will very rarely need to write your own. --jwr

;; If your widget stores some of its children in its own slots, you
;; should see the docstring for `map-subwidgets' to take care of
;; additional complexities.

(defgeneric (setf widget-children) (widgets obj &optional type)
  (:documentation "Set the list of children of type TYPE for OBJ to be
  WIDGETS. TYPE is a symbol (usually a keyword) and serves as a handle
  so that these particular children may later be retrieved.
  
  The children alist is copied before modification so the changed
  list of children does not share structure with the old one.
  
  This is especially important for DO-WIDGET and friends.")
  (:method (widgets (obj widget) &optional (type :widget))
    (let* ((children (copy-alist (slot-value obj 'children)))
           (cell (assoc type children)))
      (setf (slot-value obj 'children) 
            (cond
              ((and cell widgets)
               (rplacd cell (ensure-list widgets))
               children)
              (cell
               (remove type children :key #'car))
              (widgets
               (cons (cons type (ensure-list widgets)) children))))
      (when (and (ajax-request-p)
                 (not *tree-update-pending*)
                 (get-widgets-by-type 'selector :root obj))
        (handler-case (update-widget-tree)
          (http-not-found () (abort-request-handler
                               (page-not-found-handler *current-webapp*)))))
      (update-parent-for-children obj))))

(defgeneric map-subwidgets (function widget)
  (:argument-precedence-order widget function)
  (:method-combination progn :most-specific-last)
  (:documentation "Call FUNCTION once for every direct child of
  WIDGET.  The return value is unspecified.

  You should define a method for your widget class if it introduces
  child widgets that should be seen by `walk-widget-tree', but are not
  included in `widget-children', and are not rendered by
  `render-widget-children'.  You should also define a method on
  `make-widget-place-writer' in that case.")
  (:method progn (function widget)
    (mapc function (widget-children widget))))

(defgeneric update-children (widget)
  (:documentation "Called during the tree shakedown phase (before
  rendering) while walking the widget tree. Implement this method for
  widgets whose children might depend on external factors.")
  (:method (w) "NIL unless defined otherwise" nil))

(defgeneric update-parent-for-children (widget)
  (:documentation "Called during the tree shakedown phase, when creating
  the tree (before rendering) and when modifying the list of children
  for a widget. Updates the parent of WIDGET's children to point to
  PARENT-WIDGET.")
  (:method (w) "NIL unless defined otherwise" nil)
  (:method ((w widget))
    (map-subwidgets (lambda (child) (setf (widget-parent child) w)) w)))

(defgeneric update-widget-parameters (widget request-method uri-parameters)
  (:documentation "Given an alist of parameters, widget updates its state
  appropriately.  The default method for plain widgets does
  nothing.  The default method for uri-parameters-mixin uses a
  method (parameter-slot-map that is automatically defined by
  the macro to get the list slots to update and the associated
  parameter names.")
  (:method (w m u) (declare (ignore w m u)) :default)
  (:method ((w uri-parameters-mixin) method uri-params)
    (when (eq method :get)
      (update-widget-slots-with-map
       w (uri-parameters-slotmap w) uri-params))))

(defun update-widget-slots-with-map (w slot-map uri-params)
   "A helper function we export to users if they want to define
    their own method for updating parameters (for example based
    on a widget name) without relying on the mixin or the MOP"
   (loop for (sname . pname) in slot-map
	 for param = (assoc pname uri-params :test #'equalp)
	 when param
         do (setf (slot-value w sname) (cdr param))))

(defgeneric walk-widget-tree (obj fn &optional depth)
  (:documentation "Walk the widget tree starting at obj and calling fn
  at every node. Fn is a function that takes two arguments: a widget
  being processed and an integer indicating the current depth in the
  widget tree.
  
  TODO: should support an optional kwarg COLLECT to collect the return
  values of applying FN.")
  (:method ((obj function) fn &optional (depth 0)) (funcall fn obj depth))
  (:method ((obj string) fn &optional (depth 0)) (funcall fn obj depth))
  (:method ((obj symbol) fn &optional (depth 0)) (funcall fn obj depth))
  (:method ((obj widget) fn &optional (depth 0))
    (funcall fn obj depth)
    (map-subwidgets (curry-after #'walk-widget-tree fn (+ depth 1)) obj)))


(defgeneric page-title (w)
  (:documentation "Generate a page title. This method will be called
  when walking the widget tree before rendering. Widgets that wish to
  set the page title should have a method defined. If multiple widgets
  provide a page title, one deepest in the widget tree (most specific)
  will be chosen.")
  (:method ((obj widget)) nil)
  (:method ((obj string)) nil)
  (:method ((obj function)) nil)
  (:method ((obj symbol)) nil))

(defgeneric page-description (w)
  (:documentation "Generate a page description. See page-title for
  details on how this description is built.")
  (:method ((obj widget)) nil)
  (:method ((obj string)) nil)
  (:method ((obj function)) nil)
  (:method ((obj symbol)) nil))

(defgeneric page-keywords (w)
  (:documentation "Meta keywords for the widget. See also
  *accumulate-page-keywords* and application-page-title.")
  (:method ((obj widget)) nil)
  (:method ((obj string)) nil)
  (:method ((obj function)) nil)
  (:method ((obj symbol)) nil))

(defgeneric page-headers (w)
  (:documentation "Additional headers for the widget.
  This is useful for things like RSS. The returned list
  may contain strings and nullary functions.")
  (:method ((obj widget)) nil)
  (:method ((obj string)) nil)
  (:method ((obj function)) nil)
  (:method ((obj symbol)) nil))

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
	       (rplaca place callee)
	       (setf (widget-parent callee) obj)
	       (mark-dirty obj))
	      (t (car place))))
	  (lambda (&rest args)
	    (declare (ignore args))
	    (style-warn 'widget-not-in-parent :widget child :parent obj)
	    (mark-dirty obj))))))


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
    (remf args :widget-prefix-fn)
    (remf args :widget-suffix-fn)
    (with-html
      (:div :class (dom-classes obj)
	    :id (dom-id obj)
	    (safe-apply widget-prefix-fn obj args)
	    (apply body-fn obj args)
	    (safe-apply widget-suffix-fn obj args)))))

(defgeneric render-widget-children (obj &rest args)
  (:documentation "Renders the widget's children.")
  (:method (obj &rest args)
    (warn "Cannot update the widget children of ~S because it is not a widget."
          obj))
  (:method ((obj widget) &rest args)
 "Render all children. Specialize this method if you only want
to render widgets of a certain type."
    (mapc (lambda (child)
            (apply #'render-widget child args))
          (widget-children obj))))

(defgeneric render-widget-body (obj &rest args &key &allow-other-keys)
  (:documentation
   "A generic function that renders a widget in its current state. In
order to actually render the widget, call 'render-widget' instead.

'obj' - widget object to render.")
  (:method (obj &rest args)
    (typecase obj
      ((or string symbol function)
       (warn "Implicitly calling MAKE-WIDGET to render ~S." obj)
       (apply #'render-widget-body (make-widget obj) args))
      (t
       (error "I don't know how to render ~S.~%" obj))))
  (:method ((obj widget) &rest args)
    (declare (ignore args))
    "By default this method does nothing."))

(defmethod widget-prefix-fn (obj)
  nil)

(defmethod widget-suffix-fn (obj)
  nil)

(defvar *current-widget* nil
  "The widget being rendered right now.")

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
               args)))))

(defgeneric mark-dirty (w &key propagate putp)
  (:documentation
   "Default implementation adds a widget to a list of dirty
widgets. Normally used during an AJAX request. If there are any
widgets in the 'propagate-dirty' slot of 'w' and 'propagate' is true
(the default), these widgets are added to the dirty list as well.

Note that this function is automatically called when widget slots are
modified, unless slots are marked with affects-dirty-status-p.

Returns NIL if the widget is already dirty or T and the results
of calling MARK-DIRTY on the list of dependents (propagate-dirty).

PUTP is a legacy argument. Do not use it in new code."))

(defmethod mark-dirty ((w widget) &key (propagate t propagate-supplied)
                                       (putp nil putp-supplied))
  (declare (special *dirty-widgets*))
  (and propagate-supplied putp-supplied
       (error "You specified both PROPAGATE and PUTP as arguments to MARK-DIRTY. Are you kidding me?"))
  (unless (widget-dirty-p w)
    (push w *dirty-widgets*)
    ;; NOTE: we have to check for unbound slots because this function
    ;; may get called at initialization time before those slots are bound
    (values t (when (and propagate (slot-boundp w 'propagate-dirty))
                (mapc #'mark-dirty (remove nil (widget-propagate-dirty w)))))))

(defun widget-dirty-p (w)
  "Returns true if the widget 'w' has been marked dirty."
  (member w *dirty-widgets* :test #'eq))

;;; When slots of a widget are modified, the widget should be marked
;;; as dirty to service AJAX calls.
(defmethod (setf slot-value-using-class) (new-value (class widget-class) (object widget)
					  (slot-name widget-effective-slot-definition))
  (when (widget-slot-affects-dirty-status-p slot-name)
    (mark-dirty object))
  (call-next-method new-value class object slot-name))


(defmethod print-object ((obj widget) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~S" (ensure-dom-id obj))))

(defmethod get-widgets-by-type (type &key (include-subtypes-p t) (root (root-widget)))
  "Find all widgets of a specific type (or one of its subtypes
if INCLUDE-SUBTYPES-P) in the widget tree starting at ROOT
(defaults to the current root widget)."
  (let (widgets)
    (walk-widget-tree root
                      (lambda (widget d)
                        (declare (ignore d))
                        (when (or (eq (type-of widget) type)
                                  (and include-subtypes-p
                                       (typep widget type)))
                          (push widget widgets))))
    widgets))

(defmethod get-widget-by-id (id &key (test #'string-equal))
  "Find a widget by its DOM id."
  (walk-widget-tree (root-widget)
                    (lambda (widget d)
                      (declare (ignore d))
                      (when (funcall test id (dom-id widget))
                        (return-from get-widget-by-id widget)))))

(defun widget-parents (widget)
  "Return the parent chain of a widget."
  (let ((parent (widget-parent widget)))
    (when parent
      (cons parent (widget-parents parent)))))

(defun widgets-roots (widgets)
  "Find the common roots of the passed list of WIDGETS
  that are still part of the list."
  (declare (list widgets))
  (let (roots)
    (dolist (widget widgets)
      (setf widgets (remove widget widgets))
      (let ((parents (widget-parents widget)))
        (unless (some (curry-after #'member (union roots widgets)) parents)
          (push widget roots))))
    roots))

(defun copy-widget-tree (root)
  "Copy the widget tree at ROOT including all widgets below.
Slots will be copied shallowly except for CHILDREN."
  (declare (type (or widget string function) root))
  (etypecase root
    ((or string function)
      root)
    (widget
      (let ((slotnames (mapcar #'slot-definition-name (class-slots (class-of root))))
            ;; we use MAKE-INSTANCE in case side effects are attached
            ;; to the INITIALIZE-INSTANCE stage.
            (copy (make-instance (type-of root))))
        (dolist (slotname slotnames)
          (if (slot-boundp root slotname)
            (case slotname
              (children (mapc #'copy-widget-tree slotname))
              (t (setf (slot-value copy slotname) (slot-value root slotname))))
            (slot-makunbound root slotname)))
        copy))))

(defun widget-equal (w1 w2)
  ;; TODO: ensure parental sanity
  (slot-equal w1 w2 :exclude '(children parent)))

(defun widget-tree-equal (tree1 tree2)
  (and (widget-equal tree1 tree2)
       (every #'widget-tree-equal
              (widget-children tree1) (widget-children tree2))))

(defmethod render-widget-body :around ((widget widget) &rest args)
  "Record profiling information."
  (webapp-update-thread-status (concatenate 'string "rendering widget " (princ-to-string widget)))
  (timing (concatenate 'string "render-widget " (princ-to-string widget))
    (call-next-method)))


;;; Widgets that depend on the location hash to
;;; set their state. This is a draft that doesn't
;;; offer much convenience.
(export '(location-hash-dependent update-state-from-location-hash))

(defwidget location-hash-dependent ()
  ())

(defmethod update-state-from-location-hash ((widget location-hash-dependent) hash)
  (declare (ignore widget hash))
  nil)

