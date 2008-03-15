
(in-package :weblocks)

(export '(*default-cascade-delete-mixins-p* gridedit
	  gridedit-on-add-item gridedit-allow-add-p
	  gridedit-on-delete-items gridedit-cascade-delete-mixins-p
	  gridedit-allow-delete-p gridedit-allow-select-p
	  gridedit-drilldown-type gridedit-item-data-view
	  gridedit-item-form-view gridedit-ui-state
	  gridedit-item-widget gridedit-reset-state
	  gridedit-create-new-item-widget
	  gridedit-create-drilldown-widget gridedit-add-item
	  gridedit-delete-items gridedit-delete-item))

(defparameter *default-cascade-delete-mixins-p* t
  "Default value for 'gridedit-cascade-delete-mixins-p'. See its
documentation for more details.")

(defwidget gridedit (datagrid)
  ((on-add-item :accessor gridedit-on-add-item
		:initform nil
		:initarg :on-add-item
		:documentation "A function called by gridedit when an
		item is added. The function should accept two
		arguments (the gridedit object and a new item), and
		should take appropriate action. Note that the
		'dataform' widget will persist the item to the store
		via the API.")
   (allow-add-p :accessor gridedit-allow-add-p
		:initform t
		:initarg :allow-add-p
		:documentation "If true, gridedit provides the UI to
		add entries to the collection.")
   (on-delete-items :accessor gridedit-on-delete-items
		    :initform nil
		    :initarg :on-delete-items
		    :documentation "A function called by gridedit when
		    one or more items are deleted. The function should
		    accept two arguments (the gridedit object and a
		    value that has semantics similar to datagrid's
		    'selection' slot). If the function is missing, the
		    item is deleted via store API.")
   (cascade-delete-mixins-p :accessor gridedit-cascade-delete-mixins-p
			    :initform *default-cascade-delete-mixins-p*
			    :initarg :cascade-delete-mixins-p
			    :documentation "If set to true, mixin
                            objects will be deleted from the store
                            when the parent object is deleted. By
                            default set to
                            *default-cascade-delete-mixins-p*.")
   (allow-delete-p :accessor gridedit-allow-delete-p
		   :initform t
		   :initarg :allow-delete-p
		   :documentation "If true, gridedit provides the UI
		   to delete entries in the collection.")
   (allow-select-p :initform t)
   (autoset-drilled-down-item-p :initform t)
   (drilldown-type :accessor gridedit-drilldown-type
		 :initform :edit
		 :initarg :drilldown-type
		 :documentation "Type of the drilldown UI to be provided
		 by the gridedit. The default, :edit, provides a form
		 via 'dataform' widget when the user drills down into
		 a given item. If :view is specified the UI will
		 provide a dataform in the view mode. You can also
		 customize 'gridedit-create-drilldown-widget' to provide
		 more fine grained behavior.")
   (item-data-view :accessor gridedit-item-data-view
		   :initform nil
		   :initarg :item-data-view
		   :documentation "An optional custom data view that,
		   if provided, is used to instantiate the
		   'item-widget'.")
   (item-form-view :accessor gridedit-item-form-view
		   :initform nil
		   :initarg :item-form-view
		   :documentation "An optional custom form view that,
		   if provided, is used to instantiate the
		   'item-widget'.")
   (ui-state :accessor gridedit-ui-state
	     :initform nil
	     :initarg :ui-state
	     :documentation "A state of the gridedit control. When set
	     to nil, gridedit simply renders an 'add entry' button and
	     a 'delete entry' button, assuming they are permitted (see
	     'allow-add-p' and 'allow-delete-p'). When set to :add,
	     renders an empty form that allows adding a new
	     entry. When set to :drilldown, renders details for a
	     particular item.")
   (item-widget :accessor gridedit-item-widget
		:initform nil
		:documentation "A widget used by gridedit to display a
	        form for adding and editing items. This widget will be
	        created and destroyed as necessary."))
  (:documentation "A widget based on the 'datagrid' that enhances it
  with user interface to add, remove, and modify data entries."))

;;; We need to set up the value of datagrid-on-drilldown
(defmethod initialize-instance :after ((obj gridedit) &rest initargs &key &allow-other-keys)
  (setf (datagrid-on-drilldown obj)
	(cons 'edit #'gridedit-drilldown-action)))

(defun gridedit-reset-state (grid)
  "Resets the state of the gridedit. This function should be used by
'gridedit-create-new-item-widget' and 'gridedit-create-drilldown-widget'
in order to reset the state after the widget has done its job."
  (setf (gridedit-ui-state grid) nil)
  (setf (gridedit-item-widget grid) nil)
  (setf (datagrid-drilled-down-item grid) nil))

(defmethod datagrid-render-item-ops-bar ((grid gridedit) &rest args)
  (when (null (gridedit-ui-state grid))
    (call-next-method)))

(defmethod datagrid-render-pagination-widget ((grid gridedit) &rest args)
  (when (null (gridedit-ui-state grid))
    (call-next-method)))

(defgeneric gridedit-create-new-item-widget (grid)
  (:documentation
   "The default implementation creates a dataform that allows adding a
new entry. Note, in order for this functionality to work properly, the
object in question needs to have a slot named 'id' (see 'object-id'),
with an initform that assigns an ID to the object.

Specialize this method to provide different ways to add items to the
grid."))

(defmethod gridedit-create-new-item-widget ((grid gridedit))
  (make-instance 'dataform
		 :data (make-instance (datagrid-data-class grid))
		 :ui-state :form
		 :on-cancel (lambda (obj)
			      (declare (ignore obj))
			      (gridedit-reset-state grid)
			      (throw 'annihilate-dataform nil))
		 :on-success (lambda (obj)
			       (gridedit-add-item grid (dataform-data obj))
			       (gridedit-reset-state grid))
		 :on-close (lambda (obj)
			     (declare (ignore obj))
			     (gridedit-reset-state grid))
		 :data-view (gridedit-item-data-view grid)
		 :form-view (gridedit-item-form-view grid)))

(defgeneric gridedit-create-drilldown-widget (grid item)
  (:documentation
   "The default implementation creates a dataform that allows editing
an existing entry. Specialize this method to provide different ways to
view and edit items in the grid.

Note, the implementation of this function must pay attention to the
value of 'gridedit-drilldown-type' and create its widget accordingly."))

(defmethod gridedit-create-drilldown-widget ((grid gridedit) item)
  (make-instance 'dataform
		 :data item
		 :ui-state (if (eql (gridedit-drilldown-type grid) :edit)
			       :form
			       :data)
		 :on-success (lambda (obj)
			       (declare (ignore obj))
			       (flash-message (datagrid-flash grid) "Item Modified.")
			       (if (eql (gridedit-drilldown-type grid) :edit)
				   (gridedit-reset-state grid)
				   (mark-dirty grid)))
		 :on-cancel (when (eql (gridedit-drilldown-type grid) :edit)
			      (lambda (obj)
				(declare (ignore obj))
				(gridedit-reset-state grid)))
		 :on-close (lambda (obj)
			     (declare (ignore obj))
			     (gridedit-reset-state grid))
		 :data-view (gridedit-item-data-view grid)
		 :form-view (gridedit-item-form-view grid)))

(defgeneric gridedit-add-item (grid item)
  (:documentation
   "If 'on-add-item' is specified, simply calls
'on-add-item'. Otherwise, if 'data' is a sequence, adds the item to
the beginning of the sequence. Specialize this function to modify
standard behavior for adding items to a sequence."))

(defmethod gridedit-add-item ((grid gridedit) item)
  (when (gridedit-on-add-item grid)
    (funcall (gridedit-on-add-item grid) grid item))
  ; dataform persists this item for us, no need to do it again
  (flash-message (datagrid-flash grid) "Item added."))

;;; This file needs to be loaded with CMU because CMUCL doesn't
;;; compile it properly. Bytecompiler, however, works.
#+cmu (load (merge-pathnames
	     (make-pathname :directory '(:relative "src" "widgets" "gridedit")
			    :name "delete-action" :type "lisp")
	     (asdf-system-directory :weblocks)))

(defgeneric gridedit-delete-item (grid item-id)
  (:documentation
   "Called by 'gridedit-delete-items' to delete a single item. The
default implementation deletes the item, and if
'gridedit-cascade-delete-mixins-p' is set to true, deletes mixin
objects. Specialize this method to customize the way items are
deleted.

grid - the gridedit object.
item - the integer id of the object to be deleted.")
  (:method ((grid gridedit) item-id)
    ;; delete mixin objects
    (if (gridedit-cascade-delete-mixins-p grid)
	(labels ((delete-mixin-objects (view obj)
		   (map-mixin-fields
		    (lambda (field-info)
		      (let* ((field (field-info-field field-info))
			     (mixin-obj (obtain-view-field-value field obj)))
			(delete-mixin-objects (mixin-view-field-view field) mixin-obj)))
		    view obj)
		   (delete-persistent-object (class-store (class-name (class-of obj))) obj)))
	  (delete-mixin-objects (datagrid-view grid)
				(find-persistent-object-by-id (class-store (datagrid-data-class grid))
							      (datagrid-data-class grid)
							      item-id)))
	(delete-persistent-object-by-id (class-store (datagrid-data-class grid))
					(datagrid-data-class grid)
					item-id))))

#|
;;; TODO: implement defgeneric/cc defmethod/cc in cl-cont and make
;;; gridedit-delete-items a generic function

(defgeneric gridedit-delete-items (grid items)
  (:documentation
   "If 'on-delete-items' is specified, simply calls
'on-delete-items'. Otherwise, if 'data' is a sequence, deletes the
specified items from the sequence. The 'items' parameter is similar to
datagrid's 'selection' slot. Specialize this function to modify
standard behavior for deleting items from a sequence."))
|#

;;; Drilldown action
(defun gridedit-drilldown-action (grid item)
  "This callback function will be called by the datagrid when the user
attempts to drill down on a given item."
  (setf (gridedit-item-widget grid)
	(gridedit-create-drilldown-widget grid item))
  (setf (gridedit-ui-state grid) :drilldown))

;;; Renders the body of the gridedit. Essentially, just calls
;;; datagrid's render method, except that proper controls (add item,
;;; delete item, etc.) are added or removed depending on the settings
(defmethod render-widget-body ((obj gridedit) &rest args)
  (setf (datagrid-item-ops obj)
	(remove-if (lambda (i)
		     (or (string-equal i 'add)
			 (string-equal i 'delete)))
		   (datagrid-item-ops obj)
		   :key #'car))
  (when (and (gridedit-allow-delete-p obj)
	     (> (datagrid-data-count obj :totalp t) 0)
	     (datagrid-allow-select-p obj))
    (pushnew (cons 'delete #'gridedit-delete-items)
	     (datagrid-item-ops obj)
	     :key #'car))
  (when (gridedit-allow-add-p obj)
    (pushnew `(add . ,(lambda (&rest args)
			      (setf (gridedit-item-widget obj) (gridedit-create-new-item-widget obj))
			      (setf (gridedit-ui-state obj) :add)))
	     (datagrid-item-ops obj)
	     :key #'car))
  (apply #'call-next-method obj args)
  (case (gridedit-ui-state obj)
    (:add (render-widget (gridedit-item-widget obj)))
    (:drilldown (render-widget (gridedit-item-widget obj)))))

(defmethod render-datagrid-table-body ((grid gridedit) &rest args)
  (apply #'call-next-method grid args))

(defmethod widget-public-dependencies ((obj gridedit))
  (append (list (public-file-relative-path :stylesheet "dataform"))
	  (call-next-method)))

