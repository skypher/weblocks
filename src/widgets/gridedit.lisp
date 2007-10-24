
(in-package :weblocks)

(export '(gridedit gridedit-on-add-item gridedit-allow-add-p
	  gridedit-on-delete-items gridedit-allow-delete-p
	  gridedit-allow-select-p gridedit-drilldown-type
	  gridedit-ui-state gridedit-item-widget gridedit-reset-state
	  gridedit-create-new-item-widget
	  gridedit-create-drilldown-widget gridedit-add-item
	  gridedit-delete-items))

(defwidget gridedit (datagrid)
  ((on-add-item :accessor gridedit-on-add-item
		:initform nil
		:initarg :on-add-item
		:documentation "A function called by gridedit when an
		item is added. The function should accept two
		arguments (the gridedit object and a new item), and
		should take appropriate action (write to the database,
		etc.) If 'data' is set to a function and 'on-add-item'
		is not specified, the UI to add items will not be
		provided regardless of the value of 'allow-add-p'. If
		'data' is a sequence, 'on-add-item' will be
		responsible for adding the data to the sequence, if
		specified.")
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
		    'selection' slot). 'on-delete-items' has the same
		    semantics as 'on-add-item' if terms of managing
		    items and providing deletion UI.")
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
   (item-widget-args :accessor gridedit-item-widget-args
		     :initform nil
		     :initarg :item-widget-args
		     :documentation "If this slot is bound to
		     nil (default), 'widget-args' will be passed to
		     'item-widget' when it is rendered. Otherwise,
		     'item-widget-args' will be passed.")
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
			      (gridedit-reset-state grid))
		 :on-success (lambda (obj)
			       (gridedit-add-item grid (dataform-data obj))
			       (gridedit-reset-state grid))
		 :on-close (lambda (obj)
			     (gridedit-reset-state grid))
		 :widget-args (append '(:title-action "Adding")
				      (if (gridedit-item-widget-args grid)
					  (gridedit-item-widget-args grid)
					  (widget-args grid)))))

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
			       (flash-message (datagrid-flash grid) "Item Modified.")
			       (if (eql (gridedit-drilldown-type grid) :edit)
				   (gridedit-reset-state grid)
				   (mark-dirty grid)))
		 :on-cancel (when (eql (gridedit-drilldown-type grid) :edit)
			      (lambda (obj)
				(gridedit-reset-state grid)))
		 :on-close (lambda (obj)
			     (gridedit-reset-state grid))
		 :widget-args (if (gridedit-item-widget-args grid)
				  (gridedit-item-widget-args grid)
				  (widget-args grid))))

(defgeneric gridedit-add-item (grid item)
  (:documentation
   "If 'on-add-item' is specified, simply calls
'on-add-item'. Otherwise, if 'data' is a sequence, adds the item to
the beginning of the sequence. Specialize this function to modify
standard behavior for adding items to a sequence."))

(defmethod gridedit-add-item ((grid gridedit) item)
  (if (gridedit-on-add-item grid)
      (funcall (gridedit-on-add-item grid) grid item)
      (when (typep (slot-value grid 'data) 'sequence)
	(push item (slot-value grid 'data))))
  (flash-message (datagrid-flash grid) "Item added."))


(defgeneric gridedit-delete-items (grid items)
  (:documentation
   "If 'on-delete-items' is specified, simply calls
'on-delete-items'. Otherwise, if 'data' is a sequence, deletes the
specified items from the sequence. The 'items' parameter is similar to
datagrid's 'selection' slot. Specialize this function to modify
standard behavior for adding items to a sequence."))

(defmethod gridedit-delete-items ((grid gridedit) items)
  (when (datagrid-selection-empty-p items)
    (flash-message (datagrid-flash grid) "Please select items to delete.")
    (mark-dirty grid)
    (return-from gridedit-delete-items))
  (let ((initial-items-count (datagrid-data-count grid :totalp t))
	deleted-items-count)
    (if (gridedit-on-delete-items grid)
	(funcall (gridedit-on-delete-items grid) grid items)
	(when (typep (slot-value grid 'data) 'sequence)
	  (setf (slot-value grid 'data)
		(ecase (car items)
		  ;; 		(:all (delete-if (compose #'not (curry-after #'member
		  ;; 							     (cdr items)
		  ;; 							     :test #'equalp))
		  ;; 				 (slot-value grid 'data)
		  ;; 				 :key #'object-id))
		  (:none (delete-if (curry-after #'member
						 (cdr items)
						 :key #'princ-to-string
						 :test #'equalp)
				    (slot-value grid 'data)
				    :key (compose #'princ-to-string #'object-id)))))))
    (setf deleted-items-count (- initial-items-count (datagrid-data-count grid :totalp t)))
    (flash-message (datagrid-flash grid)
		   (format nil "~A ~A deleted."
			   deleted-items-count
			   (proper-number-form deleted-items-count "item")))))

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
	     (datagrid-allow-select-p obj)
	     (or (typep (slot-value obj 'data) 'sequence)
		 (gridedit-on-add-item obj)))
    (pushnew (cons 'delete #'gridedit-delete-items)
	     (datagrid-item-ops obj)
	     :key #'car))
  (when (and (gridedit-allow-add-p obj)
	     (or (typep (slot-value obj 'data) 'sequence)
		 (gridedit-on-add-item obj)))
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

