
(in-package :weblocks)

(export '(gridedit gridedit-allow-add-p))

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
		    list of IDs (see object-id) of the items to be
		    deleted). If all items are to be deleted the
		    keyword :all is passed as a second argument
		    instead of the list. 'on-delete-items' has the
		    same semantics as 'on-add-item' if terms of
		    managing items and providing deletion UI.")
   (allow-delete-p :accessor gridedit-allow-delete-p
		   :initform t
		   :initarg :allow-delete-p
		   :documentation "If true, gridedit provides the UI
		   to delete entries in the collection.")
   (allow-select-p :initform t)
   (ui-state :accessor gridedit-ui-state
	     :initform nil
	     :initarg :ui-state
	     :documentation "A state of the gridedit control. When set
	     to nil, gridedit simply renders an 'add entry' button,
	     assuming its permitted (see 'allow-add-p'). When set
	     to :add, renders an empty form that allows adding a new
	     entry."))
  (:documentation "A widget based on the 'datagrid' that enhances it
  with user interface to add, remove, and modify data entries."))

(defun gridedit-render-new-item-form (grid)
  "Renders a form that allows adding a new entry utilizing
'dataform' widget."
  (render-widget (make-instance 'dataform
				:data (make-instance (class-of (make-instance (datagrid-data-class grid))))
				:ui-state :form
				:on-cancel (lambda (obj)
					     (setf (gridedit-ui-state grid) nil)
					     (setf (datagrid-allow-item-ops-p grid) t))
				:on-success (lambda (obj)
					      (gridedit-add-item grid (dataform-data obj))
					      (setf (datagrid-allow-item-ops-p grid) t)
					      (setf (gridedit-ui-state grid) nil))
				:widget-args '(:title-action "Adding"))))

(defun gridedit-add-item (grid item)
  "If 'on-add-item' is specified, simply calls
'on-add-item'. Otherwise, if 'data' is a sequence, adds the item to
the beginning of the sequence."
  (if (gridedit-on-add-item grid)
      (funcall (gridedit-on-add-item grid) grid item)
      (when (typep (slot-value grid 'data) 'sequence)
	(push item (slot-value grid 'data)))))

(defun gridedit-delete-items (grid items)
  "If 'on-delete-items' is specified, simply calls
'on-delete-items'. Otherwise, if 'data' is a sequence, deletes the
specified items from the sequence. The 'items' parameter is similar to
datagrid's 'selection' slot."
  (if (gridedit-on-delete-items grid)
      (funcall (gridedit-on-delete-items grid) grid items)
      (when (typep (slot-value grid 'data) 'sequence)
	(setf (slot-value grid 'data)
	      (ecase (car items)
		(:all (delete-if (compose #'not (curry-after #'member
							     (cdr items)
							     :test #'string-equal))
				 (slot-value grid 'data)
				 :key #'object-id))
		(:none (delete-if (curry-after #'member
					       (cdr items)
					       :test #'string-equal)
				  (slot-value grid 'data)
				  :key #'object-id)))))))

(defmethod render-widget-body ((obj gridedit) &rest args)
  (setf (datagrid-item-ops obj)
	(remove-if (lambda (i)
		     (or (string-equal i 'add)
			 (string-equal i 'delete)))
		   (datagrid-item-ops obj)
		   :key #'car))
  (when (and (gridedit-allow-add-p obj)
	     (or (typep (slot-value obj 'data) 'sequence)
		 (gridedit-on-add-item obj)))
    (pushnew (cons 'delete #'gridedit-delete-items)
	     (datagrid-item-ops obj)
	     :key #'car))
  (when (and (gridedit-allow-add-p obj)
	     (datagrid-allow-select-p obj)
	     (or (typep (slot-value obj 'data) 'sequence)
		 (gridedit-on-add-item obj)))
    (pushnew `(add . ,(lambda (&rest args)
			      (setf (gridedit-ui-state obj) :add)
			      (setf (datagrid-allow-item-ops-p obj) nil)))
	     (datagrid-item-ops obj)
	     :key #'car))
  (call-next-method)
  (case (gridedit-ui-state obj)
    (:add (gridedit-render-new-item-form obj))))

