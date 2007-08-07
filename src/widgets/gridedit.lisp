
(in-package :weblocks)

(export '(gridedit gridedit-on-add-item gridedit-allow-add-p
	  gridedit-on-delete-items gridedit-allow-delete-p
	  gridedit-allow-select-p gridedit-ui-state
	  gridedit-render-new-item-form))

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
   (ui-state :accessor gridedit-ui-state
	     :initform nil
	     :initarg :ui-state
	     :documentation "A state of the gridedit control. When set
	     to nil, gridedit simply renders an 'add entry' button and
	     a 'delete entry' button, assuming they are permitted (see
	     'allow-add-p' and 'allow-delete-p'). When set to :add,
	     renders an empty form that allows adding a new entry.")
   (flash :accessor gridedit-flash
	  :initform (make-instance 'flash)
	  :initarg :flash
	  :documentation "A flash widget used by gridedit to display
	  relevant information to the user (how many items have been
	  deleted, etc.)")
   (dataform :accessor gridedit-dataform
	     :initform nil
	     :documentation "A dataform widget used by gridedit to
	     display a form for adding and editing items. This widget
	     will be created and destroyed as necessary."))
  (:documentation "A widget based on the 'datagrid' that enhances it
  with user interface to add, remove, and modify data entries."))

(defgeneric gridedit-render-new-item-form (grid)
  (:documentation
   "The default implementation renders a form that allows adding a new
entry utilizing 'dataform' widget. Note, in order for this
functionality to work properly, the object in question needs to have a
slot named 'id', with an initform that assigns an ID to the object.

Specialize this method to provide different ways to add items to the
grid."))

(defmethod gridedit-render-new-item-form ((grid datagrid))
  (unless (gridedit-dataform grid)
    (setf (gridedit-dataform grid)
	  (make-instance 'dataform
			 :data (make-instance (datagrid-data-class grid))
			 :ui-state :form
			 :on-cancel (lambda (obj)
				      (setf (gridedit-ui-state grid) nil)
				      (setf (datagrid-allow-item-ops-p grid) t)
				      (setf (gridedit-dataform grid) nil))
			 :on-success (lambda (obj)
				       (gridedit-add-item grid (dataform-data obj))
				       (setf (datagrid-allow-item-ops-p grid) t)
				       (setf (gridedit-ui-state grid) nil)
				       (setf (gridedit-dataform grid) nil))
			 :widget-args '(:title-action "Adding"
					:ignore-unbound-slots-p t))))
  (render-widget (gridedit-dataform grid)))

(defun gridedit-add-item (grid item)
  "If 'on-add-item' is specified, simply calls
'on-add-item'. Otherwise, if 'data' is a sequence, adds the item to
the beginning of the sequence."
  (if (gridedit-on-add-item grid)
      (funcall (gridedit-on-add-item grid) grid item)
      (when (typep (slot-value grid 'data) 'sequence)
	(push item (slot-value grid 'data))))
  (flash-message (gridedit-flash grid) "Item added."))

(defun gridedit-delete-items (grid items)
  "If 'on-delete-items' is specified, simply calls
'on-delete-items'. Otherwise, if 'data' is a sequence, deletes the
specified items from the sequence. The 'items' parameter is similar to
datagrid's 'selection' slot."
  (when (datagrid-selection-empty-p items)
    (flash-message (gridedit-flash grid) "Please select items to delete.")
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
    (flash-message (gridedit-flash grid)
		   (format nil "~A ~A deleted."
			   deleted-items-count
			   (proper-number-form deleted-items-count "item")))))

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
  (apply #'call-next-method obj
	 :post-data-mining-fn
	 (compose #'render-widget #'gridedit-flash)
	 args)
  (case (gridedit-ui-state obj)
    (:add (gridedit-render-new-item-form obj))))

