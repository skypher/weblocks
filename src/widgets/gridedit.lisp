
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
   (allow-delete-p :accessor gridedit-allow-delete-p
		   :initform t
		   :initarg :allow-delete-p
		   :documentation "If true, gridedit provides the UI
		   to delete entries in the collection.")
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

;;; Ensure we never try to sort on our 'delete' slot
(defmethod initialize-instance :after ((obj gridedit) &rest initargs &key &allow-other-keys)
  (pushnew 'delete (datagrid-forbid-sorting-on obj)))

(defun gridedit-render-add-button (grid)
  "Renders a button that allows adding a new entry."
  (let ((action (make-action (lambda (&rest args)
			       (setf (gridedit-ui-state grid) :add)))))
    (with-html
      (:form :class "add-entry" :action "" :method "get"
	     :onsubmit (format nil "initiateFormAction(\"~A\", $(this), \"~A\"); ~
                                    return false;"
			       action
			       (session-name-string-pair))
	     (:fieldset
	      (:input :name *submit-control-name*
		      :type "submit"
		      :class "submit"
		      :value (format nil "Add ~A" (humanize-name
						   (object-class-name
						    (make-instance (datagrid-data-class grid))))))
	      (:input :name "action"
		      :type "hidden"
		      :value action))))))

(defun gridedit-render-add-form (grid)
  "Renders a form that allows adding a new entry utilizing
'dataform' widget."
  (render-widget (make-instance 'dataform
				:data (make-instance (class-of (make-instance (datagrid-data-class grid))))
				:ui-state :form
				:on-cancel (lambda (obj)
					     (setf (gridedit-ui-state grid) nil))
				:on-success (lambda (obj)
					      (gridedit-add-item grid
								 (dataform-data obj))
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

(defmethod render-widget-body ((obj gridedit) &rest args)
  (call-next-method)
  (with-slots (allow-add-p ui-state) obj
    (cond ((and (null ui-state)
		allow-add-p
		(or (gridedit-on-add-item obj)
		    (typep (slot-value obj 'data) 'sequence))) (gridedit-render-add-button obj))
	  ((eql ui-state :add) (gridedit-render-add-form obj)))))

(defmethod render-datagrid-table-body ((grid gridedit) &rest args)
  (apply #'call-next-method grid
	 :custom-slots (append-custom-slots
			(when (gridedit-allow-delete-p grid)
			  `((0 . (delete . ,(lambda (&rest args)
						    (with-html (:td "Delete")))))))
			args)
	 args))


(defun append-custom-slots (custom-slots args)
  "Appends gridedit specific custom slots to the ones already defined
in 'args'."
  (append (cadr (member :custom-slots args))
	  custom-slots))
