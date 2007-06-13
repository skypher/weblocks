
(in-package :weblocks)

(export '(gridedit gridedit-allow-add-p gridedit-add-item))

(defwidget gridedit (datagrid)
  ((allow-add-p :accessor gridedit-allow-add-p
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

(defun gridedit-render-add-button (grid)
  "Renders a button that allows adding a new entry."
  (let ((action (make-action (lambda (&rest args)
			       (setf (gridedit-ui-state grid) :add)))))
    (with-html
      (:form :class "add-entry" :action "" :method "get"
	     :onsubmit (format nil "initiateFormAction(\"~A\", $(this), \"~A\"); return false;"
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
								 (dataform-data obj)
								 (slot-value grid 'data))
					      (setf (gridedit-ui-state grid) nil))
				:widget-args '(:title-action "Adding"))))

(defgeneric gridedit-add-item (grid item collection)
  (:documentation
   "Default implementation adds 'item' to the start of '(datagrid-data
grid)' via 'push'. Specialize this function for different types of
collections, storage methods, and/or grids. Note that in the default
implementation 'collection' is used for specialization only, the
actual place to push 'item' is obtained from the grid."))

(defmethod gridedit-add-item ((grid gridedit) item collection)
  (push item (datagrid-data grid)))

(defmethod render-widget-body ((obj gridedit) &rest args)
  (pushnew 'delete (datagrid-forbid-sorting-on obj))
  (apply #'call-next-method obj
	 :custom-slots (append-custom-slots
			(when (gridedit-allow-delete-p obj)
			  `((0 . (delete . ,(lambda (&rest args)
						    (with-html (:td "Delete")))))))
			args)
	 args)
  (with-slots (allow-add-p ui-state) obj
    (cond ((and (null ui-state)
		allow-add-p) (gridedit-render-add-button obj))
	  ((eql ui-state :add) (gridedit-render-add-form obj)))))


(defun append-custom-slots (custom-slots args)
  "Appends gridedit specific custom slots to the ones already defined
in 'args'."
  (append (cadr (member :custom-slots args))
	  custom-slots))
