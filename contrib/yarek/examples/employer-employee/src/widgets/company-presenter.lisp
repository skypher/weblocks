(in-package :employer-employee)

(defwidget company-presenter (composite)
  ((data
     :reader 	company-presenter-data
     :initform 	nil
     :initarg 	:data
     :documentation
       "Company data object rendered and modified by this widget.")

   (data-class
     :accessor 	company-presenter-data-class
     :initform 	nil
     :initarg 	:data-class
     :documentation
       "Company data object rendered and modified by this widget.")

   (class-store
     :accessor 	company-presenter-class-store
     :initform 	nil
     :initarg 	:class-store
     :documentation
       "Company data store used by this widget.")

   (properties-data-view
     :accessor	company-presenter-properties-data-view
     :initform	nil
     :initarg	:properties-data-view
     :documentation
       "An optional custom data view for the company properties.")

   (properties-form-view
     :accessor	company-presenter-properties-form-view
     :initform	nil
     :initarg	:properties-form-view
     :documentation
       "An optional custom form view for the company properties.")
   
   (properties-ui-state
     :reader	company-presenter-properties-ui-state
     :initform	:data
     :initarg	:properties-ui-state
     :documentation
       "UI stete for the company properties. By default it's :data")
   
   (properties-editor
     :reader	company-presenter-properties-editor
     :initform	nil
     :documentation
       "Editor for company properties")

   (employee-list
     :reader	company-presenter-employee-list
     :initform	nil
     :documentation
       "Shows a list of employees.  Employees are implicit, in that they are not directly
	referred by the company data object. Instead, we rely on company's
        storage to extract employees with slot 'data set to the value of company's id.")

   (employee-data-class
     :reader	company-presenter-employee-data-class
     :initform	nil
     :initarg	:employee-data-class
     :documentation
       "Data class of the company employees")

   (employee-class-store
     :reader	company-presenter-employee-class-store
     :initform	nil
     :initarg	:employee-class-store
     :documentation
       "Data store of the company employees")

   (employee-list-view
     :reader	company-presenter-employee-list-view
     :initform	nil
     :initarg	:employee-list-view
     :documentation
       "View for the company employee list.")

   (employee-list-on-add-employee
     :reader	company-presenter-employee-list-on-add-employee
     :initform	nil
     :initarg	:employee-list-on-add-employee
     :documentation
       "Function called when add employee is being added on the company's employee list.
	Accepts three agruments: the employee list widget, the company object and the new
	employee object.")

   (employee-list-on-query
     :reader	company-presenter-employee-list-on-query
     :initform	nil
     :initarg	:employee-list-on-query
     :documentation
       "Function called when query for employees performed for the company's employee list.
	Accepts four arguments: the employee list widget, the company object, the order
	and the range parameters. ")

   (employee-form-view
     :reader	company-presenter-employee-form-view
     :initform	nil
     :initarg	:employee-form-view
     :documentation
       "Company employees form view")


   (employee-list-popover-editor-class
     :reader	company-presenter-employee-list-popover-editor-class
     :initform	nil
     :initarg	:employee-list-popover-editor-class
     :documentation
       "Symbol for the popover editor class.")
   
   (state
     :accessor company-presenter-state
     :initform :drilldown
     :initarg  :state
     :documentation
       "State of the widget, either :drilldown or :add.  The widget
	will transition from :add to :drilldown when the new employee's
	property values are successfully saved by the user for the first
	time. From then on, the widget will remain in :drilldown state.")

   (on-new-company-success
     :accessor company-presenter-on-new-company-success
     :initform nil
     :initarg  :on-new-company-success
     :documentation
       "Function called when new employee is successfully created")


   (on-new-company-cancel
     :accessor company-presenter-on-new-company-cancel
     :initform nil
     :initarg  :on-new-company-cancel
     :documentation
       "Function called when new employee creation is cancelled.")

   (on-drilldown-edit-success
     :accessor company-presenter-on-drilldown-edit-success
     :initform nil
     :initarg  :on-drilldown-edit-success
     :documentation
       "Function called when new employee is successfully created")

   (on-drilldown-edit-cancel
     :accessor company-presenter-on-drilldown-edit-cancel
     :initform nil
     :initarg  :on-drilldown-edit-cancel
     :documentation
       "Function called when new employee creation is cancelled."))
  
  (:documentation "A widget showing company properties and all company employees."))


(defgeneric (setf company-presenter-data) (value obj))

(defmethod (setf company-presenter-data) (value (cpw company-presenter))
  (setf (slot-value cpw 'data) value)
  (let ((properties-editor (company-presenter-properties-editor cpw)))
    (when properties-editor
      (setf (dataform-data properties-editor) value))))


(defmethod initialize-instance :after ((cpw company-presenter) &rest args)
  (declare (ignore args)) 
  (make-company-properties-widget cpw) 
  (unless (eql (company-presenter-state cpw) :add)
    (make-employee-list-widget cpw)))


(defgeneric  make-company-properties-widget (cpw)
  (:documentation "Creates properties widget")
  (:method ((cpw company-presenter))
    (let ((company-properties-w
           (make-instance 'dataform
                          :data		(company-presenter-data cpw)
                          :data-class	(company-presenter-data-class cpw)
                          :class-store	(company-presenter-class-store cpw)
                          :data-view	(company-presenter-properties-data-view cpw)
                          :form-view	(company-presenter-properties-form-view cpw)
                          :ui-state	(company-presenter-properties-ui-state cpw)
                        
                          :on-cancel	(lambda (dfw)
                                          (declare (ignore dfw))
                                          (if (eql (company-presenter-state cpw) :add)
                                              (safe-funcall (company-presenter-on-new-company-cancel cpw) cpw)
                                              (safe-funcall (company-presenter-on-drilldown-edit-cancel cpw) cpw)))
                        
                          :on-success	(lambda (dfw)
                                          (if (eql (company-presenter-state cpw) :add)
                                              (progn
                                                (safe-funcall (company-presenter-on-new-company-success cpw) cpw) 
                                                (setf (company-presenter-state cpw) :drilldown)
                                                ;; note: we're setting the slot directly instead of
                                                ;; using the accessor to avoid setting
                                                ;; data on the editor (the dfw) - see method
                                                ;; (setf company-presenter-data)
                                                (setf (slot-value cpw 'data) (dataform-data dfw))
                                                (make-employee-list-widget cpw))
                                              (safe-funcall (company-presenter-on-drilldown-edit-success cpw) cpw))))))
    
      (setf (slot-value cpw 'properties-editor) company-properties-w)
      (setf (widget-propagate-dirty company-properties-w) (list cpw))
      (set-composite-widgets cpw))))


(defgeneric make-employee-list-widget (cpw)
  (:documentation "Creates set's item list widget")
  (:method ((cpw company-presenter)) 
    (let ((employee-list-w
           (make-instance 'popover-gridedit
                          :view			(company-presenter-employee-list-view cpw)
                          :data-class		(company-presenter-employee-data-class cpw)
                          :class-store		(company-presenter-employee-class-store cpw)
                          :item-form-view	(company-presenter-employee-form-view cpw)
                          :on-add-item		(lambda (pop-grid employee)
                                                  (safe-funcall (company-presenter-employee-list-on-add-employee cpw)
                                                                pop-grid (company-presenter-data cpw)
                                                                employee))
                          :on-query		(lambda (pop-grid order range &rest args)
                                                  (safe-apply (company-presenter-employee-list-on-query cpw)
                                                              pop-grid (company-presenter-data cpw)
                                                              order range args))))) 
      (setf (slot-value cpw 'employee-list) employee-list-w) 
      (setf (widget-propagate-dirty employee-list-w) (list cpw)) 
      (set-composite-widgets cpw))))


(defun set-composite-widgets (cpw)
  (let ((widget-list (list (company-presenter-properties-editor cpw) (company-presenter-employee-list cpw))))
      (setf (composite-widgets cpw) (remove nil widget-list))))


(defmethod render-widget-body ((cpw company-presenter) &rest args)
  (declare (ignore args))
  (render-widget (company-presenter-properties-editor cpw)) 
  (when (company-presenter-employee-list cpw)
    (render-widget (company-presenter-employee-list cpw))))

(defmethod dataedit-item-widget-data ((cpw company-presenter))
  (company-presenter-data cpw))