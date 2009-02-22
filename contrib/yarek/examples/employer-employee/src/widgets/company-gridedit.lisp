(in-package :employer-employee)

(defwidget company-gridedit (gridedit)
  ((employee-list-view
     :reader	company-gridedit-employee-list-view
     :initform	nil
     :initarg	:employee-list-view
     :documentation
       "View for the company employee list.")

   (employee-list-data-class
     :reader	company-gridedit-employee-list-data-class
     :initform	nil
     :initarg	:employee-list-data-class
     :documentation
       "The company data class.")

   (employee-list-form-view
     :reader	company-gridedit-employee-list-form-view
     :initform	nil
     :initarg	:employee-list-form-view
     :documentation
       "View for the company employee list.")

   (employee-list-on-add-employee
     :reader	company-gridedit-employee-list-on-add-employee
     :initform	nil
     :initarg	:employee-list-on-add-employee
     :documentation
       "Function called when add employee is being performed on the company's employee list.
	Accepts three agruments: the employee list widget, the company object and the new
	employee object.")

   (employee-list-on-query
     :reader	company-gridedit-employee-list-on-query
     :initform	nil
     :initarg	:employee-list-on-query
     :documentation
       "Function called when query for employees performed for the company's employee list.
	Accepts four arguments: the employee list widget, the company object, the order
	and the range parameters. ")

   (employee-list-popover-editor-class
    :accessor	company-gridedit-employee-list-popover-editor-class
     :initform	'company-employee-popover-editor
     :initarg	:employee-list-popover-editor-class
     :documentation
       "Symbol for the popover editor class. Defaults to 'popover-data-editor.
        Specialize to change popover content."))
  
  (:documentation "A specialized gridedit for a company with nested widget showing all company employees."))


(defmethod initialize-instance :after ((cgw company-gridedit) &rest args)
  (declare (ignore args))
  (setf (dataedit-on-delete-items-completed cgw)
        (lambda (w items-cons-cell)
          (let ((shown-item (company-presenter-data (dataedit-item-widget w))))
            (when (member (object-id shown-item) (cdr items-cons-cell))
              (setf (dataedit-item-widget w) nil)
              (mark-dirty w))))))

(defmethod (setf widget-parent) (val (cgw company-gridedit))
  (call-next-method))

(defun set-drilldown-company (cgw company)
  (dataedit-reset-state cgw)
  (setf (dataseq-drilled-down-item cgw) company)
  (when company
    (dataedit-drilldown-action cgw company))
  (mark-dirty cgw :putp t))

(defmethod dataedit-create-drilldown-widget ((cgw company-gridedit) company)
  (make-instance 'company-presenter
                 :data	 		company
                 :data-class		(dataseq-data-class cgw)
                 :class-store		(dataseq-class-store cgw)
                 :properties-data-view	(dataedit-item-data-view cgw)
                 :properties-form-view	(dataedit-item-form-view cgw)
                 :properties-ui-state	(if (eql (gridedit-drilldown-type cgw) :edit)
                                            :form
                                            :data)
                 
                 :employee-class-store	(dataseq-class-store cgw)
                 :employee-list-popover-editor-class (company-gridedit-employee-list-popover-editor-class cgw)
                 :employee-list-view        (company-gridedit-employee-list-view cgw)
                 :employee-list-on-add-employee	(company-gridedit-employee-list-on-add-employee cgw)
                 :employee-list-on-query	(company-gridedit-employee-list-on-query cgw)
                 :employee-data-class	(company-gridedit-employee-list-data-class cgw)
                 :employee-form-view	(company-gridedit-employee-list-form-view cgw)
                 :on-drilldown-edit-success
                 			(lambda (op)
                                          (declare (ignore op))
                                          (mark-dirty cgw :putp t))))

(defmethod dataedit-create-new-item-widget ((cgw company-gridedit))
  (let ((current-company (dataseq-drilled-down-item cgw))
        (new-company (make-instance (dataseq-data-class cgw))))
    (set-drilldown-company cgw nil)
    (make-instance 'company-presenter
                 :data 			new-company
                 :state			:add
                 
                 :properties-data-view	(dataedit-item-data-view cgw)
                 :properties-form-view	(dataedit-item-form-view cgw)
                 :properties-ui-state	:form
                 
                 :employee-class-store	(dataseq-class-store cgw)
                 :employee-list-view        (company-gridedit-employee-list-view cgw)
                 :employee-list-on-add-employee	(company-gridedit-employee-list-on-add-employee cgw)
                 :employee-list-on-query	(company-gridedit-employee-list-on-query cgw)
                 :employee-data-class	(company-gridedit-employee-list-data-class cgw)
                 :employee-form-view	(company-gridedit-employee-list-form-view cgw)
                 
                 :on-new-item-success
                 			(lambda (op)
                                          (declare (ignore op))
                                          (safe-funcall (dataedit-on-add-item cgw) cgw new-company)
                                          (set-drilldown-company cgw new-company)
                                          (safe-funcall (dataedit-on-add-item-completed cgw) cgw new-company))
                 :on-new-item-cancel
                 			(lambda (op)
                                          (declare (ignore op))
                                          (set-drilldown-company cgw current-company))
                 :on-drilldown-edit-success
                 			(lambda (op)
                                          (declare (ignore op))
                                          (mark-dirty cgw :putp t)))))


(defmethod dataseq-render-operations ((cgw company-gridedit) &rest args)
  (apply #'dataseq-render-operations-default cgw args))


(defmethod dataseq-render-pagination-widget ((cgw company-gridedit) &rest args)
  (apply #'dataseq-render-pagination-widget-default cgw args))


(defmethod render-widget-body ((cgw company-gridedit) &rest args)
  (declare (ignore args))
  (with-html
    (:div :class "company-gridedit-body" (call-next-method))))