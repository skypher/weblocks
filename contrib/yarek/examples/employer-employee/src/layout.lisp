
(in-package :employer-employee)

(defun make-main-page ()
  "Lays out the main page. It consists of a FLASH widget for showing
initial message, and a NAVIGATION widget with panes that hold
employees page and companies page."
  (make-instance 'composite :widgets
		 (list 
		  (make-navigation 'main-menu
				   'companies (make-companies-page)))))

(defun make-companies-page ()
  "Lays out the widgets for the comppanies page."
  (make-instance 'composite :widgets
		 (list
		  (make-instance 'company-gridedit
				 :name			'company-grid
				 :view			'company-table-view
				 :item-data-view	'company-data-view
				 :item-form-view	'company-form-view
				 :drilldown-type 	:view                                 
				 :data-class		'company

                                 :on-query		#'company-list-on-query
                                      
                                 :employee-list-data-class	'employee
                                 :employee-list-form-view	'employee-form-view
                                 :employee-list-view	'employee-table-view
                                 :employee-list-on-add-employee	#'company-employee-list-on-add-employee
                                 :employee-list-on-query	#'company-employee-list-on-query
                                 ))))



