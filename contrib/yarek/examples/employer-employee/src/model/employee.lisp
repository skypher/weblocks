
(in-package :employer-employee)

;;; Employee
(defclass employee (person)
  ((company-id
     :accessor	employee-company-id
     :initarg	:company-id
     :type	integer)
   
   (contract :accessor employee-contract
	     :initarg :contract)))

;;; Table View
(defview employee-table-view (:type table :inherit-from 'person-table-view))

;;; Data View
(defview employee-data-view (:type data :inherit-from 'person-data-view)
  contract)

;;; Form View
(defview employee-form-view (:type form :inherit-from 'person-form-view)
  (contract :present-as (radio :choices '(:full-time :part-time :consultant :intern))
	    :parse-as keyword))



