
(in-package :weblocks-elephant-demo)

;;; Model
(defpclass employee ()
  ((first-name :initarg :first-name
	       :accessor first-name
	       :type string)
   (last-name :initarg :last-name
	      :accessor last-name
	      :type string)
   (age :initarg :age
	:accessor age
	:type integer)
   (address :accessor employee-address
	    :initform (make-instance 'address)
	    :type address)
   (company :accessor employee-company
	    :type company)))

;;; Table View
(defview employee-table-view (:type table :inherit-from '(:scaffold employee))
  (address :type mixin
	   :view 'address-table-view)
  (company :reader (compose #'company-name #'employee-company)
	   :order-by '(company name)))

;;; Data view
(defview employee-data-view (:type data :inherit-from '(:scaffold employee))
  (address :type mixin
	   :view 'address-data-view)
  (company :reader (compose #'company-name #'employee-company)))

;;; Form View
(defview employee-form-view (:type form :inherit-from '(:scaffold employee))
  (address :type mixin
	   :view 'address-form-view
	   :initform (make-instance 'address))
  (company :present-as (dropdown :choices #'all-companies
				 :label-key #'company-name)
	   :parse-as (object-id :class-name 'company)
	   :reader (compose #'object-id #'employee-company)
	   :requiredp t))

