
(in-package :weblocks-clsql-demo)

;;; Model
(clsql:def-view-class employee ()
  ((id :accessor employee-id
       :db-kind :key
       :db-constraints (:not-null :unique)
       :type integer)
   (first-name :initarg :first-name
	       :accessor first-name
	       :type string)
   (last-name :initarg :last-name
	      :accessor last-name
	      :db-constraints :not-null
	      :type string)
   (age :initarg :age
	:accessor age
	:type integer)
   (address-id :initarg :address-id
	       :accessor employee-address-id
	       :type integer)
   (address :accessor employee-address
	    :db-kind :join
	    :db-info (:join-class address
		      :home-key address-id
		      :foreign-key id
		      :set nil))
   (company-id :initarg :company-id
	       :accessor employee-company-id
	       :db-constraints :not-null
	       :type integer)
   (company :accessor employee-company
	    :db-kind :join
	    :db-info (:join-class company
		      :home-key company-id
		      :foreign-key id
		      :set nil))))

;;; Table View
(defview employee-table-view (:type table :inherit-from '(:scaffold employee))
  (id :hidep t)
  (address-id :hidep t)
  (address :type mixin
	   :view 'address-table-view)
  (company-id :hidep t)
  (company :reader (compose #'company-name #'employee-company)
	   :order-by '(company name)))

;;; Data view
(defview employee-data-view (:type data :inherit-from '(:scaffold employee))
  (id :hidep t)
  (address :type mixin
	   :view 'address-data-view)
  (company-id :hidep t)
  (company :reader (compose #'company-name #'employee-company)))

;;; Form View
(defview employee-form-view (:type form :inherit-from '(:scaffold employee))
  (id :hidep t)
  (address-id :hidep t)
  (address :type mixin
	   :view 'address-form-view
	   :initform (make-instance 'address)
	   :writer (make-slot-writer 'address-id #'address-id))
  (company-id :hidep t)
  (company :present-as (dropdown :choices #'all-companies
				 :label-key #'company-name)
	   :parse-as (object-id :class-name 'company)
	   :writer (make-slot-writer 'company-id #'company-id)
	   :reader #'employee-company-id
	   :requiredp t))

