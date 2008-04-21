
(in-package :weblocks-clsql-demo)

;;; Model
(clsql:def-view-class address ()
  ((id :accessor address-id
       :db-kind :key
       :db-constraints (:not-null :unique)
       :type integer)
   (street :initarg :street
	   :accessor address-street
	   :type string)
   (city :initarg :city
	 :accessor address-city
	 :type string)
   (state :initarg :state
	  :accessor address-state
	  :type string)))

;;; Table View
(defview address-table-view (:type table :inherit-from '(:scaffold address))
  (id :hidep t))

;;; Data View
(defview address-data-view (:type data :inherit-from '(:scaffold address))
  (id :hidep t)
  (address-id :hidep t))

;;; Form View
(defview address-form-view (:type form :inherit-from '(:scaffold address))
  (id :hidep t)
  (state :present-as us-state
	 :parse-as us-state))

