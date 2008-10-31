
(in-package :employer-employee)

;;; Person
(defclass person ()
  ((id :accessor person-id)
   (first-name :accessor person-first-name
	       :initarg :first-name)
   (last-name :accessor person-last-name
	      :initarg :last-name
	      :type string)
   (age :accessor person-age
	:initarg :age
	:type (or null integer))
   (address :initform (make-instance 'address)
	    :accessor person-address
	    :initarg :address)))

;;; Table View
(defview person-table-view (:type table :inherit-from '(:scaffold person))
  (id :hidep t)
  (address :type mixin
	   :view '(table address))
  (street :hidep t)
  (city :hidep t))

;;; Data View
(defview person-data-view (:type data :inherit-from '(:scaffold person))
  (id :hidep t)
  (address :type mixin
	   :view '(data address)))

;;; Form View
(defview person-form-view (:type form :inherit-from '(:scaffold person))
  (id :hidep t)
  (address :type mixin
	   :view 'address-form-view))

