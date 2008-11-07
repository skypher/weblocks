
(in-package :employer-employee)

;;; Address
(defclass address ()
  ((street :initform nil
	   :accessor address-street
	   :initarg :street)
   (city :initform nil
	 :accessor address-city
	 :initarg :city)
   (state :initform nil
	  :accessor address-state
	  :type (or us-state null)
	  :initarg :state)))

;;; Form View
(defview address-form-view (:type form
			    :inherit-from '(:scaffold address)
			    :persistp nil))

