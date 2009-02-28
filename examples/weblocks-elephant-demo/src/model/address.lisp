
(in-package :weblocks-elephant-demo)

;;; Model
(defpclass address ()
  ((street :initarg :street
	   :accessor address-street
	   :type string)
   (city :initarg :city
	 :accessor address-city
	 :type string)
   (state :initarg :state
	  :accessor address-state
	  :type string)))

;;; Table View
(defview address-table-view (:type table :inherit-from '(:scaffold address)))

;;; Data View
(defview address-data-view (:type data :inherit-from '(:scaffold address)))

;;; Form View
(defview address-form-view (:type form :inherit-from '(:scaffold address))
  (state :present-as us-state
	 :parse-as us-state))

