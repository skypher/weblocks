
(in-package :weblocks-clsql-demo)

;;; All companies
(defun all-companies (&optional arg)
  "Accepts an argument (passed by dropdown choices) and returns all
available companies."
  (declare (ignore arg))
  (find-persistent-objects *default-store* 'company
			   :order-by (cons 'name :asc)))

;;; Model
(clsql:def-view-class company ()
  ((id :accessor company-id
       :db-kind :key
       :db-constraints (:not-null :unique)
       :type integer)
   (name :initarg :name
	 :accessor company-name
	 :db-constraints :not-null
         :type string)
   (industry :initarg :industry
	     :accessor company-industry
	     :type string)
   (non-profit :initarg :non-profit-p
	       :accessor company-non-profit-p
	       :type boolean
	       :accessor non-profit-p)))

;;; Table View
(defview company-table-view (:type table :inherit-from '(:scaffold company))
  (id :hidep t))

;;; Form View
(defview company-form-view (:type form :inherit-from '(:scaffold company))
  (id :hidep t))

