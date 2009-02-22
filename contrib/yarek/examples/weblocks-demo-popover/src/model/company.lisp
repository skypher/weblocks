
(in-package :weblocks-demo-popover)

;;; All companies
(defun all-companies (&optional arg)
  "Accepts an argument (passed by dropdown choices) and returns all
available companies."
  (declare (ignore arg))
  (find-persistent-objects (sandbox-store) 'company
			   :order-by (cons 'name :asc)))

;;; Company
(defclass company ()
  ((id :accessor company-id)
   (name :accessor company-name
	 :initarg :name
	 :type string)
   (industry :initform nil
	     :accessor company-industry
	     :initarg :industry)
   (non-profit :initform nil
	       :accessor company-non-profit-p
	       :initarg :non-profit-p
	       :type boolean)))

;;; Table View
(defview company-table-view (:type table :inherit-from '(:scaffold company))
  (id :hidep t))

;;; Form View
(defview company-form-view (:type form :inherit-from '(:scaffold company))
  (id :hidep t))

