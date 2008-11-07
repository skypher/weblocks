
(in-package :employer-employee)

;;; All companies
(defun all-companies (&optional arg)
  "Accepts an argument (passed by dropdown choices) and returns all
available companies."
  (declare (ignore arg))
  (find-persistent-objects *prevalence-store* 'company
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


(defun company-list-on-query (company-list-w order range &key countp) 
  (let ((store (dataseq-class-store company-list-w))
        (class (dataseq-data-class company-list-w)))
    (if countp
        (funcall #'count-companies store class)
        (funcall #'search-companies store class order range))))


(defgeneric count-companies (store class)
  (:documentation "Does the search for appropriate items for the company"))


;; (defmethod count-companies ((store weblocks-memory:memory-store) class)
;;   (length (funcall #'search-companies store class nil nil)))

(defmethod count-companies ((store cl-prevalence:prevalence-system) class)
  (length (funcall #'search-companies store class nil nil)))


(defgeneric search-companies (store class order range)
  (:documentation "Does the search for appropriate items for the company"))


;; (defmethod search-companies ((store weblocks-memory:memory-store) class order range)
;;   (funcall #'find-persistent-objects
;;            store
;;            class
;;            :order-by order
;;            :range range))

(defmethod search-companies ((store cl-prevalence:prevalence-system) class order range)
  (funcall #'find-persistent-objects
           store
           class
           :order-by order
           :range range))

(defun company-employee-list-on-add-employee (employee-list-w company employee)
  (declare (ignore employee-list-w))
  (setf (employee-company-id employee) (object-id company)))


(defun company-employee-list-on-query (employee-list-w company order range &key countp)
  (when company
    (let ((store (dataseq-class-store employee-list-w))
          (class (dataseq-data-class employee-list-w))) 
      (if countp
          (funcall #'count-employees-for-company store class company)
          (funcall #'search-employees-for-company store class order range company)))))


(defgeneric count-employees-for-company (store class company)
  (:documentation "Does the search for appropriate employees for the company"))

;; (defmethod count-employees-for-company ((store weblocks-memory:memory-store) class
;;                                         (fs company))
;;   (length (funcall #'search-employees-for-company store class nil nil fs)))

(defmethod count-employees-for-company ((store cl-prevalence:prevalence-system) class
                                        (fs company))
  (length (funcall #'search-employees-for-company store class nil nil fs)))



(defgeneric search-employees-for-company (store class order range company)
  (:documentation "Does the search for appropriate employees for the company"))

;; (defmethod search-employees-for-company ((store weblocks-memory:memory-store) class order range
;;                                          (fs company)) 
;;   (funcall #'find-persistent-objects
;;            store
;;            class
;;            :order-by order
;;            :range range
;;            :filter (lambda (employee)
;;                      (eql (slot-value employee 'company-id) (object-id fs)))))


(defmethod search-employees-for-company ((store cl-prevalence:prevalence-system) class order range
                                         (fs company)) 
  (funcall #'find-persistent-objects
           store
           class
           :order-by order
           :range range
           :filter (lambda (employee)
                     (eql (slot-value employee 'company-id) (object-id fs)))))

;;; Table View
(defview company-table-view (:type table :inherit-from '(:scaffold company))
  (id :hidep t))

;;; Form View
(defview company-form-view (:type form :inherit-from '(:scaffold company))
  (id :hidep t))

;;; Data View
(defview company-data-view (:type data :inherit-from '(:scaffold company))
  (id :hidep t))




