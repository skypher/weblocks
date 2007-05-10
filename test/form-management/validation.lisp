
(in-package :weblocks-test)

;;; test slot-value-required-p
(deftest slot-value-required-p-1
    (weblocks::slot-value-required-p 'employee (get-slot-definition 'employee 'age))
  :required)

(deftest slot-value-required-p-2
    (weblocks::slot-value-required-p 'education-history
				     (get-slot-definition 'education-history 'graduation-year))
  nil)

(deftest slot-value-required-p-3
    (weblocks::slot-value-required-p 'employee (get-slot-definition 'employee 'name))
  :required)

;;; validate-slot-from-request
(deftest validate-slot-from-request-1
    (multiple-value-bind (success error)
	(ignore-errors
	  (weblocks::validate-slot-from-request *joe* `(,(get-slot-definition 'employee 'age) . age) nil))
      (values success (format nil "~A" error)))
    
  nil
  "Age is a required field.")

(deftest validate-slot-from-request-2
    (weblocks::validate-slot-from-request *joe* `(,(get-slot-definition 'employee 'age) . age) 20)
  t)

(deftest validate-slot-from-request-3
    (weblocks::validate-slot-from-request *joe* `(,(get-slot-definition 'employee 'name) . age) "Bob")
  t)

;;; apply-validator
(deftest apply-validator-1
    (apply-validator :required *joe* 'age 20)
  nil)

(deftest apply-validator-2
    (multiple-value-bind (success error)
	(ignore-errors
	  (apply-validator :required *joe* 'age nil))
      (values success (format nil "~A" error)))
  nil
  "Age is a required field.")

;;; Note, declare-validators and decl-validate get tested as a
;;; consequence of the previous tests since we use decl-validate in
;;; fixtures



