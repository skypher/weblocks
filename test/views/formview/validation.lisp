
(in-package :weblocks-test)

;;; Test validate-object-form-view
(deftest validate-object-form-view-1
    (validate-object-form-view *joe*
			       (find-view '(form employee))
			       nil)
  t)

(deftest validate-object-form-view-2
    (let ((view (find-view '(form employee))))
      (validate-object-form-view *joe* view
				 (map-view-fields (lambda (fi)
						    (cons fi "bar"))
						  view *joe*)))
  t)

(deftest validate-object-form-view-3
    (let ((view (find-view '(form employee))))
      (view-field-slot-name
       (caaadr
	(multiple-value-list
	 (validate-object-form-view *joe* view
				    (map-view-fields (lambda (fi)
						       (cons fi 1))
						     view *joe*))))))
  name)

;;; Test validate-form-view-field
(deftest validate-form-view-field-1
    (validate-form-view-field 'manager *joe*
			      (make-instance 'form-view-field
					     :slot-name 'manager)
			      (find-view '(form employee))
			      1)
  t)

(deftest validate-form-view-field-2
    (validate-form-view-field 'manager *joe*
			      (make-instance 'form-view-field
					     :slot-name 'manager
					     :satisfies #'oddp)
			      (find-view '(form employee))
			      1)
  t)

(deftest validate-form-view-field-3
    (validate-form-view-field 'manager *joe*
			      (make-instance 'form-view-field
					     :slot-name 'manager
					     :satisfies (list #'oddp))
			      (find-view '(form employee))
			      1)
  t)

(deftest validate-form-view-field-4
    (validate-form-view-field 'manager *joe*
			      (make-instance 'form-view-field
					     :slot-name 'manager
					     :satisfies #'evenp)
			      (find-view '(form employee))
			      1)
  nil "This value must be valid")

(deftest validate-form-view-field-5
    (validate-form-view-field 'manager *joe*
			      (make-instance 'form-view-field
					     :slot-name 'manager
					     :satisfies (lambda (field)
							  (values nil "error message")))
			      (find-view '(form employee))
			      1)
  nil "error message")

(deftest validate-form-view-field-6
    (validate-form-view-field 'manager *joe*
			      (make-instance 'form-view-field
					     :slot-name 'manager
					     :satisfies (list
							 #'oddp
							 (lambda (field)
							   (values nil "error message 1"))
							 (lambda (field)
							   (values nil "error message 2"))))
			      (find-view '(form employee))
			      1)
  nil "error message 1")
