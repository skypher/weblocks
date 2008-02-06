
(in-package :weblocks-test)

;;; Test dropdown-presentation render-view-field-value
(deftest-html dropdown-presentation-render-view-field-value-1
    (render-view-field-value nil
			     (make-instance 'dropdown-presentation
					    :choices (list *joe* *bob*))
			     (make-instance 'form-view-field
					    :slot-name 'foo)
			     (make-instance 'form-view)
			     nil *joe*)
  (:select :name "foo"
	   (:option :value "" "[Select Foo]")
	   (:option :value "1" "Employee")
	   (:option :value "2" "Employee")))

(deftest-html dropdown-presentation-render-view-field-value-2
    (render-view-field-value (object-id *bob*)
			     (make-instance 'dropdown-presentation
					    :choices (list *joe* *bob*))
			     (make-instance 'form-view-field
					    :slot-name 'foo)
			     (make-instance 'form-view)
			     nil *joe*)
  (:select :name "foo"
	   (:option :value "" "[Select None]")
	   (:option :value "1" "Employee")
	   (:option :value "2" :selected "selected" "Employee")))

(deftest-html dropdown-presentation-render-view-field-value-3
    (render-view-field-value (object-id *bob*)
			     (make-instance 'dropdown-presentation
					    :choices (list *joe* *bob*))
			     (make-instance 'form-view-field
					    :slot-name 'foo
					    :requiredp t)
			     (make-instance 'form-view)
			     nil *joe*)
  (:select :name "foo"
	   (:option :value "1" "Employee")
	   (:option :value "2" :selected "selected" "Employee")))

