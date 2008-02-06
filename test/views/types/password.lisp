
(in-package :weblocks-test)

;;; Test password render-view-field-value
(deftest-html password-render-view-field-value-1
    (render-view-field-value  "foo" (make-instance 'password-presentation)
			      (make-instance 'form-view-field
					     :slot-name 'password)
			      (find-view '(form employee))
			      nil *joe*)
  (:input :type "password" :name "password" :maxlength "10" :class "password"))

;;; Test password print-view-field-value
(deftest password-print-view-field-value-1
    (print-view-field-value  "foo" (make-instance 'password-presentation)
			     (make-instance 'form-view-field
					    :slot-name 'password)
			     (find-view '(form employee))
			     nil *joe*)
  "*******")

