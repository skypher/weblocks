
(in-package :weblocks-test)

;;; Test password render-view-field-value
(deftest-html password-render-view-field-value-1
  (let ((*presentation-dom-id* 0))
    (render-view-field-value  "foo" (make-instance 'password-presentation)
			      (make-instance 'form-view-field
					     :slot-name 'password)
			      (find-view '(form employee))
			      nil *joe*))
  (:input :type "password" :name "password" :id "0" :maxlength "12" :class "password"))

;;; Test password print-view-field-value
(deftest password-print-view-field-value-1
    (print-view-field-value  "foo" (make-instance 'password-presentation)
			     (make-instance 'form-view-field
					    :slot-name 'password)
			     (find-view '(form employee))
			     nil *joe*)
  "*******")

