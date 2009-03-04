
(in-package :weblocks-test)

;;; Test radio-presentation render-view-field-value
(deftest-html radio-presentation-render-view-field-value-1
    (render-view-field-value nil
			     (make-instance 'radio-presentation
					    :choices (list *joe* *bob*))
			     (make-instance 'form-view-field
					    :slot-name 'foo)
			     (make-instance 'form-view)
			     nil *joe*)
  (htm
   (:label :class "radio first"
	   (:input :name "foo" :type "radio" :class "radio"
		   :value "1")
	   (:span "Employee&nbsp;"))
   (:label :class "radio last"
	   (:input :name "foo" :type "radio" :class "radio"
		   :value "2")
	   (:span "Employee&nbsp;"))))

(deftest-html radio-presentation-render-view-field-value-2
    (render-view-field-value 4
			     (make-instance 'radio-presentation
					    :choices (list (cons 1 2)
							   (cons 3 4)))
			     (make-instance 'form-view-field
					    :slot-name 'foo)
			     (make-instance 'form-view)
			     nil *joe*)
  (htm
   (:label :class "radio first"
	   (:input :name "foo" :type "radio" :class "radio"
		   :value "2")
	   (:span "1&nbsp;"))
   (:label :class "radio last"
	   (:input :name "foo" :type "radio" :class "radio"
		   :value "4"
		   :checked "checked")
	   (:span "3&nbsp;"))))

