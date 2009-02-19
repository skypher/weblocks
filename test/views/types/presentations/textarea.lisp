
(in-package :weblocks-test)

;;; Test textarea-presentation render-view-field-value
(deftest-html textarea-presentation-render-view-field-value-1
    (render-view-field-value "Hello World!"
			     (make-instance 'textarea-presentation)
			     (make-instance 'form-view-field
					    :slot-name 'foo)
			     (make-instance 'form-view)
			     nil *joe*)
  (:textarea :name "foo" :rows "10" :cols "50" "Hello World!"))

