
(in-package :weblocks-test)

;;; Test excerpt-presentation render-view-field-value
(deftest-html excerpt-presentation-render-view-field-value-1
    (render-view-field-value "Hello World!"
			     (make-instance 'excerpt-presentation)
			     (make-instance 'data-view-field
					    :slot-name 'foo)
			     (make-instance 'data-view)
			     nil *joe*)
  (:span :class "value" "Hello World!"))

(deftest-html excerpt-presentation-render-view-field-value-2
    (render-view-field-value "Hello World! Hello World! Hello World! Hello World!"
			     (make-instance 'excerpt-presentation)
			     (make-instance 'data-view-field
					    :slot-name 'foo)
			     (make-instance 'data-view)
			     nil *joe*)
  (:span :class "value" "Hello World! He"
	 (:span :class "ellipsis" "...")))

;;; Test excerpt-presentation print-view-field-value
(deftest excerpt-presentation-print-view-field-value-1
    (print-view-field-value "Hello World!"
			     (make-instance 'excerpt-presentation)
			     (make-instance 'data-view-field
					    :slot-name 'foo)
			     (make-instance 'data-view)
			     nil *joe*)
  "Hello World!")

(deftest excerpt-presentation-print-view-field-value-2
    (print-view-field-value "Hello World! Hello World! Hello World! Hello World!"
			     (make-instance 'excerpt-presentation)
			     (make-instance 'data-view-field
					    :slot-name 'foo)
			     (make-instance 'data-view)
			     nil *joe*)
  "Hello World! He")

