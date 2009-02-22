
(in-package :weblocks-test)

;;; Test paragraph-presentation render-view-field-value
(deftest-html paragraph-presentation-paragraph-presentation-1
    (render-view-field-value "Hello World!"
			     (make-instance 'paragraph-presentation)
			     (make-instance 'data-view-field
					    :slot-name 'foo)
			     (make-instance 'data-view)
			     nil *joe*)
  (:p :class "value text" "Hello World!"))

(deftest-html paragraph-presentation-paragraph-presentation-2
    (render-view-field-value (format nil "Hello~%World!")
			     (make-instance 'paragraph-presentation)
			     (make-instance 'data-view-field
					    :slot-name 'foo)
			     (make-instance 'data-view)
			     nil *joe*)
  (:p :class "value text" "Hello<br />World!"))

