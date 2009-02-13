
(in-package :weblocks-test)

;;; Test image-presentation render-view-field-value
(deftest-html image-presentation-1
    (render-view-field-value "www.hello.com"
			     (make-instance 'image-presentation)
			     (make-instance 'data-view-field
					    :slot-name 'foo)
			     (make-instance 'data-view)
			     nil *joe*)
  (:div
    (:img :src "www.hello.com")))

(deftest-html image-presentation-2
    (render-view-field-value "www.hello.com"
			     (make-instance 'image-presentation
					    :alt "foo"
					    :title "bar")
			     (make-instance 'data-view-field
					    :slot-name 'foo)
			     (make-instance 'data-view)
			     nil *joe*)
    (:div
      (:img :src "www.hello.com" :alt "foo" :title "bar")))

