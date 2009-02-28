
(in-package :weblocks-test)

;;; Test url-presentation render-view-field-value
(deftest-html url-presentation-1
    (render-view-field-value "www.hello.com"
			     (make-instance 'url-presentation)
			     (make-instance 'data-view-field
					    :slot-name 'foo)
			     (make-instance 'data-view)
			     nil *joe*)
  (:a :href "www.hello.com" :onclick "stopPropagation(event);" "www.hello.com"))

(deftest-html url-presentation-2
    (render-view-field-value "www.hello.com"
			     (make-instance 'url-presentation
					    :body "Foo")
			     (make-instance 'data-view-field
					    :slot-name 'foo)
			     (make-instance 'data-view)
			     nil *joe*)
  (:a :href "www.hello.com" :onclick "stopPropagation(event);" "Foo"))

(deftest-html url-presentation-3
    (render-view-field-value "www.hello.com"
			     (make-instance 'url-presentation
					    :body (lambda (&rest args)
						    (declare (ignore args))
						    (with-html "Bar")))
			     (make-instance 'data-view-field
					    :slot-name 'foo)
			     (make-instance 'data-view)
			     nil *joe*)
  (:a :href "www.hello.com" :onclick "stopPropagation(event);" "Bar"))
