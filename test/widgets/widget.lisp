
(in-package :weblocks-test)

;;; test render-widget-body
(deftest-html render-widget-body-1
    (render-widget-body (lambda (&rest args)
			  (with-html (:p "blah"))))
  (:p "blah"))

;;; test with-widget-header
(deftest-html with-widget-header-1
    (with-request :get nil
      (with-widget-header (make-instance 'dataform :data *joe*)
	(lambda (obj &rest args)
	  (with-html (:p "test")))))
  (:div :class "widget dataform" :id "widget-123"
	(:p "test")))

;;; test widget-name specialization for functions
(deftest widget-name-1
    (widget-name #'identity)
  nil)

;;; render function as a widget
(deftest-html render-function-1
    (render-widget (lambda (&rest args)
		     (with-html (:p "blah"))))
  (:div :class "widget function"
	(:p "blah")))

;;; render some other widget without a name
(deftest-html render-widget-1
    (with-request :get nil
      (render-widget (make-instance 'dataform :data *joe*)))
  (:div :class "widget dataform" :id "widget-123"
	#.(data-header-template "abc123"
	   '((:li :class "name" (:span :class "label" "Name:&nbsp;") (:span :class "value" "Joe"))
	     (:li :class "manager" (:span :class "label" "Manager:&nbsp;") (:span :class "value" "Jim"))))))

;;; render some widget with a name
(deftest-html render-widget-2
    (with-request :get nil
      (render-widget (make-instance 'dataform :data *joe* :name "Test Widget")))
  (:div :class "widget dataform" :id "test-widget"
	#.(data-header-template "abc123"
	   '((:li :class "name" (:span :class "label" "Name:&nbsp;") (:span :class "value" "Joe"))
	     (:li :class "manager" (:span :class "label" "Manager:&nbsp;") (:span :class "value" "Jim"))))))

(deftest-html render-widget-3
    (with-request :get nil
      (render-widget (make-instance 'dataform :data *joe*) :inlinep t))
  #.(data-header-template "abc123"
     '((:li :class "name" (:span :class "label" "Name:&nbsp;") (:span :class "value" "Joe"))
       (:li :class "manager" (:span :class "label" "Manager:&nbsp;") (:span :class "value" "Jim")))))
