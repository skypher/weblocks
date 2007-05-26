
(in-package :weblocks-test)

;;; testing render for composite widget
(deftest-html render-composite-1
    (with-request :get nil
      (let ((comp (make-instance 'composite)))
	(push-end (make-instance 'dataform :data *joe*)
		  (composite-widgets comp))
	(push-end (make-instance 'dataform :data *home-address*)
		  (composite-widgets comp))
	(render-widget comp)))
  (htm
   (:div :class "widget composite" :id "widget-123"
	 (:div :class "widget dataform" :id "widget-123"
	       #.(data-header-template "abc123"
		  '((:li :class "name" (:span :class "label" "Name:&nbsp;") (:span :class "value" "Joe"))
		    (:li :class "manager"
			   (:span :class "label" "Manager:&nbsp;") (:span :class "value" "Jim")))))
	 (:div :class "widget dataform" :id "widget-123"
	       #.(data-header-template "abc124"
		  '((:li :class "street"
			   (:span :class "label" "Street:&nbsp;") (:span :class "value" "100 Broadway"))
		    (:li :class "city"
			   (:span :class "label" "City:&nbsp;") (:span :class "value" "New York")))
		  :data-class-name "address")))))

