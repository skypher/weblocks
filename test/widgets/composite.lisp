
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
   (:div :class "widget composite"
	 (:div :class "widget dataform"
	       (:div :class "renderer data employee"
		     (:div :class "extra-top-1" "&nbsp;")
		     (:div :class "extra-top-2" "&nbsp;")
		     (:div :class "extra-top-3" "&nbsp;")
		     (:h1 (:span :class "action" "Viewing:&nbsp;")
			  (:span :class "object" "Employee"))
		     (:ul
		      (:li (:span :class "label" "Name:&nbsp;") (:span :class "value" "Joe"))
		      (:li (:span :class "label" "Manager:&nbsp;") (:span :class "value" "Jim")))
		     (:div :class "submit" (:a :href "?action=abc123" "Modify"))
		     (:div :class "extra-bottom-1" "&nbsp;")
		     (:div :class "extra-bottom-2" "&nbsp;")
		     (:div :class "extra-bottom-3" "&nbsp;")))
	 (:div :class "widget dataform"
	       (:div :class "renderer data address"
		     (:div :class "extra-top-1" "&nbsp;")
		     (:div :class "extra-top-2" "&nbsp;")
		     (:div :class "extra-top-3" "&nbsp;")
		     (:h1 (:span :class "action" "Viewing:&nbsp;")
			  (:span :class "object" "Address"))
		     (:ul
		      (:li (:span :class "label" "Street:&nbsp;") (:span :class "value" "100 Broadway"))
		      (:li (:span :class "label" "City:&nbsp;") (:span :class "value" "New York")))
		     (:div :class "submit" (:a :href "?action=abc124" "Modify"))
		     (:div :class "extra-bottom-1" "&nbsp;")
		     (:div :class "extra-bottom-2" "&nbsp;")
		     (:div :class "extra-bottom-3" "&nbsp;"))))))
