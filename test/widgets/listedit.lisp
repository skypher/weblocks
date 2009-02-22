
(in-package :weblocks-test)

(deftest-html render-widget-body-listedit-1
    (with-request :get nil
      (persist-object *default-store* *joe*)
      (let ((list (make-instance 'listedit :data-class 'employee
				 :allow-sorting-p nil
				 :allow-drilldown-p nil
				 :allow-operations-p nil
				 :allow-pagination-p nil)))
	(render-widget-body list)))
  (htm
   (:div :class "data-mining-bar"
	 (:span :class "total-items" "(Total of 1 Employee)"))
   (:div :class "widget flash" :id "id-123" "<!-- empty flash -->")
   (:div :class "datalist-body"
	 (:ol
	  (:li
	   #.(data-header-template
	      nil
	      '((:li :class "name" (:span :class "label text" "Name:&nbsp;")
		 (:span :class "value" "Joe"))
		(:li :class "manager" (:span :class "label text" "Manager:&nbsp;")
		 (:span :class "value" "Jim")))
	      :postslots nil))))))

