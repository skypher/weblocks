
(in-package :weblocks-test)

;;; Make sure we can't turn on selection
(deftest datalist-initialize-instance-1
    (with-request :get nil
      (not (null (cdr
		  (multiple-value-list
		   (ignore-errors
		     (make-instance 'datalist :data-class 'employee
				    :allow-select-p t)))))))
  t)

(deftest datalist-setf-dataseq-allow-select-p-1
    (with-request :get nil
      (let ((list (make-instance 'datalist :data-class 'employee)))
	(not (null (cdr
		  (multiple-value-list
		   (ignore-errors
		     (setf (dataseq-allow-select-p list) t))))))))
  t)

;;; Ensure scaffold item view is created
(deftest datalist-item-data-view-1
    (with-request :get nil
      (let ((list (make-instance 'datalist :data-class 'employee)))
	(not (null (datalist-item-data-view list)))))
  t)

;;; Test datalist-render-sort-dropdown
(deftest-html datalist-render-sort-dropdown-1
    (with-request :get nil
      (let ((list (make-instance 'datalist :data-class 'employee)))
	(dataseq-update-sort-column list)
	(weblocks::datalist-render-sort-dropdown list)))
  (:form :class "datalist-sort-bar" :action "/foo/bar" :method "get"
	 :onsubmit "initiateFormAction(\"abc123\", $(this), \"weblocks-session=1%3ATEST\"); return false;"
	 (:div :class "extra-top-1" "<!-- empty -->")
	 (:div :class "extra-top-2" "<!-- empty -->")
	 (:div :class "extra-top-3" "<!-- empty -->")
	 (:fieldset (:label (:span :class "label" "Sort&nbsp;")
			    (:script :type "text/javascript"
				     #.(format nil "~%// <![CDATA[
document.write('<a class=\\'sort-asc\\' href=\\'/foo/bar?action=abc124\\' onclick=\\'initiateAction(&quot;abc124&quot;, &quot;weblocks-session=1%3ATEST&quot;); return false;\\'><span class=\\'direction\\'>Ascending</span></a>');
// ]]>~%"))
			    (:noscript (:span :class "sort-asc"
					      (:span :class "direction" "Ascending")))
			    (:span :class "preposition" "&nbsp;By&nbsp;")
			    (:select :name "datalist-sort" :onchange "if(this.form.onsubmit) { this.form.onsubmit(); } else { this.form.submit(); }"
				     (:option :value "name" :selected "selected" "Name")
				     (:option :value "manager" "Manager"))
			    (:noscript (:input :name "submit" :type "submit" :class "submit" :value "Go"
					       :onclick "disableIrrelevantButtons(this);")))
		    (:input :name "action" :type "hidden" :value "abc123"))
	 (:div :class "extra-bottom-1" "<!-- empty -->")
	 (:div :class "extra-bottom-2" "<!-- empty -->")
	 (:div :class "extra-bottom-3" "<!-- empty -->")))

;;; Test dataseq-render-mining-bar for datalist
(deftest-html dataseq-render-mining-bar-datalist-1
    (with-request :get nil
      (let ((list (make-instance 'datalist :data-class 'employee
				 :allow-sorting-p nil)))
	(dataseq-render-mining-bar list)))
  (:div :class "data-mining-bar"
	(:span :class "total-items" "(Total of 0 Employees)")))

;;; Test dataseq-wrap-body-in-form-p for datalist
(deftest dataseq-wrap-body-in-form-p-datalist-1
    (with-request :get nil
      (let ((list (make-instance 'datalist :data-class 'employee)))
	(dataseq-wrap-body-in-form-p list)))
    nil)

;;; Test rendering datalist
(deftest-html render-widget-body-datalist-1
    (with-request :get nil
      (persist-object *default-store* *joe*)
      (let ((list (make-instance 'datalist :data-class 'employee
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

;;; Test dataseq-render-operations for datalist
(deftest-html dataseq-render-operations-datalist-1
    (with-request :get nil
      (let ((list (make-instance 'datalist :data-class 'employee
				 :item-ops '(("foo" . nil))
				 :common-ops '(("bar" . nil)))))
	(dataseq-render-operations list)))
  (:form :class "operations-form" :action "/foo/bar" :method "get"
	 :onsubmit "initiateFormAction(\"abc123\", $(this), \"weblocks-session=1%3ATEST\"); return false;"
	 (:div :class "extra-top-1" "<!-- empty -->")
	 (:div :class "extra-top-2" "<!-- empty -->")
	 (:div :class "extra-top-3" "<!-- empty -->")
	 (:fieldset
	  (:div :class "operations"
		(:input :name "bar" :type "submit" :class "submit" :value "Bar"
			:onclick "disableIrrelevantButtons(this);"))
	  (:input :name "action" :type "hidden" :value "abc123"))
	 (:div :class "extra-bottom-1" "<!-- empty -->")
	 (:div :class "extra-bottom-2" "<!-- empty -->")
	 (:div :class "extra-bottom-3" "<!-- empty -->")))
		
