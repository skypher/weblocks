
(in-package :weblocks-test)

;;; testing create-new-item-widget-gridedit
(deftest-html create-new-item-widget-gridedit-1
    (with-request :get nil
      (render-widget
       (dataedit-create-new-item-widget
	(make-instance 'gridedit :data (list *joe*)
		       :data-class 'employee))))
  (:div :class "widget data-editor dataform" :id "id-123"
	#.(form-header-template
	   "abc123"
	   '((:li :class "name"
	      (:label :class "input" :for "id-123"
	       (:span :class "slot-name"
		      (:span :class "extra" "Name:&nbsp;"
			     (:em :class "required-slot" "(required)&nbsp;"))))
              (:input :type "text" :name "name" :maxlength "40" :id "id-123"))
	     (:li :class "manager"
	      (:label :class "input" :for "id-123"
	       (:span :class "slot-name"
		      (:span :class "extra" "Manager:&nbsp;")))
              (:input :type "text" :name "manager" :value "Jim" :maxlength "40" :id "id-123")))
	   :method "post")))

(deftest-html create-new-item-widget-gridedit-2
    (with-request :get nil
      (render-widget
       (dataedit-create-new-item-widget
	(make-instance 'gridedit :data (list *joe*)
		       :data-class 'employee
		       :item-form-view (defview () (:type form :inherit-from '(:scaffold employee))
					 (name :label "Foo"
					       :requiredp t))))))
  (:div :class "widget data-editor dataform" :id "id-123"
	#.(form-header-template
	   "abc123"
	   '((:li :class "name"
	      (:label :class "input" :for "id-123"
	       (:span :class "slot-name"
		      (:span :class "extra" "Foo:&nbsp;"
			     (:em :class "required-slot" "(required)&nbsp;"))))
              (:input :type "text" :name "name" :maxlength "40" :id "id-123"))
	     (:li :class "manager"
	      (:label :class "input" :for "id-123"
	       (:span :class "slot-name"
		      (:span :class "extra" "Manager:&nbsp;")))
              (:input :type "text" :name "manager" :value "Jim" :maxlength "40" :id "id-123")))
	   :method "post")))

(deftest-html create-new-item-widget-gridedit-3
    (with-request :get nil
      (render-widget
       (dataedit-create-new-item-widget
	(make-instance 'gridedit :data (list *joe*)
		       :data-class 'employee
		       :view (defview () (:type table :inherit-from '(:scaffold employee))
			       (name :label "Foo"))
		       :item-form-view (defview () (:type form :inherit-from '(:scaffold employee))
					 (name :label "Bar"
					       :requiredp t))))))
  (:div :class "widget data-editor dataform" :id "id-123"
	#.(form-header-template
	   "abc123"
	   '((:li :class "name"
	      (:label :class "input" :for "id-123"
	       (:span :class "slot-name"
		      (:span :class "extra" "Bar:&nbsp;"
			     (:em :class "required-slot" "(required)&nbsp;"))))
              (:input :type "text" :name "name" :maxlength "40" :id "id-123"))
	     (:li :class "manager"
	      (:label :class "input" :for "id-123"
	       (:span :class "slot-name"
		      (:span :class "extra" "Manager:&nbsp;")))
              (:input :type "text" :name "manager" :value "Jim" :maxlength "40" :id "id-123")))
	   :method "post")))

;;; testing gridedit-create-drilldown-widget
(deftest-html create-drilldown-widget-gridedit-1
    (with-request :get nil
      (render-widget
       (dataedit-create-drilldown-widget (make-instance 'gridedit :data (list *joe*)
								  :data-class 'employee)
					 *joe*)))
  (:div :class "widget data-editor dataform" :id "id-123"
	#.(form-header-template
	   "abc123"
	   '((:li :class "name"
	      (:label :class "input" :for "id-123"
	       (:span :class "slot-name"
		      (:span :class "extra" "Name:&nbsp;"
			     (:em :class "required-slot" "(required)&nbsp;"))))
              (:input :type "text" :name "name" :value "Joe" :maxlength "40" :id "id-123"))
	     (:li :class "manager"
	      (:label :class "input" :for "id-123"
	       (:span :class "slot-name"
		      (:span :class "extra" "Manager:&nbsp;")))
              (:input :type "text" :name "manager" :value "Jim" :maxlength "40" :id "id-123")))
	   :method "post"
	   :title-action "Modifying:&nbsp;")))

(deftest-html create-drilldown-widget-gridedit-2
    (with-request :get nil
      (render-widget
       (dataedit-create-drilldown-widget (make-instance 'gridedit :data (list *joe*)
								  :data-class 'employee
								  :drilldown-type :view)
					 *joe*)))
  (:div :class "widget data-editor dataform" :id "id-123"
	#.(data-header-template
	   "abc123"
	   '((:li :class "name" (:span :class "label text" "Name:&nbsp;") (:span :class "value" "Joe"))
	      (:li :class "manager" (:span :class "label text" "Manager:&nbsp;")
	       (:span :class "value" "Jim")))
	   :postslots
	   `((:div :class "submit"
		   ,(link-action-template "abc123" "Modify" :class "modify") (str "&nbsp;")
		   ,(link-action-template "abc124" "Close" :class "close"))))))

(deftest-html create-drilldown-widget-gridedit-3
    (with-request :get nil
      (render-widget
       (dataedit-create-drilldown-widget
	(make-instance 'gridedit :data (list *joe*)
		       :data-class 'employee
		       :item-form-view (defview () (:type form :inherit-from '(:scaffold employee))
					 (name :label "Foo"
					       :requiredp t)))
	*joe*)))
  (:div :class "widget data-editor dataform" :id "id-123"
	#.(form-header-template
	   "abc123"
	   '((:li :class "name"
	      (:label :class "input" :for "id-123"
	       (:span :class "slot-name"
		      (:span :class "extra" "Foo:&nbsp;"
			     (:em :class "required-slot" "(required)&nbsp;"))))
              (:input :type "text" :name "name" :value "Joe" :maxlength "40" :id "id-123"))
	     (:li :class "manager"
	      (:label :class "input" :for "id-123"
	       (:span :class "slot-name"
		      (:span :class "extra" "Manager:&nbsp;")))
              (:input :type "text" :name "manager" :value "Jim" :maxlength "40" :id "id-123")))
	   :method "post"
	   :title-action "Modifying:&nbsp;")))

(deftest-html create-drilldown-widget-gridedit-4
    (with-request :get nil
      (render-widget
       (dataedit-create-drilldown-widget
	(make-instance 'gridedit :data (list *joe*)
		       :data-class 'employee
		       :view (defview () (:type table :inherit-from '(:scaffold employee))
				 (name :label "Foo"))
		       :item-form-view (defview () (:type form :inherit-from '(:scaffold employee))
					 (name :label "Bar"
					       :requiredp t)))
	*joe*)))
  (:div :class "widget data-editor dataform" :id "id-123"
	#.(form-header-template
	   "abc123"
	   '((:li :class "name"
	      (:label :class "input" :for "id-123"
	       (:span :class "slot-name"
		      (:span :class "extra" "Bar:&nbsp;"
			     (:em :class "required-slot" "(required)&nbsp;"))))
              (:input :type "text" :name "name" :value "Joe" :maxlength "40" :id "id-123"))
	     (:li :class "manager"
	      (:label :class "input" :for "id-123"
	       (:span :class "slot-name"
		      (:span :class "extra" "Manager:&nbsp;")))
              (:input :type "text" :name "manager" :value "Jim" :maxlength "40" :id "id-123")))
	   :method "post"
	   :title-action "Modifying:&nbsp;")))

;;; testing render-widget-body for gridedit
(defclass rwbg-1-dummy-field (table-view-field)
  ((weblocks::label :initform "Test")
   (weblocks::reader :initform nil)
   (weblocks::allow-sorting-p :initform nil)))

(deftest-html render-widget-body-gridedit-1
    (with-request :get nil
      (persist-object *default-store* *joe*)
      (let ((grid (make-instance 'gridedit :data-class 'employee
					   :allow-searching-p nil
					   :allow-drilldown-p nil
					   :allow-pagination-p nil
					   :show-total-items-count-p nil)))
	;; render datagrid
	(render-widget-body grid :custom-fields (list (make-instance 'rwbg-1-dummy-field))
			    :form-id "I1" :input-id "I2" :search-id "I3")
	;; click "Add"
	(do-request `(("add" . "Add")
		      (,weblocks::*action-string* . "abc125")))
	(render-widget-body grid :form-id "I1" :input-id "I2" :search-id "I3")
	;; click "submit"
	(do-request `(("name" . "Jill")
		      ("manager" . "Jim")
		      ("submit" . "Submit")
		      (,weblocks::*action-string* . "abc133")))
	(render-widget-body grid :form-id "I1" :input-id "I2" :search-id "I3")))
  (htm
   ;; datagrid
   (:div :class "data-mining-bar"
	 (:p :class "datagrid-select-bar"
	     (:strong "Select: ")
	     (:a :href "/foo/bar?action=abc123"
		 :onclick "initiateAction(\"abc123\", \"weblocks-session=1%3ATEST\"); return false;" "All")
	     ", "
	     (:a :href "/foo/bar?action=abc124"
		 :onclick "initiateAction(\"abc124\", \"weblocks-session=1%3ATEST\"); return false;" "None")))
   (:div :class "widget flash" :id "id-123" "<!-- empty flash -->")
   (:form
    :class "dataseq-form"
    :action "/foo/bar"
    :method "get"
    :onsubmit "initiateFormAction(\"abc125\", $(this), \"weblocks-session=1%3ATEST\"); return false;"
    (:div :class "extra-top-1" "<!-- empty -->")
    (:div :class "extra-top-2" "<!-- empty -->")
    (:div :class "extra-top-3" "<!-- empty -->")
    (:fieldset
     (:div :class "datagrid-body"
	   #.(table-header-template
	      '((:th :class "select" "")
		(:th :class "name sort-asc" (:span #.(link-action-template "abc126" "Name")))
		(:th :class "manager" (:span #.(link-action-template "abc127" "Manager")))
		(:th (:span :class "label" "Test")))
	      '((:tr
		 (:td :class "select"
		  (:div (:input :name "item-1" :type "checkbox" :value "f")))
		 (:td :class "name" (:span :class "value" "Joe"))
		 (:td :class "manager" (:span :class "value" "Jim"))
		 (:td (:span :class "value missing" "Not Specified"))))
	      :summary "Ordered by name, ascending."))
     (:div :class "operations"
	   (:input :name "add" :type "submit" :class "submit" :value "Add"
		   :onclick "disableIrrelevantButtons(this);")
	   (:input :name "delete" :type "submit" :class "submit" :value "Delete"
		   :onclick "disableIrrelevantButtons(this);"))
     (:input :name "action" :type "hidden" :value "abc125"))
    (:div :class "extra-bottom-1" "<!-- empty -->")
    (:div :class "extra-bottom-2" "<!-- empty -->")
    (:div :class "extra-bottom-3" "<!-- empty -->"))
   ;; "Add Employee" clicked
   (:div :class "data-mining-bar"
	 (:p :class "datagrid-select-bar"
	     (:strong "Select: ")
	     (:a :href "/foo/bar?action=abc128"
		 :onclick "initiateAction(\"abc128\", \"weblocks-session=1%3ATEST\"); return false;" "All")
	     ", "
	     (:a :href "/foo/bar?action=abc129"
		 :onclick "initiateAction(\"abc129\", \"weblocks-session=1%3ATEST\"); return false;" "None")))
   (:div :class "widget flash" :id "id-123" "<!-- empty flash -->")
   (:form
    :class "dataseq-form"
    :action "/foo/bar"
    :method "get"
    :onsubmit "initiateFormAction(\"abc130\", $(this), \"weblocks-session=1%3ATEST\"); return false;"
    (:div :class "extra-top-1" "<!-- empty -->")
    (:div :class "extra-top-2" "<!-- empty -->")
    (:div :class "extra-top-3" "<!-- empty -->")
    (:fieldset
     (:div :class "datagrid-body"
	   #.(table-header-template
	      '((:th :class "select" "")
		(:th :class "name sort-asc" (:span #.(link-action-template "abc131" "Name")))
		(:th :class "manager" (:span #.(link-action-template "abc132" "Manager"))))
	      '((:tr
		 (:td :class "select"
		  (:div (:input :name "item-1" :type "checkbox" :value "f")))
		 (:td :class "name" (:span :class "value" "Joe"))
		 (:td :class "manager" (:span :class "value" "Jim"))))
	      :summary "Ordered by name, ascending."))
     (:input :name "action" :type "hidden" :value "abc130"))
    (:div :class "extra-bottom-1" "<!-- empty -->")
    (:div :class "extra-bottom-2" "<!-- empty -->")
    (:div :class "extra-bottom-3" "<!-- empty -->"))
   (:div :class "widget data-editor dataform" :id "id-123"
	 #.(form-header-template
	    "abc133"
	    '((:li :class "name"
	       (:label :class "input" :for "id-123"
		(:span :class "slot-name"
		       (:span :class "extra" "Name:&nbsp;"
			      (:em :class "required-slot" "(required)&nbsp;"))))
               (:input :type "text" :name "name" :maxlength "40" :id "id-123"))
	      (:li :class "manager"
	       (:label :class "input" :for "id-123"
		(:span :class "slot-name"
		       (:span :class "extra" "Manager:&nbsp;")))
               (:input :type "text" :name "manager" :value "Jim" :maxlength "40" :id "id-123")))
	    :method "post"))
   ;; "Submit" clicked
   (:div :class "data-mining-bar"
	 (:p :class "datagrid-select-bar"
	     (:strong "Select: ")
	     (:a :href "/foo/bar?action=abc134"
		 :onclick "initiateAction(\"abc134\", \"weblocks-session=1%3ATEST\"); return false;" "All")
	     ", "
	     (:a :href "/foo/bar?action=abc135"
		 :onclick "initiateAction(\"abc135\", \"weblocks-session=1%3ATEST\"); return false;" "None")))
   (:div :class "widget flash" :id "id-123"
	 (:div :class "view"
	 (:div :class "extra-top-1" "<!-- empty -->")
	 (:div :class "extra-top-2" "<!-- empty -->")
	 (:div :class "extra-top-3" "<!-- empty -->")
	 (:ul :class "messages"
	      (:li
	       (:div :class "widget string"
		     (:p "Added Employee."))))
	 (:div :class "extra-bottom-1" "<!-- empty -->")
	 (:div :class "extra-bottom-2" "<!-- empty -->")
	 (:div :class "extra-bottom-3" "<!-- empty -->"))
         (str (with-javascript-to-string "$('id-123').show();")))
   (:form
    :class "dataseq-form"
    :action "/foo/bar"
    :method "get"
    :onsubmit "initiateFormAction(\"abc136\", $(this), \"weblocks-session=1%3ATEST\"); return false;"
    (:div :class "extra-top-1" "<!-- empty -->")
    (:div :class "extra-top-2" "<!-- empty -->")
    (:div :class "extra-top-3" "<!-- empty -->")
    (:fieldset
     (:div :class "datagrid-body"
	   #.(table-header-template
	      '((:th :class "select" "")
		(:th :class "name sort-asc" (:span #.(link-action-template "abc137" "Name")))
		(:th :class "manager" (:span #.(link-action-template "abc138" "Manager"))))
	      '((:tr
		 (:td :class "select"
		  (:div (:input :name "item-3" :type "checkbox" :value "f")))
		 (:td :class "name" (:span :class "value" "Jill"))
		 (:td :class "manager" (:span :class "value" "Jim")))
		(:tr :class "altern"
		 (:td :class "select"
		  (:div (:input :name "item-1" :type "checkbox" :value "f")))
		 (:td :class "name" (:span :class "value" "Joe"))
		 (:td :class "manager" (:span :class "value" "Jim"))))
	      :summary "Ordered by name, ascending."))
     (:div :class "operations"
	   (:input :name "add" :type "submit" :class "submit" :value "Add"
		   :onclick "disableIrrelevantButtons(this);")
	   (:input :name "delete" :type "submit" :class "submit" :value "Delete"
		   :onclick "disableIrrelevantButtons(this);"))
     (:input :name "action" :type "hidden" :value "abc136"))
    (:div :class "extra-bottom-1" "<!-- empty -->")
    (:div :class "extra-bottom-2" "<!-- empty -->")
    (:div :class "extra-bottom-3" "<!-- empty -->"))))

(deftest-html render-widget-body-gridedit-2
    (with-request :get nil
      (persist-object *default-store* *joe*)
      (make-request-ajax)
      (let ((grid (make-instance 'gridedit :data-class 'employee
					   :forbid-sorting-on '(test)
					   :allow-searching-p nil
					   :allow-drilldown-p nil
					   :allow-pagination-p nil
					   :show-total-items-count-p nil)))
	;; render datagrid
	(render-widget-body grid :form-id "I1" :input-id "I2" :search-id "I3")
	;; click "Delete"
	(do-request `(("item-1" . "on")
		      ("delete" . "Delete")
		      (,weblocks::*action-string* . "abc125")))
	(do-request `(("yes" . "Yes")
		      (,weblocks::*action-string* . "abc128")))
	(render-widget-body grid :form-id "I1" :input-id "I2" :search-id "I3")))
  (htm
   ;; datagrid
   (:div :class "data-mining-bar"
	 (:p :class "datagrid-select-bar"
	     (:strong "Select: ")
	     (:a :href "/foo/bar?action=abc123"
		 :onclick "initiateAction(\"abc123\", \"weblocks-session=1%3ATEST\"); return false;" "All")
	     ", "
	     (:a :href "/foo/bar?action=abc124"
		 :onclick "initiateAction(\"abc124\", \"weblocks-session=1%3ATEST\"); return false;" "None")))
   (:div :class "widget flash" :id "id-123" "<!-- empty flash -->")
   (:form
    :class "dataseq-form"
    :action "/foo/bar"
    :method "get"
    :onsubmit "initiateFormAction(\"abc125\", $(this), \"weblocks-session=1%3ATEST\"); return false;"
    (:div :class "extra-top-1" "<!-- empty -->")
    (:div :class "extra-top-2" "<!-- empty -->")
    (:div :class "extra-top-3" "<!-- empty -->")
    (:fieldset
     (:div :class "datagrid-body"
	   #.(table-header-template
	      '((:th :class "select" "")
		(:th :class "name sort-asc" (:span #.(link-action-template "abc126" "Name")))
		(:th :class "manager" (:span #.(link-action-template "abc127" "Manager"))))
	      '((:tr
		 (:td :class "select"
		  (:div (:input :name "item-1" :type "checkbox" :value "f")))
		 (:td :class "name" (:span :class "value" "Joe"))
		 (:td :class "manager" (:span :class "value" "Jim"))))
	      :summary "Ordered by name, ascending."))
     (:div :class "operations"
	   (:input :name "add" :type "submit" :class "submit" :value "Add"
		   :onclick "disableIrrelevantButtons(this);")
	   (:input :name "delete" :type "submit" :class "submit" :value "Delete"
		   :onclick "disableIrrelevantButtons(this);"))
     (:input :name "action" :type "hidden" :value "abc125"))
    (:div :class "extra-bottom-1" "<!-- empty -->")
    (:div :class "extra-bottom-2" "<!-- empty -->")
    (:div :class "extra-bottom-3" "<!-- empty -->"))
   ;; "Delete" clicked
   (:div :class "widget flash" :id "id-123"
	 (:div :class "view"
	 (:div :class "extra-top-1" "<!-- empty -->")
	 (:div :class "extra-top-2" "<!-- empty -->")
	 (:div :class "extra-top-3" "<!-- empty -->")
	 (:ul :class "messages"
	      (:li
	       (:div :class "widget string"
		     (:p "Deleted 1 Employee."))))
	 (:div :class "extra-bottom-1" "<!-- empty -->")
	 (:div :class "extra-bottom-2" "<!-- empty -->")
	 (:div :class "extra-bottom-3" "<!-- empty -->")))
   (:form
    :class "dataseq-form"
    :action "/foo/bar"
    :method "get"
    :onsubmit "initiateFormAction(\"abc129\", $(this), \"weblocks-session=1%3ATEST\"); return false;"
    (:div :class "extra-top-1" "<!-- empty -->")
    (:div :class "extra-top-2" "<!-- empty -->")
    (:div :class "extra-top-3" "<!-- empty -->")
    (:fieldset
     (:div :class "datagrid-body"
	   (:div :class "view table empty"
	       (:div :class "extra-top-1" "<!-- empty -->")
	       (:div :class "extra-top-2" "<!-- empty -->")
	       (:div :class "extra-top-3" "<!-- empty -->")
	       (:p :class "user-message" (:span :class "message" "No information available."))
	       (:div :class "extra-bottom-1" "<!-- empty -->")
	       (:div :class "extra-bottom-2" "<!-- empty -->")
	       (:div :class "extra-bottom-3" "<!-- empty -->")))
     (:div :class "operations"
	   (:input :name "add" :type "submit" :class "submit" :value "Add"
		   :onclick "disableIrrelevantButtons(this);"))
     (:input :name "action" :type "hidden" :value "abc129"))
    (:div :class "extra-bottom-1" "<!-- empty -->")
    (:div :class "extra-bottom-2" "<!-- empty -->")
    (:div :class "extra-bottom-3" "<!-- empty -->"))))

(deftest-html render-widget-body-gridedit-3
    (with-request :get nil
      (persist-object *default-store* (copy-template *joe*))
      (let ((grid (make-instance 'gridedit :data-class 'employee
					   :forbid-sorting-on '(test)
					   :allow-searching-p nil
					   :allow-pagination-p nil
					   :show-total-items-count-p nil)))
	;; render datagrid
	(render-widget-body grid :form-id "I1" :input-id "I2" :search-id "I3")
	;; Drilldown
	(do-request `((,weblocks::*action-string* . "abc128")))
	(render-widget-body grid :form-id "I1" :input-id "I2" :search-id "I3")
	;; click "submit"
	(do-request `(("name" . "Jill")
		      ("manager" . "Jim")
		      ("submit" . "Submit")
		      (,weblocks::*action-string* . "abc135")))
	(render-widget-body grid :form-id "I1" :input-id "I2" :search-id "I3")))
  (htm
   ;; datagrid
   (:div :class "data-mining-bar"
	 (:p :class "datagrid-select-bar"
	     (:strong "Select: ")
	     (:a :href "/foo/bar?action=abc123"
		 :onclick "initiateAction(\"abc123\", \"weblocks-session=1%3ATEST\"); return false;" "All")
	     ", "
	     (:a :href "/foo/bar?action=abc124"
		 :onclick "initiateAction(\"abc124\", \"weblocks-session=1%3ATEST\"); return false;" "None")))
   (:div :class "widget flash" :id "id-123" "<!-- empty flash -->")
   (:form
    :class "dataseq-form"
    :action "/foo/bar"
    :method "get"
    :onsubmit "initiateFormAction(\"abc125\", $(this), \"weblocks-session=1%3ATEST\"); return false;"
    (:div :class "extra-top-1" "<!-- empty -->")
    (:div :class "extra-top-2" "<!-- empty -->")
    (:div :class "extra-top-3" "<!-- empty -->")
    (:fieldset
     (:div :class "datagrid-body"
	   #.(table-header-template
	      '((:th :class "select" "")
		(:th :class "name sort-asc" (:span #.(link-action-template "abc126" "Name")))
		(:th :class "manager" (:span #.(link-action-template "abc127" "Manager")))
		(:th :class "drilldown modify" ""))
	      '((:tr :onclick "initiateActionOnEmptySelection(\"abc128\", \"weblocks-session=1%3ATEST\");"
		     :onmouseover "this.style.cursor = \"pointer\";"
		     :style "cursor: expression(\"hand\");"
		 (:td :class "select"
		      :onclick "stopPropagation(event);"
		      :style "cursor: default;"
		  (:div (:input :name "item-1" :type "checkbox" :value "f")))
		 (:td :class "name" (:span :class "value" "Joe"))
		 (:td :class "manager" (:span :class "value" "Jim"))
		 (:td :class "drilldown modify"
		  (:noscript (:div (:a :href "/foo/bar?action=abc128" "Modify"))))))
	      :summary "Ordered by name, ascending."))
     (:div :class "operations"
	   (:input :name "add" :type "submit" :class "submit" :value "Add"
		   :onclick "disableIrrelevantButtons(this);")
	   (:input :name "delete" :type "submit" :class "submit" :value "Delete"
		   :onclick "disableIrrelevantButtons(this);"))
     (:input :name "action" :type "hidden" :value "abc125"))
    (:div :class "extra-bottom-1" "<!-- empty -->")
    (:div :class "extra-bottom-2" "<!-- empty -->")
    (:div :class "extra-bottom-3" "<!-- empty -->"))
   ;; "Drilldown" clicked
   (:div :class "data-mining-bar"
	 (:p :class "datagrid-select-bar"
	     (:strong "Select: ")
	     (:a :href "/foo/bar?action=abc129"
		 :onclick "initiateAction(\"abc129\", \"weblocks-session=1%3ATEST\"); return false;" "All")
	     ", "
	     (:a :href "/foo/bar?action=abc130"
		 :onclick "initiateAction(\"abc130\", \"weblocks-session=1%3ATEST\"); return false;" "None")))
   (:div :class "widget flash" :id "id-123" "<!-- empty flash -->")
   (:form
    :class "dataseq-form"
    :action "/foo/bar"
    :method "get"
    :onsubmit "initiateFormAction(\"abc131\", $(this), \"weblocks-session=1%3ATEST\"); return false;"
    (:div :class "extra-top-1" "<!-- empty -->")
    (:div :class "extra-top-2" "<!-- empty -->")
    (:div :class "extra-top-3" "<!-- empty -->")
    (:fieldset
     (:div :class "datagrid-body"
	   #.(table-header-template
	      '((:th :class "select" "")
		(:th :class "name sort-asc" (:span #.(link-action-template "abc132" "Name")))
		(:th :class "manager" (:span #.(link-action-template "abc133" "Manager")))
		(:th :class "drilldown modify" ""))
	      '((:tr :class "drilled-down"
		     :onclick "initiateActionOnEmptySelection(\"abc134\", \"weblocks-session=1%3ATEST\");"
		     :onmouseover "this.style.cursor = \"pointer\";"
		     :style "cursor: expression(\"hand\");"
		 (:td :class "select"
		      :onclick "stopPropagation(event);"
		      :style "cursor: default;"
		  (:div (:input :name "item-1" :type "checkbox" :value "f")))
		 (:td :class "name" (:span :class "value" "Joe"))
		 (:td :class "manager" (:span :class "value" "Jim"))
		 (:td :class "drilldown modify"
		  (:noscript (:div (:a :href "/foo/bar?action=abc134" "Modify"))))))
	      :summary "Ordered by name, ascending."))
     (:input :name "action" :type "hidden" :value "abc131"))
    (:div :class "extra-bottom-1" "<!-- empty -->")
    (:div :class "extra-bottom-2" "<!-- empty -->")
    (:div :class "extra-bottom-3" "<!-- empty -->"))
    (:div :class "widget data-editor dataform" :id "id-123"
	 #.(form-header-template
	    "abc135"
	    '((:li :class "name"
	       (:label :class "input" :for "id-123"
		(:span :class "slot-name"
		       (:span :class "extra" "Name:&nbsp;"
			      (:em :class "required-slot" "(required)&nbsp;"))))
               (:input :type "text" :name "name" :value "Joe" :maxlength "40" :id "id-123"))
	      (:li :class "manager"
	       (:label :class "input" :for "id-123"
		(:span :class "slot-name"
		       (:span :class "extra" "Manager:&nbsp;")))
               (:input :type "text" :name "manager" :value "Jim" :maxlength "40" :id "id-123")))
	    :method "post"
	    :title-action "Modifying:&nbsp;"))
    ;; Submit clicked
    (:div :class "data-mining-bar"
	 (:p :class "datagrid-select-bar"
	     (:strong "Select: ")
	     (:a :href "/foo/bar?action=abc136"
		 :onclick "initiateAction(\"abc136\", \"weblocks-session=1%3ATEST\"); return false;" "All")
	     ", "
	     (:a :href "/foo/bar?action=abc137"
		 :onclick "initiateAction(\"abc137\", \"weblocks-session=1%3ATEST\"); return false;" "None")))
    (:div :class "widget flash" :id "id-123"
	  (:div :class "view"
		(:div :class "extra-top-1" "<!-- empty -->")
		(:div :class "extra-top-2" "<!-- empty -->")
		(:div :class "extra-top-3" "<!-- empty -->")
		(:ul :class "messages" (:li (:div :class "widget string" (:p "Modified Employee."))))
		(:div :class "extra-bottom-1" "<!-- empty -->")
		(:div :class "extra-bottom-2" "<!-- empty -->")
		(:div :class "extra-bottom-3" "<!-- empty -->"))
          (str (with-javascript-to-string "$('id-123').show();")))
    (:form
     :class "dataseq-form"
     :action "/foo/bar"
     :method "get"
     :onsubmit "initiateFormAction(\"abc138\", $(this), \"weblocks-session=1%3ATEST\"); return false;"
     (:div :class "extra-top-1" "<!-- empty -->")
     (:div :class "extra-top-2" "<!-- empty -->")
     (:div :class "extra-top-3" "<!-- empty -->")
     (:fieldset
      (:div :class "datagrid-body"
	    #.(table-header-template
	       '((:th :class "select" "")
		 (:th :class "name sort-asc" (:span #.(link-action-template "abc139" "Name")))
		 (:th :class "manager" (:span #.(link-action-template "abc140" "Manager")))
		 (:th :class "drilldown modify" ""))
	       '((:tr :onclick "initiateActionOnEmptySelection(\"abc141\", \"weblocks-session=1%3ATEST\");"
		  :onmouseover "this.style.cursor = \"pointer\";"
		  :style "cursor: expression(\"hand\");"
		  (:td :class "select"
		   :onclick "stopPropagation(event);"
		   :style "cursor: default;"
		   (:div (:input :name "item-1" :type "checkbox" :value "f")))
		  (:td :class "name" (:span :class "value" "Jill"))
		  (:td :class "manager" (:span :class "value" "Jim"))
		  (:td :class "drilldown modify"
		   (:noscript (:div (:a :href "/foo/bar?action=abc141" "Modify"))))))
	       :summary "Ordered by name, ascending."))
      (:div :class "operations"
	    (:input :name "add" :type "submit" :class "submit" :value "Add"
							       :onclick "disableIrrelevantButtons(this);")
	    (:input :name "delete" :type "submit" :class "submit" :value "Delete"
								  :onclick "disableIrrelevantButtons(this);"))
      (:input :name "action" :type "hidden" :value "abc138"))
     (:div :class "extra-bottom-1" "<!-- empty -->")
     (:div :class "extra-bottom-2" "<!-- empty -->")
     (:div :class "extra-bottom-3" "<!-- empty -->"))))

(deftest-html render-widget-body-gridedit-4
    (with-request :get nil
      (persist-object *default-store* (copy-template *joe*))
      (let ((grid (make-instance 'gridedit :data-class 'employee
					   :forbid-sorting-on '(test)
					   :allow-searching-p nil
					   :drilldown-type :view
					   :allow-pagination-p nil
					   :show-total-items-count-p nil)))
	;; render datagrid
	(render-widget-body grid :form-id "I1" :input-id "I2" :search-id "I3")
	;; Drilldown
	(do-request `((,weblocks::*action-string* . "abc128")))
	(render-widget-body grid :form-id "I1" :input-id "I2" :search-id "I3")
	;; click "Close"
	(do-request `((,weblocks::*action-string* . "abc136")))
	(render-widget-body grid :form-id "I1" :input-id "I2" :search-id "I3")))
  (htm
   ;; datagrid
   (:div :class "data-mining-bar"
	 (:p :class "datagrid-select-bar"
	     (:strong "Select: ")
	     (:a :href "/foo/bar?action=abc123"
		 :onclick "initiateAction(\"abc123\", \"weblocks-session=1%3ATEST\"); return false;" "All")
	     ", "
	     (:a :href "/foo/bar?action=abc124"
		 :onclick "initiateAction(\"abc124\", \"weblocks-session=1%3ATEST\"); return false;" "None")))
   (:div :class "widget flash" :id "id-123" "<!-- empty flash -->")
   (:form
    :class "dataseq-form"
    :action "/foo/bar"
    :method "get"
    :onsubmit "initiateFormAction(\"abc125\", $(this), \"weblocks-session=1%3ATEST\"); return false;"
    (:div :class "extra-top-1" "<!-- empty -->")
    (:div :class "extra-top-2" "<!-- empty -->")
    (:div :class "extra-top-3" "<!-- empty -->")
    (:fieldset
     (:div :class "datagrid-body"
	   #.(table-header-template
	      '((:th :class "select" "")
		(:th :class "name sort-asc" (:span #.(link-action-template "abc126" "Name")))
		(:th :class "manager" (:span #.(link-action-template "abc127" "Manager")))
		(:th :class "drilldown modify" ""))
	      '((:tr :onclick "initiateActionOnEmptySelection(\"abc128\", \"weblocks-session=1%3ATEST\");"
		     :onmouseover "this.style.cursor = \"pointer\";"
		     :style "cursor: expression(\"hand\");"
		 (:td :class "select"
		      :onclick "stopPropagation(event);"
		      :style "cursor: default;"
		  (:div (:input :name "item-1" :type "checkbox" :value "f")))
		 (:td :class "name" (:span :class "value" "Joe"))
		 (:td :class "manager" (:span :class "value" "Jim"))
		 (:td :class "drilldown modify"
		  (:noscript (:div (:a :href "/foo/bar?action=abc128" "Modify"))))))
	      :summary "Ordered by name, ascending."))
     (:div :class "operations"
	   (:input :name "add" :type "submit" :class "submit" :value "Add"
		   :onclick "disableIrrelevantButtons(this);")
	   (:input :name "delete" :type "submit" :class "submit" :value "Delete"
		   :onclick "disableIrrelevantButtons(this);"))
     (:input :name "action" :type "hidden" :value "abc125"))
    (:div :class "extra-bottom-1" "<!-- empty -->")
    (:div :class "extra-bottom-2" "<!-- empty -->")
    (:div :class "extra-bottom-3" "<!-- empty -->"))
   ;; "Drilldown" clicked
   (:div :class "data-mining-bar"
	 (:p :class "datagrid-select-bar"
	     (:strong "Select: ")
	     (:a :href "/foo/bar?action=abc129"
		 :onclick "initiateAction(\"abc129\", \"weblocks-session=1%3ATEST\"); return false;" "All")
	     ", "
	     (:a :href "/foo/bar?action=abc130"
		 :onclick "initiateAction(\"abc130\", \"weblocks-session=1%3ATEST\"); return false;" "None")))
   (:div :class "widget flash" :id "id-123" "<!-- empty flash -->")
   (:form
    :class "dataseq-form"
    :action "/foo/bar"
    :method "get"
    :onsubmit "initiateFormAction(\"abc131\", $(this), \"weblocks-session=1%3ATEST\"); return false;"
    (:div :class "extra-top-1" "<!-- empty -->")
    (:div :class "extra-top-2" "<!-- empty -->")
    (:div :class "extra-top-3" "<!-- empty -->")
    (:fieldset
     (:div :class "datagrid-body"
	   #.(table-header-template
	      '((:th :class "select" "")
		(:th :class "name sort-asc" (:span #.(link-action-template "abc132" "Name")))
		(:th :class "manager" (:span #.(link-action-template "abc133" "Manager")))
		(:th :class "drilldown modify" ""))
	      '((:tr :class "drilled-down"
		     :onclick "initiateActionOnEmptySelection(\"abc134\", \"weblocks-session=1%3ATEST\");"
		     :onmouseover "this.style.cursor = \"pointer\";"
		     :style "cursor: expression(\"hand\");"
		 (:td :class "select"
		      :onclick "stopPropagation(event);"
		      :style "cursor: default;"
		  (:div (:input :name "item-1" :type "checkbox" :value "f")))
		 (:td :class "name" (:span :class "value" "Joe"))
		 (:td :class "manager" (:span :class "value" "Jim"))
		 (:td :class "drilldown modify"
		  (:noscript (:div (:a :href "/foo/bar?action=abc134" "Modify"))))))
	      :summary "Ordered by name, ascending."))
     (:input :name "action" :type "hidden" :value "abc131"))
    (:div :class "extra-bottom-1" "<!-- empty -->")
    (:div :class "extra-bottom-2" "<!-- empty -->")
    (:div :class "extra-bottom-3" "<!-- empty -->"))
    (:div :class "widget data-editor dataform" :id "id-123"
	 #.(data-header-template
	    "abc135"
	    '((:li :class "name" (:span :class "label text" "Name:&nbsp;") (:span :class "value" "Joe"))
	      (:li :class "manager" (:span :class "label text" "Manager:&nbsp;")
	       (:span :class "value" "Jim")))
	    :postslots
	    `((:div :class "submit"
		    ,(link-action-template "abc135" "Modify" :class "modify") (str "&nbsp;")
		    ,(link-action-template "abc136" "Close" :class "close")))))
    ;; Submit clicked
    (:div :class "data-mining-bar"
	 (:p :class "datagrid-select-bar"
	     (:strong "Select: ")
	     (:a :href "/foo/bar?action=abc137"
		 :onclick "initiateAction(\"abc137\", \"weblocks-session=1%3ATEST\"); return false;" "All")
	     ", "
	     (:a :href "/foo/bar?action=abc138"
		 :onclick "initiateAction(\"abc138\", \"weblocks-session=1%3ATEST\"); return false;" "None")))
    (:div :class "widget flash" :id "id-123" "<!-- empty flash -->")
    (:form
     :class "dataseq-form"
     :action "/foo/bar"
     :method "get"
     :onsubmit "initiateFormAction(\"abc139\", $(this), \"weblocks-session=1%3ATEST\"); return false;"
     (:div :class "extra-top-1" "<!-- empty -->")
     (:div :class "extra-top-2" "<!-- empty -->")
     (:div :class "extra-top-3" "<!-- empty -->")
     (:fieldset
      (:div :class "datagrid-body"
	    #.(table-header-template
	       '((:th :class "select" "")
		 (:th :class "name sort-asc" (:span #.(link-action-template "abc140" "Name")))
		 (:th :class "manager" (:span #.(link-action-template "abc141" "Manager")))
		 (:th :class "drilldown modify" ""))
	       '((:tr :onclick "initiateActionOnEmptySelection(\"abc142\", \"weblocks-session=1%3ATEST\");"
		  :onmouseover "this.style.cursor = \"pointer\";"
		  :style "cursor: expression(\"hand\");"
		  (:td :class "select"
		   :onclick "stopPropagation(event);"
		   :style "cursor: default;"
		   (:div (:input :name "item-1" :type "checkbox" :value "f")))
		  (:td :class "name" (:span :class "value" "Joe"))
		  (:td :class "manager" (:span :class "value" "Jim"))
		  (:td :class "drilldown modify"
		   (:noscript (:div (:a :href "/foo/bar?action=abc142" "Modify"))))))
	       :summary "Ordered by name, ascending."))
      (:div :class "operations"
	    (:input :name "add" :type "submit" :class "submit" :value "Add"
							       :onclick "disableIrrelevantButtons(this);")
	    (:input :name "delete" :type "submit" :class "submit" :value "Delete"
								  :onclick "disableIrrelevantButtons(this);"))
      (:input :name "action" :type "hidden" :value "abc139"))
     (:div :class "extra-bottom-1" "<!-- empty -->")
     (:div :class "extra-bottom-2" "<!-- empty -->")
     (:div :class "extra-bottom-3" "<!-- empty -->"))))

(deftest-html render-widget-body-gridedit-5
    (with-request :get nil
      (persist-object *default-store* (copy-template *joe*))
      (make-request-ajax)
      (let ((grid (make-instance 'gridedit :data-class 'employee
					   :forbid-sorting-on '(test)
					   :allow-searching-p nil
					   :allow-pagination-p nil
					   :show-total-items-count-p nil)))
	;; render datagrid
	(render-widget-body grid :form-id "I1" :input-id "I2" :search-id "I3")))
  (htm
   ;; datagrid
   (:div :class "data-mining-bar"
	 (:p :class "datagrid-select-bar"
	     (:strong "Select: ")
	     (:a :href "/foo/bar?action=abc123"
		 :onclick "initiateAction(\"abc123\", \"weblocks-session=1%3ATEST\"); return false;" "All")
	     ", "
	     (:a :href "/foo/bar?action=abc124"
		 :onclick "initiateAction(\"abc124\", \"weblocks-session=1%3ATEST\"); return false;" "None")))
   (:div :class "widget flash" :id "id-123" "<!-- empty flash -->")
   (:form
    :class "dataseq-form"
    :action "/foo/bar"
    :method "get"
    :onsubmit "initiateFormAction(\"abc125\", $(this), \"weblocks-session=1%3ATEST\"); return false;"
    (:div :class "extra-top-1" "<!-- empty -->")
    (:div :class "extra-top-2" "<!-- empty -->")
    (:div :class "extra-top-3" "<!-- empty -->")
    (:fieldset
     (:div :class "datagrid-body"
	   #.(table-header-template
	      '((:th :class "select" "")
		(:th :class "name sort-asc" (:span #.(link-action-template "abc126" "Name")))
		(:th :class "manager" (:span #.(link-action-template "abc127" "Manager")))
		(:th :class "drilldown modify" ""))
	      '((:tr :onclick "initiateActionOnEmptySelection(\"abc128\", \"weblocks-session=1%3ATEST\");"
		     :onmouseover "this.style.cursor = \"pointer\";"
		     :style "cursor: expression(\"hand\");"
		 (:td :class "select"
		      :onclick "stopPropagation(event);"
		      :style "cursor: default;"
		  (:div (:input :name "item-1" :type "checkbox" :value "f")))
		 (:td :class "name" (:span :class "value" "Joe"))
		 (:td :class "manager" (:span :class "value" "Jim"))
		 (:td :class "drilldown modify" "")))
	      :summary "Ordered by name, ascending."))
     (:div :class "operations"
	   (:input :name "add" :type "submit" :class "submit" :value "Add"
		   :onclick "disableIrrelevantButtons(this);")
	   (:input :name "delete" :type "submit" :class "submit" :value "Delete"
		   :onclick "disableIrrelevantButtons(this);"))
     (:input :name "action" :type "hidden" :value "abc125"))
    (:div :class "extra-bottom-1" "<!-- empty -->")
    (:div :class "extra-bottom-2" "<!-- empty -->")
    (:div :class "extra-bottom-3" "<!-- empty -->"))))

(deftest-html render-widget-body-gridedit-6
    (with-request :get nil
      (persist-object *default-store* *joe*)
      (let ((grid (make-instance 'gridedit :data-class 'employee
					   :allow-searching-p nil
					   :allow-drilldown-p nil
					   :show-total-items-count-p nil
					   :view (defview () (:type table
								    :inherit-from '(:scaffold employee))
						   (test :allow-sorting-p nil
							 :reader nil)))))
	;; set items per page
	(setf (pagination-items-per-page (dataseq-pagination-widget grid)) 1)
	;; render datagrid
	(render-widget-body grid :form-id "I1" :input-id "I2" :search-id "I3")
	;; click "Add"
	(do-request `(("add" . "Add")
		      (,weblocks::*action-string* . "abc125")))
	(render-widget-body grid :form-id "I1" :input-id "I2" :search-id "I3")))
  (htm
   ;; datagrid
   (:div :class "data-mining-bar"
	 (:p :class "datagrid-select-bar"
	     (:strong "Select: ")
	     (:a :href "/foo/bar?action=abc123"
		 :onclick "initiateAction(\"abc123\", \"weblocks-session=1%3ATEST\"); return false;" "All")
	     ", "
	     (:a :href "/foo/bar?action=abc124"
		 :onclick "initiateAction(\"abc124\", \"weblocks-session=1%3ATEST\"); return false;" "None")))
   (:div :class "widget flash" :id "id-123" "<!-- empty flash -->")
   (:form
    :class "dataseq-form"
    :action "/foo/bar"
    :method "get"
    :onsubmit "initiateFormAction(\"abc125\", $(this), \"weblocks-session=1%3ATEST\"); return false;"
    (:div :class "extra-top-1" "<!-- empty -->")
    (:div :class "extra-top-2" "<!-- empty -->")
    (:div :class "extra-top-3" "<!-- empty -->")
    (:fieldset
     (:div :class "datagrid-body"
	   #.(table-header-template
	      '((:th :class "select" "")
		(:th :class "name sort-asc" (:span #.(link-action-template "abc126" "Name")))
		(:th :class "manager" (:span #.(link-action-template "abc127" "Manager")))
		(:th :class "test" (:span :class "label" "Test")))
	      '((:tr
		 (:td :class "select"
		  (:div (:input :name "item-1" :type "checkbox" :value "f")))
		 (:td :class "name" (:span :class "value" "Joe"))
		 (:td :class "manager" (:span :class "value" "Jim"))
		 (:td :class "test" (:span :class "value missing" "Not Specified"))))
	      :summary "Ordered by name, ascending."))
     (:div :class "operations"
	   (:input :name "add" :type "submit" :class "submit" :value "Add"
		   :onclick "disableIrrelevantButtons(this);")
	   (:input :name "delete" :type "submit" :class "submit" :value "Delete"
		   :onclick "disableIrrelevantButtons(this);"))
     (:input :name "action" :type "hidden" :value "abc125"))
    (:div :class "extra-bottom-1" "<!-- empty -->")
    (:div :class "extra-bottom-2" "<!-- empty -->")
    (:div :class "extra-bottom-3" "<!-- empty -->"))
   (:div :class "widget pagination" :id "id-123"
	 #.(pagination-page-info-template 1 1))
   ;; "Add Employee" clicked
   (:div :class "data-mining-bar"
	 (:p :class "datagrid-select-bar"
	     (:strong "Select: ")
	     (:a :href "/foo/bar?action=abc128"
		 :onclick "initiateAction(\"abc128\", \"weblocks-session=1%3ATEST\"); return false;" "All")
	     ", "
	     (:a :href "/foo/bar?action=abc129"
		 :onclick "initiateAction(\"abc129\", \"weblocks-session=1%3ATEST\"); return false;" "None")))
   (:div :class "widget flash" :id "id-123" "<!-- empty flash -->")
   (:form
    :class "dataseq-form"
    :action "/foo/bar"
    :method "get"
    :onsubmit "initiateFormAction(\"abc130\", $(this), \"weblocks-session=1%3ATEST\"); return false;"
    (:div :class "extra-top-1" "<!-- empty -->")
    (:div :class "extra-top-2" "<!-- empty -->")
    (:div :class "extra-top-3" "<!-- empty -->")
    (:fieldset
     (:div :class "datagrid-body"
	   #.(table-header-template
	      '((:th :class "select" "")
		(:th :class "name sort-asc" (:span #.(link-action-template "abc131" "Name")))
		(:th :class "manager" (:span #.(link-action-template "abc132" "Manager")))
		(:th :class "test" (:span :class "label" "Test")))
	      '((:tr
		 (:td :class "select"
		  (:div (:input :name "item-1" :type "checkbox" :value "f")))
		 (:td :class "name" (:span :class "value" "Joe"))
		 (:td :class "manager" (:span :class "value" "Jim"))
		 (:td :class "test" (:span :class "value missing" "Not Specified"))))
	      :summary "Ordered by name, ascending."))
     (:input :name "action" :type "hidden" :value "abc130"))
    (:div :class "extra-bottom-1" "<!-- empty -->")
    (:div :class "extra-bottom-2" "<!-- empty -->")
    (:div :class "extra-bottom-3" "<!-- empty -->"))
   (:div :class "widget data-editor dataform" :id "id-123"
	 #.(form-header-template
	    "abc133"
	    '((:li :class "name"
	       (:label :class "input" :for "id-123"
		(:span :class "slot-name"
		       (:span :class "extra" "Name:&nbsp;"
			      (:em :class "required-slot" "(required)&nbsp;"))))
               (:input :type "text" :name "name" :maxlength "40" :id "id-123"))
	      (:li :class "manager"
	       (:label :class "input" :for "id-123"
		(:span :class "slot-name"
		       (:span :class "extra" "Manager:&nbsp;")))
               (:input :type "text" :name "manager" :value "Jim" :maxlength "40" :id "id-123")))
	    :method "post"))))

(deftest-html render-widget-body-gridedit-7
    (with-request :get nil
      (persist-object *default-store* *joe*)
      (let ((grid (make-instance 'gridedit :data-class 'employee
				 :data-store *test-store*
				 :allow-searching-p nil
				 :allow-drilldown-p nil
				 :allow-pagination-p nil
				 :show-total-items-count-p nil)))
	;; render datagrid
	(render-widget-body grid :custom-fields (list (make-instance 'rwbg-1-dummy-field))
			    :form-id "I1" :input-id "I2" :search-id "I3")
	;; click "Add"
	(do-request `(("add" . "Add")
		      (,weblocks::*action-string* . "abc125")))
	(render-widget-body grid :form-id "I1" :input-id "I2" :search-id "I3")
	;; click "submit"
	(do-request `(("name" . "Jill")
		      ("manager" . "Jim")
		      ("submit" . "Submit")
		      (,weblocks::*action-string* . "abc133")))
	(render-widget-body grid :form-id "I1" :input-id "I2" :search-id "I3")))
  (htm
   ;; datagrid
   (:div :class "data-mining-bar"
	 (:p :class "datagrid-select-bar"
	     (:strong "Select: ")
	     (:a :href "/foo/bar?action=abc123"
		 :onclick "initiateAction(\"abc123\", \"weblocks-session=1%3ATEST\"); return false;" "All")
	     ", "
	     (:a :href "/foo/bar?action=abc124"
		 :onclick "initiateAction(\"abc124\", \"weblocks-session=1%3ATEST\"); return false;" "None")))
   (:div :class "widget flash" :id "id-123" "<!-- empty flash -->")
   (:form
    :class "dataseq-form"
    :action "/foo/bar"
    :method "get"
    :onsubmit "initiateFormAction(\"abc125\", $(this), \"weblocks-session=1%3ATEST\"); return false;"
    (:div :class "extra-top-1" "<!-- empty -->")
    (:div :class "extra-top-2" "<!-- empty -->")
    (:div :class "extra-top-3" "<!-- empty -->")
    (:fieldset
     (:div :class "datagrid-body"
	   #.(table-header-template
	      '((:th :class "select" "")
		(:th :class "name sort-asc" (:span #.(link-action-template "abc126" "Name")))
		(:th :class "manager" (:span #.(link-action-template "abc127" "Manager")))
		(:th (:span :class "label" "Test")))
	      '((:tr
		 (:td :class "select"
		  (:div (:input :name "item-1" :type "checkbox" :value "f")))
		 (:td :class "name" (:span :class "value" "Joe"))
		 (:td :class "manager" (:span :class "value" "Jim"))
		 (:td (:span :class "value missing" "Not Specified"))))
	      :summary "Ordered by name, ascending."))
     (:div :class "operations"
	   (:input :name "add" :type "submit" :class "submit" :value "Add"
		   :onclick "disableIrrelevantButtons(this);")
	   (:input :name "delete" :type "submit" :class "submit" :value "Delete"
		   :onclick "disableIrrelevantButtons(this);"))
     (:input :name "action" :type "hidden" :value "abc125"))
    (:div :class "extra-bottom-1" "<!-- empty -->")
    (:div :class "extra-bottom-2" "<!-- empty -->")
    (:div :class "extra-bottom-3" "<!-- empty -->"))
   ;; "Add Employee" clicked
   (:div :class "data-mining-bar"
	 (:p :class "datagrid-select-bar"
	     (:strong "Select: ")
	     (:a :href "/foo/bar?action=abc128"
		 :onclick "initiateAction(\"abc128\", \"weblocks-session=1%3ATEST\"); return false;" "All")
	     ", "
	     (:a :href "/foo/bar?action=abc129"
		 :onclick "initiateAction(\"abc129\", \"weblocks-session=1%3ATEST\"); return false;" "None")))
   (:div :class "widget flash" :id "id-123" "<!-- empty flash -->")
   (:form
    :class "dataseq-form"
    :action "/foo/bar"
    :method "get"
    :onsubmit "initiateFormAction(\"abc130\", $(this), \"weblocks-session=1%3ATEST\"); return false;"
    (:div :class "extra-top-1" "<!-- empty -->")
    (:div :class "extra-top-2" "<!-- empty -->")
    (:div :class "extra-top-3" "<!-- empty -->")
    (:fieldset
     (:div :class "datagrid-body"
	   #.(table-header-template
	      '((:th :class "select" "")
		(:th :class "name sort-asc" (:span #.(link-action-template "abc131" "Name")))
		(:th :class "manager" (:span #.(link-action-template "abc132" "Manager"))))
	      '((:tr
		 (:td :class "select"
		  (:div (:input :name "item-1" :type "checkbox" :value "f")))
		 (:td :class "name" (:span :class "value" "Joe"))
		 (:td :class "manager" (:span :class "value" "Jim"))))
	      :summary "Ordered by name, ascending."))
     (:input :name "action" :type "hidden" :value "abc130"))
    (:div :class "extra-bottom-1" "<!-- empty -->")
    (:div :class "extra-bottom-2" "<!-- empty -->")
    (:div :class "extra-bottom-3" "<!-- empty -->"))
   (:div :class "widget data-editor dataform" :id "id-123"
	 #.(form-header-template
	    "abc133"
	    '((:li :class "name"
	       (:label :class "input" :for "id-123"
		(:span :class "slot-name"
		       (:span :class "extra" "Name:&nbsp;"
			      (:em :class "required-slot" "(required)&nbsp;"))))
               (:input :type "text" :name "name" :maxlength "40" :id "id-123"))
	      (:li :class "manager"
	       (:label :class "input" :for "id-123"
		(:span :class "slot-name"
		       (:span :class "extra" "Manager:&nbsp;")))
               (:input :type "text" :name "manager" :value "Jim" :maxlength "40" :id "id-123")))
	    :method "post"))
   ;; "Submit" clicked
   (:div :class "data-mining-bar"
	 (:p :class "datagrid-select-bar"
	     (:strong "Select: ")
	     (:a :href "/foo/bar?action=abc134"
		 :onclick "initiateAction(\"abc134\", \"weblocks-session=1%3ATEST\"); return false;" "All")
	     ", "
	     (:a :href "/foo/bar?action=abc135"
		 :onclick "initiateAction(\"abc135\", \"weblocks-session=1%3ATEST\"); return false;" "None")))
   (:div :class "widget flash" :id "id-123"
	 (:div :class "view"
	 (:div :class "extra-top-1" "<!-- empty -->")
	 (:div :class "extra-top-2" "<!-- empty -->")
	 (:div :class "extra-top-3" "<!-- empty -->")
	 (:ul :class "messages"
	      (:li
	       (:div :class "widget string"
		     (:p "Added Employee."))))
	 (:div :class "extra-bottom-1" "<!-- empty -->")
	 (:div :class "extra-bottom-2" "<!-- empty -->")
	 (:div :class "extra-bottom-3" "<!-- empty -->"))
         (str (with-javascript-to-string "$('id-123').show();")))
   (:form
    :class "dataseq-form"
    :action "/foo/bar"
    :method "get"
    :onsubmit "initiateFormAction(\"abc136\", $(this), \"weblocks-session=1%3ATEST\"); return false;"
    (:div :class "extra-top-1" "<!-- empty -->")
    (:div :class "extra-top-2" "<!-- empty -->")
    (:div :class "extra-top-3" "<!-- empty -->")
    (:fieldset
     (:div :class "datagrid-body"
	   #.(table-header-template
	      '((:th :class "select" "")
		(:th :class "name sort-asc" (:span #.(link-action-template "abc137" "Name")))
		(:th :class "manager" (:span #.(link-action-template "abc138" "Manager"))))
	      '((:tr
		 (:td :class "select"
		  (:div (:input :name "item-3" :type "checkbox" :value "f")))
		 (:td :class "name" (:span :class "value" "Jill"))
		 (:td :class "manager" (:span :class "value" "Jim")))
		(:tr :class "altern"
		 (:td :class "select"
		  (:div (:input :name "item-1" :type "checkbox" :value "f")))
		 (:td :class "name" (:span :class "value" "Joe"))
		 (:td :class "manager" (:span :class "value" "Jim"))))
	      :summary "Ordered by name, ascending."))
     (:div :class "operations"
	   (:input :name "add" :type "submit" :class "submit" :value "Add"
		   :onclick "disableIrrelevantButtons(this);")
	   (:input :name "delete" :type "submit" :class "submit" :value "Delete"
		   :onclick "disableIrrelevantButtons(this);"))
     (:input :name "action" :type "hidden" :value "abc136"))
    (:div :class "extra-bottom-1" "<!-- empty -->")
    (:div :class "extra-bottom-2" "<!-- empty -->")
    (:div :class "extra-bottom-3" "<!-- empty -->"))))

;;; testing render-item-ops-bar for gridedit
(deftest-html dataseq-render-operations-gridedit-1
    (with-request :get nil
      (persist-object *default-store* *joe*)
      (let ((grid (make-instance 'gridedit
				 :data-class 'employee)))
	(let ((*weblocks-output-stream* (make-string-output-stream)))
	  (declare (special *weblocks-output-stream*))
	  (render-widget-body grid))
	(dataseq-render-operations grid)))
  (:div :class "operations"
	(:input :name "add" :type "submit" :class "submit" :value "Add"
		:onclick "disableIrrelevantButtons(this);")
	(:input :name "delete" :type "submit" :class "submit" :value "Delete"
		:onclick "disableIrrelevantButtons(this);")))

(deftest-html dataseq-render-operations-gridedit-2
    (with-request :get nil
      (persist-object *default-store* *joe*)
      (let ((grid (make-instance 'gridedit
				 :data-class 'employee
				 :allow-select-p nil)))
	(let ((*weblocks-output-stream* (make-string-output-stream)))
	  (declare (special *weblocks-output-stream*))
	  (render-widget-body grid))
	(dataseq-render-operations grid)))
  (:div :class "operations"
	(:input :name "add" :type "submit" :class "submit" :value "Add"
		:onclick "disableIrrelevantButtons(this);")))

