
(in-package :weblocks-test)

;;; testing gridedit-create-new-item-widget
(deftest-html gridedit-create-new-item-widget-1
    (with-request :get nil
      (render-widget
       (gridedit-create-new-item-widget (make-instance 'gridedit :data (list *joe*)
								 :data-class 'employee))))
  (:div :class "widget dataform" :id "widget-123"
	#.(form-header-template
	   "abc123"
	   '((:li :class "name"
	      (:label
	       (:span :class "slot-name"
		      (:span :class "extra" "Name:&nbsp;"
			     (:em :class "required-slot" "(required)&nbsp;")))
	       (:input :type "text" :name "name" :maxlength "40")))
	     (:li :class "manager"
	      (:label
	       (:span :class "slot-name"
		      (:span :class "extra" "Manager:&nbsp;"))
	       (:input :type "text" :name "manager" :value "Jim" :maxlength "40"))))
	   :method "post"
	   :title-action "Adding:&nbsp;")))

(deftest-html gridedit-create-new-item-widget-2
    (with-request :get nil
      (render-widget
       (gridedit-create-new-item-widget (make-instance 'gridedit :data (list *joe*)
								 :data-class 'employee
								 :widget-args '(:slots ((name . foo)))))))
  (:div :class "widget dataform" :id "widget-123"
	#.(form-header-template
	   "abc123"
	   '((:li :class "name"
	      (:label
	       (:span :class "slot-name"
		      (:span :class "extra" "Foo:&nbsp;"
			     (:em :class "required-slot" "(required)&nbsp;")))
	       (:input :type "text" :name "name" :maxlength "40")))
	     (:li :class "manager"
	      (:label
	       (:span :class "slot-name"
		      (:span :class "extra" "Manager:&nbsp;"))
	       (:input :type "text" :name "manager" :value "Jim" :maxlength "40"))))
	   :method "post"
	   :title-action "Adding:&nbsp;")))

(deftest-html gridedit-create-new-item-widget-3
    (with-request :get nil
      (render-widget
       (gridedit-create-new-item-widget (make-instance 'gridedit :data (list *joe*)
								 :data-class 'employee
								 :widget-args '(:slots ((name . foo)))
								 :item-widget-args
								 '(:slots ((name . bar)))))))
  (:div :class "widget dataform" :id "widget-123"
	#.(form-header-template
	   "abc123"
	   '((:li :class "name"
	      (:label
	       (:span :class "slot-name"
		      (:span :class "extra" "Bar:&nbsp;"
			     (:em :class "required-slot" "(required)&nbsp;")))
	       (:input :type "text" :name "name" :maxlength "40")))
	     (:li :class "manager"
	      (:label
	       (:span :class "slot-name"
		      (:span :class "extra" "Manager:&nbsp;"))
	       (:input :type "text" :name "manager" :value "Jim" :maxlength "40"))))
	   :method "post"
	   :title-action "Adding:&nbsp;")))

;;; testing gridedit-create-drilldown-widget
(deftest-html gridedit-create-drilldown-widget-1
    (with-request :get nil
      (render-widget
       (gridedit-create-drilldown-widget (make-instance 'gridedit :data (list *joe*)
								  :data-class 'employee)
					 *joe*)))
  (:div :class "widget dataform" :id "widget-123"
	#.(form-header-template
	   "abc123"
	   '((:li :class "name"
	      (:label
	       (:span :class "slot-name"
		      (:span :class "extra" "Name:&nbsp;"
			     (:em :class "required-slot" "(required)&nbsp;")))
	       (:input :type "text" :name "name" :value "Joe" :maxlength "40")))
	     (:li :class "manager"
	      (:label
	       (:span :class "slot-name"
		      (:span :class "extra" "Manager:&nbsp;"))
	       (:input :type "text" :name "manager" :value "Jim" :maxlength "40"))))
	   :method "post"
	   :title-action "Modifying:&nbsp;")))

(deftest-html gridedit-create-drilldown-widget-2
    (with-request :get nil
      (render-widget
       (gridedit-create-drilldown-widget (make-instance 'gridedit :data (list *joe*)
								  :data-class 'employee
								  :drilldown-type :view)
					 *joe*)))
  (:div :class "widget dataform" :id "widget-123"
	#.(data-header-template
	   "abc123"
	   '((:li :class "name" (:span :class "label" "Name:&nbsp;") (:span :class "value" "Joe"))
	      (:li :class "manager" (:span :class "label" "Manager:&nbsp;")
	       (:span :class "value" "Jim")))
	   :postslots
	   `((:div :class "submit"
		   ,(link-action-template "abc123" "Modify") (str "&nbsp;")
		   ,(link-action-template "abc124" "Close"))))))

(deftest-html gridedit-create-drilldown-widget-3
    (with-request :get nil
      (render-widget
       (gridedit-create-drilldown-widget (make-instance 'gridedit :data (list *joe*)
								  :data-class 'employee
								  :widget-args '(:slots ((name . foo))))
					 *joe*)))
  (:div :class "widget dataform" :id "widget-123"
	#.(form-header-template
	   "abc123"
	   '((:li :class "name"
	      (:label
	       (:span :class "slot-name"
		      (:span :class "extra" "Foo:&nbsp;"
			     (:em :class "required-slot" "(required)&nbsp;")))
	       (:input :type "text" :name "name" :value "Joe" :maxlength "40")))
	     (:li :class "manager"
	      (:label
	       (:span :class "slot-name"
		      (:span :class "extra" "Manager:&nbsp;"))
	       (:input :type "text" :name "manager" :value "Jim" :maxlength "40"))))
	   :method "post"
	   :title-action "Modifying:&nbsp;")))

(deftest-html gridedit-create-drilldown-widget-4
    (with-request :get nil
      (render-widget
       (gridedit-create-drilldown-widget (make-instance 'gridedit :data (list *joe*)
								  :data-class 'employee
								  :widget-args '(:slots ((name . foo)))
								  :item-widget-args
								  '(:slots ((name . bar))))
					 *joe*)))
  (:div :class "widget dataform" :id "widget-123"
	#.(form-header-template
	   "abc123"
	   '((:li :class "name"
	      (:label
	       (:span :class "slot-name"
		      (:span :class "extra" "Bar:&nbsp;"
			     (:em :class "required-slot" "(required)&nbsp;")))
	       (:input :type "text" :name "name" :value "Joe" :maxlength "40")))
	     (:li :class "manager"
	      (:label
	       (:span :class "slot-name"
		      (:span :class "extra" "Manager:&nbsp;"))
	       (:input :type "text" :name "manager" :value "Jim" :maxlength "40"))))
	   :method "post"
	   :title-action "Modifying:&nbsp;")))

;;; testing gridedit-add-item
(deftest gridedit-add-item-1
    (with-request :get nil
      (let ((grid (make-instance 'gridedit :data (list *joe*)
					   :data-class 'employee
					   :allow-pagination-p nil)))
	(weblocks::gridedit-add-item grid *joe*)
	(length (datagrid-data grid))))
  2)

(deftest gridedit-add-item-2
    (with-request :get nil
      (let ((grid (make-instance 'gridedit :data (list *joe*)
					   :data-class 'employee
					   :on-add-item (lambda (grid item)
							  (push item (slot-value grid 'weblocks::data))
							  (push item (slot-value grid 'weblocks::data)))
					   :allow-pagination-p nil)))
	(weblocks::gridedit-add-item grid *joe*)
	(length (datagrid-data grid))))
  3)

;;; testing gridedit-delete-items
(deftest gridedit-delete-items-1
    (with-request :get nil
      (let ((grid (make-instance 'gridedit
				 :data (list *joe* *bob*)
				 :data-class 'employee
				 :allow-pagination-p nil)))
	(gridedit-delete-items grid (cons :none (list (object-id *joe*))))
	(mapcar #'first-name (datagrid-data grid))))
  ("Bob"))

(deftest gridedit-delete-items-2
    (with-request :get nil
      (let ((grid (make-instance 'gridedit
				 :data (list *joe* *bob*)
				 :data-class 'employee
				 :allow-pagination-p nil)))
	(gridedit-delete-items grid (cons :none (list (object-id *joe*)
						      (object-id *bob*))))
	(mapcar #'first-name (datagrid-data grid))))
  nil)

;;; testing render-widget-body for gridedit
(deftest-html render-widget-body-gridedit-1
    (with-request :get nil
      (let ((grid (make-instance 'gridedit :data (list *joe*)
					   :data-class 'employee
					   :forbid-sorting-on '(test)
					   :allow-searching-p nil
					   :allow-drilldown-p nil
					   :allow-pagination-p nil)))
	;; render datagrid
	(render-widget-body grid :custom-slots '(test) :form-id "I1" :input-id "I2" :search-id "I3")
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
   (:div :class "widget flash" :id "widget-123" "<!-- empty flash -->")
   (:form
    :class "datagrid-form"
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
		(:th :class "name sort-ascending" (:span #.(link-action-template "abc126" "Name")))
		(:th :class "manager" (:span #.(link-action-template "abc127" "Manager")))
		(:th :class "test" "Test"))
	      '((:tr
		 (:td :class "select"
		  (:div (:input :name "item-1" :type "checkbox" :value "t")))
		 (:td :class "name" (:span :class "value" "Joe"))
		 (:td :class "manager" (:span :class "value" "Jim"))
		 (:td :class "test" (:span :class "value missing" "Not Specified"))))
	      :summary "Ordered by name, ascending."))
     (:div :class "item-operations"
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
   (:div :class "widget flash" :id "widget-123" "<!-- empty flash -->")
   (:form
    :class "datagrid-form"
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
		(:th :class "name sort-ascending" (:span #.(link-action-template "abc131" "Name")))
		(:th :class "manager" (:span #.(link-action-template "abc132" "Manager"))))
	      '((:tr
		 (:td :class "select"
		  (:div (:input :name "item-1" :type "checkbox" :value "t")))
		 (:td :class "name" (:span :class "value" "Joe"))
		 (:td :class "manager" (:span :class "value" "Jim"))))
	      :summary "Ordered by name, ascending."))
     (:input :name "action" :type "hidden" :value "abc130"))
    (:div :class "extra-bottom-1" "<!-- empty -->")
    (:div :class "extra-bottom-2" "<!-- empty -->")
    (:div :class "extra-bottom-3" "<!-- empty -->"))
   (:div :class "widget dataform" :id "widget-123"
	 #.(form-header-template
	    "abc133"
	    '((:li :class "name"
	       (:label
		(:span :class "slot-name"
		       (:span :class "extra" "Name:&nbsp;"
			      (:em :class "required-slot" "(required)&nbsp;")))
		(:input :type "text" :name "name" :maxlength "40")))
	      (:li :class "manager"
	       (:label
		(:span :class "slot-name"
		       (:span :class "extra" "Manager:&nbsp;"))
		(:input :type "text" :name "manager" :value "Jim" :maxlength "40"))))
	    :method "post"
	    :title-action "Adding:&nbsp;"))
   ;; "Submit" clicked
   (:div :class "data-mining-bar"
	 (:p :class "datagrid-select-bar"
	     (:strong "Select: ")
	     (:a :href "/foo/bar?action=abc134"
		 :onclick "initiateAction(\"abc134\", \"weblocks-session=1%3ATEST\"); return false;" "All")
	     ", "
	     (:a :href "/foo/bar?action=abc135"
		 :onclick "initiateAction(\"abc135\", \"weblocks-session=1%3ATEST\"); return false;" "None")))
   (:div :class "widget flash" :id "widget-123"
	 (:div :class "renderer"
	 (:div :class "extra-top-1" "<!-- empty -->")
	 (:div :class "extra-top-2" "<!-- empty -->")
	 (:div :class "extra-top-3" "<!-- empty -->")
	 (:ul :class "messages"
	      (:li
	       (:div :class "widget string"
		     (:p "Item added."))))
	 (:div :class "extra-bottom-1" "<!-- empty -->")
	 (:div :class "extra-bottom-2" "<!-- empty -->")
	 (:div :class "extra-bottom-3" "<!-- empty -->")))
   (:form
    :class "datagrid-form"
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
		(:th :class "name sort-ascending" (:span #.(link-action-template "abc137" "Name")))
		(:th :class "manager" (:span #.(link-action-template "abc138" "Manager"))))
	      '((:tr
		 (:td :class "select"
		  (:div (:input :name "item-1002" :type "checkbox" :value "t")))
		 (:td :class "name" (:span :class "value" "Jill"))
		 (:td :class "manager" (:span :class "value" "Jim")))
		(:tr :class "altern"
		 (:td :class "select"
		  (:div (:input :name "item-1" :type "checkbox" :value "t")))
		 (:td :class "name" (:span :class "value" "Joe"))
		 (:td :class "manager" (:span :class "value" "Jim"))))
	      :summary "Ordered by name, ascending."))
     (:div :class "item-operations"
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
      (let ((grid (make-instance 'gridedit :data (list *joe*)
					   :data-class 'employee
					   :forbid-sorting-on '(test)
					   :allow-searching-p nil
					   :allow-drilldown-p nil
					   :allow-pagination-p nil)))
	;; render datagrid
	(render-widget-body grid :form-id "I1" :input-id "I2" :search-id "I3")
	;; click "Delete"
	(do-request `(("item-1" . "on")
		      ("delete" . "Delete")
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
   (:div :class "widget flash" :id "widget-123" "<!-- empty flash -->")
   (:form
    :class "datagrid-form"
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
		(:th :class "name sort-ascending" (:span #.(link-action-template "abc126" "Name")))
		(:th :class "manager" (:span #.(link-action-template "abc127" "Manager"))))
	      '((:tr
		 (:td :class "select"
		  (:div (:input :name "item-1" :type "checkbox" :value "t")))
		 (:td :class "name" (:span :class "value" "Joe"))
		 (:td :class "manager" (:span :class "value" "Jim"))))
	      :summary "Ordered by name, ascending."))
     (:div :class "item-operations"
	   (:input :name "add" :type "submit" :class "submit" :value "Add"
		   :onclick "disableIrrelevantButtons(this);")
	   (:input :name "delete" :type "submit" :class "submit" :value "Delete"
		   :onclick "disableIrrelevantButtons(this);"))
     (:input :name "action" :type "hidden" :value "abc125"))
    (:div :class "extra-bottom-1" "<!-- empty -->")
    (:div :class "extra-bottom-2" "<!-- empty -->")
    (:div :class "extra-bottom-3" "<!-- empty -->"))
   ;; "Delete" clicked
   (:div :class "widget flash" :id "widget-123"
	 (:div :class "renderer"
	 (:div :class "extra-top-1" "<!-- empty -->")
	 (:div :class "extra-top-2" "<!-- empty -->")
	 (:div :class "extra-top-3" "<!-- empty -->")
	 (:ul :class "messages"
	      (:li
	       (:div :class "widget string"
		     (:p "1 item deleted."))))
	 (:div :class "extra-bottom-1" "<!-- empty -->")
	 (:div :class "extra-bottom-2" "<!-- empty -->")
	 (:div :class "extra-bottom-3" "<!-- empty -->")))
   (:form
    :class "datagrid-form"
    :action "/foo/bar"
    :method "get"
    :onsubmit "initiateFormAction(\"abc128\", $(this), \"weblocks-session=1%3ATEST\"); return false;"
    (:div :class "extra-top-1" "<!-- empty -->")
    (:div :class "extra-top-2" "<!-- empty -->")
    (:div :class "extra-top-3" "<!-- empty -->")
    (:fieldset
     (:div :class "datagrid-body"
	   (:div :class "renderer table empty-table"
	       (:div :class "extra-top-1" "<!-- empty -->")
	       (:div :class "extra-top-2" "<!-- empty -->")
	       (:div :class "extra-top-3" "<!-- empty -->")
	       (:p (:span :class "message" "No information available."))
	       (:div :class "extra-bottom-1" "<!-- empty -->")
	       (:div :class "extra-bottom-2" "<!-- empty -->")
	       (:div :class "extra-bottom-3" "<!-- empty -->")))
     (:div :class "item-operations"
	   (:input :name "add" :type "submit" :class "submit" :value "Add"
		   :onclick "disableIrrelevantButtons(this);"))
     (:input :name "action" :type "hidden" :value "abc128"))
    (:div :class "extra-bottom-1" "<!-- empty -->")
    (:div :class "extra-bottom-2" "<!-- empty -->")
    (:div :class "extra-bottom-3" "<!-- empty -->"))))

(deftest-html render-widget-body-gridedit-3
    (with-request :get nil
      (let ((grid (make-instance 'gridedit :data (list (copy-template *joe*))
					   :data-class 'employee
					   :forbid-sorting-on '(test)
					   :allow-searching-p nil
					   :allow-pagination-p nil)))
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
   (:div :class "widget flash" :id "widget-123" "<!-- empty flash -->")
   (:form
    :class "datagrid-form"
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
		(:th :class "name sort-ascending" (:span #.(link-action-template "abc126" "Name")))
		(:th :class "manager" (:span #.(link-action-template "abc127" "Manager")))
		(:th :class "drilldown edit" ""))
	      '((:tr :onclick "initiateActionOnEmptySelection(\"abc128\", \"weblocks-session=1%3ATEST\");"
		     :onmouseover "this.style.cursor = \"pointer\";"
		     :style "cursor: expression(\"hand\");"
		 (:td :class "select"
		      :onclick "stopPropagation(event);"
		      :style "cursor: default;"
		  (:div (:input :name "item-1" :type "checkbox" :value "t")))
		 (:td :class "name" (:span :class "value" "Joe"))
		 (:td :class "manager" (:span :class "value" "Jim"))
		 (:td :class "drilldown edit"
		  (:noscript (:div (:a :href "/foo/bar?action=abc128" "Edit"))))))
	      :summary "Ordered by name, ascending."))
     (:div :class "item-operations"
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
   (:div :class "widget flash" :id "widget-123" "<!-- empty flash -->")
   (:form
    :class "datagrid-form"
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
		(:th :class "name sort-ascending" (:span #.(link-action-template "abc132" "Name")))
		(:th :class "manager" (:span #.(link-action-template "abc133" "Manager")))
		(:th :class "drilldown edit" ""))
	      '((:tr :class "drilled-down"
		     :onclick "initiateActionOnEmptySelection(\"abc134\", \"weblocks-session=1%3ATEST\");"
		     :onmouseover "this.style.cursor = \"pointer\";"
		     :style "cursor: expression(\"hand\");"
		 (:td :class "select"
		      :onclick "stopPropagation(event);"
		      :style "cursor: default;"
		  (:div (:input :name "item-1" :type "checkbox" :value "t")))
		 (:td :class "name" (:span :class "value" "Joe"))
		 (:td :class "manager" (:span :class "value" "Jim"))
		 (:td :class "drilldown edit"
		  (:noscript (:div (:a :href "/foo/bar?action=abc134" "Edit"))))))
	      :summary "Ordered by name, ascending."))
     (:input :name "action" :type "hidden" :value "abc131"))
    (:div :class "extra-bottom-1" "<!-- empty -->")
    (:div :class "extra-bottom-2" "<!-- empty -->")
    (:div :class "extra-bottom-3" "<!-- empty -->"))
    (:div :class "widget dataform" :id "widget-123"
	 #.(form-header-template
	    "abc135"
	    '((:li :class "name"
	       (:label
		(:span :class "slot-name"
		       (:span :class "extra" "Name:&nbsp;"
			      (:em :class "required-slot" "(required)&nbsp;")))
		(:input :type "text" :name "name" :value "Joe" :maxlength "40")))
	      (:li :class "manager"
	       (:label
		(:span :class "slot-name"
		       (:span :class "extra" "Manager:&nbsp;"))
		(:input :type "text" :name "manager" :value "Jim" :maxlength "40"))))
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
    (:div :class "widget flash" :id "widget-123"
	  (:div :class "renderer"
		(:div :class "extra-top-1" "<!-- empty -->")
		(:div :class "extra-top-2" "<!-- empty -->")
		(:div :class "extra-top-3" "<!-- empty -->")
		(:ul :class "messages" (:li (:div :class "widget string" (:p "Item Modified."))))
		(:div :class "extra-bottom-1" "<!-- empty -->")
		(:div :class "extra-bottom-2" "<!-- empty -->")
		(:div :class "extra-bottom-3" "<!-- empty -->")))
    (:form
     :class "datagrid-form"
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
		 (:th :class "name sort-ascending" (:span #.(link-action-template "abc139" "Name")))
		 (:th :class "manager" (:span #.(link-action-template "abc140" "Manager")))
		 (:th :class "drilldown edit" ""))
	       '((:tr :onclick "initiateActionOnEmptySelection(\"abc141\", \"weblocks-session=1%3ATEST\");"
		  :onmouseover "this.style.cursor = \"pointer\";"
		  :style "cursor: expression(\"hand\");"
		  (:td :class "select"
		   :onclick "stopPropagation(event);"
		   :style "cursor: default;"
		   (:div (:input :name "item-1" :type "checkbox" :value "t")))
		  (:td :class "name" (:span :class "value" "Jill"))
		  (:td :class "manager" (:span :class "value" "Jim"))
		  (:td :class "drilldown edit"
		   (:noscript (:div (:a :href "/foo/bar?action=abc141" "Edit"))))))
	       :summary "Ordered by name, ascending."))
      (:div :class "item-operations"
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
      (let ((grid (make-instance 'gridedit :data (list (copy-template *joe*))
					   :data-class 'employee
					   :forbid-sorting-on '(test)
					   :allow-searching-p nil
					   :drilldown-type :view
					   :allow-pagination-p nil)))
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
   (:div :class "widget flash" :id "widget-123" "<!-- empty flash -->")
   (:form
    :class "datagrid-form"
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
		(:th :class "name sort-ascending" (:span #.(link-action-template "abc126" "Name")))
		(:th :class "manager" (:span #.(link-action-template "abc127" "Manager")))
		(:th :class "drilldown edit" ""))
	      '((:tr :onclick "initiateActionOnEmptySelection(\"abc128\", \"weblocks-session=1%3ATEST\");"
		     :onmouseover "this.style.cursor = \"pointer\";"
		     :style "cursor: expression(\"hand\");"
		 (:td :class "select"
		      :onclick "stopPropagation(event);"
		      :style "cursor: default;"
		  (:div (:input :name "item-1" :type "checkbox" :value "t")))
		 (:td :class "name" (:span :class "value" "Joe"))
		 (:td :class "manager" (:span :class "value" "Jim"))
		 (:td :class "drilldown edit"
		  (:noscript (:div (:a :href "/foo/bar?action=abc128" "Edit"))))))
	      :summary "Ordered by name, ascending."))
     (:div :class "item-operations"
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
   (:div :class "widget flash" :id "widget-123" "<!-- empty flash -->")
   (:form
    :class "datagrid-form"
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
		(:th :class "name sort-ascending" (:span #.(link-action-template "abc132" "Name")))
		(:th :class "manager" (:span #.(link-action-template "abc133" "Manager")))
		(:th :class "drilldown edit" ""))
	      '((:tr :class "drilled-down"
		     :onclick "initiateActionOnEmptySelection(\"abc134\", \"weblocks-session=1%3ATEST\");"
		     :onmouseover "this.style.cursor = \"pointer\";"
		     :style "cursor: expression(\"hand\");"
		 (:td :class "select"
		      :onclick "stopPropagation(event);"
		      :style "cursor: default;"
		  (:div (:input :name "item-1" :type "checkbox" :value "t")))
		 (:td :class "name" (:span :class "value" "Joe"))
		 (:td :class "manager" (:span :class "value" "Jim"))
		 (:td :class "drilldown edit"
		  (:noscript (:div (:a :href "/foo/bar?action=abc134" "Edit"))))))
	      :summary "Ordered by name, ascending."))
     (:input :name "action" :type "hidden" :value "abc131"))
    (:div :class "extra-bottom-1" "<!-- empty -->")
    (:div :class "extra-bottom-2" "<!-- empty -->")
    (:div :class "extra-bottom-3" "<!-- empty -->"))
    (:div :class "widget dataform" :id "widget-123"
	 #.(data-header-template
	    "abc135"
	    '((:li :class "name" (:span :class "label" "Name:&nbsp;") (:span :class "value" "Joe"))
	      (:li :class "manager" (:span :class "label" "Manager:&nbsp;")
	       (:span :class "value" "Jim")))
	    :postslots
	    `((:div :class "submit"
		    ,(link-action-template "abc135" "Modify") (str "&nbsp;")
		    ,(link-action-template "abc136" "Close")))))
    ;; Submit clicked
    (:div :class "data-mining-bar"
	 (:p :class "datagrid-select-bar"
	     (:strong "Select: ")
	     (:a :href "/foo/bar?action=abc137"
		 :onclick "initiateAction(\"abc137\", \"weblocks-session=1%3ATEST\"); return false;" "All")
	     ", "
	     (:a :href "/foo/bar?action=abc138"
		 :onclick "initiateAction(\"abc138\", \"weblocks-session=1%3ATEST\"); return false;" "None")))
    (:div :class "widget flash" :id "widget-123" "<!-- empty flash -->")
    (:form
     :class "datagrid-form"
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
		 (:th :class "name sort-ascending" (:span #.(link-action-template "abc140" "Name")))
		 (:th :class "manager" (:span #.(link-action-template "abc141" "Manager")))
		 (:th :class "drilldown edit" ""))
	       '((:tr :onclick "initiateActionOnEmptySelection(\"abc142\", \"weblocks-session=1%3ATEST\");"
		  :onmouseover "this.style.cursor = \"pointer\";"
		  :style "cursor: expression(\"hand\");"
		  (:td :class "select"
		   :onclick "stopPropagation(event);"
		   :style "cursor: default;"
		   (:div (:input :name "item-1" :type "checkbox" :value "t")))
		  (:td :class "name" (:span :class "value" "Joe"))
		  (:td :class "manager" (:span :class "value" "Jim"))
		  (:td :class "drilldown edit"
		   (:noscript (:div (:a :href "/foo/bar?action=abc142" "Edit"))))))
	       :summary "Ordered by name, ascending."))
      (:div :class "item-operations"
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
      (make-request-ajax)
      (let ((grid (make-instance 'gridedit :data (list (copy-template *joe*))
					   :data-class 'employee
					   :forbid-sorting-on '(test)
					   :allow-searching-p nil
					   :allow-pagination-p nil)))
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
   (:div :class "widget flash" :id "widget-123" "<!-- empty flash -->")
   (:form
    :class "datagrid-form"
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
		(:th :class "name sort-ascending" (:span #.(link-action-template "abc126" "Name")))
		(:th :class "manager" (:span #.(link-action-template "abc127" "Manager")))
		(:th :class "drilldown edit" ""))
	      '((:tr :onclick "initiateActionOnEmptySelection(\"abc128\", \"weblocks-session=1%3ATEST\");"
		     :onmouseover "this.style.cursor = \"pointer\";"
		     :style "cursor: expression(\"hand\");"
		 (:td :class "select"
		      :onclick "stopPropagation(event);"
		      :style "cursor: default;"
		  (:div (:input :name "item-1" :type "checkbox" :value "t")))
		 (:td :class "name" (:span :class "value" "Joe"))
		 (:td :class "manager" (:span :class "value" "Jim"))
		 (:td :class "drilldown edit" "")))
	      :summary "Ordered by name, ascending."))
     (:div :class "item-operations"
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
      (let ((grid (make-instance 'gridedit :data (list *joe*)
					   :data-class 'employee
					   :forbid-sorting-on '(test)
					   :allow-searching-p nil
					   :allow-drilldown-p nil)))
	;; set items per page
	(setf (pagination-items-per-page (datagrid-pagination-widget grid)) 1)
	;; render datagrid
	(render-widget-body grid :custom-slots '(test) :form-id "I1" :input-id "I2" :search-id "I3")
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
   (:div :class "widget flash" :id "widget-123" "<!-- empty flash -->")
   (:form
    :class "datagrid-form"
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
		(:th :class "name sort-ascending" (:span #.(link-action-template "abc126" "Name")))
		(:th :class "manager" (:span #.(link-action-template "abc127" "Manager")))
		(:th :class "test" "Test"))
	      '((:tr
		 (:td :class "select"
		  (:div (:input :name "item-1" :type "checkbox" :value "t")))
		 (:td :class "name" (:span :class "value" "Joe"))
		 (:td :class "manager" (:span :class "value" "Jim"))
		 (:td :class "test" (:span :class "value missing" "Not Specified"))))
	      :summary "Ordered by name, ascending."))
     (:div :class "item-operations"
	   (:input :name "add" :type "submit" :class "submit" :value "Add"
		   :onclick "disableIrrelevantButtons(this);")
	   (:input :name "delete" :type "submit" :class "submit" :value "Delete"
		   :onclick "disableIrrelevantButtons(this);"))
     (:input :name "action" :type "hidden" :value "abc125"))
    (:div :class "extra-bottom-1" "<!-- empty -->")
    (:div :class "extra-bottom-2" "<!-- empty -->")
    (:div :class "extra-bottom-3" "<!-- empty -->"))
   (:div :class "widget pagination" :id "widget-123"
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
   (:div :class "widget flash" :id "widget-123" "<!-- empty flash -->")
   (:form
    :class "datagrid-form"
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
		(:th :class "name sort-ascending" (:span #.(link-action-template "abc131" "Name")))
		(:th :class "manager" (:span #.(link-action-template "abc132" "Manager"))))
	      '((:tr
		 (:td :class "select"
		  (:div (:input :name "item-1" :type "checkbox" :value "t")))
		 (:td :class "name" (:span :class "value" "Joe"))
		 (:td :class "manager" (:span :class "value" "Jim"))))
	      :summary "Ordered by name, ascending."))
     (:input :name "action" :type "hidden" :value "abc130"))
    (:div :class "extra-bottom-1" "<!-- empty -->")
    (:div :class "extra-bottom-2" "<!-- empty -->")
    (:div :class "extra-bottom-3" "<!-- empty -->"))
   (:div :class "widget dataform" :id "widget-123"
	 #.(form-header-template
	    "abc133"
	    '((:li :class "name"
	       (:label
		(:span :class "slot-name"
		       (:span :class "extra" "Name:&nbsp;"
			      (:em :class "required-slot" "(required)&nbsp;")))
		(:input :type "text" :name "name" :maxlength "40")))
	      (:li :class "manager"
	       (:label
		(:span :class "slot-name"
		       (:span :class "extra" "Manager:&nbsp;"))
		(:input :type "text" :name "manager" :value "Jim" :maxlength "40"))))
	    :method "post"
	    :title-action "Adding:&nbsp;"))))

