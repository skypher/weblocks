
(in-package :weblocks-test)

;;; testing gridedit-render-new-item-form
(deftest-html gridedit-render-new-item-form-1
    (with-request :get nil
      (gridedit-render-new-item-form (make-instance 'gridedit :data (list *joe*)
						    :data-class 'employee)))
  (:div :class "widget dataform" :id "widget-123"
	#.(form-header-template
	   "abc123"
	   '((:li :class "name"
	      (:label (:span "Name:&nbsp;") (:input :type "text" :name "name")))
	     (:li :class "manager"
	      (:label (:span "Manager:&nbsp;") (:input :type "text" :name "manager" :value "Jim"))))
	   :method "post"
	   :title-action "Adding:&nbsp;")))

;;; testing gridedit-add-item
(deftest gridedit-add-item-1
    (let ((grid (make-instance 'gridedit :data (list *joe*)
			       :data-class 'employee)))
      (weblocks::gridedit-add-item grid *joe*)
      (length (datagrid-data grid)))
  2)

(deftest gridedit-add-item-2
    (let ((grid (make-instance 'gridedit :data (list *joe*)
			       :data-class 'employee
			       :on-add-item (lambda (grid item)
					      (push item (slot-value grid 'weblocks::data))
					      (push item (slot-value grid 'weblocks::data))))))
      (weblocks::gridedit-add-item grid *joe*)
      (length (datagrid-data grid)))
  3)

;;; testing render-widget-body for gridedit
(deftest-html render-widget-body-gridedit-1
    (with-request :get nil
      (let ((grid (make-instance 'gridedit :data (list *joe*)
					   :data-class 'employee
					   :forbid-sorting-on '(test)
					   :allow-searching-p nil)))
	;; render datagrid
	(render-widget-body grid :custom-slots '(test) :form-id "I1" :input-id "I2" :search-id "I3")
	;; click "Add"
	(do-request `(("add" . "Add")
		      (,weblocks::*action-string* . "abc123")))
	(render-widget-body grid :form-id "I1" :input-id "I2" :search-id "I3")
	;; click "submit"
	(do-request `(("name" . "Jill")
		      ("submit" . "Submit")
		      (,weblocks::*action-string* . "abc129")))
	(render-widget-body grid :form-id "I1" :input-id "I2" :search-id "I3")))
  (htm
   ;; datagrid
   (:form
    :action ""
    :method "get"
    :onsubmit "initiateFormAction(\"abc123\", $(this), \"weblocks-session=1%3Atest\"); return false;"
    (:fieldset
     (:div :class "datagrid-body"
	   #.(table-header-template
	      '((:th :class "select" "")
		(:th :class "name sort-ascending" (:span #.(link-action-template "abc124" "Name")))
		(:th :class "manager" (:span #.(link-action-template "abc125" "Manager")))
		(:th :class "test" "Test"))
	      '((:tr
		 (:td :class "select"
		  (:input :type "checkbox" :name "item-1"))
		 (:td :class "name" (:span :class "value" "Joe"))
		 (:td :class "manager" (:span :class "value" "Jim"))
		 (:td :class "test" (:span :class "value missing" "Not Specified"))))
	      :summary "Ordered by name, ascending."))
     (:div :class "item-operations"
	   (:input :name "add" :type "submit" :class "submit" :value "Add"
		   :onclick "disableIrrelevantButtons(this);")
	   (:input :name "delete" :type "submit" :class "submit" :value "Delete"
		   :onclick "disableIrrelevantButtons(this);"))
     (:input :name "action" :type "hidden" :value "abc123")))
   ;; "Add Employee" clicked
   (:form
    :action ""
    :method "get"
    :onsubmit "initiateFormAction(\"abc126\", $(this), \"weblocks-session=1%3Atest\"); return false;"
    (:fieldset
     (:div :class "datagrid-body"
	   #.(table-header-template
	      '((:th :class "select" "")
		(:th :class "name sort-ascending" (:span #.(link-action-template "abc127" "Name")))
		(:th :class "manager" (:span #.(link-action-template "abc128" "Manager"))))
	      '((:tr
		 (:td :class "select"
		  (:input :type "checkbox" :name "item-1"))
		 (:td :class "name" (:span :class "value" "Joe"))
		 (:td :class "manager" (:span :class "value" "Jim"))))
	      :summary "Ordered by name, ascending."))
     (:input :name "action" :type "hidden" :value "abc126")))
   (:div :class "widget dataform" :id "widget-123"
	 #.(form-header-template
	    "abc129"
	    '((:li :class "name"
	       (:label (:span "Name:&nbsp;") (:input :type "text" :name "name")))
	      (:li :class "manager"
	       (:label (:span "Manager:&nbsp;") (:input :type "text" :name "manager" :value "Jim"))))
	    :method "post"
	    :title-action "Adding:&nbsp;"))
   ;; "Submit" clicked
   (:p :class "datagrid-select-bar"
       (:strong "Select: ")
       (:a :href "?action=abc130" :onclick "initiateAction(\"abc130\", \"weblocks-session=1%3Atest\"); return false;" "All")
       ", "
       (:a :href "?action=abc131" :onclick "initiateAction(\"abc131\", \"weblocks-session=1%3Atest\"); return false;" "None"))
   (:form
    :action ""
    :method "get"
    :onsubmit "initiateFormAction(\"abc132\", $(this), \"weblocks-session=1%3Atest\"); return false;"
    (:fieldset
     (:div :class "datagrid-body"
	   #.(table-header-template
	      '((:th :class "select" "")
		(:th :class "name sort-ascending" (:span #.(link-action-template "abc133" "Name")))
		(:th :class "manager" (:span #.(link-action-template "abc134" "Manager"))))
	      '((:tr
		 (:td :class "select"
		  (:input :type "checkbox" :name "item-1002"))
		 (:td :class "name" (:span :class "value" "Jill"))
		 (:td :class "manager" (:span :class "value" "Jim")))
		(:tr :class "altern"
		 (:td :class "select"
		  (:input :type "checkbox" :name "item-1"))
		 (:td :class "name" (:span :class "value" "Joe"))
		 (:td :class "manager" (:span :class "value" "Jim"))))
	      :summary "Ordered by name, ascending."))
     (:div :class "item-operations"
	   (:input :name "add" :type "submit" :class "submit" :value "Add"
		   :onclick "disableIrrelevantButtons(this);")
	   (:input :name "delete" :type "submit" :class "submit" :value "Delete"
		   :onclick "disableIrrelevantButtons(this);"))
     (:input :name "action" :type "hidden" :value "abc132")))))

