
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
    (with-request :get nil
      (let ((grid (make-instance 'gridedit :data (list *joe*)
					   :data-class 'employee)))
	(weblocks::gridedit-add-item grid *joe*)
	(length (datagrid-data grid))))
  2)

(deftest gridedit-add-item-2
    (with-request :get nil
      (let ((grid (make-instance 'gridedit :data (list *joe*)
					   :data-class 'employee
					   :on-add-item (lambda (grid item)
							  (push item (slot-value grid 'weblocks::data))
							  (push item (slot-value grid 'weblocks::data))))))
	(weblocks::gridedit-add-item grid *joe*)
	(length (datagrid-data grid))))
  3)

;;; testing gridedit-delete-items
(deftest gridedit-delete-items-1
    (with-request :get nil
      (let ((grid (make-instance 'gridedit
				 :data (list *joe* *bob*)
				 :data-class 'employee)))
	(weblocks::gridedit-delete-items grid (cons :none (list (object-id *joe*))))
	(mapcar #'first-name (datagrid-data grid))))
  ("Bob"))

(deftest gridedit-delete-items-2
    (with-request :get nil
      (let ((grid (make-instance 'gridedit
				 :data (list *joe* *bob*)
				 :data-class 'employee)))
	(weblocks::gridedit-delete-items grid (cons :none (list (object-id *joe*)
								(object-id *bob*))))
	(mapcar #'first-name (datagrid-data grid))))
  nil)

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
		      (,weblocks::*action-string* . "abc125")))
	(render-widget-body grid :form-id "I1" :input-id "I2" :search-id "I3")
	;; click "submit"
	(do-request `(("name" . "Jill")
		      ("submit" . "Submit")
		      (,weblocks::*action-string* . "abc133")))
	(render-widget-body grid :form-id "I1" :input-id "I2" :search-id "I3")))
  (htm
   ;; datagrid
   (:div :class "data-mining-bar"
	 (:p :class "datagrid-select-bar"
	     (:strong "Select: ")
	     (:a :href "?action=abc123" :onclick "initiateAction(\"abc123\", \"weblocks-session=1%3Atest\"); return false;" "All")
	     ", "
	     (:a :href "?action=abc124" :onclick "initiateAction(\"abc124\", \"weblocks-session=1%3Atest\"); return false;" "None")))
   (:div :class "widget flash" :id "widget-123" "")
   (:form
    :action ""
    :method "get"
    :onsubmit "initiateFormAction(\"abc125\", $(this), \"weblocks-session=1%3Atest\"); return false;"
    (:fieldset
     (:div :class "datagrid-body"
	   #.(table-header-template
	      '((:th :class "select" "")
		(:th :class "name sort-ascending" (:span #.(link-action-template "abc126" "Name")))
		(:th :class "manager" (:span #.(link-action-template "abc127" "Manager")))
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
     (:input :name "action" :type "hidden" :value "abc125")))
   ;; "Add Employee" clicked
   (:div :class "data-mining-bar"
	 (:p :class "datagrid-select-bar"
	     (:strong "Select: ")
	     (:a :href "?action=abc128" :onclick "initiateAction(\"abc128\", \"weblocks-session=1%3Atest\"); return false;" "All")
	     ", "
	     (:a :href "?action=abc129" :onclick "initiateAction(\"abc129\", \"weblocks-session=1%3Atest\"); return false;" "None")))
   (:div :class "widget flash" :id "widget-123" "")
   (:form
    :action ""
    :method "get"
    :onsubmit "initiateFormAction(\"abc130\", $(this), \"weblocks-session=1%3Atest\"); return false;"
    (:fieldset
     (:div :class "datagrid-body"
	   #.(table-header-template
	      '((:th :class "select" "")
		(:th :class "name sort-ascending" (:span #.(link-action-template "abc131" "Name")))
		(:th :class "manager" (:span #.(link-action-template "abc132" "Manager"))))
	      '((:tr
		 (:td :class "select"
		  (:input :type "checkbox" :name "item-1"))
		 (:td :class "name" (:span :class "value" "Joe"))
		 (:td :class "manager" (:span :class "value" "Jim"))))
	      :summary "Ordered by name, ascending."))
     (:input :name "action" :type "hidden" :value "abc130")))
   (:div :class "widget dataform" :id "widget-123"
	 #.(form-header-template
	    "abc133"
	    '((:li :class "name"
	       (:label (:span "Name:&nbsp;") (:input :type "text" :name "name")))
	      (:li :class "manager"
	       (:label (:span "Manager:&nbsp;") (:input :type "text" :name "manager" :value "Jim"))))
	    :method "post"
	    :title-action "Adding:&nbsp;"))
   ;; "Submit" clicked
   (:div :class "data-mining-bar"
	 (:p :class "datagrid-select-bar"
	     (:strong "Select: ")
	     (:a :href "?action=abc134" :onclick "initiateAction(\"abc134\", \"weblocks-session=1%3Atest\"); return false;" "All")
	     ", "
	     (:a :href "?action=abc135" :onclick "initiateAction(\"abc135\", \"weblocks-session=1%3Atest\"); return false;" "None")))
   (:div :class "widget flash" :id "widget-123"
	 (:div :class "renderer"
	 (:div :class "extra-top-1" "&nbsp;")
	 (:div :class "extra-top-2" "&nbsp;")
	 (:div :class "extra-top-3" "&nbsp;")
	 (:ul :class "messages"
	      (:li
	       (:div :class "widget string"
		     (:p "Item added."))))
	 (:div :class "extra-bottom-1" "&nbsp;")
	 (:div :class "extra-bottom-2" "&nbsp;")
	 (:div :class "extra-bottom-3" "&nbsp;")))
   (:form
    :action ""
    :method "get"
    :onsubmit "initiateFormAction(\"abc136\", $(this), \"weblocks-session=1%3Atest\"); return false;"
    (:fieldset
     (:div :class "datagrid-body"
	   #.(table-header-template
	      '((:th :class "select" "")
		(:th :class "name sort-ascending" (:span #.(link-action-template "abc137" "Name")))
		(:th :class "manager" (:span #.(link-action-template "abc138" "Manager"))))
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
     (:input :name "action" :type "hidden" :value "abc136")))))

(deftest-html render-widget-body-gridedit-2
    (with-request :get nil
      (let ((grid (make-instance 'gridedit :data (list *joe*)
					   :data-class 'employee
					   :forbid-sorting-on '(test)
					   :allow-searching-p nil)))
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
	     (:a :href "?action=abc123" :onclick "initiateAction(\"abc123\", \"weblocks-session=1%3Atest\"); return false;" "All")
	     ", "
	     (:a :href "?action=abc124" :onclick "initiateAction(\"abc124\", \"weblocks-session=1%3Atest\"); return false;" "None")))
   (:div :class "widget flash" :id "widget-123" "")
   (:form
    :action ""
    :method "get"
    :onsubmit "initiateFormAction(\"abc125\", $(this), \"weblocks-session=1%3Atest\"); return false;"
    (:fieldset
     (:div :class "datagrid-body"
	   #.(table-header-template
	      '((:th :class "select" "")
		(:th :class "name sort-ascending" (:span #.(link-action-template "abc126" "Name")))
		(:th :class "manager" (:span #.(link-action-template "abc127" "Manager"))))
	      '((:tr
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
     (:input :name "action" :type "hidden" :value "abc125")))
   ;; "Delete" clicked
   (:div :class "widget flash" :id "widget-123"
	 (:div :class "renderer"
	 (:div :class "extra-top-1" "&nbsp;")
	 (:div :class "extra-top-2" "&nbsp;")
	 (:div :class "extra-top-3" "&nbsp;")
	 (:ul :class "messages"
	      (:li
	       (:div :class "widget string"
		     (:p "1 item deleted."))))
	 (:div :class "extra-bottom-1" "&nbsp;")
	 (:div :class "extra-bottom-2" "&nbsp;")
	 (:div :class "extra-bottom-3" "&nbsp;")))
   (:form
    :action ""
    :method "get"
    :onsubmit "initiateFormAction(\"abc128\", $(this), \"weblocks-session=1%3Atest\"); return false;"
    (:fieldset
     (:div :class "datagrid-body"
	   (:div :class "renderer table empty-table"
	       (:div :class "extra-top-1" "&nbsp;")
	       (:div :class "extra-top-2" "&nbsp;")
	       (:div :class "extra-top-3" "&nbsp;")
	       (:p (:span :class "message" "No information available."))
	       (:div :class "extra-bottom-1" "&nbsp;")
	       (:div :class "extra-bottom-2" "&nbsp;")
	       (:div :class "extra-bottom-3" "&nbsp;")))
     (:div :class "item-operations"
	   (:input :name "add" :type "submit" :class "submit" :value "Add"
		   :onclick "disableIrrelevantButtons(this);"))
     (:input :name "action" :type "hidden" :value "abc128")))))
