
(in-package :weblocks-test)

;;; testing gridedit-render-add-button
(deftest-html gridedit-render-add-button-1
    (with-request :get nil
      (weblocks::gridedit-render-add-button (make-instance 'gridedit)))
  (:form :class "add-entry"
	 :action ""
	 :method "get"
	 :onsubmit "initiateFormAction(\"abc123\", $(this), \"weblocks-session=1%3Atest\"); return false;"
	 (:fieldset
	  (:input :name "submit" :type "submit" :class "submit" :value "Add Null")
	  (:input :name "action" :type "hidden" :value "abc123"))))

;;; testing gridedit-render-add-form
(deftest-html gridedit-render-add-form-1
    (with-request :get nil
      (weblocks::gridedit-render-add-form (make-instance 'gridedit :data (list *joe*))))
  (:div :class "widget dataform" :id "widget-123"
	 (:div :class "widget-body"
	       #.(form-header-template
		  "abc123"
		  '((:li :class "name"
		     (:label (:span "Name:&nbsp;") (:input :type "text" :name "name" :value "Joe")))
		    (:li :class "manager"
		     (:label (:span "Manager:&nbsp;") (:input :type "text" :name "manager" :value "Jim"))))
		  :method "post"
		  :title-action "Adding:&nbsp;"))))

;;; testing gridedit-add-item
(deftest gridedit-add-item-1
    (let ((grid (make-instance 'gridedit :data (list *joe*))))
      (weblocks::gridedit-add-item grid *joe* nil)
      (length (datagrid-data grid)))
  2)

;;; testing render-widget-body for gridedit
(deftest-html render-widget-body-gridedit-1
    (with-request :get nil
      (let ((grid (make-instance 'gridedit :data (list *joe*))))
	;; render datagrid
	(render-widget-body grid :custom-slots '(test))
	;; click "Add Employee"
	(do-request `((,weblocks::*action-string* . "abc125")))
	(render-widget-body grid)
	;; click "submit"
	(do-request `(("submit" . "Submit")
		      (,weblocks::*action-string* . "abc128")))
	(render-widget-body grid)))
  (htm
   ;; datagrid
   #.(table-header-template
      '((:th :class "delete" "Delete")
	(:th :class "name sort-ascending" (:span #.(link-action-template "abc123" "Name")))
	(:th :class "manager" (:span #.(link-action-template "abc124" "Manager")))
	(:th :class "test" "Test"))
      '((:tr
	 (:td "Delete")
	 (:td :class "name" (:span :class "value" "Joe"))
	 (:td :class "manager" (:span :class "value" "Jim"))
	 (:td :class "test" (:span :class "value missing" "Not Specified"))))
      :summary "Ordered by name, ascending.")
   (:form :class "add-entry"
	 :action ""
	 :method "get"
	 :onsubmit "initiateFormAction(\"abc125\", $(this), \"weblocks-session=1%3Atest\"); return false;"
	 (:fieldset
	  (:input :name "submit" :type "submit" :class "submit" :value "Add Employee")
	  (:input :name "action" :type "hidden" :value "abc125")))
   ;; "Add Employee" clicked
   #.(table-header-template
      '((:th :class "delete" "Delete")
	(:th :class "name sort-ascending" (:span #.(link-action-template "abc126" "Name")))
	(:th :class "manager" (:span #.(link-action-template "abc127" "Manager"))))
      '((:tr
	 (:td "Delete")
	 (:td :class "name" (:span :class "value" "Joe"))
	 (:td :class "manager" (:span :class "value" "Jim"))))
      :summary "Ordered by name, ascending.")
   (:div :class "widget dataform" :id "widget-123"
	 (:div :class "widget-body"
	       #.(form-header-template
		  "abc128"
		  '((:li :class "name"
		     (:label (:span "Name:&nbsp;") (:input :type "text" :name "name" :value "Joe")))
		    (:li :class "manager"
		     (:label (:span "Manager:&nbsp;") (:input :type "text" :name "manager" :value "Jim"))))
		  :method "post"
		  :title-action "Adding:&nbsp;")))
   ;; "Submit" clicked
   #.(table-header-template
      '((:th :class "delete" "Delete")
	(:th :class "name sort-ascending" (:span #.(link-action-template "abc129" "Name")))
	(:th :class "manager" (:span #.(link-action-template "abc130" "Manager"))))
      '((:tr
	 (:td "Delete")
	 (:td :class "name" (:span :class "value" "Joe"))
	 (:td :class "manager" (:span :class "value" "Jim")))
	(:tr :class "altern"
	 (:td "Delete")
	 (:td :class "name" (:span :class "value" "Joe"))
	 (:td :class "manager" (:span :class "value" "Jim"))))
      :summary "Ordered by name, ascending.")
   (:form :class "add-entry"
	 :action ""
	 :method "get"
	 :onsubmit "initiateFormAction(\"abc131\", $(this), \"weblocks-session=1%3Atest\"); return false;"
	 (:fieldset
	  (:input :name "submit" :type "submit" :class "submit" :value "Add Employee")
	  (:input :name "action" :type "hidden" :value "abc131")))))

;;; test append-custom-slots
(deftest append-custom-slots-1
    (weblocks::append-custom-slots '(a b c) '(:a 1 :b 2 :custom-slots (d e f) :c 3))
  (d e f a b c))
