
(in-package :weblocks-test)

;;; testing gridedit-render-add-button
(deftest-html gridedit-render-add-button-1
    (with-request :get nil
      (weblocks::gridedit-render-add-button (make-instance 'gridedit
							   :data-class 'employee)))
  (:form :class "add-entry"
	 :action ""
	 :method "get"
	 :onsubmit "initiateFormAction(\"abc123\", $(this), \"weblocks-session=1%3Atest\"); return false;"
	 (:fieldset
	  (:input :name "submit" :type "submit" :class "submit" :value "Add Employee")
	  (:input :name "action" :type "hidden" :value "abc123"))))

;;; testing gridedit-render-add-form
(deftest-html gridedit-render-add-form-1
    (with-request :get nil
      (weblocks::gridedit-render-add-form (make-instance 'gridedit :data (list *joe*)
							 :data-class 'employee)))
  (:div :class "widget dataform" :id "widget-123"
	#.(form-header-template
	   "abc123"
	   '((:li :class "name"
	      (:label (:span "Name:&nbsp;") (:input :type "text" :name "name" :value "Joe")))
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
				 :forbid-sorting-on '(test))))
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

(deftest-html render-widget-body-gridedit-2
    (with-request :get nil
      (let ((grid (make-instance 'gridedit :data (list *joe*)
				 :data-class 'employee
				 :search "doesn't exist")))
	;; render datagrid
	(render-widget-body grid)))
  (htm
   ;; datagrid
   (:div :class "renderer table empty-table"
	(:div :class "extra-top-1" "&nbsp;")
	(:div :class "extra-top-2" "&nbsp;")
	(:div :class "extra-top-3" "&nbsp;")
	(:p (:span :class "message" "No information available."))
	(:div :class "extra-bottom-1" "&nbsp;")
	(:div :class "extra-bottom-2" "&nbsp;")
	(:div :class "extra-bottom-3" "&nbsp;"))
   (:form :class "add-entry"
	 :action ""
	 :method "get"
	 :onsubmit "initiateFormAction(\"abc123\", $(this), \"weblocks-session=1%3Atest\"); return false;"
	 (:fieldset
	  (:input :name "submit" :type "submit" :class "submit" :value "Add Employee")
	  (:input :name "action" :type "hidden" :value "abc123")))))

(deftest-html render-widget-body-gridedit-3
    (with-request :get nil
      (let ((grid (make-instance 'gridedit :data (lambda (&rest args)
						   (list *joe*))
				 :data-class 'employee
				 :allow-add-p t)))
	;; render datagrid
	(render-widget-body grid)))
  (htm
   ;; datagrid
   #.(table-header-template
      '((:th :class "delete" "Delete")
	(:th :class "name sort-ascending" (:span #.(link-action-template "abc123" "Name")))
	(:th :class "manager" (:span #.(link-action-template "abc124" "Manager"))))
      '((:tr
	 (:td "Delete")
	 (:td :class "name" (:span :class "value" "Joe"))
	 (:td :class "manager" (:span :class "value" "Jim"))))
      :summary "Ordered by name, ascending.")))

(deftest render-widget-body-gridedit-4
    (with-request :get nil
      (let ((grid (make-instance 'gridedit :data (list *joe*)
				 :data-class 'employee))
	    (*weblocks-output-stream* (make-string-output-stream)))
	(declare (special *weblocks-output-stream*))
	;; render datagrid
	(render-widget-body grid)
	(datagrid-forbid-sorting-on grid)))
  (delete))

;;; test append-custom-slots
(deftest append-custom-slots-1
    (weblocks::append-custom-slots '(a b c) '(:a 1 :b 2 :custom-slots (d e f) :c 3))
  (d e f a b c))
