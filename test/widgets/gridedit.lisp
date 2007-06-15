
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
	(render-widget-body grid :custom-slots '(test) :form-id "I1" :input-id "I2" :search-id "I3")
	;; click "Add Employee"
	(do-request `((,weblocks::*action-string* . "abc126")))
	(render-widget-body grid :form-id "I1" :input-id "I2" :search-id "I3")
	;; click "submit"
	(do-request `(("submit" . "Submit")
		      (,weblocks::*action-string* . "abc130")))
	(render-widget-body grid :form-id "I1" :input-id "I2" :search-id "I3")))
  (htm
   ;; datagrid
   #.(searchbar-template "I1" "I2" "I3" "abc123")
   (:div :class "datagrid-body"
	 #.(table-header-template
	    '((:th :class "delete" "Delete")
	      (:th :class "name sort-ascending" (:span #.(link-action-template "abc124" "Name")))
	      (:th :class "manager" (:span #.(link-action-template "abc125" "Manager")))
	      (:th :class "test" "Test"))
	    '((:tr
	       (:td "Delete")
	       (:td :class "name" (:span :class "value" "Joe"))
	       (:td :class "manager" (:span :class "value" "Jim"))
	       (:td :class "test" (:span :class "value missing" "Not Specified"))))
	    :summary "Ordered by name, ascending."))
   (:form :class "add-entry"
	 :action ""
	 :method "get"
	 :onsubmit "initiateFormAction(\"abc126\", $(this), \"weblocks-session=1%3Atest\"); return false;"
	 (:fieldset
	  (:input :name "submit" :type "submit" :class "submit" :value "Add Employee")
	  (:input :name "action" :type "hidden" :value "abc126")))
   ;; "Add Employee" clicked
   #.(searchbar-template "I1" "I2" "I3" "abc127")
   (:div :class "datagrid-body"
	 #.(table-header-template
	    '((:th :class "delete" "Delete")
	      (:th :class "name sort-ascending" (:span #.(link-action-template "abc128" "Name")))
	      (:th :class "manager" (:span #.(link-action-template "abc129" "Manager"))))
	    '((:tr
	       (:td "Delete")
	       (:td :class "name" (:span :class "value" "Joe"))
	       (:td :class "manager" (:span :class "value" "Jim"))))
	    :summary "Ordered by name, ascending."))
   (:div :class "widget dataform" :id "widget-123"
	 #.(form-header-template
	    "abc130"
	    '((:li :class "name"
	       (:label (:span "Name:&nbsp;") (:input :type "text" :name "name" :value "Joe")))
	      (:li :class "manager"
	       (:label (:span "Manager:&nbsp;") (:input :type "text" :name "manager" :value "Jim"))))
	    :method "post"
	    :title-action "Adding:&nbsp;"))
   ;; "Submit" clicked
   #.(searchbar-template "I1" "I2" "I3" "abc131")
   (:div :class "datagrid-body"
	 #.(table-header-template
	    '((:th :class "delete" "Delete")
	      (:th :class "name sort-ascending" (:span #.(link-action-template "abc132" "Name")))
	      (:th :class "manager" (:span #.(link-action-template "abc133" "Manager"))))
	    '((:tr
	       (:td "Delete")
	       (:td :class "name" (:span :class "value" "Joe"))
	       (:td :class "manager" (:span :class "value" "Jim")))
	      (:tr :class "altern"
	       (:td "Delete")
	       (:td :class "name" (:span :class "value" "Joe"))
	       (:td :class "manager" (:span :class "value" "Jim"))))
	    :summary "Ordered by name, ascending."))
   (:form :class "add-entry"
	 :action ""
	 :method "get"
	 :onsubmit "initiateFormAction(\"abc134\", $(this), \"weblocks-session=1%3Atest\"); return false;"
	 (:fieldset
	  (:input :name "submit" :type "submit" :class "submit" :value "Add Employee")
	  (:input :name "action" :type "hidden" :value "abc134")))))

(deftest-html render-widget-body-gridedit-2
    (with-request :get nil
      (let ((grid (make-instance 'gridedit :data (list *joe*)
				 :data-class 'employee
				 :search "doesn't exist")))
	;; render datagrid
	(render-widget-body grid :form-id "I1" :input-id "I2" :search-id "I3")))
  (htm
   ;; datagrid
   #.(searchbar-template "I1" "I2" "I3" "abc123" :value "doesn't exist"
			 :hidden-items-text "(1 item is hidden by the search)")
   (:div :class "datagrid-body"
	 (:div :class "renderer table empty-table"
	       (:div :class "extra-top-1" "&nbsp;")
	       (:div :class "extra-top-2" "&nbsp;")
	       (:div :class "extra-top-3" "&nbsp;")
	       (:p (:span :class "message" "No information available."))
	       (:div :class "extra-bottom-1" "&nbsp;")
	       (:div :class "extra-bottom-2" "&nbsp;")
	       (:div :class "extra-bottom-3" "&nbsp;")))
   (:form :class "add-entry"
	 :action ""
	 :method "get"
	 :onsubmit "initiateFormAction(\"abc124\", $(this), \"weblocks-session=1%3Atest\"); return false;"
	 (:fieldset
	  (:input :name "submit" :type "submit" :class "submit" :value "Add Employee")
	  (:input :name "action" :type "hidden" :value "abc124")))))

(deftest-html render-widget-body-gridedit-3
    (with-request :get nil
      (let ((grid (make-instance 'gridedit :data (lambda (&rest args &key countp &allow-other-keys)
						   (if countp
						       1
						       (list *joe*)))
				 :data-class 'employee
				 :allow-add-p t)))
	;; render datagrid
	(render-widget-body grid :form-id "I1" :input-id "I2" :search-id "I3")))
  (htm
   #.(searchbar-template "I1" "I2" "I3" "abc123")
   (:div :class "datagrid-body"
	 #.(table-header-template
	    '((:th :class "delete" "Delete")
	      (:th :class "name sort-ascending" (:span #.(link-action-template "abc124" "Name")))
	      (:th :class "manager" (:span #.(link-action-template "abc125" "Manager"))))
	    '((:tr
	       (:td "Delete")
	       (:td :class "name" (:span :class "value" "Joe"))
	       (:td :class "manager" (:span :class "value" "Jim"))))
	    :summary "Ordered by name, ascending."))))

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
