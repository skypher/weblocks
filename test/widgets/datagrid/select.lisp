
(in-package :weblocks-test)

;;; test render-select-bar
(deftest-html render-select-bar-1
    (with-request :get nil
      (let ((grid (make-instance 'datagrid
				 :data-class 'employee)))
	(weblocks::render-select-bar grid)))
  (:p :class "datagrid-select-bar"
      (:strong "Select: ")
      (:a :href "/foo/bar?action=abc123"
	  :onclick "initiateAction(\"abc123\", \"weblocks-session=1%3ATEST\"); return false;"
	  "All")
      ", "
      (:a :href "/foo/bar?action=abc124"
	  :onclick "initiateAction(\"abc124\", \"weblocks-session=1%3ATEST\"); return false;" "None")))

;;; test datagrid-render-view-field-select
(deftest-html datagrid-render-view-field-select-1
    (with-request :get nil
      (let* ((view (defview () (:type table :inherit-from '(:scaffold employee))))
	     (grid (make-instance 'datagrid :data-class 'employee
				  :view view
				  :sort '(name . :asc)))
	     (field (weblocks::make-select-field grid))
	     (presentation (view-field-presentation field))
	     (value (first-name *joe*)))
	(render-view-field field view grid presentation value *joe*)))
  (:td :class "select"
       (:div (:input :name "item-1" :type "checkbox" :value "f"))))

(deftest-html datagrid-render-view-field-select-2
    (with-request :get nil
      (let* ((view (defview () (:type table :inherit-from '(:scaffold employee))))
	     (grid (make-instance 'datagrid :data-class 'employee
				  :selection '(:none . (1))
				  :view view
				  :sort '(name . :asc)))
	     (field (weblocks::make-select-field grid))
	     (presentation (view-field-presentation field))
	     (value (first-name *joe*)))
	(render-view-field field view grid presentation value *joe*)))
  (:td :class "select"
       (:div (:input :name "item-1" :type "checkbox" :value "t" :checked "checked"))))

;;; test datagrid-render-view-field-header-select-1
(deftest-html datagrid-render-view-field-header-select-1
    (with-request :get nil
      (let* ((view (defview () (:type table :inherit-from '(:scaffold employee))))
	     (grid (make-instance 'datagrid :data-class 'employee
				  :selection '(:none . (1))
				  :view view
				  :sort '(name . :asc)))
	     (field (weblocks::make-select-field grid))
	     (presentation (view-field-presentation field))
	     (value (first-name *joe*)))
	(render-view-field-header field view grid presentation value *joe*)))
  (:th :class "select" ""))

;;; test selection process
(deftest-html render-widget-body-datagrid-select-1
    (with-request :get nil
      (persist-objects *default-store* (list *joe* *bob*))
      (let ((grid (make-instance 'datagrid
				 :data-class 'employee
				 :allow-select-p t
				 :show-total-items-count-p nil
				 :allow-pagination-p nil))
	    (*on-ajax-complete-scripts* nil))
	(declare (special *on-ajax-complete-scripts*))
	;; render datagrid
	(render-widget-body grid :form-id "I1" :input-id "I2" :search-id "I3")
	;; select all
	(do-request `((,weblocks::*action-string* . "abc123")))
	(render-widget-body grid :form-id "I1" :input-id "I2" :search-id "I3")
	;; select none
	(do-request `((,weblocks::*action-string* . "abc129")))
	(render-widget-body grid :form-id "I1" :input-id "I2" :search-id "I3")))
  (htm
   ;; initial
   (:div :class "data-mining-bar"
	 (:p :class "datagrid-select-bar"
	     (:strong "Select: ")
	     (:a :href "/foo/bar?action=abc123"
		 :onclick "initiateAction(\"abc123\", \"weblocks-session=1%3ATEST\"); return false;"
		 "All")
	     ", "
	     (:a :href "/foo/bar?action=abc124"
		 :onclick "initiateAction(\"abc124\", \"weblocks-session=1%3ATEST\"); return false;" "None")))
   (:div :class "widget flash" :id "id-123" "<!-- empty flash -->")
   (:form :class "dataseq-form"
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
			(:div (:input :name "item-2" :type "checkbox" :value "f")))
		       (:td :class "name" (:span :class "value" "Bob"))
		       (:td :class "manager" (:span :class "value" "Jim")))
		      (:tr :class "altern"
		       (:td :class "select"
			(:div (:input :name "item-1" :type "checkbox" :value "f")))
		       (:td :class "name" (:span :class "value" "Joe"))
		       (:td :class "manager" (:span :class "value" "Jim"))))
		    :summary "Ordered by name, ascending."))
	   (:input :name "action" :type "hidden" :value "abc125"))
	  (:div :class "extra-bottom-1" "<!-- empty -->")
	  (:div :class "extra-bottom-2" "<!-- empty -->")
	  (:div :class "extra-bottom-3" "<!-- empty -->"))
   ;; select all clicked
   (:div :class "data-mining-bar"
	 (:p :class "datagrid-select-bar"
	     (:strong "Select: ")
	     (:a :href "/foo/bar?action=abc128"
		 :onclick "initiateAction(\"abc128\", \"weblocks-session=1%3ATEST\"); return false;"
		 "All")
	     ", "
	     (:a :href "/foo/bar?action=abc129"
		 :onclick "initiateAction(\"abc129\", \"weblocks-session=1%3ATEST\"); return false;" "None")))
   (:div :class "widget flash" :id "id-123" "<!-- empty flash -->")
   (:form :class "dataseq-form"
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
			(:div (:input :name "item-2" :type "checkbox" :value "t" :checked "checked")))
		       (:td :class "name" (:span :class "value" "Bob"))
		       (:td :class "manager" (:span :class "value" "Jim")))
		      (:tr :class "altern"
		       (:td :class "select"
			(:div (:input :name "item-1" :type "checkbox" :value "t" :checked "checked")))
		       (:td :class "name" (:span :class "value" "Joe"))
		       (:td :class "manager" (:span :class "value" "Jim"))))
		    :summary "Ordered by name, ascending."))
	   (:input :name "action" :type "hidden" :value "abc130"))
	  (:div :class "extra-bottom-1" "<!-- empty -->")
	  (:div :class "extra-bottom-2" "<!-- empty -->")
	  (:div :class "extra-bottom-3" "<!-- empty -->"))
   ;; select none clicked
   (:div :class "data-mining-bar"
	 (:p :class "datagrid-select-bar"
	     (:strong "Select: ")
	     (:a :href "/foo/bar?action=abc133"
		 :onclick "initiateAction(\"abc133\", \"weblocks-session=1%3ATEST\"); return false;"
		 "All")
	     ", "
	     (:a :href "/foo/bar?action=abc134"
		 :onclick "initiateAction(\"abc134\", \"weblocks-session=1%3ATEST\"); return false;" "None")))
   (:div :class "widget flash" :id "id-123" "<!-- empty flash -->")
   (:form :class "dataseq-form"
	  :action "/foo/bar"
	  :method "get"
	  :onsubmit "initiateFormAction(\"abc135\", $(this), \"weblocks-session=1%3ATEST\"); return false;"
	  (:div :class "extra-top-1" "<!-- empty -->")
	  (:div :class "extra-top-2" "<!-- empty -->")
	  (:div :class "extra-top-3" "<!-- empty -->")
	  (:fieldset
	   (:div :class "datagrid-body"
		 #.(table-header-template
		    '((:th :class "select" "")
		      (:th :class "name sort-asc" (:span #.(link-action-template "abc136" "Name")))
		      (:th :class "manager" (:span #.(link-action-template "abc137" "Manager"))))
		    '((:tr
		       (:td :class "select"
			(:div (:input :name "item-2" :type "checkbox" :value "f")))
		       (:td :class "name" (:span :class "value" "Bob"))
		       (:td :class "manager" (:span :class "value" "Jim")))
		      (:tr :class "altern"
		       (:td :class "select"
			(:div (:input :name "item-1" :type "checkbox" :value "f")))
		       (:td :class "name" (:span :class "value" "Joe"))
		       (:td :class "manager" (:span :class "value" "Jim"))))
		    :summary "Ordered by name, ascending."))
	   (:input :name "action" :type "hidden" :value "abc135"))
	  (:div :class "extra-bottom-1" "<!-- empty -->")
	  (:div :class "extra-bottom-2" "<!-- empty -->")
	  (:div :class "extra-bottom-3" "<!-- empty -->"))))

