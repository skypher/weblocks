
(in-package :weblocks-test)

;;; test datagrid initialize-instance
(deftest datagrid-initialize-instance-1
    (with-request :get nil
      (let ((dg (make-instance 'datagrid
			       :data-class 'employee)))
	(not (null (datagrid-pagination-widget dg)))))
  t)

(deftest datagrid-initialize-instance-2
    (with-request :get nil
      (let* ((pw (make-instance 'pagination))
	     (dg (make-instance 'datagrid
			       :data-class 'employee
			       :pagination-widget pw)))
	(eq (datagrid-pagination-widget dg) pw)))
  t)

;;; test datagrid-data
(deftest datagrid-data-1
    (with-request :get nil
      (datagrid-data (make-instance 'datagrid
				    :data (list 1 2)
				    :data-class 'employee)))
  (1 2))

(deftest datagrid-data-2
    (with-request :get nil
      (datagrid-data (make-instance 'datagrid
				    :data (lambda (search sort pagination &key countp)
					    (if countp
						2 (list 1 2)))
				    :data-class 'employee)))
  (1 2))

(deftest datagrid-data-3
    (with-request :get nil
      (datagrid-data (make-instance 'datagrid
				    :data (list 1 2 3 4 5 6)
				    :data-class 'integer
				    :pagination-widget
				    (make-instance 'pagination
						   :total-items 6
						   :items-per-page 4
						   :current-page 1))))
  (1 2 3 4))

(deftest datagrid-data-4
    (with-request :get nil
      (datagrid-data (make-instance 'datagrid
				    :data (lambda (search sort pagination &key countp)
					    pagination)
				    :data-class 'integer
				    :pagination-widget
				    (make-instance 'pagination
						   :total-items 6
						   :items-per-page 4
						   :current-page 1))))
  (0 . 4))

(defun datagrid-data-foo (search sort pagination &key countp)
  (if countp
      2 (list 1 2)))

(deftest datagrid-data-5
    (with-request :get nil
      (datagrid-data (make-instance 'datagrid
				    :data 'datagrid-data-foo
				    :data-class 'employee)))
  (1 2))

(deftest datagrid-data-6
    (with-request :get nil
      (datagrid-data (make-instance 'datagrid
				    :data nil
				    :data-class 'employee)))
  nil)

;;; test datagrid-data-count
(deftest datagrid-data-count-1
    (with-request :get nil
      (datagrid-data-count (make-instance 'datagrid
					  :data (list 1 2)
					  :data-class 'employee)))
  2)

(deftest datagrid-data-count-2
    (with-request :get nil
      (datagrid-data-count (make-instance 'datagrid
					  :data (lambda (search sort pagination &key countp)
						  (list search sort pagination countp))
					  :search "foo"
					  :data-class 'employee)))
  ("foo" nil nil t))

(deftest datagrid-data-count-3
    (with-request :get nil
      (datagrid-data-count (make-instance 'datagrid
					  :data (lambda (search sort pagination &key countp)
						  (list search sort countp))
					  :search "foo"
					  :data-class 'employee)
			   :totalp t))
  (nil nil t))

(defun datagrid-data-count-foo (search sort pagination &key countp)
  (list search sort pagination countp))

(deftest datagrid-data-count-4
    (with-request :get nil
      (datagrid-data-count (make-instance 'datagrid
					  :data 'datagrid-data-count-foo
					  :search "foo"
					  :data-class 'employee)))
  ("foo" nil nil t))

(deftest datagrid-data-count-5
    (with-request :get nil
      (datagrid-data-count (make-instance 'datagrid
					  :data 'datagrid-data-count-foo
					  :search "foo"
					  :data-class 'employee)
			   :totalp t))
  (nil nil nil t))

(deftest datagrid-data-count-6
    (with-request :get nil
      (datagrid-data-count (make-instance 'datagrid
					  :data nil
					  :data-class 'employee)))
  0)

;;; test append-custom-slots
(deftest append-custom-slots-1
    (weblocks::append-custom-slots '(a b c) '(:a 1 :b 2 :custom-slots (d e f) :c 3))
  (d e f a b c))

;;; test datagrid-render-item-ops-bar
(deftest-html datagrid-render-item-ops-bar-1
    (with-request :get nil
      (let ((grid (make-instance 'datagrid
				 :data-class 'employee)))
	(datagrid-render-item-ops-bar grid)))
  "")

(deftest-html datagrid-render-item-ops-bar-2
    (with-request :get nil
      (let ((grid (make-instance 'datagrid
				 :data-class 'employee
				 :item-ops (list (cons 'hello #'identity)
						 (cons 'world #'identity)))))
	(datagrid-render-item-ops-bar grid)))
  (:div :class "item-operations"
	(:input :name "hello" :type "submit" :class "submit" :value "Hello"
		:onclick "disableIrrelevantButtons(this);")
	(:input :name "world" :type "submit" :class "submit" :value "World"
		:onclick "disableIrrelevantButtons(this);")))

;;; test datagrid-render-pagination-widget
(deftest-html datagrid-render-pagination-widget-1
    (with-request :get nil
      (datagrid-render-pagination-widget (make-instance 'datagrid :data-class 'employee)))
  (:div :class "widget pagination"
	:id "widget-123"))

(deftest-html datagrid-render-pagination-widget-2
    (with-request :get nil
      (datagrid-render-pagination-widget (make-instance 'datagrid :data-class 'employee
							:allow-pagination-p nil)))
  "")

;;; test total-items-message
(deftest total-items-message-1
    (with-request :get nil
      (weblocks::total-items-message (make-instance 'datagrid :data (list *joe* *bob*)
						    :data-class 'employee)))
  "(Total of 2 Items)")

(deftest total-items-message-2
    (with-request :get nil
      (weblocks::total-items-message (make-instance 'datagrid :data (list *joe* *bob*)
						    :search "Test"
						    :data-class 'employee)))
  "(Found 0 of 2 Items)")

;;; test render-total-items-message
(deftest-html render-total-items-message-1
    (with-request :get nil
      (weblocks::render-total-items-message (make-instance 'datagrid :data (list *joe* *bob*)
							   :data-class 'employee)))
  (:span :class "total-items" "(Total of 2 Items)"))

;;; test datagrid-render-mining-bar
(deftest-html datagrid-render-mining-bar-1
    (with-request :get nil
      (datagrid-render-mining-bar
       (make-instance 'datagrid
		      :data (list *joe* *bob*)
		      :data-class 'employee
		      :show-total-items-count-p nil
		      :allow-searching-p nil
		      :allow-select-p nil)))
  nil)

(deftest-html datagrid-render-mining-bar-2
    (with-request :get nil
      (datagrid-render-mining-bar
       (make-instance 'datagrid
		      :data (list *joe* *bob*)
		      :data-class 'employee
		      :show-total-items-count-p t
		      :allow-searching-p nil
		      :allow-select-p nil)))
  (:div :class "data-mining-bar"
	(:span :class "total-items" "(Total of 2 Items)")))

;;; test render-widget-body for datagrid
(deftest-html render-widget-body-datagrid-1
    (with-request :get nil
      (let ((grid (make-instance 'datagrid
				 :data (list *joe* *bob*)
				 :data-class 'employee
				 :show-total-items-count-p nil
				 :allow-pagination-p nil))
	    (*on-ajax-complete-scripts* nil))
	(declare (special *on-ajax-complete-scripts*))
	;; render datagrid
	(render-widget-body grid :form-id "I1" :input-id "I2" :search-id "I3")))
  (htm
   (:div :class "data-mining-bar"
	 #.(searchbar-template "I1" "I2" "I3" "abc123"))
   (:div :class "widget flash" :id "widget-123" "<!-- empty flash -->")
   (:form :class "datagrid-form"
	  :action "/foo/bar"
	  :method "get"
	  :onsubmit "initiateFormAction(\"abc124\", $(this), \"weblocks-session=1%3ATEST\"); return false;"
	  (:div :class "extra-top-1" "<!-- empty -->")
	  (:div :class "extra-top-2" "<!-- empty -->")
	  (:div :class "extra-top-3" "<!-- empty -->")
	  (:fieldset
	   (:div :class "datagrid-body"
		 #.(table-header-template
		    '((:th :class "name sort-ascending" (:span #.(link-action-template "abc125" "Name")))
		      (:th :class "manager" (:span #.(link-action-template "abc126" "Manager"))))
		    '((:tr
		       (:td :class "name" (:span :class "value" "Bob"))
		       (:td :class "manager" (:span :class "value" "Jim")))
		      (:tr :class "altern"
		       (:td :class "name" (:span :class "value" "Joe"))
		       (:td :class "manager" (:span :class "value" "Jim"))))
		    :summary "Ordered by name, ascending."))
	   (:input :name "action" :type "hidden" :value "abc124"))
	  (:div :class "extra-bottom-1" "<!-- empty -->")
	  (:div :class "extra-bottom-2" "<!-- empty -->")
	  (:div :class "extra-bottom-3" "<!-- empty -->"))))

(deftest-html render-widget-body-datagrid-2
    (with-request :get nil
      (let ((grid (make-instance 'datagrid
				 :data (list *joe*)
				 :data-class 'employee
				 :allow-select-p nil
				 :allow-searching-p nil
				 :allow-pagination-p nil
				 :show-total-items-count-p nil))
	    (*on-ajax-complete-scripts* nil))
	(declare (special *on-ajax-complete-scripts*))
	;; render datagrid
	(render-widget-body grid)))
  (htm
   (:div :class "widget flash" :id "widget-123" "<!-- empty flash -->")
   (:form :class "datagrid-form"
	  :action "/foo/bar"
	  :method "get"
	  :onsubmit "initiateFormAction(\"abc123\", $(this), \"weblocks-session=1%3ATEST\"); return false;"
	  (:div :class "extra-top-1" "<!-- empty -->")
	  (:div :class "extra-top-2" "<!-- empty -->")
	  (:div :class "extra-top-3" "<!-- empty -->")
	  (:fieldset
	   (:div :class "datagrid-body"
		 #.(table-header-template
		    '((:th :class "name sort-ascending" (:span #.(link-action-template "abc124" "Name")))
		      (:th :class "manager" (:span #.(link-action-template "abc125" "Manager"))))
		    '((:tr
		       (:td :class "name" (:span :class "value" "Joe"))
		       (:td :class "manager" (:span :class "value" "Jim"))))
		    :summary "Ordered by name, ascending."))
	   (:input :name "action" :type "hidden" :value "abc123"))
	  (:div :class "extra-bottom-1" "<!-- empty -->")
	  (:div :class "extra-bottom-2" "<!-- empty -->")
	  (:div :class "extra-bottom-3" "<!-- empty -->"))))

(deftest-html render-widget-body-datagrid-3
    (with-request :get nil
      (let ((grid (make-instance 'datagrid :data (list *joe* *bob*)
					   :data-class 'employee
					   :search "doesn't exist"
					   :allow-pagination-p nil)))
	;; render datagrid
	(render-widget-body grid :form-id "I1" :input-id "I2" :search-id "I3")))
  (htm
   ;; datagrid
   (:div :class "data-mining-bar"
	 #.(searchbar-template "I1" "I2" "I3" "abc123" :value "doesn't exist"
						       :total-items-text
						       "(Found 0 of 2 Items)"))
   (:div :class "widget flash" :id "widget-123" "<!-- empty flash -->")
   (:form :class "datagrid-form"
	  :action "/foo/bar"
	  :method "get"
	  :onsubmit "initiateFormAction(\"abc124\", $(this), \"weblocks-session=1%3ATEST\"); return false;"
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
	   (:input :name "action" :type "hidden" :value "abc124"))
	  (:div :class "extra-bottom-1" "<!-- empty -->")
	  (:div :class "extra-bottom-2" "<!-- empty -->")
	  (:div :class "extra-bottom-3" "<!-- empty -->"))))

(deftest render-widget-body-datagrid-4
    (with-request :get nil
      (let ((grid (make-instance 'datagrid :data (list *joe*)
					   :data-class 'employee))
	    (*weblocks-output-stream* (make-string-output-stream)))
	(declare (special *weblocks-output-stream*))
	;; render datagrid
	(render-widget-body grid)
	(datagrid-forbid-sorting-on grid)))
  (weblocks::select))

(deftest-html render-widget-body-datagrid-5
    (with-request :get nil
      (let ((grid (make-instance 'datagrid
				 :data (list *joe* *bob*)
				 :data-class 'employee
				 :show-total-items-count-p nil))
	    (*on-ajax-complete-scripts* nil))
	(declare (special *on-ajax-complete-scripts*))
	;; set items per page...
	(setf (pagination-items-per-page (datagrid-pagination-widget grid)) 1)
	;; render datagrid
	(render-widget-body grid :form-id "I1" :input-id "I2" :search-id "I3")
	;; add another item
	(push-end *joe* (slot-value grid 'weblocks::data))
	;; go to next page
	(do-request `((,weblocks::*action-string* . "abc127")))
	(render-widget-body grid :form-id "I1" :input-id "I2" :search-id "I3")))
  (htm
   ; Page 1
   (:div :class "data-mining-bar"
	 #.(searchbar-template "I1" "I2" "I3" "abc123"))
   (:div :class "widget flash" :id "widget-123" "<!-- empty flash -->")
   (:form :class "datagrid-form"
	  :action "/foo/bar"
	  :method "get"
	  :onsubmit "initiateFormAction(\"abc124\", $(this), \"weblocks-session=1%3ATEST\"); return false;"
	  (:div :class "extra-top-1" "<!-- empty -->")
	  (:div :class "extra-top-2" "<!-- empty -->")
	  (:div :class "extra-top-3" "<!-- empty -->")
	  (:fieldset
	   (:div :class "datagrid-body"
		 #.(table-header-template
		    '((:th :class "name sort-ascending" (:span #.(link-action-template "abc125" "Name")))
		      (:th :class "manager" (:span #.(link-action-template "abc126" "Manager"))))
		    '((:tr
		       (:td :class "name" (:span :class "value" "Bob"))
		       (:td :class "manager" (:span :class "value" "Jim"))))
		    :summary "Ordered by name, ascending."))
	   (:input :name "action" :type "hidden" :value "abc124"))
	  (:div :class "extra-bottom-1" "<!-- empty -->")
	  (:div :class "extra-bottom-2" "<!-- empty -->")
	  (:div :class "extra-bottom-3" "<!-- empty -->"))
   (:div :class "widget pagination" :id "widget-123"
	 #.(pagination-page-info-template 1 2) "&nbsp;"
	 #.(link-action-template "abc127" "Next >" :class "next-page")
	 #.(pagination-goto-form-template "abc128" :page-one-p t))
   ; Page 2
   (:div :class "data-mining-bar"
	 #.(searchbar-template "I1" "I2" "I3" "abc129"))
   (:div :class "widget flash" :id "widget-123" "<!-- empty flash -->")
   (:form :class "datagrid-form"
	  :action "/foo/bar"
	  :method "get"
	  :onsubmit "initiateFormAction(\"abc130\", $(this), \"weblocks-session=1%3ATEST\"); return false;"
	  (:div :class "extra-top-1" "<!-- empty -->")
	  (:div :class "extra-top-2" "<!-- empty -->")
	  (:div :class "extra-top-3" "<!-- empty -->")
	  (:fieldset
	   (:div :class "datagrid-body"
		 #.(table-header-template
		    '((:th :class "name sort-ascending" (:span #.(link-action-template "abc131" "Name")))
		      (:th :class "manager" (:span #.(link-action-template "abc132" "Manager"))))
		    '((:tr
		       (:td :class "name" (:span :class "value" "Joe"))
		       (:td :class "manager" (:span :class "value" "Jim"))))
		    :summary "Ordered by name, ascending."))
	   (:input :name "action" :type "hidden" :value "abc130"))
	  (:div :class "extra-bottom-1" "<!-- empty -->")
	  (:div :class "extra-bottom-2" "<!-- empty -->")
	  (:div :class "extra-bottom-3" "<!-- empty -->"))
   (:div :class "widget pagination" :id "widget-123"
	 #.(link-action-template "abc133" "< Previous" :class "previous-page")
	 "&nbsp;" #.(pagination-page-info-template 2 3) "&nbsp;"
	 #.(link-action-template "abc134" "Next >" :class "next-page")
	 #.(pagination-goto-form-template "abc135"))))

(deftest render-widget-body-datagrid-6
    (with-request :get nil
      (let ((grid (make-instance 'datagrid
				 :data (list *joe* *bob*)
				 :data-class 'employee
				 :show-total-items-count-p nil))
	    (*on-ajax-complete-scripts* nil)
	    (*weblocks-output-stream* (make-string-output-stream)))
	(declare (special *on-ajax-complete-scripts* *weblocks-output-stream*))
	;; set items per page and current page
	(setf (pagination-items-per-page (datagrid-pagination-widget grid)) 1)
	(setf (pagination-current-page (datagrid-pagination-widget grid)) 2)
	;; render datagrid
	(render-widget-body grid :form-id "I1" :input-id "I2" :search-id "I3")
	;; search
	(do-request `(("search" . "bob")
		      (,weblocks::*action-string* . "abc123")))
	(render-widget-body grid :form-id "I1" :input-id "I2" :search-id "I3")
	(assert (eq (pagination-current-page (datagrid-pagination-widget grid)) 1))
	(assert (eq (pagination-page-count (datagrid-pagination-widget grid)) 1))
	(assert (eq (weblocks::datagrid-search-pagination-history grid) 2))
	;; remove search
	(do-request `(("search" . nil)
		      (,weblocks::*action-string* . "abc131")))
	(render-widget-body grid :form-id "I1" :input-id "I2" :search-id "I3")
	(assert (eq (pagination-current-page (datagrid-pagination-widget grid)) 2))
	t))
  t)

;;; test render-datagrid-table-body
(deftest-html render-datagrid-table-body-1
    (with-request :get nil
      (let ((grid (make-instance 'datagrid
				 :data (list *joe* *bob*)
				 :data-class 'employee))
	    (*on-ajax-complete-scripts* nil))
	(declare (special *on-ajax-complete-scripts*))
	;; render datagrid
	(render-datagrid-table-body grid)
	;; sort by name (should be descending)
	(do-request `((,weblocks::*action-string* . "abc123")))
	(render-datagrid-table-body grid)
	;; refresh the action (should still be descending)
	(do-request `((,weblocks::*action-string* . "abc123")))
	(render-datagrid-table-body grid)))
  (htm
   (:div :class "datagrid-body"
	 #.(table-header-template
	    '((:th :class "name sort-ascending" (:span #.(link-action-template "abc123" "Name")))
	      (:th :class "manager" (:span #.(link-action-template "abc124" "Manager"))))
	    '((:tr
	       (:td :class "name" (:span :class "value" "Bob"))
	       (:td :class "manager" (:span :class "value" "Jim")))
	      (:tr :class "altern"
	       (:td :class "name" (:span :class "value" "Joe"))
	       (:td :class "manager" (:span :class "value" "Jim"))))
	    :summary "Ordered by name, ascending."))
   (:div :class "datagrid-body"
	 #.(table-header-template
	    '((:th :class "name sort-descending" (:span #.(link-action-template "abc125" "Name")))
	      (:th :class "manager" (:span #.(link-action-template "abc126" "Manager"))))
	    '((:tr
	       (:td :class "name" (:span :class "value" "Joe"))
	       (:td :class "manager" (:span :class "value" "Jim")))
	      (:tr :class "altern"
	       (:td :class "name" (:span :class "value" "Bob"))
	       (:td :class "manager" (:span :class "value" "Jim"))))
	    :summary "Ordered by name, descending."))
   (:div :class "datagrid-body"
	 #.(table-header-template
	    '((:th :class "name sort-descending" (:span #.(link-action-template "abc127" "Name")))
	      (:th :class "manager" (:span #.(link-action-template "abc128" "Manager"))))
	    '((:tr
	       (:td :class "name" (:span :class "value" "Joe"))
	       (:td :class "manager" (:span :class "value" "Jim")))
	      (:tr :class "altern"
	       (:td :class "name" (:span :class "value" "Bob"))
	       (:td :class "manager" (:span :class "value" "Jim"))))
	    :summary "Ordered by name, descending."))))

(deftest-html render-datagrid-table-body-2
    (with-request :get nil
      (let ((grid (make-instance 'datagrid
				 :data (list *joe* *bob*)
				 :allow-sorting '(manager)
				 :data-class 'employee)))
	(render-datagrid-table-body grid)))
   (:div :class "datagrid-body"
	 #.(table-header-template
	    '((:th :class "name" "Name")
	      (:th :class "manager sort-ascending" (:span #.(link-action-template "abc123" "Manager"))))
	    '((:tr
	       (:td :class "name" (:span :class "value" "Joe"))
	       (:td :class "manager" (:span :class "value" "Jim")))
	      (:tr :class "altern"
	       (:td :class "name" (:span :class "value" "Bob"))
	       (:td :class "manager" (:span :class "value" "Jim"))))
	    :summary "Ordered by manager, ascending.")))

(deftest-html render-datagrid-table-body-3
    (with-request :get nil
      (let ((grid (make-instance 'datagrid
				 :data (list *joe* *bob*)
				 :allow-sorting nil
				 :data-class 'employee)))
	(render-datagrid-table-body grid)))
     (:div :class "datagrid-body"
	   #.(table-header-template
	      '((:th :class "name" "Name")
		(:th :class "manager" "Manager"))
	      '((:tr
		 (:td :class "name" (:span :class "value" "Joe"))
		 (:td :class "manager" (:span :class "value" "Jim")))
		(:tr :class "altern"
		 (:td :class "name" (:span :class "value" "Bob"))
		 (:td :class "manager" (:span :class "value" "Jim")))))))

(deftest-html render-datagrid-table-body-4
    (with-request :get nil
      (let ((grid (make-instance 'datagrid
				 :data (list *joe* *bob*)
				 :data-class 'employee)))
	(render-datagrid-table-body grid)
	(setf (first-name *bob*) "Zed")
	(render-datagrid-table-body grid)
	(setf (first-name *bob*) "Bob")))
  (htm
   (:div :class "datagrid-body"
	 #.(table-header-template
	    '((:th :class "name sort-ascending" (:span #.(link-action-template "abc123" "Name")))
	      (:th :class "manager" (:span #.(link-action-template "abc124" "Manager"))))
	    '((:tr
	       (:td :class "name" (:span :class "value" "Bob"))
	       (:td :class "manager" (:span :class "value" "Jim")))
	      (:tr :class "altern"
	       (:td :class "name" (:span :class "value" "Joe"))
	       (:td :class "manager" (:span :class "value" "Jim"))))
	    :summary "Ordered by name, ascending."))
   (:div :class "datagrid-body"
	 #.(table-header-template
	    '((:th :class "name sort-ascending" (:span #.(link-action-template "abc125" "Name")))
	      (:th :class "manager" (:span #.(link-action-template "abc126" "Manager"))))
	    '((:tr
	       (:td :class "name" (:span :class "value" "Joe"))
	       (:td :class "manager" (:span :class "value" "Jim")))
	      (:tr :class "altern"
	       (:td :class "name" (:span :class "value" "Zed"))
	       (:td :class "manager" (:span :class "value" "Jim"))))
	    :summary "Ordered by name, ascending."))))

(deftest-html render-datagrid-table-body-5
    (with-request :get nil
      (let ((grid (make-instance 'datagrid
				 :data (list *joe* *bob*)
				 :search "J"
				 :data-class 'employee)))
	;; render datagrid
	(render-datagrid-table-body grid)))
  (htm
   (:div :class "datagrid-body"
   #.(table-header-template
      '((:th :class "name sort-ascending" (:span #.(link-action-template "abc123" "Name")))
	(:th :class "manager" (:span #.(link-action-template "abc124" "Manager"))))
      '((:tr
	 (:td :class "name" (:span :class "value" "Bob"))
	 (:td :class "manager" (:span :class "value" "<strong>J</strong>im")))
	(:tr :class "altern"
	 (:td :class "name" (:span :class "value" "<strong>J</strong>oe"))
	 (:td :class "manager" (:span :class "value" "<strong>J</strong>im"))))
      :summary "Ordered by name, ascending."))))

(deftest-html render-datagrid-table-body-6
    (with-request :get nil
      (let ((grid (make-instance 'datagrid
				 :data (list *joe* *bob*)
				 :sort '(address-ref . :ascending)
				 :data-class 'employee)))
	;; render datagrid
	(render-datagrid-table-body grid :slots '(address-ref))
	;; sort by name (should be descending)
	(do-request `((,weblocks::*action-string* . "abc124")))
	(render-datagrid-table-body grid :slots '(address-ref))))
  (htm
   (:div :class "datagrid-body"
	 #.(table-header-template
	    '((:th :class "name" (:span #.(link-action-template "abc123" "Name")))
	      (:th :class "address-ref sort-ascending" (:span #.(link-action-template "abc124" "Address")))
	      (:th :class "manager" (:span #.(link-action-template "abc125" "Manager"))))
	    '((:tr
	       (:td :class "name" (:span :class "value" "Joe"))
	       (:td :class "address-ref" (:span :class "value" "Address"))
	       (:td :class "manager" (:span :class "value" "Jim")))
	      (:tr :class "altern"
	       (:td :class "name" (:span :class "value" "Bob"))
	       (:td :class "address-ref" (:span :class "value" "Address"))
	       (:td :class "manager" (:span :class "value" "Jim"))))
	    :summary "Ordered by address, ascending."))
   (:div :class "datagrid-body"
	 #.(table-header-template
	    '((:th :class "name" (:span #.(link-action-template "abc126" "Name")))
	      (:th :class "address-ref sort-descending" (:span #.(link-action-template "abc127" "Address")))
	      (:th :class "manager" (:span #.(link-action-template "abc128" "Manager"))))
	    '((:tr
	       (:td :class "name" (:span :class "value" "Joe"))
	       (:td :class "address-ref" (:span :class "value" "Address"))
	       (:td :class "manager" (:span :class "value" "Jim")))
	      (:tr :class "altern"
	       (:td :class "name" (:span :class "value" "Bob"))
	       (:td :class "address-ref" (:span :class "value" "Address"))
	       (:td :class "manager" (:span :class "value" "Jim"))))
	    :summary "Ordered by address, descending."))))

;;; test specialization of widget-public-dependencies
(deftest datagrid-widget-public-dependencies-1
    (with-request :get nil
      (not (null
	    (member "stylesheets/pagination.css"
		    (widget-public-dependencies (make-instance 'datagrid :data-class 'employee))
		    :key (curry #'format nil "~A")
		    :test #'string-equal))))
  t)

