
(in-package :weblocks-test)

;;; test datagrid-data
(deftest datagrid-data-1
    (datagrid-data (make-instance 'datagrid
				  :data (list 1 2)
				  :data-class 'employee))
  (1 2))

(deftest datagrid-data-2
    (datagrid-data (make-instance 'datagrid
				  :data (lambda (search sort)
					  (list 1 2))
				  :data-class 'employee))
  (1 2))

(defun datagrid-data-foo (search sort)
  (list 1 2))

(deftest datagrid-data-3
    (datagrid-data (make-instance 'datagrid
                                  :data 'datagrid-data-foo
                                  :data-class 'employee))
  (1 2))

(deftest datagrid-data-4
    (datagrid-data (make-instance 'datagrid
                                  :data nil
                                  :data-class 'employee))
  nil)

;;; test datagrid-data-count
(deftest datagrid-data-count-1
    (datagrid-data-count (make-instance 'datagrid
				  :data (list 1 2)
				  :data-class 'employee))
  2)

(deftest datagrid-data-count-2
    (datagrid-data-count (make-instance 'datagrid
				  :data (lambda (search sort &key countp)
					  (list search sort countp))
				  :search "foo"
				  :data-class 'employee))
  ("foo" nil t))

(deftest datagrid-data-count-3
    (datagrid-data-count (make-instance 'datagrid
				  :data (lambda (search sort &key countp)
					  (list search sort countp))
				  :search "foo"
				  :data-class 'employee)
			 :totalp t)
  (nil nil t))

(defun datagrid-data-count-foo (search sort &key countp)
  (list search sort countp))

(deftest datagrid-data-count-4
    (datagrid-data-count (make-instance 'datagrid
                                        :data 'datagrid-data-count-foo
                                        :search "foo"
                                        :data-class 'employee))
  ("foo" nil t))

(deftest datagrid-data-count-5
    (datagrid-data-count (make-instance 'datagrid
                                        :data 'datagrid-data-count-foo
                                        :search "foo"
                                        :data-class 'employee)
			 :totalp t)
  (nil nil t))

(deftest datagrid-data-count-6
    (datagrid-data-count (make-instance 'datagrid
                                        :data nil
                                        :data-class 'employee))
  0)

;;; test append-custom-slots
(deftest append-custom-slots-1
    (weblocks::append-custom-slots '(a b c) '(:a 1 :b 2 :custom-slots (d e f) :c 3))
  (d e f a b c))

;;; test render-item-ops-bar
(deftest-html render-item-ops-bar-1
    (let ((grid (make-instance 'datagrid
			       :data-class 'employee)))
    (weblocks::render-item-ops-bar grid))
  (:div :class "item-operations" ""))

(deftest-html render-item-ops-bar-2
    (let ((grid (make-instance 'datagrid
			       :data-class 'employee
			       :item-ops (list (cons 'hello #'identity)
					       (cons 'world #'identity)))))
    (weblocks::render-item-ops-bar grid))
  (:div :class "item-operations"
	(:input :name "hello" :type "submit" :class "submit" :value "Hello"
		:onclick "disableIrrelevantButtons(this);")
	(:input :name "world" :type "submit" :class "submit" :value "World"
		:onclick "disableIrrelevantButtons(this);")))

;;; test render-widget-body for datagrid
(deftest-html render-widget-body-datagrid-1
    (with-request :get nil
      (let ((grid (make-instance 'datagrid
				 :data (list *joe* *bob*)
				 :data-class 'employee
				 :show-hidden-entries-count-p nil))
	    (*on-ajax-complete-scripts* nil))
	(declare (special *on-ajax-complete-scripts*))
	;; render datagrid
	(render-widget-body grid :form-id "I1" :input-id "I2" :search-id "I3")))
  (htm
   (:div :class "data-mining-bar"
	 #.(searchbar-template "I1" "I2" "I3" "abc123"))
   (:form :class "datagrid-form"
	  :action "/foo/bar"
	  :method "get"
	  :onsubmit "initiateFormAction(\"abc124\", $(this), \"weblocks-session=1%3Atest\"); return false;"
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
				 :allow-searching-p nil))
	    (*on-ajax-complete-scripts* nil))
	(declare (special *on-ajax-complete-scripts*))
	;; render datagrid
	(render-widget-body grid)))
  (htm
   (:div :class "data-mining-bar" "")
   (:form :class "datagrid-form"
	  :action "/foo/bar"
	  :method "get"
	  :onsubmit "initiateFormAction(\"abc123\", $(this), \"weblocks-session=1%3Atest\"); return false;"
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
					   :search "doesn't exist")))
	;; render datagrid
	(render-widget-body grid :form-id "I1" :input-id "I2" :search-id "I3")))
  (htm
   ;; datagrid
   (:div :class "data-mining-bar"
	 #.(searchbar-template "I1" "I2" "I3" "abc123" :value "doesn't exist"
						       :hidden-items-text
						       "(2 items are hidden by the search)"))
   (:form :class "datagrid-form"
	  :action "/foo/bar"
	  :method "get"
	  :onsubmit "initiateFormAction(\"abc124\", $(this), \"weblocks-session=1%3Atest\"); return false;"
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

;;; test render-datagrid-table-body
(deftest-html render-datagrid-table-body-1
    (with-request :get nil
      (let ((grid (make-instance 'datagrid
				 :data (list *joe* *bob*)
				 :data-class 'employee))
	    (*on-ajax-complete-scripts* nil))
	(declare (special *on-ajax-complete-scripts*))
	;; render datagrid
	(weblocks::render-datagrid-table-body grid)
	;; sort by name (should be descending)
	(do-request `((,weblocks::*action-string* . "abc123")))
	(weblocks::render-datagrid-table-body grid)
	;; refresh the action (should still be descending)
	(do-request `((,weblocks::*action-string* . "abc123")))
	(weblocks::render-datagrid-table-body grid)))
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
	(weblocks::render-datagrid-table-body grid)))
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
	(weblocks::render-datagrid-table-body grid)))
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
	(weblocks::render-datagrid-table-body grid)
	(setf (first-name *bob*) "Zed")
	(weblocks::render-datagrid-table-body grid)
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
	(weblocks::render-datagrid-table-body grid)))
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
	(weblocks::render-datagrid-table-body grid :slots '(address-ref))
	;; sort by name (should be descending)
	(do-request `((,weblocks::*action-string* . "abc124")))
	(weblocks::render-datagrid-table-body grid :slots '(address-ref))))
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

