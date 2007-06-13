
(in-package :weblocks-test)

;;; test with-widget-header for datagrid
(deftest-html with-widget-header-data-grid-1
    (with-request :get nil
      (let ((grid (make-instance 'datagrid)))
	(with-widget-header grid (lambda (&rest args)
				   nil)
			    :form-id "I1"
			    :input-id "I2"
			    :search-id "I3")))
  (:div :class "widget datagrid" :id "widget-123"
	(:div :class "datagrid-search-bar"
	      (:div :class "extra-top-1" "&nbsp;")
	      (:div :class "extra-top-2" "&nbsp;")
	      (:div :class "extra-top-3" "&nbsp;")
	      (:span :class "title" "Search table")
	      (:form :id "I1" :class "isearch" :action "" :method "get"
		     (:fieldset
		      (:input :type "text" :id "I2" :name "search" :class "search-bar")
		      (:input :id "I3" :name "submit" :type "submit" :class "submit" :value "Search")
		      (:input :name "action" :type "hidden" :value "abc123")))
	      (:script :type "text/javascript"
		       (fmt "~%// <![CDATA[~%")
		       (fmt "new Form.Element.DelayedObserver('I2', 0.4, function(elem, value) {initiateFormAction('abc123', $('I1'), 'weblocks-session=1%3Atest');
});$('I3').remove();")
		       (fmt "~%// ]]>~%"))
	      (:span :class "hidden-items" "&nbsp;")
	      (:div :class "extra-bottom-1" "&nbsp;")
	      (:div :class "extra-bottom-2" "&nbsp;")
	      (:div :class "extra-bottom-3" "&nbsp;"))
	(:div :class "widget-body" "")))

;;; test datagrid-filter-data
(deftest datagrid-filter-data-1
    (length (weblocks::datagrid-filter-data
	     (make-instance 'datagrid :search "Joe")
	     (list *joe* *bob*)))
  1)

(deftest datagrid-filter-data-2
    (length (weblocks::datagrid-filter-data
	     (make-instance 'datagrid :search "o")
	     (list *joe* *bob*)))
  2)

;;; test object-satisfies-search-p
(deftest object-satisfies-search-p-1
    (weblocks::object-satisfies-search-p "hi" *joe*)
  nil)

(deftest object-satisfies-search-p-2
    (weblocks::object-satisfies-search-p "Joe" *joe*)
  t)

(deftest object-satisfies-search-p-3
    (weblocks::object-satisfies-search-p "30" *joe*)
  nil)

(deftest object-satisfies-search-p-4
    (weblocks::object-satisfies-search-p "30" *joe* :slots '(age))
  t)

(deftest object-satisfies-search-p-5
    (weblocks::object-satisfies-search-p "Broadway" *joe*)
  nil)

(deftest object-satisfies-search-p-6
    (weblocks::object-satisfies-search-p "address" *joe*)
  nil)

;;; test make-isearch-regex
(deftest make-isearch-regex-1
    (let ((regex (weblocks::make-isearch-regex "hello")))
      (values (ppcre:scan regex "hello")
	      (ppcre:scan regex "HeLlO")
	      (ppcre:scan regex "test")))
  0 0 nil)

(deftest make-isearch-regex-2
    (let ((regex (weblocks::make-isearch-regex "Hello")))
      (values (ppcre:scan regex "Hello")
	      (ppcre:scan regex "hello")
	      (ppcre:scan regex "test")))
  0 nil nil)

;;; test hidden-items-message
(deftest hidden-items-message-1
    (weblocks::hidden-items-message (make-instance 'datagrid :data (list *joe* *bob*)))
  "&nbsp;")

(deftest hidden-items-message-2
    (weblocks::hidden-items-message (make-instance 'datagrid :data (list *joe* *bob*)
						   :search "Test"))
  "(<span class=\"item-count\">2</span> items are hidden by the search)")

;;; test datagrid-render-search-bar
(deftest datagrid-render-search-bar-1
    (with-request :get nil
      (let ((grid (make-instance 'datagrid :search "test"))
	    (*weblocks-output-stream* (make-string-output-stream))
	    (*on-ajax-complete-scripts* nil))
	(declare (special *weblocks-output-stream* *on-ajax-complete-scripts*))
	(weblocks::datagrid-render-search-bar grid)
	(do-request `((,weblocks::*action-string* . "abc123")
				   ("search" . "hello")))
	(values (datagrid-search grid))))
  "hello")

(deftest datagrid-render-search-bar-2
    (with-request :get nil
      (let ((grid (make-instance 'datagrid :data (list *joe* *bob*)))
	    (*weblocks-output-stream* (make-string-output-stream))
	    (*on-ajax-complete-scripts* nil))
	(declare (special *weblocks-output-stream* *on-ajax-complete-scripts*))
	(weblocks::datagrid-render-search-bar grid)
	(do-request `((,weblocks::*action-string* . "abc123")
				   ("search" . "hello")))
	(do-request `((,weblocks::*action-string* . "abc123")
				   ("search" . "bob")))
	(reverse *on-ajax-complete-scripts*)))
  ("function () { $('widget-123').getElementsByClassName('hidden-items')[0].innerHTML = '(<span class=\"item-count\">2</span> items are hidden by the search)';}"
   "function () { $('widget-123').getElementsByClassName('hidden-items')[0].innerHTML = '(<span class=\"item-count\">1</span> item is hidden by the search)';}"))

(deftest-html datagrid-render-search-bar-3
    (with-request :get nil
      (let ((grid (make-instance 'datagrid :data (list *joe* *bob*)))
	    (*on-ajax-complete-scripts* nil))
	(declare (special *weblocks-output-stream* *on-ajax-complete-scripts*))
	(weblocks::datagrid-render-search-bar grid
					      :form-id "I1"
					      :input-id "I2"
					      :search-id "I3")
	(do-request `((,weblocks::*action-string* . "abc123")
				   ("search" . "hello")))
	(weblocks::datagrid-render-search-bar grid
					      :form-id "I1"
					      :input-id "I2"
					      :search-id "I3")))
  (htm
   (:div :class "datagrid-search-bar"
	 (:div :class "extra-top-1" "&nbsp;")
	 (:div :class "extra-top-2" "&nbsp;")
	 (:div :class "extra-top-3" "&nbsp;")
	 (:span :class "title" "Search table")
	 (:form :id "I1" :class "isearch" :action "" :method "get"
		(:fieldset
		 (:input :type "text" :id "I2" :name "search" :class "search-bar")
		 (:input :id "I3" :name "submit" :type "submit" :class "submit" :value "Search")
		 (:input :name "action" :type "hidden" :value "abc123")))
	 (:script :type "text/javascript"
		  (fmt "~%// <![CDATA[~%")
		  (fmt "new Form.Element.DelayedObserver('I2', 0.4, function(elem, value) {initiateFormAction('abc123', $('I1'), 'weblocks-session=1%3Atest');
});$('I3').remove();")
		  (fmt "~%// ]]>~%"))
	 (:span :class "hidden-items" "&nbsp;")
	 (:div :class "extra-bottom-1" "&nbsp;")
	 (:div :class "extra-bottom-2" "&nbsp;")
	 (:div :class "extra-bottom-3" "&nbsp;"))
   (:div :class "datagrid-search-bar"
	 (:div :class "extra-top-1" "&nbsp;")
	 (:div :class "extra-top-2" "&nbsp;")
	 (:div :class "extra-top-3" "&nbsp;")
	 (:span :class "title" "Search table")
	 (:form :id "I1" :class "isearch" :action "" :method "get"
		(:fieldset
		 (:input :type "text" :id "I2" :name "search" :class "search-bar" :value "hello")
		 (:input :id "I3" :name "submit" :type "submit" :class "submit" :value "Search")
		 (:input :name "action" :type "hidden" :value "abc124")))
	 (:script :type "text/javascript"
		  (fmt "~%// <![CDATA[~%")
		  (fmt "new Form.Element.DelayedObserver('I2', 0.4, function(elem, value) {initiateFormAction('abc124', $('I1'), 'weblocks-session=1%3Atest');
});$('I3').remove();")
		  (fmt "~%// ]]>~%"))
	 (:span :class "hidden-items"
		"(<span class=\"item-count\">2</span> items are hidden by the search)")
	 (:div :class "extra-bottom-1" "&nbsp;")
	 (:div :class "extra-bottom-2" "&nbsp;")
	 (:div :class "extra-bottom-3" "&nbsp;"))))

;;; test datagrid's hook into render-table-header-cell
(deftest-html render-table-header-cell-around
    (render-table-header-cell *joe* 'name "Joe"
			      :grid-obj (make-instance 'datagrid :allow-sorting nil))
  (:th :class "name" "Name"))

;;; test render-datagrid-header-cell
(deftest-html render-datagrid-header-cell-1
    (with-request :get nil
      (render-datagrid-header-cell *joe* 'name "Joe"
				   :slot-path '(name)
				   :grid-obj (make-instance 'datagrid :sort '((name) . :ascending))))
  (:th :class "name sort-ascending"
       (:span #.(link-action-template "abc123" "Name"))))

(deftest-html render-datagrid-header-cell-2
    (with-request :get nil
      (render-datagrid-header-cell *joe* 'name "Joe"
				   :slot-path '(name)
				   :grid-obj (make-instance 'datagrid :sort '((manager) . :ascending))))
  (:th :class "name"
       (:span #.(link-action-template "abc123" "Name"))))

;;; test datagrid-update-sort-column/aux-datagrid-update-sort-column
(deftest datagrid-update-sort-column-1
    (let ((grid (make-instance 'datagrid :data (list *joe* *joe*)))
	  sort1 sort2)
      (setf sort1 (datagrid-sort grid))
      (weblocks::datagrid-update-sort-column grid)
      (setf sort2 (datagrid-sort grid))
      (values sort1 sort2))
  nil
  ((name) . :ascending))

(deftest datagrid-update-sort-column-2
    (let ((grid (make-instance 'datagrid
			       :data (list *joe* *joe*)
			       :allow-sorting '(manager))))
      (weblocks::datagrid-update-sort-column grid)
      (datagrid-sort grid))
  ((manager) . :ascending))

(deftest datagrid-update-sort-column-3
    (let ((grid (make-instance 'datagrid
			       :data (list *joe* *joe*)
			       :allow-sorting '(university))))
      (weblocks::datagrid-update-sort-column grid :slots '(education university))
      (datagrid-sort grid))
  ((education university) . :ascending))

(deftest datagrid-update-sort-column-4
    (let ((grid (make-instance 'datagrid
			       :data (list *joe* *joe*)
			       :allow-sorting '(address-ref))))
      (weblocks::datagrid-update-sort-column grid :slots '(address-ref))
      (datagrid-sort grid))
  ((address-ref) . :ascending))

;;; test datagrid-column-sortable-p
(deftest datagrid-column-sortable-p-1
    (not (null
	  (weblocks::datagrid-column-sortable-p (make-instance 'datagrid
							       :data (list *joe*)
							       :allow-sorting nil)
						'(name) "Joe")))
  nil)

(deftest datagrid-column-sortable-p-2
    (not (null
	  (weblocks::datagrid-column-sortable-p (make-instance 'datagrid
							       :data (list *joe*)
							       :allow-sorting t)
						'(name) "Joe")))
  t)

(deftest datagrid-column-sortable-p-3
    (not (null
	  (weblocks::datagrid-column-sortable-p (make-instance 'datagrid
							       :data (list *joe*)
							       :allow-sorting '(manager))
						'(name) "Joe")))
  nil)

(deftest datagrid-column-sortable-p-4
    (not (null
	  (weblocks::datagrid-column-sortable-p (make-instance 'datagrid
							       :data (list *joe*)
							       :allow-sorting '(manager))
						'(manager) "Jim")))
  t)

(deftest datagrid-column-sortable-p-5
    (not (null
	  (weblocks::datagrid-column-sortable-p (make-instance 'datagrid
							       :data (list *joe*)
							       :allow-sorting '(education graduation-year))
						'(education university) "Jim")))
  nil)

(deftest datagrid-column-sortable-p-6
    (not (null
	  (weblocks::datagrid-column-sortable-p (make-instance 'datagrid
							       :data (list *joe*)
							       :allow-sorting '(education graduation-year))
						'(education graduation-year) "Jim")))
  t)

;;; test datagrid-sorted-slot-name
(deftest datagrid-sorted-slot-name-1
    (datagrid-sorted-slot-name '(address name))
  name)

(deftest datagrid-sorted-slot-name-2
    (datagrid-sorted-slot-name '(name))
  name)

(deftest datagrid-sorted-slot-name-3
    (datagrid-sorted-slot-name 'name)
  name)

(deftest datagrid-sorted-slot-name-4
    (datagrid-sorted-slot-name '((address name) . :ascending))
  name)

;;; test datagrid-sort-data
(deftest datagrid-sort-data-1
    (let ((grid (make-instance 'datagrid
			       :data (list *joe* *bob*)
			       :sort '((name) . :ascending)))
	  res)
      (setf res (weblocks::datagrid-sort-data grid
					      (slot-value grid 'weblocks::data)))
      (values
       (first-name (car res))
       (first-name (cadr res))))
  "Bob" "Joe")

(deftest datagrid-sort-data-2
    (let ((grid (make-instance 'datagrid
			       :data (list *joe* *bob*)
			       :sort '((name) . :descending)))
	  res)
      (setf res (weblocks::datagrid-sort-data grid
					      (slot-value grid 'weblocks::data)))
      (values
       (first-name (car res))
       (first-name (cadr res))))
  "Joe" "Bob")

(deftest datagrid-sort-data-3
    (let ((grid (make-instance 'datagrid
			       :data (list *joe* *bob*)
			       :sort '((age) . :descending)))
	  res)
      (setf res (weblocks::datagrid-sort-data grid
					      (slot-value grid 'weblocks::data)))
      (values
       (first-name (car res))
       (first-name (cadr res))))
  "Bob" "Joe")

;;; test datagrid-data
(deftest datagrid-data-1
    (datagrid-data (make-instance 'datagrid
				  :data (list 1 2)))
  (1 2))

(deftest datagrid-data-2
    (datagrid-data (make-instance 'datagrid
				  :data (lambda (search sort)
					  (list 1 2))))
  (1 2))

;;; test datagrid-data-count
(deftest datagrid-data-count-1
    (datagrid-data-count (make-instance 'datagrid
				  :data (list 1 2)))
  2)

(deftest datagrid-data-count-2
    (datagrid-data-count (make-instance 'datagrid
				  :data (lambda (search sort &key countp)
					  (list search sort countp))
				  :search "foo"))
  ("foo" nil t))

(deftest datagrid-data-count-3
    (datagrid-data-count (make-instance 'datagrid
				  :data (lambda (search sort &key countp)
					  (list search sort countp))
				  :search "foo")
			 :totalp t)
  (nil nil t))

;;; test negate-sort-direction
(deftest negate-sort-direction-1
    (weblocks::negate-sort-direction :ascending)
  :descending)

(deftest negate-sort-direction-2
    (weblocks::negate-sort-direction :descending)
  :ascending)

;;; test render-widget-body
(deftest-html render-widget-body-datagrid-1
    (with-request :get nil
      (let ((grid (make-instance 'datagrid
				 :data (list *joe* *bob*))))
	;; render datagrid
	(render-widget-body grid)
	;; sort by name (should be descending)
	(do-request `((,weblocks::*action-string* . "abc123")))
	(render-widget-body grid)
	;; refresh the action (should still be descending)
	(do-request `((,weblocks::*action-string* . "abc123")))
	(render-widget-body grid)))
  (htm
   #.(table-header-template
      '((:th :class "name sort-ascending" (:span #.(link-action-template "abc123" "Name")))
	(:th :class "manager" (:span #.(link-action-template "abc124" "Manager"))))
      '((:tr
	 (:td :class "name" (:span :class "value" "Bob"))
	 (:td :class "manager" (:span :class "value" "Jim")))
	(:tr :class "altern"
	 (:td :class "name" (:span :class "value" "Joe"))
	 (:td :class "manager" (:span :class "value" "Jim"))))
      :summary "Ordered by name, ascending.")
   #.(table-header-template
     '((:th :class "name sort-descending" (:span #.(link-action-template "abc125" "Name")))
       (:th :class "manager" (:span #.(link-action-template "abc126" "Manager"))))
     '((:tr
	(:td :class "name" (:span :class "value" "Joe"))
	(:td :class "manager" (:span :class "value" "Jim")))
       (:tr :class "altern"
	(:td :class "name" (:span :class "value" "Bob"))
	(:td :class "manager" (:span :class "value" "Jim"))))
     :summary "Ordered by name, descending.")
   #.(table-header-template
     '((:th :class "name sort-descending" (:span #.(link-action-template "abc127" "Name")))
       (:th :class "manager" (:span #.(link-action-template "abc128" "Manager"))))
     '((:tr
	(:td :class "name" (:span :class "value" "Joe"))
	(:td :class "manager" (:span :class "value" "Jim")))
       (:tr :class "altern"
	(:td :class "name" (:span :class "value" "Bob"))
	(:td :class "manager" (:span :class "value" "Jim"))))
     :summary "Ordered by name, descending.")))

(deftest-html render-widget-body-datagrid-2
    (with-request :get nil
      (let ((grid (make-instance 'datagrid
				 :data (list *joe* *bob*)
				 :allow-sorting '(manager))))
	(render-widget-body grid)))
  #.(table-header-template
     '((:th :class "name" "Name")
       (:th :class "manager sort-ascending" (:span #.(link-action-template "abc123" "Manager"))))
     '((:tr
	(:td :class "name" (:span :class "value" "Joe"))
	(:td :class "manager" (:span :class "value" "Jim")))
       (:tr :class "altern"
	(:td :class "name" (:span :class "value" "Bob"))
	(:td :class "manager" (:span :class "value" "Jim"))))
     :summary "Ordered by manager, ascending."))

(deftest-html render-widget-body-datagrid-3
    (with-request :get nil
      (let ((grid (make-instance 'datagrid
				 :data (list *joe* *bob*)
				 :allow-sorting nil)))
	(render-widget-body grid)))
  #.(table-header-template
     '((:th :class "name" "Name")
       (:th :class "manager" "Manager"))
     '((:tr
	(:td :class "name" (:span :class "value" "Joe"))
	(:td :class "manager" (:span :class "value" "Jim")))
       (:tr :class "altern"
	(:td :class "name" (:span :class "value" "Bob"))
	(:td :class "manager" (:span :class "value" "Jim"))))))

(deftest-html render-widget-body-datagrid-4
    (with-request :get nil
      (let ((grid (make-instance 'datagrid
				 :data (list *joe* *bob*))))
	(render-widget-body grid)
	(setf (first-name *bob*) "Zed")
	(render-widget-body grid)
	(setf (first-name *bob*) "Bob")))
  (htm
   #.(table-header-template
      '((:th :class "name sort-ascending" (:span #.(link-action-template "abc123" "Name")))
	(:th :class "manager" (:span #.(link-action-template "abc124" "Manager"))))
      '((:tr
	 (:td :class "name" (:span :class "value" "Bob"))
	 (:td :class "manager" (:span :class "value" "Jim")))
	(:tr :class "altern"
	 (:td :class "name" (:span :class "value" "Joe"))
	 (:td :class "manager" (:span :class "value" "Jim"))))
      :summary "Ordered by name, ascending.")
   #.(table-header-template
      '((:th :class "name sort-ascending" (:span #.(link-action-template "abc125" "Name")))
	(:th :class "manager" (:span #.(link-action-template "abc126" "Manager"))))
      '((:tr
	 (:td :class "name" (:span :class "value" "Joe"))
	 (:td :class "manager" (:span :class "value" "Jim")))
	(:tr :class "altern"
	 (:td :class "name" (:span :class "value" "Zed"))
	 (:td :class "manager" (:span :class "value" "Jim"))))
      :summary "Ordered by name, ascending.")))

(deftest-html render-widget-body-datagrid-5
    (with-request :get nil
      (let ((grid (make-instance 'datagrid
				 :data (list *joe* *bob*)
				 :search "J")))
	;; render datagrid
	(render-widget-body grid)))
  (htm
   #.(table-header-template
      '((:th :class "name sort-ascending" (:span #.(link-action-template "abc123" "Name")))
	(:th :class "manager" (:span #.(link-action-template "abc124" "Manager"))))
      '((:tr
	 (:td :class "name" (:span :class "value" "Bob"))
	 (:td :class "manager" (:span :class "value" "<strong>J</strong>im")))
	(:tr :class "altern"
	 (:td :class "name" (:span :class "value" "<strong>J</strong>oe"))
	 (:td :class "manager" (:span :class "value" "<strong>J</strong>im"))))
      :summary "Ordered by name, ascending.")))

(deftest-html render-widget-body-datagrid-6
    (with-request :get nil
      (let ((grid (make-instance 'datagrid
				 :data (list *joe* *bob*)
				 :sort '(address-ref . :ascending))))
	;; render datagrid
	(render-widget-body grid :slots '(address-ref))
	;; sort by name (should be descending)
	(do-request `((,weblocks::*action-string* . "abc124")))
	(render-widget-body grid :slots '(address-ref))))
  (htm
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
      :summary "Ordered by address, ascending.")
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
     :summary "Ordered by address, descending.")))
