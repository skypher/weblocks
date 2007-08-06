
(in-package :weblocks-test)

;;; test datagrid-filter-data
(deftest datagrid-filter-data-1
    (length (weblocks::datagrid-filter-data
	     (make-instance 'datagrid :search "Joe"
			    :data-class 'employee)
	     (list *joe* *bob*)))
  1)

(deftest datagrid-filter-data-2
    (length (weblocks::datagrid-filter-data
	     (make-instance 'datagrid :search "o"
			    :data-class 'employee)
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
    (weblocks::hidden-items-message (make-instance 'datagrid :data (list *joe* *bob*)
						   :data-class 'employee))
  "&nbsp;")

(deftest hidden-items-message-2
    (weblocks::hidden-items-message (make-instance 'datagrid :data (list *joe* *bob*)
						   :search "Test"
						   :data-class 'employee))
  "(2 items are hidden by the search)")

;;; test datagrid-render-search-bar
(deftest datagrid-render-search-bar-1
    (with-request :get nil
      (let ((grid (make-instance 'datagrid :search "test"
				 :data-class 'employee))
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
      (let ((grid (make-instance 'datagrid :data (list *joe* *bob*)
				 :data-class 'employee))
	    (*weblocks-output-stream* (make-string-output-stream))
	    (*on-ajax-complete-scripts* nil))
	(declare (special *weblocks-output-stream* *on-ajax-complete-scripts*))
	(weblocks::datagrid-render-search-bar grid)
	(do-request `((,weblocks::*action-string* . "abc123")
				   ("search" . "hello")))
	(do-request `((,weblocks::*action-string* . "abc123")
				   ("search" . "bob")))
	(reverse *on-ajax-complete-scripts*)))
  ("new Function(\"updateElementBody($('widget-123').getElementsByClassName('datagrid-body')[0], \\\"<div class='datagrid-body'><div class='renderer table empty-table'><div class='extra-top-1'>&nbsp;</div><div class='extra-top-2'>&nbsp;</div><div class='extra-top-3'>&nbsp;</div><p><span class='message'>No information available.</span></p><div class='extra-bottom-1'>&nbsp;</div><div class='extra-bottom-2'>&nbsp;</div><div class='extra-bottom-3'>&nbsp;</div></div></div>\\\");\")"
               "new Function(\"updateElementBody($('widget-123').getElementsByClassName('hidden-items')[0], '(2 items are hidden by the search)');\")"
               "new Function(\"updateElementBody($('widget-123').getElementsByClassName('datagrid-body')[0], \\\"<div class='datagrid-body'><div class='renderer table employee'><div class='extra-top-1'>&nbsp;</div><div class='extra-top-2'>&nbsp;</div><div class='extra-top-3'>&nbsp;</div><table summary='Ordered by name, ascending.'><thead><tr><th class='name sort-ascending'><span><a href='/foo/bar?action=abc124' onclick='initiateAction(\\\\\\\"abc124\\\\\\\", \\\\\\\"weblocks-session=1%3Atest\\\\\\\"); return false;'>Name</a></span></th><th class='manager'><span><a href='/foo/bar?action=abc125' onclick='initiateAction(\\\\\\\"abc125\\\\\\\", \\\\\\\"weblocks-session=1%3Atest\\\\\\\"); return false;'>Manager</a></span></th></tr></thead><tbody><tr><td class='name'><span class='value'><strong>Bob</strong></span></td><td class='manager'><span class='value'>Jim</span></td></tr></tbody></table><div class='extra-bottom-1'>&nbsp;</div><div class='extra-bottom-2'>&nbsp;</div><div class='extra-bottom-3'>&nbsp;</div></div></div>\\\");\")"
               "new Function(\"updateElementBody($('widget-123').getElementsByClassName('hidden-items')[0], '(1 item is hidden by the search)');\")"))

(deftest-html datagrid-render-search-bar-3
    (with-request :get nil
      (let ((grid (make-instance 'datagrid :data (list *joe* *bob*)
				 :data-class 'employee))
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
   #.(searchbar-template "I1" "I2" "I3" "abc123"
			 :hidden-items-text "&nbsp;")
   #.(searchbar-template "I1" "I2" "I3" "abc124"
			 :hidden-items-text "(2 items are hidden by the search)"
			 :value "hello")))

