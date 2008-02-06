
(in-package :weblocks-test)

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
      (persist-objects *default-store* (list *joe* *bob*))
      (let ((grid (make-instance 'datagrid
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
  ("new Function(\"updateElementBody($('widget-123').getElementsByClassName('datagrid-body')[0], \\\"<div class='datagrid-body'><div class='view table empty-table'><div class='extra-top-1'><!-- empty --></div><div class='extra-top-2'><!-- empty --></div><div class='extra-top-3'><!-- empty --></div><p><span class='message'>No information available.</span></p><div class='extra-bottom-1'><!-- empty --></div><div class='extra-bottom-2'><!-- empty --></div><div class='extra-bottom-3'><!-- empty --></div></div></div>\\\");\")"
               "new Function(\"updateElementBody($('widget-123').getElementsByClassName('total-items')[0], '(Found 0 of 2 Items)');\")"
               "new Function(\"updateElementBody($('widget-123').getElementsByClassName('datagrid-body')[0], \\\"<div class='datagrid-body'><div class='view table employee'><div class='extra-top-1'><!-- empty --></div><div class='extra-top-2'><!-- empty --></div><div class='extra-top-3'><!-- empty --></div><table summary='Ordered by name, ascending.'><thead><tr><th class='name sort-asc'><span><a href='/foo/bar?action=abc124' onclick='initiateAction(\\\\\\\"abc124\\\\\\\", \\\\\\\"weblocks-session=1%3ATEST\\\\\\\"); return false;'>Name</a></span></th><th class='manager'><span><a href='/foo/bar?action=abc125' onclick='initiateAction(\\\\\\\"abc125\\\\\\\", \\\\\\\"weblocks-session=1%3ATEST\\\\\\\"); return false;'>Manager</a></span></th></tr></thead><tbody><tr><td class='name'><span class='value'><strong>Bob</strong></span></td><td class='manager'><span class='value'>Jim</span></td></tr></tbody></table><div class='extra-bottom-1'><!-- empty --></div><div class='extra-bottom-2'><!-- empty --></div><div class='extra-bottom-3'><!-- empty --></div></div></div>\\\");\")"
               "new Function(\"updateElementBody($('widget-123').getElementsByClassName('total-items')[0], '(Found 1 of 2 Items)');\")"))

(deftest-html datagrid-render-search-bar-3
    (with-request :get nil
      (persist-objects *default-store* (list *joe* *bob*))
      (let ((grid (make-instance 'datagrid
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
			 :total-items-text "(Total of 2 Items)")
   #.(searchbar-template "I1" "I2" "I3" "abc124"
			 :total-items-text "(Found 0 of 2 Items)"
			 :value "hello")))

(deftest-html datagrid-render-search-bar-4
    (with-request :get nil
      (persist-object *default-store* *joe*)
      (let ((grid (make-instance 'datagrid
				 :data-class 'employee))
	    (*on-ajax-complete-scripts* nil))
	(declare (special *weblocks-output-stream* *on-ajax-complete-scripts*))
	(weblocks::datagrid-render-search-bar grid
					      :form-id "I1"
					      :input-id "I2"
					      :search-id "I3")))
  (htm
   #.(searchbar-template "I1" "I2" "I3" "abc123"
			 :total-items-text "(Total of 1 Item)")))

