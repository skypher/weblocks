
(in-package :weblocks-test)

;;; test datagrid-update-sort-column/aux-datagrid-update-sort-column
(deftest datagrid-update-sort-column-1
    (with-request :get nil
      (let ((grid (make-instance 'datagrid :data (list *joe* *joe*)
					   :data-class 'employee))
	    sort1 sort2)
	(setf sort1 (datagrid-sort grid))
	(weblocks::datagrid-update-sort-column grid)
	(setf sort2 (datagrid-sort grid))
	(values sort1 sort2)))
  nil
  ((name) . :ascending))

(deftest datagrid-update-sort-column-2
    (with-request :get nil
      (let ((grid (make-instance 'datagrid
				 :data (list *joe* *joe*)
				 :allow-sorting '(manager)
				 :data-class 'employee)))
	(weblocks::datagrid-update-sort-column grid)
	(datagrid-sort grid)))
  ((manager) . :ascending))

(deftest datagrid-update-sort-column-3
    (with-request :get nil
      (let ((grid (make-instance 'datagrid
				 :data (list *joe* *joe*)
				 :allow-sorting '(university)
				 :data-class 'employee)))
	(weblocks::datagrid-update-sort-column grid :slots '(education university))
	(datagrid-sort grid)))
  ((education university) . :ascending))

(deftest datagrid-update-sort-column-4
    (with-request :get nil
      (let ((grid (make-instance 'datagrid
				 :data (list *joe* *joe*)
				 :allow-sorting '(address-ref)
				 :data-class 'employee)))
	(weblocks::datagrid-update-sort-column grid :slots '(address-ref))
	(datagrid-sort grid)))
  ((address-ref) . :ascending))

;;; test datagrid-column-sortable-p
(deftest datagrid-column-sortable-p-1
    (with-request :get nil
      (not (null
	    (weblocks::datagrid-column-sortable-p (make-instance 'datagrid
								 :data (list *joe*)
								 :allow-sorting nil
								 :data-class 'employee)
						  '(name) "Joe"))))
  nil)

(deftest datagrid-column-sortable-p-2
    (with-request :get nil
      (not (null
	    (weblocks::datagrid-column-sortable-p (make-instance 'datagrid
								 :data (list *joe*)
								 :allow-sorting t
								 :data-class 'employee)
						  '(name) "Joe"))))
  t)

(deftest datagrid-column-sortable-p-3
    (with-request :get nil
      (not (null
	    (weblocks::datagrid-column-sortable-p (make-instance 'datagrid
								 :data (list *joe*)
								 :allow-sorting '(manager)
								 :data-class 'employee)
						  '(name) "Joe"))))
  nil)

(deftest datagrid-column-sortable-p-4
    (with-request :get nil
      (not (null
	    (weblocks::datagrid-column-sortable-p (make-instance 'datagrid
								 :data (list *joe*)
								 :allow-sorting '(manager)
								 :data-class 'employee)
						  '(manager) "Jim"))))
  t)

(deftest datagrid-column-sortable-p-5
    (with-request :get nil
      (not (null
	    (weblocks::datagrid-column-sortable-p (make-instance 'datagrid
								 :data (list *joe*)
								 :allow-sorting '(education graduation-year)
								 :data-class 'employee)
						  '(education university) "Jim"))))
  nil)

(deftest datagrid-column-sortable-p-6
    (with-request :get nil
      (not (null
	    (weblocks::datagrid-column-sortable-p (make-instance 'datagrid
								 :data (list *joe*)
								 :allow-sorting '(education graduation-year)
								 :data-class 'employee)
						  '(education graduation-year) "Jim"))))
  t)

(deftest datagrid-column-sortable-p-7
    (with-request :get nil
      (not (null
	    (weblocks::datagrid-column-sortable-p (make-instance 'datagrid
								 :data (list *joe*)
								 :forbid-sorting-on '(name)
								 :data-class 'employee)
						  '(name) "Joe"))))
  nil)

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
    (with-request :get nil
      (let ((grid (make-instance 'datagrid
				 :data (list *joe* *bob*)
				 :sort '((name) . :ascending)
				 :data-class 'employee))
	    res)
	(setf res (weblocks::datagrid-sort-data grid
						(slot-value grid 'weblocks::data)))
	(values
	 (first-name (car res))
	 (first-name (cadr res)))))
  "Bob" "Joe")

(deftest datagrid-sort-data-2
    (with-request :get nil
      (let ((grid (make-instance 'datagrid
				 :data (list *joe* *bob*)
				 :sort '((name) . :descending)
				 :data-class 'employee))
	    res)
	(setf res (weblocks::datagrid-sort-data grid
						(slot-value grid 'weblocks::data)))
	(values
	 (first-name (car res))
	 (first-name (cadr res)))))
  "Joe" "Bob")

(deftest datagrid-sort-data-3
    (with-request :get nil
      (let ((grid (make-instance 'datagrid
				 :data (list *joe* *bob*)
				 :sort '((age) . :descending)
				 :data-class 'employee))
	    res)
	(setf res (weblocks::datagrid-sort-data grid
						(slot-value grid 'weblocks::data)))
	(values
	 (first-name (car res))
	 (first-name (cadr res)))))
  "Bob" "Joe")

;;; test negate-sort-direction
(deftest negate-sort-direction-1
    (weblocks::negate-sort-direction :ascending)
  :descending)

(deftest negate-sort-direction-2
    (weblocks::negate-sort-direction :descending)
  :ascending)

;;; test datagrid's hook into render-table-header-cell
(deftest-html render-table-header-cell-around
    (with-request :get nil
		  (render-table-header-cell *joe* 'name 'string "Joe"
					    :grid-obj (make-instance 'datagrid :allow-sorting nil
									       :data-class 'employee)))
  (:th :class "name" "Name"))

;;; test render-datagrid-header-cell
(deftest-html render-datagrid-header-cell-1
    (with-request :get nil
      (render-datagrid-header-cell *joe* 'name 'string "Joe"
				   :slot-path '(name)
				   :grid-obj (make-instance 'datagrid :sort '((name) . :ascending)
							    :data-class 'employee)))
  (:th :class "name sort-ascending"
       (:span #.(link-action-template "abc123" "Name"))))

(deftest-html render-datagrid-header-cell-2
    (with-request :get nil
      (render-datagrid-header-cell *joe* 'name 'string "Joe"
				   :slot-path '(name)
				   :grid-obj (make-instance 'datagrid :sort '((manager) . :ascending)
							    :data-class 'employee)))
  (:th :class "name"
       (:span #.(link-action-template "abc123" "Name"))))



