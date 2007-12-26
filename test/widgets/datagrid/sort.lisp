
(in-package :weblocks-test)

;;; test datagrid-update-sort-column/aux-datagrid-update-sort-column
(deftest datagrid-update-sort-column-1
    (with-request :get nil
      (persist-objects *default-store* (list *joe* *bob*))
      (let ((grid (make-instance 'datagrid :data-class 'employee))
	    sort1 sort2)
	(setf sort1 (datagrid-sort grid))
	(weblocks::datagrid-update-sort-column grid)
	(setf sort2 (datagrid-sort grid))
	(values sort1 sort2)))
  nil
  ((name) . :asc))

(deftest datagrid-update-sort-column-2
    (with-request :get nil
      (persist-objects *default-store* (list *joe* *bob*))
      (let ((grid (make-instance 'datagrid
				 :allow-sorting '(manager)
				 :data-class 'employee)))
	(weblocks::datagrid-update-sort-column grid)
	(datagrid-sort grid)))
  ((manager) . :asc))

(deftest datagrid-update-sort-column-3
    (with-request :get nil
      (persist-objects *default-store* (list *joe* *bob*))
      (let ((grid (make-instance 'datagrid
				 :allow-sorting '(university)
				 :data-class 'employee)))
	(weblocks::datagrid-update-sort-column grid :slots '(education university))
	(datagrid-sort grid)))
  ((education university) . :asc))

(deftest datagrid-update-sort-column-4
    (with-request :get nil
      (persist-objects *default-store* (list *joe* *bob*))
      (let ((grid (make-instance 'datagrid
				 :allow-sorting '(address-ref)
				 :data-class 'employee)))
	(weblocks::datagrid-update-sort-column grid :slots '(address-ref))
	(datagrid-sort grid)))
  ((address-ref) . :asc))

;;; test datagrid-column-sortable-p
(deftest datagrid-column-sortable-p-1
    (with-request :get nil
      (persist-object *default-store* *joe*)
      (not (null
	    (weblocks::datagrid-column-sortable-p (make-instance 'datagrid
								 :allow-sorting nil
								 :data-class 'employee)
						  '(name) "Joe"))))
  nil)

(deftest datagrid-column-sortable-p-2
    (with-request :get nil
      (persist-object *default-store* *joe*)
      (not (null
	    (weblocks::datagrid-column-sortable-p (make-instance 'datagrid
								 :allow-sorting t
								 :data-class 'employee)
						  '(name) "Joe"))))
  t)

(deftest datagrid-column-sortable-p-3
    (with-request :get nil
      (persist-object *default-store* *bob*)
      (not (null
	    (weblocks::datagrid-column-sortable-p (make-instance 'datagrid
								 :allow-sorting '(manager)
								 :data-class 'employee)
						  '(name) "Joe"))))
  nil)

(deftest datagrid-column-sortable-p-4
    (with-request :get nil
      (persist-object *default-store* *bob*)
      (not (null
	    (weblocks::datagrid-column-sortable-p (make-instance 'datagrid
								 :allow-sorting '(manager)
								 :data-class 'employee)
						  '(manager) "Jim"))))
  t)

(deftest datagrid-column-sortable-p-5
    (with-request :get nil
      (persist-object *default-store* *bob*)
      (not (null
	    (weblocks::datagrid-column-sortable-p (make-instance 'datagrid
								 :allow-sorting '(education graduation-year)
								 :data-class 'employee)
						  '(education university) "Jim"))))
  nil)

(deftest datagrid-column-sortable-p-6
    (with-request :get nil
      (persist-object *default-store* *bob*)
      (not (null
	    (weblocks::datagrid-column-sortable-p (make-instance 'datagrid
								 :allow-sorting '(education graduation-year)
								 :data-class 'employee)
						  '(education graduation-year) "Jim"))))
  t)

(deftest datagrid-column-sortable-p-7
    (with-request :get nil
      (persist-object *default-store* *bob*)
      (not (null
	    (weblocks::datagrid-column-sortable-p (make-instance 'datagrid
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

;;; test negate-sort-direction
(deftest negate-sort-direction-1
    (weblocks::negate-sort-direction :asc)
  :desc)

(deftest negate-sort-direction-2
    (weblocks::negate-sort-direction :desc)
  :asc)

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



