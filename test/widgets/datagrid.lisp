
(in-package :weblocks-test)

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

;;; test datagrid-update-sort-column
(deftest datagrid-update-sort-column-1
    (let ((grid (make-instance 'datagrid :data (list *joe* *joe*)))
	  sort1 sort2)
      (setf sort1 (datagrid-sort grid))
      (weblocks::datagrid-update-sort-column grid (car (datagrid-data grid)))
      (setf sort2 (datagrid-sort grid))
      (values sort1 sort2))
  nil
  ((name) . :ascending))

(deftest datagrid-update-sort-column-2
    (let ((grid (make-instance 'datagrid
			       :data (list *joe* *joe*)
			       :allow-sorting '(manager))))
      (weblocks::datagrid-update-sort-column grid (car (datagrid-data grid)))
      (datagrid-sort grid))
  ((manager) . :ascending))

(deftest datagrid-update-sort-column-3
    (let ((grid (make-instance 'datagrid
			       :data (list *joe* *joe*)
			       :allow-sorting '(university))))
      (weblocks::datagrid-update-sort-column grid (car (datagrid-data grid)) :slots '(education university))
      (datagrid-sort grid))
  ((education university) . :ascending))

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
			       :sort '((name) . :ascending))))
      (weblocks::datagrid-sort-data grid)
      (values
       (first-name (car (datagrid-data grid)))
       (first-name (cadr (datagrid-data grid)))))
  "Bob" "Joe")

(deftest datagrid-sort-data-2
    (let ((grid (make-instance 'datagrid
			       :data (list *joe* *bob*)
			       :sort '((name) . :descending))))
      (weblocks::datagrid-sort-data grid)
      (values
       (first-name (car (datagrid-data grid)))
       (first-name (cadr (datagrid-data grid)))))
  "Joe" "Bob")

(deftest datagrid-sort-data-3
    (let ((grid (make-instance 'datagrid
			       :data (list *joe* *bob*)
			       :sort '((age) . :descending))))
      (weblocks::datagrid-sort-data grid)
      (values
       (first-name (car (datagrid-data grid)))
       (first-name (cadr (datagrid-data grid)))))
  "Bob" "Joe")

(deftest datagrid-sort-data-4
    (let ((grid (make-instance 'datagrid
			       :data (lambda (sort) (list *joe* *bob*))
			       :sort '((age) . :descending))))
      (weblocks::datagrid-sort-data grid)
      (values
       (first-name (car (datagrid-data grid)))
       (first-name (cadr (datagrid-data grid)))))
  "Joe" "Bob")

;;; test datagrid-data
(deftest datagrid-data-1
    (datagrid-data (make-instance 'datagrid
				  :data (list 1 2)))
  (1 2))

(deftest datagrid-data-2
    (datagrid-data (make-instance 'datagrid
				  :data (lambda (sort)
					  (list 1 2))))
  (1 2))

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

