
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
  (name . :asc))

(deftest datagrid-update-sort-column-2
    (with-request :get nil
      (persist-objects *default-store* (list *joe* *bob*))
      (let ((grid (make-instance 'datagrid
				 :data-class 'employee
				 :view (defview-anon (:type grid :inherit-from '(:scaffold employee)
							    :allow-sorting-p nil)
					   (manager :allow-sorting-p t)))))
	(weblocks::datagrid-update-sort-column grid)
	(datagrid-sort grid)))
  (manager . :asc))

(deftest datagrid-update-sort-column-3
    (with-request :get nil
      (persist-objects *default-store* (list *joe* *bob*))
      (let ((grid (make-instance 'datagrid
				 :data-class 'employee
				 :view (defview-anon (:type grid :inherit-from '(:scaffold employee)
							    :allow-sorting-p nil)
					   (education :type mixin
						      :view (defview-anon
								(:type grid
								 :inherit-from
								   '(:scaffold education-history))
							    (university :allow-sorting-p t)))))))
	(weblocks::datagrid-update-sort-column grid)
	(datagrid-sort grid)))
  ((education university) . :asc))

(deftest datagrid-update-sort-column-4
    (with-request :get nil
      (persist-objects *default-store* (list *joe* *bob*))
      (let ((grid (make-instance 'datagrid
				 :data-class 'employee
				 :view (defview-anon (:type grid :inherit-from '(:scaffold employee)
							    :allow-sorting-p nil)
					   (address :allow-sorting-p t)))))
	(weblocks::datagrid-update-sort-column grid)
	(datagrid-sort grid)))
  (address . :asc))

;;; test datagrid-field-sortable-p
(deftest datagrid-field-sortable-p-1
    (with-request :get nil
      (let* ((view (defview-anon (:type grid :inherit-from '(:scaffold employee)
				  :allow-sorting-p nil)))
	     (field-info (car (get-object-view-fields *joe* view)))
	     (field (field-info-field field-info))
	     (grid (make-instance 'datagrid :data-class 'employee
				  :view view
				  :sort '(name . :asc))))
	(not (null
	    (weblocks::datagrid-field-sortable-p grid field)))))
  nil)

(deftest datagrid-field-sortable-p-2
    (with-request :get nil
      (let* ((view (defview-anon (:type grid :inherit-from '(:scaffold employee))))
	     (field-info (car (get-object-view-fields *joe* view)))
	     (field (field-info-field field-info))
	     (grid (make-instance 'datagrid :data-class 'employee
				  :view view
				  :sort '(name . :asc))))
	(not (null
	    (weblocks::datagrid-field-sortable-p grid field)))))
  t)

(deftest datagrid-field-sortable-p-3
    (with-request :get nil
      (let* ((view (defview-anon (:type grid :inherit-from '(:scaffold employee)
				  :allow-sorting-p nil)
		       (name :allow-sorting-p t)))
	     (field-info (car (get-object-view-fields *joe* view)))
	     (field (field-info-field field-info))
	     (grid (make-instance 'datagrid :data-class 'employee
				  :view view
				  :sort '(name . :asc))))
	(not (null
	      (weblocks::datagrid-field-sortable-p grid field)))))
  t)

;;; test datagrid-sort-slot
(deftest datagrid-sort-slot-1
    (with-request :get nil
      (let ((grid (make-instance 'datagrid :data-class 'employee
				 :sort '((address name) . :asc))))
	(datagrid-sort-slot grid)))
  name)

(deftest datagrid-sort-slot-2
    (with-request :get nil
      (let ((grid (make-instance 'datagrid :data-class 'employee
				 :sort '((name) . :asc))))
	(datagrid-sort-slot grid)))
  name)

(deftest datagrid-sort-slot-3
    (with-request :get nil
      (let ((grid (make-instance 'datagrid :data-class 'employee
				 :sort '(name . :asc))))
	(datagrid-sort-slot grid)))
  name)

;;; test negate-sort-direction
(deftest negate-sort-direction-1
    (weblocks::negate-sort-direction :asc)
  :desc)

(deftest negate-sort-direction-2
    (weblocks::negate-sort-direction :desc)
  :asc)

;;; test datagrid-render-view-field-header-sort
(deftest-html datagrid-render-view-field-header-sort-1
    (with-request :get nil
      (let* ((view (defview-anon (:type grid :inherit-from '(:scaffold employee))))
	     (field-info (car (get-object-view-fields *joe* view)))
	     (field (field-info-field field-info))
	     (grid (make-instance 'datagrid :data-class 'employee
				  :view view
				  :sort '(name . :asc)))
	     (presentation (view-field-presentation field))
	     (value (first-name *joe*)))
	(render-view-field-header field view grid presentation value *joe*
				  :field-info field-info)))
  (:th :class "name sort-asc"
       (:span #.(link-action-template "abc123" "Name"))))

(deftest-html datagrid-render-view-field-header-sort-2
    (with-request :get nil
      (let* ((view (defview-anon (:type grid :inherit-from '(:scaffold employee))))
	     (field-info (car (get-object-view-fields *joe* view)))
	     (field (field-info-field field-info))
	     (grid (make-instance 'datagrid :data-class 'employee
				  :view view
				  :sort '(manager . :asc)))
	     (presentation (view-field-presentation field))
	     (value (first-name *joe*)))
	(render-view-field-header field view grid presentation value *joe*
				  :field-info field-info)))
  (:th :class "name"
       (:span #.(link-action-template "abc123" "Name"))))

