
(in-package :weblocks-test)

(deftestsuite widgets/dataseq/dataseq-suite (weblocks-suite)
  ())

;;; test dataseq initialize-instance
(deftest dataseq-initialize-instance-1
    (with-request :get nil
      (let ((ds (make-instance 'dataseq
			       :data-class 'employee)))
	(not (null (dataseq-pagination-widget ds)))))
  t)

(deftest dataseq-initialize-instance-2
    (with-request :get nil
      (let* ((pw (make-instance 'pagination))
	     (ds (make-instance 'dataseq
			       :data-class 'employee
			       :pagination-widget pw)))
	(eq (dataseq-pagination-widget ds) pw)))
  t)

;;; test dataseq-data
(addtest dataseq-data-1
  (persist-objects *default-store* (list *joe* *bob*))
  (ensure-same (mapcar #'first-name
		       (dataseq-data (make-instance 'dataseq
						    :data-class 'employee)))
	       '("Joe" "Bob")
	       :test set-equal-equal))

(deftest dataseq-data-2
    (with-request :get nil
      (dataseq-data (make-instance 'dataseq
				   :on-query (lambda (obj sort pagination &key countp)
					       (if countp
						   2 (list 1 2)))
				   :data-class 'employee)))
  (1 2))

(deftest dataseq-data-3
    (with-request :get nil
      (persist-objects *default-store* (list *joe* *bob* *employee1* *employee2*
					     *employee3* *employee4*))
      (mapcar #'first-name
	      (dataseq-data (make-instance 'dataseq
					   :data-class 'employee
					   :sort (cons 'name :asc)
					   :pagination-widget
					   (make-instance 'pagination
							  :total-items 6
							  :items-per-page 4
							  :current-page 1)))))
  ("Andy" "Bob" "Guy" "Harry"))

(deftest dataseq-data-4
    (with-request :get nil
      (dataseq-data (make-instance 'dataseq
				   :on-query (lambda (obj sort pagination &key countp)
					       pagination)
				   :data-class 'integer
				   :pagination-widget
				   (make-instance 'pagination
						  :total-items 6
						  :items-per-page 4
						  :current-page 1))))
  (0 . 4))

(defun dataseq-data-foo (obj sort pagination &key countp)
  (declare (ignore obj sort pagination))
  (if countp
      2 (list 1 2)))

(deftest dataseq-data-5
    (with-request :get nil
      (dataseq-data (make-instance 'dataseq
				   :on-query 'dataseq-data-foo
				   :data-class 'employee)))
  (1 2))

(deftest dataseq-data-6
    (with-request :get nil
      (dataseq-data (make-instance 'dataseq
				   :data-class 'employee)))
  nil)

;;; test dataseq-data-count
(deftest dataseq-data-count-1
    (with-request :get nil
      (persist-objects *default-store* (list *joe* *bob*))
      (dataseq-data-count (make-instance 'dataseq
					 :data-class 'employee)))
  2)

(deftest dataseq-data-count-2
    (with-request :get nil
      (dataseq-data-count (make-instance 'dataseq
					 :on-query (lambda (obj sort pagination &key countp)
						     (list sort pagination countp))
					 :data-class 'employee)))
  (nil nil t))

(defun dataseq-data-count-foo (obj sort pagination &key countp)
  (declare (ignore obj))
  (list sort pagination countp))

(deftest dataseq-data-count-3
    (with-request :get nil
      (dataseq-data-count (make-instance 'dataseq
					 :on-query 'dataseq-data-count-foo
					 :data-class 'employee)))
  (nil nil t))

(deftest dataseq-data-count-4
    (with-request :get nil
      (dataseq-data-count (make-instance 'dataseq
					 :data-class 'employee)))
  0)

;;; test dataseq-persistent-query-function

(deftest dataseq-persistent-query-function-1
    (with-request :get nil
      (persist-objects *default-store* (list *employee2* *employee1*
					     *employee3* *employee4*))
      (mapcar (curry-after #'slot-value 'age)
	      (dataseq-data (make-instance 'dataseq
					   :sort '(age . :desc)
					   :on-query (dataseq-persistent-query-function nil)
					   :data-class 'employee))))
  (54 53 52 51))

(deftest dataseq-persistent-query-function-2
    (with-request :get nil
      (persist-objects *default-store* (list *employee1*
					     *employee3* *employee4*))
      (mapcar (curry-after #'slot-value 'age)
	      (dataseq-data (make-instance 'dataseq
					   :sort '(age . :asc)
					   :on-query (dataseq-persistent-query-function nil)
					   :data-class 'employee))))
  (51 53 54))

(deftest dataseq-persistent-query-function-3
    (with-request :get nil
      (persist-objects *default-store* (list *employee1*
					     *employee3* *employee4*))
      (dataseq-data-count (make-instance 'dataseq
					 :on-query (dataseq-persistent-query-function nil)
					 :data-class 'employee)))
  3)

;;; test dataseq-update-sort-column
(deftest dataseq-update-sort-column-1
    (with-request :get nil
      (persist-objects *default-store* (list *joe* *bob*))
      (let ((obj (make-instance 'dataseq :data-class 'employee
				:view '(sequence employee)))
	    sort1 sort2)
	(setf sort1 (dataseq-sort obj))
	(weblocks::dataseq-update-sort-column obj)
	(setf sort2 (dataseq-sort obj))
	(values sort1 sort2)))
  nil
  (name . :asc))

(deftest dataseq-update-sort-column-2
    (with-request :get nil
      (persist-objects *default-store* (list *joe* *bob*))
      (let ((obj (make-instance 'dataseq
				:data-class 'employee
				:view (defview () (:type sequence :inherit-from '(:scaffold employee))
					(name :allow-sorting-p nil)
					(manager :allow-sorting-p t)))))
	(weblocks::dataseq-update-sort-column obj)
	(dataseq-sort obj)))
  (manager . :asc))

(deftest dataseq-update-sort-column-3
    (with-request :get nil
      (persist-objects *default-store* (list *joe* *bob*))
      (let ((obj (make-instance 'dataseq
				:data-class 'employee
				:view (defview () (:type sequence :inherit-from '(:scaffold employee))
					(name :allow-sorting-p nil)
					(manager :allow-sorting-p nil)
					(education :type mixin
						   :view (defview ()
							     (:type sequence
								    :inherit-from
								    '(:scaffold education-history))
							   (graduation-year :allow-sorting-p nil)))))))
	(weblocks::dataseq-update-sort-column obj)
	(dataseq-sort obj)))
  ((education university) . :asc))

(deftest dataseq-update-sort-column-4
    (with-request :get nil
      (persist-objects *default-store* (list *joe* *bob*))
      (let ((obj (make-instance 'dataseq
				:data-class 'employee
				:view (defview () (:type sequence :inherit-from '(:scaffold employee))
					(name :allow-sorting-p nil)
					(manager :allow-sorting-p nil)
					(address :allow-sorting-p t)))))
	(weblocks::dataseq-update-sort-column obj)
	(dataseq-sort obj)))
  (address . :asc))

;;; test dataseq-field-sortable-p
(deftest dataseq-field-sortable-p-1
    (with-request :get nil
      (let* ((view (defview () (:type sequence :inherit-from '(:scaffold employee))
		     (name :allow-sorting-p nil)))
	     (field-info (car (get-object-view-fields *joe* view)))
	     (field (field-info-field field-info))
	     (obj (make-instance 'dataseq :data-class 'employee
				 :view view
				 :sort '(name . :asc))))
	(not (null
	      (weblocks::dataseq-field-sortable-p obj field)))))
  nil)

(deftest dataseq-field-sortable-p-2
    (with-request :get nil
      (let* ((view (defview () (:type sequence :inherit-from '(:scaffold employee))))
	     (field-info (car (get-object-view-fields *joe* view)))
	     (field (field-info-field field-info))
	     (obj (make-instance 'dataseq :data-class 'employee
				 :view view
				 :sort '(name . :asc))))
	(not (null
	      (weblocks::dataseq-field-sortable-p obj field)))))
  t)

;;; test get-field-info-sort-path
(deftest get-field-info-sort-path-1
    (let ((view (defview () (:type sequence :inherit-from '(:scaffold employee))
		  (address :type mixin)
		  (name :hidep t)
		  (manager :hidep t)
		  street)))
      (get-field-info-sort-path
       (car (get-object-view-fields nil view))))
  street)

;;; test dataseq-sort-path
(deftest dataseq-sort-path-1
    (with-request :get nil
      (let ((obj (make-instance 'dataseq :data-class 'employee
				 :sort '((address name) . :asc))))
	(dataseq-sort-path obj)))
  (address name))

;;; test dataseq-sort-direction
(deftest dataseq-sort-direction-1
    (with-request :get nil
      (let ((obj (make-instance 'dataseq :data-class 'employee
				 :sort '((address name) . :asc))))
	(dataseq-sort-direction obj)))
  :asc)

;;; test dataseq-sort-slot
(deftest dataseq-sort-slot-1
    (with-request :get nil
      (let ((obj (make-instance 'dataseq :data-class 'employee
				 :sort '((address name) . :asc))))
	(dataseq-sort-slot obj)))
  name)

(deftest dataseq-sort-slot-2
    (with-request :get nil
      (let ((obj (make-instance 'dataseq :data-class 'employee
				 :sort '((name) . :asc))))
	(dataseq-sort-slot obj)))
  name)

(deftest dataseq-sort-slot-3
    (with-request :get nil
      (let ((obj (make-instance 'dataseq :data-class 'employee
				 :sort '(name . :asc))))
	(dataseq-sort-slot obj)))
  name)

;;; test negate-sort-direction
(deftest negate-sort-direction-1
    (negate-sort-direction :asc)
  :desc)

(deftest negate-sort-direction-2
    (negate-sort-direction :desc)
  :asc)

;;; test dataseq-item-selected-p
(deftest dataseq-item-selected-p-1
    (with-request :get nil
      (let ((grid (make-instance 'dataseq
				 :data-class 'employee
				 :selection '(:none . (1 2 3)))))
	(not (null (dataseq-item-selected-p grid 1)))))
  t)

(deftest dataseq-item-selected-p-2
    (with-request :get nil
      (let ((grid (make-instance 'dataseq
				 :data-class 'employee
				 :selection '(:none . (1 2 3)))))
	(not (null (dataseq-item-selected-p grid 4)))))
  nil)

;;; test dataseq-select-item
(deftest dataseq-select-item-1
    (with-request :get nil
      (let ((grid (make-instance 'dataseq
				 :data-class 'employee
				 :selection '(:none . ()))))
	(dataseq-select-item grid 2)
	(not (null (dataseq-item-selected-p grid 2)))))
  t)

;;; test dataseq-clear-selection
(deftest dataseq-clear-selection-1
    (with-request :get nil
      (let ((grid (make-instance 'dataseq
				 :data-class 'employee
				 :selection '(:none . (1 2 3)))))
	(dataseq-clear-selection grid)
	(not (null (dataseq-item-selected-p grid 2)))))
  nil)

;;; test dataseq-selection-empty-p
(deftest dataseq-selection-empty-p-1
    (with-request :get nil
      (let ((grid (make-instance 'dataseq
				 :data-class 'employee
				 :selection '(:none . (1 2 3)))))
	(dataseq-selection-empty-p grid)))
  nil)

(deftest dataseq-selection-empty-p-2
    (with-request :get nil
      (let ((grid (make-instance 'dataseq
				 :data-class 'employee)))
	(dataseq-selection-empty-p grid)))
  t)

;;; test datagrid-render-operations
(deftest-html dataseq-render-operations-1
    (with-request :get nil
      (let ((obj (make-instance 'dataseq
				:data-class 'employee)))
	(dataseq-render-operations obj)))
  (:div :class "operations" ""))

(deftest-html dataseq-render-operations-2
    (with-request :get nil
      (let ((obj (make-instance 'dataseq
				:data-class 'employee
				:allow-select-p t
				:item-ops (list (cons 'hello #'identity)
						(cons 'world #'identity)))))
	(dataseq-render-operations obj)))
  (:div :class "operations"
	(:input :name "hello" :type "submit" :class "submit" :value "Hello"
		:onclick "disableIrrelevantButtons(this);")
	(:input :name "world" :type "submit" :class "submit" :value "World"
		:onclick "disableIrrelevantButtons(this);")))

;;; test dataseq-wrap-body-in-form-p
(deftest dataseq-wrap-body-in-form-p-1
    (with-request :get nil
      (let ((obj (make-instance 'dataseq :data-class 'employee)))
	(dataseq-wrap-body-in-form-p obj)))
  t)

;;; test render-total-items-message
(deftest-html render-total-items-message-1
    (with-request :get nil
      (persist-objects *default-store* (list *joe* *bob*))
      (render-total-items-message (make-instance 'dataseq
						 :data-class 'employee)))
  (:span :class "total-items" "(Total of 2 Employees)"))

;;; test datagrid-render-pagination-widget
(deftest-html dataseq-render-pagination-widget-1
    (with-request :get nil
      (dataseq-render-pagination-widget (make-instance 'dataseq :data-class 'employee)))
  (:div :class "widget pagination"
	:id "id-123"))

(deftest-html dataseq-render-pagination-widget-2
    (with-request :get nil
      (dataseq-render-pagination-widget (make-instance 'dataseq :data-class 'employee
							:allow-pagination-p nil)))
  (:div :class "widget pagination" :id "id-123" ""))
