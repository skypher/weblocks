
(in-package :weblocks-test)

;;; test datagrid-render-view-field-header-sort
(deftest-html datagrid-render-view-field-header-sort-1
    (with-request :get nil
      (let* ((view (defview () (:type table :inherit-from '(:scaffold employee))))
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
      (let* ((view (defview () (:type table :inherit-from '(:scaffold employee))))
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

