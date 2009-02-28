
(in-package :weblocks-test)

(deftestsuite widgets/dataedit/dataedit-suite (weblocks-suite)
  ())

;;; Test initialize instance for dataedit
(deftest initialize-instance-dataedit-1
    (with-request :get nil
      (let ((obj (make-instance 'gridedit :data-class 'employee)))
	(not (null (dataseq-on-drilldown obj)))))
  t)

(deftest initialize-instance-dataedit-2
    (with-request :get nil
      (let ((obj (make-instance 'listedit :data-class 'employee)))
	(not (null (dataseq-on-drilldown obj)))))
  t)

(addtest initialize-instance-dataedit-with-drilldown-kwarg
  (ensure-same (dataseq-on-drilldown
		(make-instance 'listedit
		  :data-class 'employee :on-drilldown '(values . values)))
	       '(values . values)))

;;; Test dataedit-drilldown-action
(deftest dataedit-drilldown-action-1
    (with-request :get nil
      (let ((obj (make-instance 'listedit :data-class 'employee)))
	(dataedit-drilldown-action obj *joe*)
	(values (not (null (dataedit-item-widget obj)))
		(dataedit-ui-state obj))))
  t :drilldown)

;;; Test dataseq-render-operations-dataedit
(deftest-html dataseq-render-operations-dataedit-1
    (with-request :get nil
      (let ((obj (make-instance 'listedit :data-class 'employee)))
	(setf (dataedit-item-widget obj) t)
	(dataseq-render-operations obj)))
  ())

;;; Test dataseq-render-pagination-widget-dataedit
(deftest-html dataseq-render-pagination-widget-dataedit-1
    (with-request :get nil
      (let ((obj (make-instance 'listedit :data-class 'employee)))
	(setf (dataedit-item-widget obj) t)
	(dataseq-pagination-widget obj)))
  ())

;;; Test dataedit-reset-state
(deftest dataedit-reset-state-1
    (with-request :get nil
      (let ((obj (make-instance 'listedit :data-class 'employee)))
	(setf (dataedit-ui-state obj) t
	      (dataedit-item-widget obj) t
	      (dataseq-drilled-down-item obj) t)
	(dataedit-reset-state obj)
	(values (dataedit-ui-state obj)
		(dataedit-item-widget obj)
		(dataseq-drilled-down-item obj))))
  nil nil nil)

;;; Test dataedit-add-items-flow
(deftest dataedit-add-items-flow-1
    (with-request :get nil
      (let ((obj (make-instance 'listedit :data-class 'employee)))
	(dataedit-add-items-flow obj nil)
	(values (not (null (dataedit-item-widget obj)))
		(dataedit-ui-state obj))))
  t :add)

;;; Test dataedit-update-operations
(deftest dataedit-update-operations-1
    (with-request :get nil
      (persist-objects *default-store* (list *joe* *bob*))
      (let ((obj (make-instance 'listedit :data-class 'employee)))
	(dataedit-update-operations obj
				    :delete-fn 1
				    :add-fn 2)
	(values (dataseq-item-ops obj)
		(dataseq-common-ops obj))))
  ((weblocks::delete . 1))
  ((weblocks::add . 2)))

