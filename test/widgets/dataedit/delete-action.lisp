
(in-package :weblocks-test)

;;; testing dataedit-delete-items
(deftest dataedit-delete-items-flow-1
    (with-request :get nil
      (persist-objects *default-store* (list *joe* *bob*))
      (make-request-ajax)
      (let ((grid (make-instance 'gridedit
				 :data-class 'employee
				 :allow-pagination-p nil)))
	(dataedit-delete-items-flow grid (cons :none (list (object-id *joe*))))
	(do-request `(("yes" . "Yes")
		      (,weblocks::*action-string* . "abc123")))
	(mapcar #'first-name (dataseq-data grid))))
  ("Bob"))

(deftest dataedit-delete-items-flow-2
    (with-request :get nil
      (persist-objects *default-store* (list *joe* *bob*))
      (make-request-ajax)
      (let ((grid (make-instance 'gridedit
				 :data-class 'employee
				 :allow-pagination-p nil)))
	(dataedit-delete-items-flow grid (cons :none (list (object-id *joe*) (object-id *bob*))))
	(do-request `(("yes" . "Yes")
		      (,weblocks::*action-string* . "abc123")))
	(mapcar #'first-name (dataseq-data grid))))
  nil)

(deftest dataedit-delete-items-flow-3
    (with-request :get nil
      (persist-objects *default-store* (list *joe* *bob*))
      (make-request-ajax)
      (let ((grid (make-instance 'gridedit
				 :data-class 'employee
				 :allow-pagination-p nil)))
	(dataedit-delete-items-flow grid (cons :none (list (object-id *joe*))))
	(do-request `(("yes" . "Yes")
		      (,weblocks::*action-string* . "abc123")))
	(not (null (widget-dirty-p grid)))))
  t)

