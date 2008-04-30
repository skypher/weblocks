
(in-package :weblocks-test)

;;; testing dataedit-delete-items
(deftest dataedit-delete-items-flow-1
    (with-request :get nil
      (persist-objects *default-store* (list *joe* *bob*))
      (make-request-ajax)
      (let ((grid (make-instance 'gridedit
				 :data-class 'employee
				 :allow-pagination-p nil)))
	(dataedit-delete-items-flow grid (cons :none (list (format nil "~A" (object-id *joe*)))))
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
	(dataedit-delete-items-flow grid (cons :none (list (format nil "~A" (object-id *joe*))
						      (format nil "~A" (object-id *bob*)))))
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
	(setf (widget-rendered-p grid) t)
	(dataedit-delete-items-flow grid (cons :none (list (format nil "~A" (object-id *joe*)))))
	(do-request `(("yes" . "Yes")
		      (,weblocks::*action-string* . "abc123")))
	(not (null (widget-dirty-p grid)))))
  t)

