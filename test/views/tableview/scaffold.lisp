
(in-package :weblocks-test)

;;; Test table scaffold generate-scaffold-view
(deftest table-scaffold-generate-scaffold-view-1
    (object-class-name
     (generate-scaffold-view (make-instance 'table-scaffold)
			     (find-class 'employee)))
  table-view)

;;; Test table scaffold generate-scaffold-view
(deftest table-scaffold-generate-scaffold-view-field-1
    (object-class-name
     (generate-scaffold-view-field (make-instance 'table-scaffold)
				   (find-class 'employee)
				   (find-slot-dsd 'employee 'name)))
  table-view-field)
