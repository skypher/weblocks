
(in-package :weblocks-test)

;;; Test data scaffold generate-scaffold-view
(deftest data-scaffold-generate-scaffold-view-1
    (object-class-name
     (generate-scaffold-view (make-instance 'data-scaffold)
			     (find-class 'employee)))
  data-view)

;;; Test data scaffold generate-scaffold-view
(deftest data-scaffold-generate-scaffold-view-field-1
    (object-class-name
     (generate-scaffold-view-field (make-instance 'data-scaffold)
				   (find-class 'employee)
				   (find-slot-dsd 'employee 'name)))
  data-view-field)
