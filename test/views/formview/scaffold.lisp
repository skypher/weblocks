
(in-package :weblocks-test)

;;; Test form scaffold generate-scaffold-view
(deftest form-scaffold-generate-scaffold-view-1
    (object-class-name
     (generate-scaffold-view (make-instance 'form-scaffold)
			     (find-class 'employee)))
  form-view)

;;; Test form scaffold generate-scaffold-view
(deftest form-scaffold-generate-scaffold-view-field-1
    (object-class-name
     (generate-scaffold-view-field (make-instance 'form-scaffold)
				   (find-class 'employee)
				   (find-slot-dsd 'employee 'name)))
  form-view-field)

;;; Test typespec->form-view-field-parser
(deftest typespec->form-view-field-parser-1
    (typespec->form-view-field-parser (make-instance 'form-scaffold)
				      'foobar nil)
  nil)

