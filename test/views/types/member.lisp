
(in-package :weblocks-test)

;;; Test member typespec->view-field-presentation
(deftest member-typespec->view-field-presentation-1
    (object-class-name
     (cadr
      (multiple-value-list
       (typespec->view-field-presentation (make-instance 'form-scaffold)
					  'member nil))))
  radio-presentation)

;;; Test member typespec->form-view-field-parser
(deftest member-typespec->form-view-field-parser-1
    (object-class-name
     (cadr
      (multiple-value-list
       (typespec->form-view-field-parser (make-instance 'form-scaffold)
					 'member nil))))
  keyword-parser)
