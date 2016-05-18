
(in-package :weblocks-test)

;;; Test scaffold-class-name
(deftest scaffold-class-name-1
    (scaffold-class-name 'form)
  form-scaffold)

;;; Test scaffold-view-type
(deftest scaffold-view-type-1
    (multiple-value-bind (res temp)
        (scaffold-view-type (make-instance 'form-scaffold))
      res)
  form-view)

;;; Test scaffold-view-field-type
(deftest scaffold-view-field-type-1
    (multiple-value-bind (res temp)
        (scaffold-view-field-type (make-instance 'form-scaffold))
      res)
  form-view-field)

;;; Test class-visible-slots
(deftest class-visible-slots-1
    (mapcar #'slot-definition-name (weblocks-stores:class-visible-slots 'employee))
  (id name age address education manager veteran))

(deftest class-visible-slots-2
    (mapcar #'slot-definition-name (weblocks-stores:class-visible-slots 'employee :writablep t))
  (name manager))

;;; Test inspect-typespec
(deftest inspect-typespec-1
    (inspect-typespec 'integer)
  integer nil)

(deftest inspect-typespec-2
    (inspect-typespec '(or null integer))
  integer nil)

(deftest inspect-typespec-3
    (inspect-typespec '(integer 1 5))
  integer (1 5))

;;; Test extract-view-property-from-type
(deftest extract-view-property-from-type-1
    (mapcar (lambda (value)
              (if (typep value 'standard-object)
                  (class-name (class-of value))
                  value))
            (extract-view-property-from-type :present-as #'typespec->form-view-field-parser
                                             (make-instance 'form-scaffold)
                                             (find-slot-dsd 'employee 'age)))
  (:present-as integer-parser))

;;; Test typespec->view-field-presentation
(deftest typespec->view-field-presentation-1
    (typespec->view-field-presentation 'foo 'bar nil)
  nil)

;;; Test default generate-scaffold-view
(deftest generate-scaffold-view-1
    (object-class-name
     (generate-scaffold-view (make-instance 'data-scaffold)
                             (find-class 'employee)))
  data-view)

;;; Test default generate-scaffold-view
(deftest generate-scaffold-view-field-1
    (object-class-name
     (generate-scaffold-view-field (make-instance 'data-scaffold)
                                   (find-class 'employee)
                                   (find-slot-dsd 'employee 'name)))
  data-view-field)

