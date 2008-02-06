
(in-package :weblocks-test)

;;; Test entity-class-name
(deftest entity-class-name-1
    (entity-class-name 'data '#:-view)
  data-view)

(deftest entity-class-name-2
    (entity-class-name 'form '#:-scaffold)
  form-scaffold)

(deftest entity-class-name-3
    (multiple-value-bind (res err)
	(ignore-errors
	  (entity-class-name 'doesnt '#:-exist))
      (declare (ignore err))
      res)
  nil)

;;; Test view-class-name
(deftest view-class-name-1
    (view-class-name 'data)
  data-view)

;;; Test view-default-field-type
(deftest view-default-field-type-1
    (view-default-field-type 'form nil)
  form)

(deftest view-default-field-type-2
    (view-default-field-type 'form 'data)
  data)

;;; Test view-field-class-name
(deftest view-field-class-name-1
    (view-field-class-name 'data)
  data-view-field)

;;; Test presentation-class-name
(deftest presentation-class-name-1
    (presentation-class-name 'input)
  input-presentation)

