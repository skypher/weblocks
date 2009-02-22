
(in-package :weblocks-test)

;;; Test parser-class-name
(deftest parser-class-name-1
    (parser-class-name 'integer)
  integer-parser)

(deftest parser-class-name-2
    (parser-class-name 'text)
  text-parser)

;;; Test text parser parse-view-field-value
(deftest text-parser-parse-view-field-value-1
    (parse-view-field-value (make-instance 'text-parser)
			    "foo" *joe*
			    (find-view '(form employee))
			    (make-instance 'form-view-field))
  t t "foo")

(deftest text-parser-parse-view-field-value-2
    (parse-view-field-value (make-instance 'text-parser
					   :matches "..a")
			    "foo" *joe*
			    (find-view '(form employee))
			    (make-instance 'form-view-field))
  nil)

(deftest text-parser-parse-view-field-value-3
    (parse-view-field-value (make-instance 'text-parser
					   :matches "..a")
			    "foa" *joe*
			    (find-view '(form employee))
			    (make-instance 'form-view-field))
  t t "foa")

(deftest text-parser-parse-view-field-value-4
    (parse-view-field-value (make-instance 'text-parser)
			    "" *joe*
			    (find-view '(form employee))
			    (make-instance 'form-view-field))
  t nil "")

;;; Test text-input-present-p
(deftest text-input-present-p-1
    (text-input-present-p "")
  nil)

(deftest text-input-present-p-2
    (text-input-present-p nil)
  nil)

(deftest text-input-present-p-3
    (text-input-present-p "foo")
  t)


