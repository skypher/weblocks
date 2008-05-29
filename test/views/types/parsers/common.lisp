
(in-package :weblocks-test)

;;; Test integer parser-error-message
(deftest integer-parser-error-message-1
    (parser-error-message (make-instance 'integer-parser
					 :min 10))
  "This value must be an integer greater than 9")

(deftest integer-parser-error-message-2
    (parser-error-message (make-instance 'integer-parser
					 :max 10))
  "This value must be an integer less than 11")

(deftest integer-parser-error-message-3
    (parser-error-message (make-instance 'integer-parser
					 :min 3 :max 10))
  "This value must be an integer greater than 2 and less than 11")

(deftest integer-parser-error-message-4
    (parser-error-message (make-instance 'integer-parser
					 :min 10
					 :error-message "custom error message"))
  "custom error message")

;;; Test integer parse-view-field-value
(deftest integer-parse-view-field-value-1
    (car
     (multiple-value-list
      (parse-view-field-value (make-instance 'integer-parser)
			      "foo"
			      *joe*
			      (find-view '(form employee))
			      (make-instance 'form-view-field))))
  nil)

;;; Test integer typespec->form-view-field-parser
(deftest integer-typespec->form-view-field-parser-1
    (object-class-name
     (cadr
      (multiple-value-list
       (typespec->form-view-field-parser (make-instance 'form-scaffold)
					 'integer nil))))
  integer-parser)

;;; Test symbol parse-view-field-value
(deftest symbol-parse-view-field-value-1
    (parse-view-field-value (make-instance 'symbol-parser)
			    "foo"
			    *joe*
			    (find-view '(form employee))
			    (make-instance 'form-view-field
					   :slot-name 'name))
  t t foo)

;;; Test keyword parse-view-field-value
(deftest keyword-parse-view-field-value-1
    (parse-view-field-value (make-instance 'keyword-parser)
			    "foo"
			    *joe*
			    (find-view '(form employee))
			    (make-instance 'form-view-field
					   :slot-name 'name))
  t t :foo)

;;; Test object-id parse-view-field-value
(deftest object-id-parse-view-field-value-1
    (with-request :get nil
      (persist-object *default-store* *joe*)
      (object-class-name
       (caddr
	(multiple-value-list
	 (parse-view-field-value (make-instance 'object-id-parser
						:class-name 'employee)
				 "1"
				 *joe*
				 (find-view '(form employee))
				 (make-instance 'form-view-field
						:slot-name 'name))))))
  employee)

