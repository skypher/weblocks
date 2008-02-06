
(in-package :weblocks-test)

;;; Test find-view
(deftest find-view-1
    (multiple-value-bind (res err)
	(ignore-errors
	  (find-view 'doesntexist))
      (values res (not (null err))))
  nil t)

(deftest find-view-2
    (find-view 'doesntexist nil)
  nil)

(defview find-view-test ())

(deftest find-view-3
    (not (null (find-view 'find-view-test)))
  t)

;;; Utils
(defun print-field-info (fi)
  (view-field-slot-name (field-info-field fi)))

;;; Test get-object-view-fields
(deftest get-object-view-fields-1
    (mapcar #'print-field-info
	    (get-object-view-fields *joe* (defview-anon (:inherit-from '(:scaffold employee)))))
  (name manager))

(deftest get-object-view-fields-2
    (mapcar #'print-field-info
	    (get-object-view-fields *joe* (defview-anon (:inherit-from '(:scaffold employee))
					      (name :hidep t))))
  (manager))

(deftest get-object-view-fields-3
    (mapcar #'print-field-info
	    (get-object-view-fields *joe* (defview-anon (:inherit-from '(:scaffold employee))
					      (name :hidep t))
				    :include-invisible-p t))
  (name manager))

(deftest get-object-view-fields-4
    (mapcar #'print-field-info
	    (get-object-view-fields *joe* (defview-anon (:inherit-from '(:scaffold employee)))
				    :custom-fields (list (cons 0 (make-instance 'data-view-field
										:slot-name 'bar))
							 (make-instance 'data-view-field
									:slot-name 'foo))))
  (bar name manager foo))

(deftest get-object-view-fields-5
    (mapcar #'print-field-info
	    (get-object-view-fields *joe* (defview-anon (:inherit-from '(:scaffold employee))
					      education)))
  (name manager education))

(deftest get-object-view-fields-6
    (mapcar #'print-field-info
	    (get-object-view-fields *joe* (defview-anon (:inherit-from '(:scaffold employee))
					      (education :type mixin
							 :view '(data education-history)))))
  (name manager university graduation-year))

(deftest get-object-view-fields-7
    (mapcar #'print-field-info
	    (get-object-view-fields *joe* (defview-anon (:inherit-from '(:scaffold employee))
					    (education :type mixin
						       :view '(data education-history))
					    (graduation-year :hidep t))))
  (name manager university))

(deftest get-object-view-fields-8
    (mapcar #'print-field-info
	    (get-object-view-fields *joe* (defview-anon (:inherit-from '(:scaffold employee))
					      (education :type mixin
							 :view '(data education-history)))
				    :expand-mixins nil))
  (name manager education))

;;; Test map-view-fields
(deftest map-view-fields-1
    (map-view-fields #'print-field-info (defview-anon (:inherit-from '(:scaffold employee)))
		     *joe*)
  (name manager))

;;; Test map-mixin-fields
(deftest map-mixin-fields-1
    (let (fields)
      (map-mixin-fields (lambda (value)
			  (push (print-field-info value) fields))
			(defview-anon (:inherit-from '(:scaffold employee))
			    (education :type mixin
				       :view '(data education-history)))
			*joe*)
      fields)
  (education))

;;; Test count-view-fields
(deftest count-view-fields-1
    (count-view-fields (defview-anon (:inherit-from '(:scaffold employee))))
  2)

;;; Test slot-reader
(deftest slot-reader-1
    (not (null (weblocks::slot-reader 'employee 'name)))
  t)

;;; Test obtain-view-field-value
(deftest obtain-view-field-value-1
    (obtain-view-field-value
     (field-info-field
      (car (get-object-view-fields *joe* (defview-anon (:inherit-from '(:scaffold employee))))))
     *joe*)
  "Joe")

(deftest obtain-view-field-value-2
    (obtain-view-field-value
     (field-info-field
      (car (get-object-view-fields *joe* (defview-anon (:inherit-from '(:scaffold employee))
					     (name :reader "foo")))))
     *joe*)
  "foo")

;;; Test attributize-presentation
(deftest attributize-presentation-1
    (attributize-presentation (make-instance 'form-presentation))
  "form")

