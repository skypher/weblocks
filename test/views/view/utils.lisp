
(in-package :weblocks-test)

(deftestsuite views/view/utils-suite (weblocks-suite)
  ())

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
	    (get-object-view-fields *joe* (defview () (:inherit-from '(:scaffold employee)))))
  (name manager))

(deftest get-object-view-fields-2
    (mapcar #'print-field-info
	    (get-object-view-fields *joe* (defview () (:inherit-from '(:scaffold employee))
					    (name :hidep t))))
  (manager))

(deftest get-object-view-fields-3
    (mapcar #'print-field-info
	    (get-object-view-fields *joe* (defview () (:inherit-from '(:scaffold employee))
					    (name :hidep t))
				    :include-invisible-p t))
  (name manager))

(deftest get-object-view-fields-4
    (mapcar #'print-field-info
	    (get-object-view-fields *joe* (defview () (:inherit-from '(:scaffold employee)))
				    :custom-fields (list (cons 0 (make-instance 'data-view-field
										:slot-name 'bar))
							 (make-instance 'data-view-field
									:slot-name 'foo))))
  (bar name manager foo))

(deftest get-object-view-fields-5
    (mapcar #'print-field-info
	    (get-object-view-fields *joe* (defview () (:inherit-from '(:scaffold employee))
					    education)))
  (name manager education))

(deftest get-object-view-fields-6
    (mapcar #'print-field-info
	    (get-object-view-fields *joe* (defview () (:inherit-from '(:scaffold employee))
					    (education :type mixin
						       :view '(data education-history)))))
  (name manager university graduation-year))

(deftest get-object-view-fields-7
    (mapcar #'print-field-info
	    (get-object-view-fields *joe* (defview () (:inherit-from '(:scaffold employee))
					    (education :type mixin
						       :view '(data education-history))
					    (graduation-year :hidep t))))
  (name manager university))

(addtest get-object-view-fields.direct-shadows-mixedin
  (dolist (view (list (defview () (:inherit-from '(:scaffold employee))
			(education :type mixin
				   :view '(data education-history))
			(graduation-year))
		      (defview () (:inherit-from '(:scaffold employee))
			(education :type mixin
				   :view '(data education-history))
			(graduation-year :hidep t))))
    (let ((fields (get-object-view-fields *joe* view :include-invisible-p t)))
      (ensure-same (mapcar #'print-field-info fields)
		   '(name manager university graduation-year))
      (ensure-same (field-info-object (car (last fields)))
		   *joe*))))

(addtest get-object-view-fields.direct-shadow-proper-mixin
  (let ((fields (get-object-view-fields
		 *joe* (defview () (:inherit-from '(:scaffold employee))
			 (education :type mixin
				    :view '(data education-history))
			 (education)))))
    (ensure-same (mapcar #'print-field-info fields)
		 '(name manager education))
    (ensure-same (field-info-object (third fields)) *joe*)))

;; XXX We believe the mixin should win here, but this is how it
;; behaves at the moment.  The easy fix will not do, because mixed-in
;; views are treated differently.
(addtest get-object-view-fields.direct-shadow-proper-mixin-after
  (let ((fields (get-object-view-fields
		 *joe* (defview () (:inherit-from '(:scaffold employee))
			 (education)
			 (education :type mixin
				    :view '(data education-history))))))
    (ensure-same (mapcar #'print-field-info fields)
		 '(name manager education))
    ;; this is what would change, to (slot-value *joe* 'education)
    (ensure-same (field-info-object (third fields)) *joe*)))

(deftest get-object-view-fields-8
    (mapcar #'print-field-info
	    (get-object-view-fields *joe* (defview () (:inherit-from '(:scaffold employee))
					    (education :type mixin
						       :view '(data education-history)))
				    :expand-mixins nil))
  (name manager education))

;;; Test map-view-fields
(deftest map-view-fields-1
    (map-view-fields #'print-field-info (defview () (:inherit-from '(:scaffold employee)))
		     *joe*)
  (name manager))

;;; Test map-mixin-fields
(deftest map-mixin-fields-1
    (let (fields)
      (map-mixin-fields (lambda (value)
			  (push (print-field-info value) fields))
			(defview () (:inherit-from '(:scaffold employee))
			  (education :type mixin
				     :view '(data education-history)))
			*joe*)
      fields)
  (education))

;;; Test count-view-fields
(deftest count-view-fields-1
    (count-view-fields (defview () (:inherit-from '(:scaffold employee))))
  2)

;;; Test slot-reader
(deftest slot-reader-1
    (not (null (weblocks::slot-reader 'employee 'name)))
  t)

;;; Test obtain-view-field-value
(deftest obtain-view-field-value-1
    (obtain-view-field-value
     (field-info-field
      (car (get-object-view-fields *joe* (defview () (:inherit-from '(:scaffold employee))))))
     *joe*)
  "Joe")

(deftest obtain-view-field-value-2
    (obtain-view-field-value
     (field-info-field
      (car (get-object-view-fields *joe* (defview () (:inherit-from '(:scaffold employee))
					   (name :reader "foo")))))
     *joe*)
  "foo")

;;; Test attributize-presentation
(deftest attributize-presentation-1
    (attributize-presentation (make-instance 'form-presentation))
  "form")

;;; Test class-from-view
(deftest class-from-view-1
    (mapcar #'slot-definition-name
	    (class-direct-slots 
	     (class-from-view (defview nil ()
				foo bar baz)
			      'class-from-view-test)))
  (foo bar baz))

