
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

(addtest get-object-view-fields-7
  (let ((fields (get-object-view-fields *joe*
		  (defview () (:inherit-from '(:scaffold employee))
		    (education :type mixin
			       :view '(data education-history))
		    (graduation-year :hidep t)))))
    (ensure-same (mapcar #'print-field-info fields)
		 '(name manager university graduation-year))
    (ensure-same (field-info-object (car (last fields)))
		 (slot-value *joe* 'education))))

(defclass educated-employee nil
  ((education1 :initform (make-instance 'education-history))
   (education2 :initform (make-instance 'education-history))))

(addtest get-object-view-fields.properly-discern-mixins
  (dolist (view (list (defview () nil 
			(education1 :type mixin
                                    :view '(data education-history))
                        (education2 :type mixin
                                    :view '(data education-history)))
		      (defview () nil 
			(education1 :type mixin
                                    :view '(data education-history))
                        (education2 :type mixin
                                    :view '(data education-history)))))
    (let ((fields (get-object-view-fields (make-instance 'educated-employee)
                                          view :include-invisible-p t)))
      (ensure-same (mapcar #'print-field-info fields)
		   '(university graduation-year university graduation-year))
      (ensure-same (mapcar #'attributize-view-field-name fields)
		   '("education1-university" "education1-graduation-year"
                     "education2-university" "education2-graduation-year")))))

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
		   '(name manager university graduation-year graduation-year))
      (ensure-same (field-info-object (car (last fields)))
		   *joe*)
      (ensure-same (field-info-object (car (last fields 2)))
		   (slot-value *joe* 'education)))))

(addtest get-object-view-fields.direct-shadow-proper-mixin
  (let ((fields (get-object-view-fields
		 *joe* (defview () (:inherit-from '(:scaffold employee))
			 (education :type mixin
				    :view '(data education-history))
			 (education)))))
    (ensure-same (mapcar #'print-field-info fields)
		 '(name manager education))
    (ensure-same (field-info-object (third fields)) *joe*)))

(addtest get-object-view-fields.direct-shadow-proper-mixin-after
  (let ((fields (get-object-view-fields
		 *joe* (defview () (:inherit-from '(:scaffold employee))
			 (education)
			 (education :type mixin
				    :view '(data education-history))))))
    (ensure-same (mapcar #'print-field-info fields)
		 '(name manager university graduation-year))
    (ensure-same (field-info-object (third fields)) (slot-value *joe* 'education))))

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

