
(in-package :weblocks-test)

;;; Test boolean render-view-field-value
(deftest-html boolean-render-view-field-value-1
    (render-view-field-value  nil (make-instance 'predicate-presentation)
			      (make-instance 'form-view-field
					     :slot-name 'predicate)
			      (find-view '(form employee))
			      nil *joe*)
  (:span :class "value" "No"))

(deftest-html boolean-render-view-field-value-2
    (render-view-field-value  t (make-instance 'predicate-presentation)
			      (make-instance 'form-view-field
					     :slot-name 'predicate)
			      (find-view '(form employee))
			      nil *joe*)
  (:span :class "value" "Yes"))

;;; Test boolean print-view-field-value
(deftest boolean-print-view-field-value-1
    (print-view-field-value  nil (make-instance 'predicate-presentation)
			     (make-instance 'form-view-field
					    :slot-name 'predicate)
			     (find-view '(form employee))
			     nil *joe*)
  "No")

(deftest boolean-print-view-field-value-2
    (print-view-field-value t (make-instance 'predicate-presentation)
			    (make-instance 'form-view-field
					   :slot-name 'predicate)
			    (find-view '(form employee))
			    nil *joe*)
  "Yes")

;;; Test checkbox render-view-field-value
(deftest-html checkbox-render-view-field-value-1
    (render-view-field-value  t (make-instance 'checkbox-presentation)
			      (make-instance 'form-view-field
					     :slot-name 'predicate)
			      (find-view '(form employee))
			      nil *joe*)
  (:input :name "predicate" :type "checkbox" :class "checkbox" :value "t" :checked "checked"))

;;; Test boolean parse-view-field-value
(deftest boolean-parse-view-field-value-1
    (parse-view-field-value (make-instance 'predicate-parser)
			    "t"
			    *joe*
			    (find-view '(form employee))
			    (make-instance 'form-view-field))
  t t t)

;;; Test boolean typespec->view-field-presentation
(deftest boolean-typespec->view-field-presentation-1
    (object-class-name
     (cadr
      (multiple-value-list
       (typespec->view-field-presentation (make-instance 'scaffold)
					  'boolean nil))))
  predicate-presentation)

;;; Test boolean typespec->form-view-field-parser
(deftest boolean-typespec->form-view-field-parser-1
    (object-class-name
     (cadr
      (multiple-value-list
       (typespec->form-view-field-parser (make-instance 'form-scaffold)
					 'boolean nil))))
  predicate-parser)

