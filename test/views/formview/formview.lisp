
(in-package :weblocks-test)

(deftestsuite views/formview/formview-suite (weblocks-suite) nil)

;;; Test view-caption
(deftest formview-view-caption-1
    (view-caption (make-instance 'form-view
				 :caption "foo"))
  "foo")

;;; Test mixin-form-view-field-persist-p
(deftest mixin-form-view-field-persist-p-1
    (mixin-form-view-field-persist-p (make-instance 'mixin-form-view-field
						    :view (defview () (:type form :persistp nil))))
  nil)

(deftest mixin-form-view-field-persist-p-2
    (mixin-form-view-field-persist-p (make-instance 'mixin-form-view-field
						    :view (defview () (:type form :persistp t))))
  t)

(deftest mixin-form-view-field-persist-p-3
    (mixin-form-view-field-persist-p (make-instance 'mixin-form-view-field
						    :view (defview () (:type form :persistp t))
						    :persistp nil))
  nil)

;;; Test form view view-default-field-type
(deftest form-view-view-default-field-type-1
    (view-default-field-type 'form 'mixin)
  mixin-form)

;;; Test render-validation-summary
(deftest-html render-validation-summary-1
    (render-validation-summary (make-instance 'form-view)
			       *joe* nil
			       (list (cons nil "Hello is a required field.")))
  (:div :class "validation-errors-summary"
	(:h2 :class "error-count"
	     "There is 1 validation error:")
	(:ul :class "non-field-validation-errors"
	 (:li "Hello is a required field."))))

(deftest-html render-validation-summary-2
    (render-validation-summary (make-instance 'form-view)
			       *joe* nil
			       (list (cons nil "Hello is a required field.")
				     (cons nil "World is a required field.")))
  (:div :class "validation-errors-summary"
	(:h2 :class "error-count"
	     "There are 2 validation errors:")
	(:ul :class "non-field-validation-errors"
	 (:li "Hello is a required field.")
	 (:li "World is a required field."))))

(deftest-html render-validation-summary-3
    (render-validation-summary (make-instance 'form-view)
			       *joe* nil
			       (list (cons 'foo "Hello is a required field.")))
  (:div :class "validation-errors-summary"
	(:h2 :class "error-count"
	     "There is 1 validation error:")
	(:ul :class "field-validation-errors"
	 (:li "Hello is a required field."))))

(deftest-html render-validation-summary-4
    (render-validation-summary (make-instance 'form-view)
			       *joe* nil
			       (list (cons nil "Hello is a required field.")
				     (cons 'foo "World is a required field.")))
  (:div :class "validation-errors-summary"
	(:h2 :class "error-count"
	     "There are 2 validation errors:")
	(:ul :class "non-field-validation-errors"
	 (:li "Hello is a required field."))
	(:ul :class "field-validation-errors"
	 (:li "World is a required field."))))


(deftest-html render-validation-summary-5
    (render-validation-summary (make-instance 'form-view)
			       *joe* nil
			       nil)
  nil)

;;; Test render-form-view-buttons
(deftest-html render-form-view-buttons-1
    (render-form-view-buttons (make-instance 'form-view)
			      *joe* nil)
  (:div :class "submit"
	(:input :name "submit" :type "submit" :class "submit" :value "Submit"
		:onclick "disableIrrelevantButtons(this);")
	(:input :name "cancel" :type "submit" :class "submit cancel" :value "Cancel"
		:onclick "disableIrrelevantButtons(this);")))

(deftest-html render-form-view-buttons-2
    (render-form-view-buttons (make-instance 'form-view
					     :buttons (list :cancel))
			      *joe* nil)
  (:div :class "submit"
	(:input :name "cancel" :type "submit" :class "submit cancel" :value "Cancel"
		:onclick "disableIrrelevantButtons(this);")))

(deftest-html render-form-view-buttons-3
    (render-form-view-buttons (make-instance 'form-view
					     :buttons (list (cons :submit "Bar")
							    (cons :cancel "Foo")))
			      *joe* nil)
  (:div :class "submit"
	(:input :name "submit" :type "submit" :class "submit" :value "Bar"
		:onclick "disableIrrelevantButtons(this);")
	(:input :name "cancel" :type "submit" :class "submit cancel" :value "Foo"
		:onclick "disableIrrelevantButtons(this);")))

;;; Test form view with-view-header
(deftest-html form-view-with-view-header-1
    (with-request :get nil
      (with-view-header (make-instance 'form-view
				       :default-action (lambda (&rest args)
							 (declare (ignore args))))
	*joe* nil
	(lambda (&rest args)
	  (declare (ignore args)))))
  #.(form-header-template "abc123"
     '()))

(deftest-html form-view-with-view-header-2
    (with-request :get nil
      (with-view-header (make-instance 'form-view
				       :default-action (lambda (&rest args)
							 (declare (ignore args)))
				       :focusp t)
	*joe* nil
	(lambda (&rest args)
	  (declare (ignore args)))))
  (htm
   #.(form-header-template "abc123"
			   '()
			   :form-id "id-123")
   #.(format nil "<script type='text/javascript'>
// <![CDATA[
$('id-123').focusFirstElement();
// ]]>
</script>")))

;;; Test form view render-view-field
(deftest-html render-view-field-1
    (render-view-field (make-instance 'form-view-field
				      :slot-name 'name
				      :requiredp t)
		       (make-instance 'form-view)
		       nil
		       (make-instance 'input-presentation)
		       "Joe" *joe*)
  (:li :class "name"
       (:label :class "input" :for "id-123"
	       (:span :class "slot-name"
		      (:span :class "extra" "Name:&nbsp;"
			     (:em :class "required-slot" "(required)&nbsp;"))))
       (:input :type "text" :name "name" :value "Joe" :maxlength "40" :id "id-123")))

(deftest-html render-view-field-2
    (let ((field (make-instance 'form-view-field
				:slot-name 'name
				:requiredp t)))
      (render-view-field field
			 (make-instance 'form-view)
			 nil
			 (make-instance 'input-presentation)
			 "Joe" *joe*
			 :validation-errors (list (cons field "Some Error!"))))
  (:li :class "name item-not-validated"
       (:label :class "input" :for "id-123"
	       (:span :class "slot-name"
		      (:span :class "extra" "Name:&nbsp;"
			     (:em :class "required-slot" "(required)&nbsp;"))))
       (:input :type "text" :name "name" :value "Joe" :maxlength "40" :id "id-123")
       (:p :class "validation-error"
           (:em (:span :class "validation-error-heading" "Error:&nbsp;")
                "Some Error!"))))

(deftest-html render-view-field-3
    (render-view-field (make-instance 'form-view-field
				      :slot-name 'name
				      :requiredp t
				      :required-indicator nil)
		       (make-instance 'form-view)
		       nil
		       (make-instance 'input-presentation)
		       "Joe" *joe*)
  (:li :class "name"
       (:label :class "input" :for "id-123"
	       (:span :class "slot-name"
		      (:span :class "extra" "Name:&nbsp;")))
       (:input :type "text" :name "name" :value "Joe" :maxlength "40" :id "id-123")))

(deftest-html render-view-field-4
    (render-view-field (make-instance 'form-view-field
				      :slot-name 'name
				      :requiredp t
				      :required-indicator t)
		       (make-instance 'form-view)
		       nil
		       (make-instance 'input-presentation)
		       "Joe" *joe*)
  (:li :class "name"
       (:label :class "input" :for "id-123"
	       (:span :class "slot-name"
		      (:span :class "extra" "Name:&nbsp;"
			     (:em :class "required-slot" "(required)&nbsp;"))))
       (:input :type "text" :name "name" :value "Joe" :maxlength "40" :id "id-123")))

(deftest-html render-view-field-5
    (render-view-field (make-instance 'form-view-field
				      :slot-name 'name
				      :requiredp t
				      :required-indicator "Required")
		       (make-instance 'form-view)
		       nil
		       (make-instance 'input-presentation)
		       "Joe" *joe*)
  (:li :class "name"
       (:label :class "input" :for "id-123"
	       (:span :class "slot-name"
		      (:span :class "extra" "Name:&nbsp;"
			     (:em :class "required-slot" "Required&nbsp;"))))
       (:input :type "text" :name "name" :value "Joe" :maxlength "40" :id "id-123")))

; Bug: error in checkbox render-view-field on view validation error (when using "satisfies")
(deftest render-view-field-value-6 
         (ensure 
           (progn 
             (render-view-field (make-instance 'form-view-field :slot-name 'name)
                                (make-instance 'form-view)
                                nil
                                (make-instance 'radio-presentation :choices (list :choice1 :choice2))
                                nil *joe* :validation-errors (list (cons nil "Some error"))) 
             t)))

;;; Test form view render-view-field-value
(deftest-html form-view-render-view-field-value-1
    (render-view-field-value "Joe" (make-instance 'input-presentation)
			     (make-instance 'form-view-field
					    :slot-name 'name)
			     (make-instance 'form-view)
			     nil *joe*)
  (:input :type "text" :name "name" :value "Joe" :maxlength "40"))

(deftest-html form-view-render-view-field-value-2
    (let ((field (make-instance 'form-view-field
				:slot-name 'name)))
      (render-view-field-value "Joe" (make-instance 'input-presentation)
			       field
			       (make-instance 'form-view)
			       nil *joe*
			       :intermediate-values (list (cons field "Jim"))))
  (:input :type "text" :name "name" :value "Jim" :maxlength "40"))

;;; Test form view print-view-field-value
(deftest form-view-print-view-field-value-1
    (print-view-field-value nil (make-instance 'input-presentation)
			    (make-instance 'form-view-field)
			    (make-instance 'form-view) nil nil)
  nil)

;;; Test form-field-intermediate-value
(deftest form-field-intermediate-value-1
    (let ((field (make-instance 'form-view-field)))
      (form-field-intermediate-value field (list (cons 1 2)
						 (cons field 3)
						 (cons 4 5))))
  3 t)

;;; Test form view render-object-view
(deftest-html form-view-render-object-view-1
    (with-request :get nil
      (render-object-view *joe* '(form employee)
			  :action (lambda (&rest args)
				    (declare (ignore args)))))
  #.(form-header-template "abc123"
     '((:li :class "name"
	(:label :class "input" :for "id-123"
	 (:span :class "slot-name"
		(:span :class "extra" "Name:&nbsp;"
		       (:em :class "required-slot" "(required)&nbsp;"))))
        (:input :type "text" :name "name" :value "Joe" :maxlength "40" :id "id-123"))
       (:li :class "manager"
	(:label :class "input" :for "id-123"
	 (:span :class "slot-name"
		(:span :class "extra" "Manager:&nbsp;")))
        (:input :type "text" :name "manager" :value "Jim" :maxlength "40" :id "id-123")))))

(deftest-html form-view-render-object-view-2
    (with-request :get nil
      (render-object-view *joe* (defview () (:type form :inherit-from '(form employee)
					     :use-ajax-p nil
					     :enctype "fooenc"))
			  :action (lambda (&rest args)
				    (declare (ignore args)))))
  #.(form-header-template "abc123"
     '((:li :class "name"
	(:label :class "input" :for "id-123"
	 (:span :class "slot-name"
		(:span :class "extra" "Name:&nbsp;"
		       (:em :class "required-slot" "(required)&nbsp;"))))
        (:input :type "text" :name "name" :value "Joe" :maxlength "40" :id "id-123"))
       (:li :class "manager"
	(:label :class "input" :for "id-123"
	 (:span :class "slot-name"
		(:span :class "extra" "Manager:&nbsp;")))
        (:input :type "text" :name "manager" :value "Jim" :maxlength "40" :id "id-123")))
     :enctype "fooenc"
     :use-ajax-p nil))

