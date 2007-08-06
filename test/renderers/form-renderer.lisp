
(in-package :weblocks-test)

;;; test with-form-header
(deftest-html with-form-header-1
    (with-request :get nil
      (with-form-header *joe* (lambda () nil) :action "test-action" :method :post))
  #.(form-header-template "test-action" '(nil) :method "post"))

(deftest-html with-form-header-2
    (with-request :get nil
      (with-form-header *joe* (lambda () nil)
			:action "test-action"
			:method :post
			:title-action "Changing"))
  #.(form-header-template "test-action" '(nil)
			  :method "post"
			  :title-action "Changing:&nbsp;"))

;;; test render-validation-summary
(deftest-html render-validation-summary-1
    (render-validation-summary `(("Hello" . ,(make-condition 'required-validation-error
							     :slot-name "HELLO"))))
  (:div :class "validation-errors-summary"
	    (:h2 :class "error-count"
		 "There is 1 validation error:")
	    (:ul
	     (:li "Hello is a required field."))))

(deftest-html render-validation-summary-2
    (render-validation-summary `(("Hello" . ,(make-condition 'required-validation-error
							     :slot-name "HELLO"))
				 ("World" . ,(make-condition 'required-validation-error
							     :slot-name "WORLD"))))
  (:div :class "validation-errors-summary"
	    (:h2 :class "error-count"
		 "There are 2 validation errors:")
	    (:ul
	     (:li "Hello is a required field.")
	     (:li "World is a required field."))))

(deftest-html render-validation-summary-3
    (render-validation-summary nil)
  nil)

;;; test render-form-controls
(deftest-html render-form-controls-1
    (render-form-controls *joe* :action "abc123")
  (:div :class "submit"
	(:input :name "submit" :type "submit" :class "submit" :value "Submit"
		:onclick "disableIrrelevantButtons(this);")
	(:input :name "cancel" :type "submit" :class "submit cancel" :value "Cancel"
		:onclick "disableIrrelevantButtons(this);")))

;;; test render-form-slot
(deftest-html render-form-slot-1
    (render-form-slot *joe* 'name "Joe" :slot-path '(name))
  (:li :class "name"
       (:label
	(:span :class "slot-name"
	       (:span :class "extra" "Name:&nbsp;"
		      (:em :class "required-slot" "(required)&nbsp;")))
	(:input :type "text" :name "name" :value "Joe"))))

(deftest-html render-form-slot-2
    (render-form-slot *joe* 'address-ref *home-address* :slot-path '(address-ref))
  (:li :class "address-ref"
       (:label
	(:span :class "slot-name"
	       (:span :class "extra" "Address:&nbsp;"))
	       (:input :type "text" :name "address-ref" :value "Address"))))

(deftest-html render-form-slot-3
    (render-form-slot *joe* 'education *some-college*)
  (htm
   (:li :class "university"
	(:label
	 (:span :class "slot-name"
		(:span :class "extra" "University:&nbsp;"))
	 (:input :type "text" :name "university" :value "Bene Gesserit University")))
   (:li :class "graduation-year"
	(:label
	 (:span :class "slot-name"
		(:span :class "extra" "Graduation Year:&nbsp;"))
	 (:input :type "text" :name "graduation-year" :value "2000")))))

;;; test render-form
(deftest-html render-form-1
    (render-form "test" :slot-path '(test))
  (:input :type "text" :name "test" :value "test"))

(deftest-html render-form-2
    (with-request :get nil
      (render-form *joe* :action "abc123"))
  #.(form-header-template "abc123"
     '((:li :class "name"
	(:label
	 (:span :class "slot-name"
		(:span :class "extra" "Name:&nbsp;"
		       (:em :class "required-slot" "(required)&nbsp;")))
	 (:input :type "text" :name "name" :value "Joe")))
       (:li :class "manager"
	(:label
	 (:span :class "slot-name"
		(:span :class "extra" "Manager:&nbsp;"))
	 (:input :type "text" :name "manager" :value "Jim"))))))

(deftest-html render-form-3
    (with-request :post nil
      (render-form *joe* :slots '(address-ref)))
  #.(form-header-template nil
     '((:li :class "name"
	(:label
	 (:span :class "slot-name"
		(:span :class "extra" "Name:&nbsp;"
		       (:em :class "required-slot" "(required)&nbsp;")))
	 (:input :type "text" :name "name" :value "Joe")))
       (:li :class "address-ref"
	(:label
	 (:span :class "slot-name"
		(:span :class "extra" "Address:&nbsp;"))
	 (:input :type "text" :name "address-ref" :value "Address")))
       (:li :class "manager"
	(:label
	 (:span :class "slot-name"
		(:span :class "extra" "Manager:&nbsp;"))
	 (:input :type "text" :name "manager" :value "Jim"))))))

(deftest-html render-form-4
    (with-request :post nil
      (render-form *joe* :slots '(education)
		   :preslots-fn (lambda (obj &rest keys)
				  (with-html
				    (:div "test1")))
		   :postslots-fn (lambda (obj &rest keys)
				   (with-html
				     (:div "test2")))
		   :action "abc123"))
  #.(form-header-template "abc123"
     '((:li :class "name"
	(:label
	 (:span :class "slot-name"
		(:span :class "extra" "Name:&nbsp;"
		       (:em :class "required-slot" "(required)&nbsp;")))
	 (:input :type "text" :name "name" :value "Joe")))
       (:li :class "university"
	(:label
	 (:span :class "slot-name"
		(:span :class "extra" "University:&nbsp;"))
	 (:input :type "text" :name "university" :value "Bene Gesserit University")))
       (:li :class "graduation-year"
	(:label
	 (:span :class "slot-name"
		(:span :class "extra" "Graduation Year:&nbsp;"))
	     (:input :type "text" :name "graduation-year" :value "2000")))
       (:li :class "manager"
	(:label
	 (:span :class "slot-name"
		(:span :class "extra" "Manager:&nbsp;"))
	 (:input :type "text" :name "manager" :value "Jim"))))
     :preslots '((:div "test1"))
     :postslots '((:div "test2"))))

(deftest-html render-form-5
    (with-request :get nil
      (render-form *joe* :slots '(address-ref) :intermediate-fields '(("name" . "Bill"))))
  #.(form-header-template nil
     '((:li :class "name"
	(:label
	 (:span :class "slot-name"
		(:span :class "extra" "Name:&nbsp;"
		       (:em :class "required-slot" "(required)&nbsp;")))
	 (:input :type "text" :name "name" :value "Bill")))
       (:li :class "address-ref"
	(:label
	 (:span :class "slot-name"
		(:span :class "extra" "Address:&nbsp;"))
	 (:input :type "text" :name "address-ref" :value "Address")))
       (:li :class "manager"
	(:label
	 (:span :class "slot-name"
		(:span :class "extra" "Manager:&nbsp;"))
	 (:input :type "text" :name "manager" :value "Jim"))))))

(deftest-html render-form-6
    (with-request :post nil
      (render-form *joe* :slots '((name . nickname))))
  #.(form-header-template nil
     '((:li :class "name"
	(:label
	 (:span :class "slot-name"
		(:span :class "extra" "Nickname:&nbsp;"
		       (:em :class "required-slot" "(required)&nbsp;")))
	 (:input :type "text" :name "name" :value "Joe")))
       (:li :class "manager"
	(:label
	 (:span :class "slot-name"
		(:span :class "extra" "Manager:&nbsp;"))
	 (:input :type "text" :name "manager" :value "Jim"))))))

(deftest-html render-form-7
    (with-request :get nil
      (render-form *joe* :slots '(address-ref) :validation-errors '(("name" . "Some error."))))
  #.(form-header-template nil
     '((:li :class "name item-not-validated"
	(:label (:span :class "slot-name"
		       (:span :class "extra" "Name:&nbsp;"
			      (:em :class "required-slot" "(required)&nbsp;")))
	 (:input :type "text" :name "name" :value "Joe")
	 (:p :class "validation-error"
	     (:em (:span :class "validation-error-heading" "Error:&nbsp;") "Some error."))))
       (:li :class "address-ref"
	(:label
	 (:span :class "slot-name"
		(:span :class "extra" "Address:&nbsp;"))
	 (:input :type "text" :name "address-ref" :value "Address")))
       (:li :class "manager"
	(:label
	 (:span :class "slot-name"
		(:span :class "extra" "Manager:&nbsp;"))
	 (:input :type "text" :name "manager" :value "Jim"))))
     :preslots '((:div :class "validation-errors-summary"
		  (:h2 :class "error-count" "There is 1 validation error:")
		  (:ul (:li "Some error."))))))
