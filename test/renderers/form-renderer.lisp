
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

(deftest-html with-form-header-3
    (let ((*form-error-summary-threshold* 1))
      (with-request :get nil
	(with-form-header *joe* (lambda () nil) :action "test-action" :method :post)))
  (:form :class "renderer form employee long-form"
	 :action "/foo/bar"
	 :method "post"
	 :onsubmit
	 "initiateFormAction(\"test-action\", $(this), \"weblocks-session=1%3ATEST\"); return false;"
	 (:div :class "extra-top-1" "<!-- empty -->")
	 (:div :class "extra-top-2" "<!-- empty -->")
	 (:div :class "extra-top-3" "<!-- empty -->")
	 (:fieldset
	  (:h1 (:span :class "action" (str "Modifying:&nbsp;"))
	       (:span :class "object" "Employee"))
	  (:h2 :class "form-fields-title" "Form fields:") (:ul nil)
	  (:div :class "submit"
		(:input :name "submit" :type "submit"
				       :class "submit"
				       :value "Submit"
				       :onclick "disableIrrelevantButtons(this);")
		(:input :name "cancel" :type "submit" :class "submit cancel"
						      :value "Cancel"
						      :onclick "disableIrrelevantButtons(this);"))
	  (:input :name "action" :type "hidden" :value "test-action"))
	 (:div :class "extra-bottom-1" "<!-- empty -->")
	 (:div :class "extra-bottom-2" "<!-- empty -->")
	 (:div :class "extra-bottom-3" "<!-- empty -->")))

;;; test render-validation-summary
(deftest-html render-validation-summary-1
    (render-validation-summary '(("Hello" . "Hello is a required field.")))
  (:div :class "validation-errors-summary"
	    (:h2 :class "error-count"
		 "There is 1 validation error:")
	    (:ul
	     (:li "Hello is a required field."))))

(deftest-html render-validation-summary-2
    (render-validation-summary '(("Hello" . "Hello is a required field.")
				 ("World" . "World is a required field.")))
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
    (render-form-slot *joe* 'name 'string "Joe" :slot-path '(name))
  (:li :class "name"
       (:label
	(:span :class "slot-name"
	       (:span :class "extra" "Name:&nbsp;"
		      (:em :class "required-slot" "(required)&nbsp;")))
	(:input :type "text" :name "name" :value "Joe" :maxlength "40"))))

(deftest-html render-form-slot-2
    (render-form-slot *joe* 'address-ref t *home-address* :slot-path '(address-ref))
  (:li :class "address-ref"
       (:label
	(:span :class "slot-name"
	       (:span :class "extra" "Address:&nbsp;"))
	       (:select :name "address-ref"
			(:option :value "" "[Select Address]")
			(:option :value "*home-address*" :selected "selected" "Address")))))

(deftest-html render-form-slot-3
    (render-form-slot *home-address* 'city t "Brooklyn")
  (htm
   (:li :class "city"
	(:label
	 (:span :class "slot-name"
		(:span :class "extra" "City:&nbsp;"))
	 (:input :type "text" :name "city" :value "Brooklyn" :maxlength "40")))))

;;; test slot-intermedia-value
(deftest slot-intermedia-value-1
    (slot-intermedia-value 'some-slot '((a . b) (c . d)))
  nil)

(deftest slot-intermedia-value-2
    (slot-intermedia-value 'some-slot '((a . b) (some-slot . d)))
  (some-slot . d))

(deftest slot-intermedia-value-3
    (slot-intermedia-value 'some-slot '((a . b) (some-slot . nil)))
  (some-slot . nil))

;;; test form-print-object
(deftest form-print-object-1
    (form-print-object nil 'name 'integer 42)
  "42")

(deftest form-print-object-2
    (form-print-object nil 'name 'integer nil)
  nil)

;;; form-potential-values
(deftest form-potential-values-1
    (mapcar #'object-id (form-potential-values *joe* 'address-ref t))
  ("*home-address*"))

;;; render-form-foreign-value
(deftest-html render-form-foreign-value-1
    (weblocks::render-form-foreign-value *joe* 'address-ref t *home-address*)
  (:select :name "address-ref"
	   (:option :value "" "[Select Address]")
	   (:option :value "*home-address*" :selected "selected" "Address")))

;;; test render-form/value
(deftest-html render-form/value-1
    (render-form-value nil nil t "test" :slot-path '(test))
  (:input :type "text" :name "test" :value "test" :maxlength "40"))

(deftest-html render-form/value-2
    (with-request :get nil
      (render-form *joe* :action "abc123"))
  #.(form-header-template "abc123"
     '((:li :class "name"
	(:label
	 (:span :class "slot-name"
		(:span :class "extra" "Name:&nbsp;"
		       (:em :class "required-slot" "(required)&nbsp;")))
	 (:input :type "text" :name "name" :value "Joe" :maxlength "40")))
       (:li :class "manager"
	(:label
	 (:span :class "slot-name"
		(:span :class "extra" "Manager:&nbsp;"))
	 (:input :type "text" :name "manager" :value "Jim" :maxlength "40"))))))

(deftest-html render-form/value-3
    (with-request :post nil
      (render-form *joe* :slots '(address-ref)))
  #.(form-header-template nil
     '((:li :class "name"
	(:label
	 (:span :class "slot-name"
		(:span :class "extra" "Name:&nbsp;"
		       (:em :class "required-slot" "(required)&nbsp;")))
	 (:input :type "text" :name "name" :value "Joe" :maxlength "40")))
       (:li :class "address-ref"
	(:label
	 (:span :class "slot-name"
		(:span :class "extra" "Address:&nbsp;"))
	 (:select :name "address-ref"
		  (:option :value "" "[Select Address]")
		  (:option :value "*home-address*" :selected "selected" "Address"))))
       (:li :class "manager"
	(:label
	 (:span :class "slot-name"
		(:span :class "extra" "Manager:&nbsp;"))
	 (:input :type "text" :name "manager" :value "Jim" :maxlength "40"))))))

(deftest-html render-form/value-4
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
	 (:input :type "text" :name "name" :value "Joe" :maxlength "40")))
       (:li :class "university"
	(:label
	 (:span :class "slot-name"
		(:span :class "extra" "University:&nbsp;"))
	 (:input :type "text" :name "university" :value "Bene Gesserit University" :maxlength "40")))
       (:li :class "graduation-year"
	(:label
	 (:span :class "slot-name"
		(:span :class "extra" "Graduation Year:&nbsp;"))
	     (:input :type "text" :name "graduation-year" :value "2000" :maxlength "40")))
       (:li :class "manager"
	(:label
	 (:span :class "slot-name"
		(:span :class "extra" "Manager:&nbsp;"))
	 (:input :type "text" :name "manager" :value "Jim" :maxlength "40"))))
     :preslots '((:div "test1"))
     :postslots '((:div "test2"))))

(deftest-html render-form/value-5
    (with-request :get nil
      (render-form *joe* :slots '(address-ref) :intermediate-fields '(("name" . "Bill"))))
  #.(form-header-template nil
     '((:li :class "name"
	(:label
	 (:span :class "slot-name"
		(:span :class "extra" "Name:&nbsp;"
		       (:em :class "required-slot" "(required)&nbsp;")))
	 (:input :type "text" :name "name" :value "Bill" :maxlength "40")))
       (:li :class "address-ref"
	(:label
	 (:span :class "slot-name"
		(:span :class "extra" "Address:&nbsp;"))
	 (:select :name "address-ref"
		  (:option :value "" "[Select Address]")
		  (:option :value "*home-address*" :selected "selected" "Address"))))
       (:li :class "manager"
	(:label
	 (:span :class "slot-name"
		(:span :class "extra" "Manager:&nbsp;"))
	 (:input :type "text" :name "manager" :value "Jim" :maxlength "40"))))))

(deftest-html render-form/value-6
    (with-request :post nil
      (render-form *joe* :slots '((name . nickname))))
  #.(form-header-template nil
     '((:li :class "name"
	(:label
	 (:span :class "slot-name"
		(:span :class "extra" "Nickname:&nbsp;"
		       (:em :class "required-slot" "(required)&nbsp;")))
	 (:input :type "text" :name "name" :value "Joe" :maxlength "40")))
       (:li :class "manager"
	(:label
	 (:span :class "slot-name"
		(:span :class "extra" "Manager:&nbsp;"))
	 (:input :type "text" :name "manager" :value "Jim" :maxlength "40"))))))

(deftest-html render-form/value-7
    (with-request :get nil
      (render-form *joe* :slots '(address-ref) :validation-errors '(("name" . "Some error."))))
  #.(form-header-template nil
     '((:li :class "name item-not-validated"
	(:label (:span :class "slot-name"
		       (:span :class "extra" "Name:&nbsp;"
			      (:em :class "required-slot" "(required)&nbsp;")))
	 (:input :type "text" :name "name" :value "Joe" :maxlength "40")
	 (:p :class "validation-error"
	     (:em (:span :class "validation-error-heading" "Error:&nbsp;") "Some error."))))
       (:li :class "address-ref"
	(:label
	 (:span :class "slot-name"
		(:span :class "extra" "Address:&nbsp;"))
	 (:select :name "address-ref"
		  (:option :value "" "[Select Address]")
		  (:option :value "*home-address*" :selected "selected" "Address"))))
       (:li :class "manager"
	(:label
	 (:span :class "slot-name"
		(:span :class "extra" "Manager:&nbsp;"))
	 (:input :type "text" :name "manager" :value "Jim" :maxlength "40"))))
     :preslots '((:div :class "validation-errors-summary"
		  (:h2 :class "error-count" "There is 1 validation error:")
		  (:ul (:li "Some error."))))))

