
(in-package :weblocks-test)

;;; test with-form-header
(deftest-html with-form-header-1
    (with-form-header *joe* (lambda () nil) :action "test-action" :method :post)
  #.(form-header-template "test-action" '(nil) :method "post"))

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
	(:input :name "submit" :type "submit" :value "Submit")
	(:input :name "cancel" :type "submit" :value "Cancel")
	(:input :name "action" :type "hidden" :value "abc123")))

;;; test render-form-slot
(deftest-html render-form-slot-1
    (render-form-slot *joe* 'first-name "Joe")
  (:li (:label
	(:span "First Name:&nbsp;")
	(:input :type "text" :name "first-name" :value "Joe"))))

(deftest-html render-form-slot-2
    (render-form-slot *joe* 'address-ref *home-address*)
  (:li (:label
	(:span "Address:&nbsp;")
	(:input :type "text" :name "address-ref" :value "ADDRESS"))))

(deftest-html render-form-slot-3
    (render-form-slot *joe* 'education *some-college*)
  (htm
   (:li (:label
	 (:span "University:&nbsp;")
	 (:input :type "text" :name "university" :value "Bene Gesserit University")))
   (:li (:label
	 (:span "Graduation Year:&nbsp;")
	 (:input :type "text" :name "graduation-year" :value "2000")))))

;;; test render-form
(deftest-html render-form-1
    (render-form "test")
  (:input :type "text" :value "test"))

(deftest-html render-form-2
    (render-form *joe* :action "abc123")
  #.(form-header-template "abc123"
     '((:li (:label (:span "Name:&nbsp;") (:input :type "text" :name "name" :value "Joe")))
       (:li (:label (:span "Manager:&nbsp;") (:input :type "text" :name "manager" :value "Jim"))))))

(deftest-html render-form-3
    (render-form *joe* :slots '(address-ref))
  #.(form-header-template nil
     '((:li (:label (:span "Name:&nbsp;") (:input :type "text" :name "name" :value "Joe")))
       (:li (:label (:span "Address:&nbsp;") (:input :type "text" :name "address-ref" :value "ADDRESS")))
       (:li (:label (:span "Manager:&nbsp;") (:input :type "text" :name "manager" :value "Jim"))))))

(deftest-html render-form-4
    (render-form *joe* :slots '(education) :preslots-fn (lambda (obj &rest keys)
							  (with-html
							    (:div "test1")))
					   :postslots-fn (lambda (obj &rest keys)
							   (with-html
							     (:div "test2"))))
  #.(form-header-template "abc123"
     '((:li (:label (:span "Name:&nbsp;") (:input :type "text" :name "name" :value "Joe")))
	   (:li (:label (:span "University:&nbsp;")
		 (:input :type "text" :name "university" :value "Bene Gesserit University")))
       (:li (:label (:span "Graduation Year:&nbsp;")
	     (:input :type "text" :name "graduation-year" :value "2000")))
       (:li (:label (:span "Manager:&nbsp;") (:input :type "text" :name "manager" :value "Jim"))))
     :preslots '((:div "test1"))
     :postslots '((:div "test2"))))

(deftest-html render-form-5
    (render-form *joe* :slots '(address-ref) :intermediate-fields '(("name" . "Bill")))
  #.(form-header-template nil
     '((:li (:label (:span "Name:&nbsp;") (:input :type "text" :name "name" :value "Bill")))
       (:li (:label (:span "Address:&nbsp;") (:input :type "text" :name "address-ref" :value "ADDRESS")))
       (:li (:label (:span "Manager:&nbsp;") (:input :type "text" :name "manager" :value "Jim"))))))

(deftest-html render-form-6
    (render-form *joe* :slots '((name . nickname)))
  #.(form-header-template nil
     '((:li (:label (:span "Nickname:&nbsp;") (:input :type "text" :name "name" :value "Joe")))
       (:li (:label (:span "Manager:&nbsp;") (:input :type "text" :name "manager" :value "Jim"))))))

(deftest-html render-form-7
    (render-form *joe* :slots '(address-ref) :validation-errors '(("name" . "Some error.")))
  #.(form-header-template nil
     '((:li :class "item-not-validated"
	(:label (:span "Name:&nbsp;") (:input :type "text" :name "name" :value "Joe")
	 (:p :class "validation-error"
	     (:em (:span :class "validation-error-heading" "Error:&nbsp;") "Some error."))))
       (:li (:label (:span "Address:&nbsp;") (:input :type "text" :name "address-ref" :value "ADDRESS")))
       (:li (:label (:span "Manager:&nbsp;") (:input :type "text" :name "manager" :value "Jim"))))
     :preslots '((:div :class "validation-errors-summary"
		  (:h2 :class "error-count" "There is 1 validation error:")
		  (:ul (:li "Some error."))))))
