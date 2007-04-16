
(in-package :weblocks-test)

;;; test with-form-header
(deftest-html with-form-header-1
    (with-form-header *joe* (lambda () nil) :action "abc123" :method :post)
  (:form :class "renderer form employee" :action "" :method "post"
	 (:div :class "extra-top-1" "&nbsp;")
	 (:div :class "extra-top-2" "&nbsp;")
	 (:div :class "extra-top-3" "&nbsp;")
	 (:fieldset 
	  (:h1 (:span :class "action" "Modifying:&nbsp;")
	       (:span :class "object" "Employee"))
	  (:ul nil)
	  (:div :class "submit"
		(:input :name "action" :type "hidden" :value "abc123")
		(:input :name "submit" :type "submit" :value "Submit")
		(:input :name "cancel" :type "submit" :value "Cancel")))
	 (:div :class "extra-bottom-1" "&nbsp;")
	 (:div :class "extra-bottom-2" "&nbsp;")
	 (:div :class "extra-bottom-3" "&nbsp;")))

;;; test render-form-controls
(deftest-html render-form-controls-1
    (render-form-controls *joe* :action "abc123")
  (:div :class "submit"
	(:input :name "action" :type "hidden" :value "abc123")
	(:input :name "submit" :type "submit" :value "Submit")
	(:input :name "cancel" :type "submit" :value "Cancel")))

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
  (:form :class "renderer form employee" :action "" :method "get"
	 (:div :class "extra-top-1" "&nbsp;")
	 (:div :class "extra-top-2" "&nbsp;")
	 (:div :class "extra-top-3" "&nbsp;")
	 (:fieldset 
	  (:h1 (:span :class "action" "Modifying:&nbsp;")
	       (:span :class "object" "Employee"))
	  (:ul
	   (:li (:label (:span "Name:&nbsp;") (:input :type "text" :name "name" :value "Joe")))
	   (:li (:label (:span "Manager:&nbsp;") (:input :type "text" :name "manager" :value "Jim"))))
	  (:div :class "submit"
		(:input :name "action" :type "hidden" :value "abc123")
		(:input :name "submit" :type "submit" :value "Submit")
		(:input :name "cancel" :type "submit" :value "Cancel")))
	 (:div :class "extra-bottom-1" "&nbsp;")
	 (:div :class "extra-bottom-2" "&nbsp;")
	 (:div :class "extra-bottom-3" "&nbsp;")))

(deftest-html render-form-3
    (render-form *joe* :slots '(address-ref))
  (:form :class "renderer form employee" :action "" :method "get"
	 (:div :class "extra-top-1" "&nbsp;")
	 (:div :class "extra-top-2" "&nbsp;")
	 (:div :class "extra-top-3" "&nbsp;")
	 (:fieldset 
	  (:h1 (:span :class "action" "Modifying:&nbsp;")
	       (:span :class "object" "Employee"))
	  (:ul
	   (:li (:label (:span "Name:&nbsp;") (:input :type "text" :name "name" :value "Joe")))
	   (:li (:label (:span "Address:&nbsp;") (:input :type "text" :name "address-ref" :value "ADDRESS")))
	   (:li (:label (:span "Manager:&nbsp;") (:input :type "text" :name "manager" :value "Jim"))))
	  (:div :class "submit"
		(:input :name "action" :type "hidden")
		(:input :name "submit" :type "submit" :value "Submit")
		(:input :name "cancel" :type "submit" :value "Cancel")))
	 (:div :class "extra-bottom-1" "&nbsp;")
	 (:div :class "extra-bottom-2" "&nbsp;")
	 (:div :class "extra-bottom-3" "&nbsp;")))

(deftest-html render-form-4
    (render-form *joe* :slots '(education) :preslots-fn (lambda (obj &rest keys)
							  (with-html
							    (:div "test1")))
					   :postslots-fn (lambda (obj &rest keys)
							   (with-html
							     (:div "test2"))))
  (:form :class "renderer form employee" :action "" :method "get"
	 (:div :class "extra-top-1" "&nbsp;")
	 (:div :class "extra-top-2" "&nbsp;")
	 (:div :class "extra-top-3" "&nbsp;")
	 (:fieldset 
	  (:h1 (:span :class "action" "Modifying:&nbsp;")
	       (:span :class "object" "Employee"))
	  (:div "test1")
	  (:ul
	   (:li (:label (:span "Name:&nbsp;") (:input :type "text" :name "name" :value "Joe")))
	   (:li (:label (:span "University:&nbsp;")
			(:input :type "text" :name "university" :value "Bene Gesserit University")))
	   (:li (:label (:span "Graduation Year:&nbsp;")
			(:input :type "text" :name "graduation-year" :value "2000")))
	   (:li (:label (:span "Manager:&nbsp;") (:input :type "text" :name "manager" :value "Jim"))))
	  (:div "test2"))
	 (:div :class "extra-bottom-1" "&nbsp;")
	 (:div :class "extra-bottom-2" "&nbsp;")
	 (:div :class "extra-bottom-3" "&nbsp;")))
