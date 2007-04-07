
(in-package :weblocks-test)

;;; test with-form-header
(deftest-html with-form-header-1
    (with-form-header *joe* (lambda () nil))
  (:form :class "renderer form employee" :action "#" :method "post"
	 (:div :class "extra-top-1" "&nbsp;")
	 (:div :class "extra-top-2" "&nbsp;")
	 (:div :class "extra-top-3" "&nbsp;")
	 (:fieldset 
	  (:h1 (:span :class "action" "Modifying:&nbsp;")
	       (:span :class "object" "Employee"))
	  (:ul
	   (:li :class "submit"
		(:input :name "ok" :type "submit" :value "Submit")
		(:input :name "cancel" :type "submit" :value "Cancel"))))
	 (:div :class "extra-bottom-1" "&nbsp;")
	 (:div :class "extra-bottom-2" "&nbsp;")
	 (:div :class "extra-bottom-3" "&nbsp;")))

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
    (render-form *joe*)
  (:form :class "renderer form employee" :action "#" :method "post"
	 (:div :class "extra-top-1" "&nbsp;")
	 (:div :class "extra-top-2" "&nbsp;")
	 (:div :class "extra-top-3" "&nbsp;")
	 (:fieldset 
	  (:h1 (:span :class "action" "Modifying:&nbsp;")
	       (:span :class "object" "Employee"))
	  (:ul
	   (:li (:label (:span "Name:&nbsp;") (:input :type "text" :name "name" :value "Joe")))
	   (:li (:label (:span "Manager:&nbsp;") (:input :type "text" :name "manager" :value "Jim")))
	   (:li :class "submit"
		(:input :name "ok" :type "submit" :value "Submit")
		(:input :name "cancel" :type "submit" :value "Cancel"))))
	 (:div :class "extra-bottom-1" "&nbsp;")
	 (:div :class "extra-bottom-2" "&nbsp;")
	 (:div :class "extra-bottom-3" "&nbsp;")))

(deftest-html render-form-3
    (render-form *joe* :slots '(address-ref))
  (:form :class "renderer form employee" :action "#" :method "post"
	 (:div :class "extra-top-1" "&nbsp;")
	 (:div :class "extra-top-2" "&nbsp;")
	 (:div :class "extra-top-3" "&nbsp;")
	 (:fieldset 
	  (:h1 (:span :class "action" "Modifying:&nbsp;")
	       (:span :class "object" "Employee"))
	  (:ul
	   (:li (:label (:span "Name:&nbsp;") (:input :type "text" :name "name" :value "Joe")))
	   (:li (:label (:span "Address:&nbsp;") (:input :type "text" :name "address-ref" :value "ADDRESS")))
	   (:li (:label (:span "Manager:&nbsp;") (:input :type "text" :name "manager" :value "Jim")))
	   (:li :class "submit"
		(:input :name "ok" :type "submit" :value "Submit")
		(:input :name "cancel" :type "submit" :value "Cancel"))))
	 (:div :class "extra-bottom-1" "&nbsp;")
	 (:div :class "extra-bottom-2" "&nbsp;")
	 (:div :class "extra-bottom-3" "&nbsp;")))

(deftest-html render-form-4
    (render-form *joe* :slots '(education))
  (:form :class "renderer form employee" :action "#" :method "post"
	 (:div :class "extra-top-1" "&nbsp;")
	 (:div :class "extra-top-2" "&nbsp;")
	 (:div :class "extra-top-3" "&nbsp;")
	 (:fieldset 
	  (:h1 (:span :class "action" "Modifying:&nbsp;")
	       (:span :class "object" "Employee"))
	  (:ul
	   (:li (:label (:span "Name:&nbsp;") (:input :type "text" :name "name" :value "Joe")))
	   (:li (:label (:span "University:&nbsp;")
			(:input :type "text" :name "university" :value "Bene Gesserit University")))
	   (:li (:label (:span "Graduation Year:&nbsp;")
			(:input :type "text" :name "graduation-year" :value "2000")))
	   (:li (:label (:span "Manager:&nbsp;") (:input :type "text" :name "manager" :value "Jim")))
	   (:li :class "submit"
		(:input :name "ok" :type "submit" :value "Submit")
		(:input :name "cancel" :type "submit" :value "Cancel"))))
	 (:div :class "extra-bottom-1" "&nbsp;")
	 (:div :class "extra-bottom-2" "&nbsp;")
	 (:div :class "extra-bottom-3" "&nbsp;")))
