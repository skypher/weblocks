
(in-package :weblocks-test)

;;; test render-data
(deftest-html render-data-1
    (render-data "test")
  (:span "test"))

(deftest-html render-data-2
    (render-data *joe*)
  (:div :class "data employee"
	(:div :class "extra-top-1" "&nbsp;")
	(:div :class "extra-top-2" "&nbsp;")
	(:div :class "extra-top-3" "&nbsp;")
	(:h1 (:span :class "action" "Viewing:&nbsp;")
	     (:span :class "object" "Employee"))
	(:ul
	 (:li (:h2 "Name:") (:span "Joe"))
	 (:li (:h2 "Manager:") (:span "Jim")))
	(:div :class "extra-bottom-1" "&nbsp;")
	(:div :class "extra-bottom-2" "&nbsp;")
	(:div :class "extra-bottom-3" "&nbsp;")))

(deftest-html render-data-3
    (render-data *joe* :slots '(address-ref))
  (:div :class "data employee"
	(:div :class "extra-top-1" "&nbsp;")
	(:div :class "extra-top-2" "&nbsp;")
	(:div :class "extra-top-3" "&nbsp;")
	(:h1 (:span :class "action" "Viewing:&nbsp;")
	     (:span :class "object" "Employee"))
	(:ul
	 (:li (:h2 "Name:") (:span "Joe"))
	 (:li (:h2 "Address:") (:span "ADDRESS"))
	 (:li (:h2 "Manager:") (:span "Jim")))
	(:div :class "extra-bottom-1" "&nbsp;")
	(:div :class "extra-bottom-2" "&nbsp;")
	(:div :class "extra-bottom-3" "&nbsp;")))

(deftest-html render-data-4
    (render-data *joe* :slots '(education))
  (:div :class "data employee"
	(:div :class "extra-top-1" "&nbsp;")
	(:div :class "extra-top-2" "&nbsp;")
	(:div :class "extra-top-3" "&nbsp;")
	(:h1 (:span :class "action" "Viewing:&nbsp;")
	     (:span :class "object" "Employee"))
	(:ul
	 (:li (:h2 "Name:") (:span "Joe"))
	 (:li (:h2 "University:") (:span "Bene Gesserit University"))
	 (:li (:h2 "Graduation Year:") (:span "2000"))
	 (:li (:h2 "Manager:") (:span "Jim")))
	(:div :class "extra-bottom-1" "&nbsp;")
	(:div :class "extra-bottom-2" "&nbsp;")
	(:div :class "extra-bottom-3" "&nbsp;")))

