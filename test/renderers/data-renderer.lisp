
(in-package :weblocks-test)

;;; test with-data-header
(deftest-html with-data-header-1
    (with-data-header *joe* (lambda () nil))
  #.(data-header-template '(nil)))

;;; test render-data-slot
(deftest-html render-data-slot-1
    (render-data-slot *joe* 'first-name "Joe")
  (:li (:span :class "label"
	      "First Name:&nbsp;")
       (:span :class "value"
	      "Joe")))

(deftest-html render-data-slot-2
    (render-data-slot *joe* 'address-ref *home-address*)
  (:li (:span :class "label"
	      "Address:&nbsp;")
       (:span :class "value"
	      "ADDRESS")))

(deftest-html render-data-slot-3
    (render-data-slot *joe* 'education *some-college*)
  (htm
   (:li (:span :class "label" "University:&nbsp;")
	(:span :class "value" "Bene Gesserit University"))
   (:li (:span :class "label" "Graduation Year:&nbsp;")
	(:span :class "value" "2000"))))

;;; test render-data
(deftest-html render-data-1
    (render-data "test")
  (:span :class "value" "test"))

(deftest-html render-data-2
    (render-data *joe* :preslots-fn (lambda (obj &rest keys)
							  (with-html
							    (:div "test1")))
		       :postslots-fn (lambda (obj &rest keys)
				       (with-html
					 (:div "test2"))))
  #.(data-header-template
     '((:li (:span :class "label" "Name:&nbsp;") (:span :class "value" "Joe"))
       (:li (:span :class "label" "Manager:&nbsp;") (:span :class "value" "Jim")))
     :preslots '((:div "test1"))
     :postslots '((:div "test2"))))

(deftest-html render-data-3
    (render-data *joe* :slots '(address-ref))
  #.(data-header-template
     '((:li (:span :class "label" "Name:&nbsp;") (:span :class "value" "Joe"))
       (:li (:span :class "label" "Address:&nbsp;") (:span :class "value" "ADDRESS"))
       (:li (:span :class "label" "Manager:&nbsp;") (:span :class "value" "Jim")))))

(deftest-html render-data-4
    (render-data *joe* :slots '(education))
  #.(data-header-template
     '((:li (:span :class "label" "Name:&nbsp;") (:span :class "value" "Joe"))
       (:li (:span :class "label" "University:&nbsp;")
	(:span :class "value" "Bene Gesserit University"))
       (:li (:span :class "label" "Graduation Year:&nbsp;") (:span :class "value" "2000"))
       (:li (:span :class "label" "Manager:&nbsp;") (:span :class "value" "Jim")))))

(deftest-html render-data-5
    (render-data *joe* :slots '((name . nickname)))
  #.(data-header-template
     '((:li (:span :class "label" "Nickname:&nbsp;") (:span :class "value" "Joe"))
       (:li (:span :class "label" "Manager:&nbsp;") (:span :class "value" "Jim")))))
