
(in-package :weblocks-test)

;;; test with-data-header
(deftest-html with-data-header-1
    (with-data-header *joe* (lambda () nil))
  #.(data-header-template nil '(nil)
			  :postslots nil))

;;; test render-data-slot
(deftest-html render-data-slot-1
    (render-data-slot *joe* 'first-name "Joe")
  (:li :class "first-name"
       (:span :class "label"
	      "First Name:&nbsp;")
       (:span :class "value"
	      "Joe")))

(deftest-html render-data-slot-2
    (render-data-slot *joe* 'address-ref *home-address*)
  (:li :class "address-ref"
       (:span :class "label"
	      "Address:&nbsp;")
       (:span :class "value"
	      "Address")))

(deftest-html render-data-slot-3
    (render-data-slot *joe* 'education *some-college*)
  (htm
   (:li :class "university"
	(:span :class "label" "University:&nbsp;")
	(:span :class "value" "Bene Gesserit University"))
   (:li :class "graduation-year"
	(:span :class "label" "Graduation Year:&nbsp;")
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
  #.(data-header-template nil
     '((:li :class "name" (:span :class "label" "Name:&nbsp;") (:span :class "value" "Joe"))
       (:li :class "manager" (:span :class "label" "Manager:&nbsp;") (:span :class "value" "Jim")))
     :preslots '((:div "test1"))
     :postslots '((:div "test2"))))

(deftest-html render-data-3
    (render-data *joe* :slots '(address-ref))
  #.(data-header-template nil
     '((:li :class "name" (:span :class "label" "Name:&nbsp;") (:span :class "value" "Joe"))
       (:li :class "address-ref" (:span :class "label" "Address:&nbsp;") (:span :class "value" "Address"))
       (:li :class "manager" (:span :class "label" "Manager:&nbsp;") (:span :class "value" "Jim")))
     :postslots nil))

(deftest-html render-data-4
    (render-data *joe* :slots '(education))
  #.(data-header-template nil
     '((:li :class "name" (:span :class "label" "Name:&nbsp;") (:span :class "value" "Joe"))
       (:li :class "university" (:span :class "label" "University:&nbsp;")
	(:span :class "value" "Bene Gesserit University"))
       (:li :class "graduation-year"
	(:span :class "label" "Graduation Year:&nbsp;") (:span :class "value" "2000"))
       (:li :class "manager" (:span :class "label" "Manager:&nbsp;") (:span :class "value" "Jim")))
     :postslots nil))

(deftest-html render-data-5
    (render-data *joe* :slots '((name . nickname)))
  #.(data-header-template nil
     '((:li :class "name" (:span :class "label" "Nickname:&nbsp;") (:span :class "value" "Joe"))
       (:li :class "manager" (:span :class "label" "Manager:&nbsp;") (:span :class "value" "Jim")))
     :postslots nil))

(deftest-html render-data-6
    (render-data "test" :highlight ".s")
  (:span :class "value" "t<strong>es</strong>t"))

(deftest-html render-data-7
    (render-data "te<st" :highlight "e<s")
  (:span :class "value" "t<strong>e&lt;s</strong>t"))

(deftest-html render-data-8
    (render-data "<script>alert('XSS Attack')</script>")
  (:span :class "value"
	 "&lt;script&gt;alert(&#039;XSS Attack&#039;)&lt;/script&gt;"))

;;; test highlight-regex-matches
(deftest highlight-regex-matches-1
    (weblocks::highlight-regex-matches "Hello World!" "el")
  "H<strong>el</strong>lo World!")

(deftest highlight-regex-matches-2
    (weblocks::highlight-regex-matches "" "el")
  "")

(deftest highlight-regex-matches-3
    (weblocks::highlight-regex-matches "te<st" "el")
  "te&lt;st")

(deftest highlight-regex-matches-4
    (weblocks::highlight-regex-matches "te<st" "")
  "te&lt;st")

(deftest highlight-regex-matches-5
    (weblocks::highlight-regex-matches "ello" "el")
  "<strong>el</strong>lo")

(deftest highlight-regex-matches-6
    (weblocks::highlight-regex-matches "ello" "lo")
  "el<strong>lo</strong>")

(deftest highlight-regex-matches-7
    (weblocks::highlight-regex-matches "te<st" "e<s")
  "t<strong>e&lt;s</strong>t")

