
(in-package :weblocks-test)

;;; test render-form-slot for member
(deftest-html render-form-slot-member-1
    (render-form-slot nil 'test '(member 1 two 3) 1)
  (:li :class "test"
       (:span :class "label"
	      (:span :class "slot-name"
		     (:span :class "extra" "Test:&nbsp;")))
       (:label :class "radio first"
	       (:input :name "test" :type "radio" :class "radio" :value "1" :checked "checked")
	       (:span "1&nbsp;"))
       (:label :class "radio"
	       (:input :name "test" :type "radio" :class "radio" :value "two")
	       (:span "Two&nbsp;"))
       (:label :class "radio last"
	       (:input :name "test" :type "radio" :class "radio" :value "3")
	       (:span "3&nbsp;"))))

(deftest-html render-form-slot-member-2
    (render-form-slot nil 'test '(member 1 two 3) nil)
  (:li :class "test"
       (:span :class "label"
	      (:span :class "slot-name"
		     (:span :class "extra" "Test:&nbsp;")))
       (:label :class "radio first"
	       (:input :name "test" :type "radio" :class "radio" :value "1" :checked "checked")
	       (:span "1&nbsp;"))
       (:label :class "radio"
	       (:input :name "test" :type "radio" :class "radio" :value "two")
	       (:span "Two&nbsp;"))
       (:label :class "radio last"
	       (:input :name "test" :type "radio" :class "radio" :value "3")
	       (:span "3&nbsp;"))))
