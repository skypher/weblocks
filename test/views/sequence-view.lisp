
(in-package :weblocks-test)

(deftest-html render-object-view-impl-empty-sequence-1
    (render-object-view-impl nil (make-instance 'sequence-view) nil)
  (:div :class "view sequence empty"
	(:div :class "extra-top-1" "<!-- empty -->")
	(:div :class "extra-top-2" "<!-- empty -->")
	(:div :class "extra-top-3" "<!-- empty -->")
	(:p :class "user-message" (:span :class "message" "No information available."))
	(:div :class "extra-bottom-1" "<!-- empty -->")
	(:div :class "extra-bottom-2" "<!-- empty -->")
	(:div :class "extra-bottom-3" "<!-- empty -->")))

