
(in-package :weblocks-test)

;;; utilities for easier testing
(defun data-header-template (action body &key (data-class-name "employee") preslots
			     (postslots `((:div :class "submit"
						,(link-action-template action "Modify"
								       :class "modify")))))
  `(:div :class ,(format nil "view data ~(~A~)" data-class-name)
	(:div :class "extra-top-1" "<!-- empty -->")
	(:div :class "extra-top-2" "<!-- empty -->")
	(:div :class "extra-top-3" "<!-- empty -->")
	(:h1 (:span :class "action" "Viewing:&nbsp;")
	     (:span :class "object" ,(humanize-name data-class-name)))
	,@preslots
	(:ul ,@body)
	,@postslots
	(:div :class "extra-bottom-1" "<!-- empty -->")
	(:div :class "extra-bottom-2" "<!-- empty -->")
	(:div :class "extra-bottom-3" "<!-- empty -->")))

