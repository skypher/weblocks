
(in-package :weblocks-test)

;;; utilities for easier testing
(defun data-header-template (action body &key (data-class-name "employee") preslots
			     (postslots `((:div :class "submit"
						,(link-action-template action "Modify")))))
  `(:div :class ,(format nil "renderer data ~A" data-class-name)
	(:div :class "extra-top-1" "&nbsp;")
	(:div :class "extra-top-2" "&nbsp;")
	(:div :class "extra-top-3" "&nbsp;")
	(:h1 (:span :class "action" "Viewing:&nbsp;")
	     (:span :class "object" ,(humanize-name data-class-name)))
	,@preslots
	(:ul ,@body)
	,@postslots
	(:div :class "extra-bottom-1" "&nbsp;")
	(:div :class "extra-bottom-2" "&nbsp;")
	(:div :class "extra-bottom-3" "&nbsp;")))

