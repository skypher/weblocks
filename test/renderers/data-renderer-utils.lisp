
(in-package :weblocks-test)

;;; utilities for easier testing
(defun data-header-template (body &key preslots postslots)
  `(:div :class "renderer data employee"
	(:div :class "extra-top-1" "&nbsp;")
	(:div :class "extra-top-2" "&nbsp;")
	(:div :class "extra-top-3" "&nbsp;")
	(:h1 (:span :class "action" "Viewing:&nbsp;")
	     (:span :class "object" "Employee"))
	,@preslots
	(:ul ,@body)
	,@postslots
	(:div :class "extra-bottom-1" "&nbsp;")
	(:div :class "extra-bottom-2" "&nbsp;")
	(:div :class "extra-bottom-3" "&nbsp;")))

