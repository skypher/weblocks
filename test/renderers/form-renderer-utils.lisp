
(in-package :weblocks-test)

;;; utilities for easier testing
(defun form-header-template (action body &key (method "get") preslots
			     (postslots `((:div :class "submit"
						(:input :name "submit" :type "submit" :value "Submit"
							:onclick "disableIrrelevantButtons(this);")
						(:input :name "cancel" :type "submit" :value "Cancel"
							:onclick "disableIrrelevantButtons(this);")
						(:input :name "action" :type "hidden" :value ,action)))))
  `(:form :class "renderer form employee" :action "" :method ,method
	  :onsubmit ,(format nil "initiateFormAction(\"~A\", ~
                                                       $(this), ~
                                                       \"weblocks-session=1%3Atest\"); ~
                                                       return false;"
			     action)
	  (:div :class "extra-top-1" "&nbsp;")
	  (:div :class "extra-top-2" "&nbsp;")
	  (:div :class "extra-top-3" "&nbsp;")
	  (:fieldset 
	   (:h1 (:span :class "action" "Modifying:&nbsp;")
		(:span :class "object" "Employee"))
	   ,@preslots
	   (:h2 :class "form-fields-title" "Form fields:")
	   (:ul ,@body)
	   ,@postslots)
	  (:div :class "extra-bottom-1" "&nbsp;")
	  (:div :class "extra-bottom-2" "&nbsp;")
	  (:div :class "extra-bottom-3" "&nbsp;")))

