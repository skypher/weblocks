
(in-package :weblocks-test)

;;; utilities for easier testing
(defun form-header-template (action body &key (method "get") (title-action "Modifying:&nbsp;") preslots
			     (postslots `((:div :class "submit"
						(:input :name "submit" :type "submit" :class "submit"
										      :value "Submit"
							:onclick "disableIrrelevantButtons(this);")
						(:input :name "cancel" :type "submit"
								       :class "submit cancel" :value "Cancel"
							:onclick "disableIrrelevantButtons(this);"))))
			     (uri "/foo/bar"))
  `(:form :class "renderer form employee" :action ,uri :method ,method
	  :onsubmit ,(format nil "initiateFormAction(\"~A\", ~
                                                       $(this), ~
                                                       \"weblocks-session=1%3Atest\"); ~
                                                       return false;"
			     (or action ""))
	  (:div :class "extra-top-1" "&nbsp;")
	  (:div :class "extra-top-2" "&nbsp;")
	  (:div :class "extra-top-3" "&nbsp;")
	  (:fieldset 
	   (:h1 (:span :class "action" (str ,title-action))
		(:span :class "object" "Employee"))
	   ,@preslots
	   (:h2 :class "form-fields-title" "Form fields:")
	   (:ul ,@body)
	   ,@postslots
	  (:input :name "action" :type "hidden" :value ,action))
	  (:div :class "extra-bottom-1" "&nbsp;")
	  (:div :class "extra-bottom-2" "&nbsp;")
	  (:div :class "extra-bottom-3" "&nbsp;")))

