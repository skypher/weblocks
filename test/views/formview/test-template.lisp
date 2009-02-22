
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
			     (uri "/foo/bar") enctype (use-ajax-p t)
			     (data-class-name "employee")
			     form-id)
  `(:form :id ,form-id
	  :class ,(format nil "view form ~A" data-class-name)
	  :action ,uri :method ,method :enctype ,enctype
	  ,@(when use-ajax-p
		  `(:onsubmit ,(format nil "initiateFormAction(\"~A\", ~
                                                       $(this), ~
                                                       \"weblocks-session=1%3ATEST\"); ~
                                                       return false;"
				       (or action ""))))
	  (:div :class "extra-top-1" "<!-- empty -->")
	  (:div :class "extra-top-2" "<!-- empty -->")
	  (:div :class "extra-top-3" "<!-- empty -->")
	  (:fieldset 
	   (:h1 (:span :class "action" (str ,title-action))
		(:span :class "object" ,(humanize-name data-class-name)))
	   ,@preslots
	   (:h2 :class "form-fields-title" "Form fields:")
	   (:ul ,@body)
	   ,@postslots
	  (:input :name "action" :type "hidden" :value ,action))
	  (:div :class "extra-bottom-1" "<!-- empty -->")
	  (:div :class "extra-bottom-2" "<!-- empty -->")
	  (:div :class "extra-bottom-3" "<!-- empty -->")))

