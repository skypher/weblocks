
(in-package :weblocks-test)

;;; utilities for easier testing
(defun pagination-goto-form-template (action &key (page-one-p nil) (validatedp t) (uri "/foo/bar"))
  `(:form :action ,uri :method "get"
	  :onsubmit ,(format nil "initiateFormAction(\"~A\", ~
                                                       $(this), ~
                                                       \"weblocks-session=1%3ATEST\"); ~
                                                       return false;"
			     (or action ""))
	  (:div :class "extra-top-1" "<!-- empty -->")
	  (:div :class "extra-top-2" "<!-- empty -->")
	  (:div :class "extra-top-3" "<!-- empty -->")
	  (:fieldset 
	   (:label (:span "Go to page:&nbsp;")
		   (:input :name "page-number"
			   :class ,(concatenate 'string "page-number"
						(unless validatedp
						  " item-not-validated"))
			   :onfocus ,(format nil "~A~A"
					     "$(this).removeClassName(\"item-not-validated\");"
					     (if page-one-p
						 ""
						 "if(this.value == \"1\") { this.value = \"\"; }"))
			   :onblur ,(unless page-one-p "if(this.value == \"\") { this.value = \"1\"; }")
			   :value ,(unless page-one-p "1")))
	   (:input :name "go-to-page" :type "submit" :class "submit"
		   :value "Go" :onclick "disableIrrelevantButtons(this);")
	   (:input :name "action" :type "hidden" :value ,action))
	  (:div :class "extra-bottom-1" "<!-- empty -->")
	  (:div :class "extra-bottom-2" "<!-- empty -->")
	  (:div :class "extra-bottom-3" "<!-- empty -->")))

(defun pagination-page-info-template (current-page total-pages)
  `(:span :class "page-info"
	  (:span :class "viewing-label" "Viewing ")
	  (:span :class "page-label" "Page ")
	  (:span :class "current-page" (:strong (str ,current-page)))
	  (:span :class "of-label" " of ")
	  (:span :class "total-pages" (str ,total-pages))))
