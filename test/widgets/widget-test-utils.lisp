
(in-package :weblocks-test)

(defun searchbar-template (form-id search-id submit-id action &key total-items-text value
			   (uri "/foo/bar"))
  `(:div :class "datagrid-search-bar"
	 (:div :class "extra-top-1" "<!-- empty -->")
	 (:div :class "extra-top-2" "<!-- empty -->")
	 (:div :class "extra-top-3" "<!-- empty -->")
	 (:span :class "title"
		(:strong "Search table&nbsp;"))
	 ,(when total-items-text
	      `(htm (:span :class "total-items" ,total-items-text)))
	 (:form :id ,form-id :class "isearch" :action ,uri :method "get" :onsubmit
		,(format
		  nil "initiateFormAction(\"~A\", $(this), \"weblocks-session=1%3ATEST\"); return false;"
		  action)
		(:div :class "extra-top-1" "<!-- empty -->")
		(:div :class "extra-top-2" "<!-- empty -->")
		(:div :class "extra-top-3" "<!-- empty -->")
		(:fieldset
		 (:input :type "text" :id ,search-id :name "search" :class "search-bar" :value ,value
			 :maxlength "80")
		 (:input :id ,submit-id :name "submit" :type "submit" :class "submit" :value "Search")
		 (:input :name "action" :type "hidden" :value ,action))
		(:div :class "extra-bottom-1" "<!-- empty -->")
		(:div :class "extra-bottom-2" "<!-- empty -->")
		(:div :class "extra-bottom-3" "<!-- empty -->"))
	 (:script :type "text/javascript"
		  (fmt "~%// <![CDATA[~%")
		  (fmt "new Form.Element.DelayedObserver('~A', 0.4, function(elem, value) {initiateFormAction('~A', $('~A'), 'weblocks-session=1%3ATEST');
});" ,search-id ,action ,form-id)
		  (fmt "~%// ]]>~%"))
	 (:script :type "text/javascript"
		  (fmt "~%// <![CDATA[~%")
		  (fmt "$('~A').remove();" ,submit-id)
		  (fmt "~%// ]]>~%"))
	 (:div :class "extra-bottom-1" "<!-- empty -->")
	 (:div :class "extra-bottom-2" "<!-- empty -->")
	 (:div :class "extra-bottom-3" "<!-- empty -->")))
