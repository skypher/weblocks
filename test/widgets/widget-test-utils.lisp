
(in-package :weblocks-test)

(defun searchbar-template (form-id search-id submit-id action &key hidden-items-text value)
  `(:div :class "datagrid-search-bar"
	 (:div :class "extra-top-1" "&nbsp;")
	 (:div :class "extra-top-2" "&nbsp;")
	 (:div :class "extra-top-3" "&nbsp;")
	 (:span :class "title"
		(:strong "Search table&nbsp;"))
	 ,(when hidden-items-text
	      `(htm (:span :class "hidden-items" ,hidden-items-text)))
	 (:form :id ,form-id :class "isearch" :action "" :method "get" :onsubmit
		,(format
		  nil "initiateFormAction(\"~A\", $(this), \"weblocks-session=1%3Atest\"); return false;"
		  action)
		(:div :class "extra-top-1" "&nbsp;")
		(:div :class "extra-top-2" "&nbsp;")
		(:div :class "extra-top-3" "&nbsp;")
		(:fieldset
		 (:input :type "text" :id ,search-id :name "search" :class "search-bar" :value ,value)
		 (:input :id ,submit-id :name "submit" :type "submit" :class "submit" :value "Search")
		 (:input :name "action" :type "hidden" :value ,action))
		(:div :class "extra-bottom-1" "&nbsp;")
		(:div :class "extra-bottom-2" "&nbsp;")
		(:div :class "extra-bottom-3" "&nbsp;"))
	 (:script :type "text/javascript"
		  (fmt "~%// <![CDATA[~%")
		  (fmt "new Form.Element.DelayedObserver('~A', 0.4, function(elem, value) {initiateFormAction('~A', $('~A'), 'weblocks-session=1%3Atest');
});" ,search-id ,action ,form-id)
		  (fmt "~%// ]]>~%"))
	 (:script :type "text/javascript"
		  (fmt "~%// <![CDATA[~%")
		  (fmt "$('~A').remove();" ,submit-id)
		  (fmt "~%// ]]>~%"))
	 (:div :class "extra-bottom-1" "&nbsp;")
	 (:div :class "extra-bottom-2" "&nbsp;")
	 (:div :class "extra-bottom-3" "&nbsp;")))
