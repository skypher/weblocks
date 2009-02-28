
(in-package :weblocks-test)

(deftest-html render-isearch-1
    (with-request :get nil
      (render-isearch "some-search" "some-action" :form-id 'i1 :input-id 'i2 :search-id 'i3
		      :value "test"))
  (htm
   (:form :id "I1" :class "isearch" :action "" :method "get" :onsubmit
	  "initiateFormAction(\"some-action\", $(this), \"weblocks-session=1%3Atest\"); return false;"
	  (:div :class "extra-top-1" "&nbsp;")
	  (:div :class "extra-top-2" "&nbsp;")
	  (:div :class "extra-top-3" "&nbsp;")
	  (:fieldset
	   (:input :type "text" :id "I2" :name "some-search" :class "search-bar" :value "test")
	   (:input :id "I3" :name "submit" :type "submit" :class "submit" :value "Search")
	   (:input :name "action" :type "hidden" :value "some-action"))
	  (:div :class "extra-bottom-1" "&nbsp;")
	  (:div :class "extra-bottom-2" "&nbsp;")
	  (:div :class "extra-bottom-3" "&nbsp;"))
   (:script :type "text/javascript"
	    (fmt "~%// <![CDATA[~%")
	    (fmt "new Form.Element.DelayedObserver('I2', 0.4, function(elem, value) {initiateFormAction('some-action', $('I1'), 'weblocks-session=1%3Atest');
});")
	    (fmt "~%// ]]>~%"))
   (:script :type "text/javascript"
	    (fmt "~%// <![CDATA[~%")
	    (fmt "$('I3').remove();")
	    (fmt "~%// ]]>~%"))))

(deftest-html render-isearch-2
    (with-request :get nil
      (make-request-ajax)
      (render-isearch "some-search" "some-action" :form-id 'i1 :input-id 'i2 :search-id 'i3
		      :value "test"))
  (htm
   (:form :id "I1" :class "isearch" :action "" :method "get" :onsubmit
	  "initiateFormAction(\"some-action\", $(this), \"weblocks-session=1%3Atest\"); return false;"
	  (:div :class "extra-top-1" "&nbsp;")
	  (:div :class "extra-top-2" "&nbsp;")
	  (:div :class "extra-top-3" "&nbsp;")
	  (:fieldset
	   (:input :type "text" :id "I2" :name "some-search" :class "search-bar" :value "test")
	   (:input :name "action" :type "hidden" :value "some-action"))
	  (:div :class "extra-bottom-1" "&nbsp;")
	  (:div :class "extra-bottom-2" "&nbsp;")
	  (:div :class "extra-bottom-3" "&nbsp;"))
   (:script :type "text/javascript"
	    (fmt "~%// <![CDATA[~%")
	    (fmt "new Form.Element.DelayedObserver('I2', 0.4, function(elem, value) {initiateFormAction('some-action', $('I1'), 'weblocks-session=1%3Atest');
});")
	    (fmt "~%// ]]>~%"))))
