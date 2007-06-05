
(in-package :weblocks-test)

(deftest-html render-isearch-1
    (with-request :get nil
      (render-isearch "some-search" "some-action" :form-id 'i1 :input-id 'i2 :search-id 'i3))
  (htm
   (:form :id "I1" :class "isearch" :action "" :method "get"
	  (:input :type "text" :id "I2" :name "some-search" :class "search-bar")
	  (:input :id "I3" :name "submit" :type "submit" :class "submit" :value "Search")
	  (:input :name "action" :type "hidden" :value "some-action"))
   (:script :type "text/javascript" :language "javascript"
	    (fmt "~%// <![CDATA[~%")
	    (fmt "new Form.Element.DelayedObserver('I2', 0.4, function(elem, value) {initiateFormAction('some-action', $('I1'), 'weblocks-session=1%3Atest');
});$('I3').hide();")
	    (fmt "~%// ]]~%"))))
