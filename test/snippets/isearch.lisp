
(in-package :weblocks-test)

;;; test render-isearch
(deftest-html render-isearch-1
    (with-request :get nil
      (render-isearch "some-search" (lambda (&rest args) nil) :form-id 'i1 :input-id 'i2 :search-id 'i3
		      :value "test"))
  (htm
   (:form :id "I1" :class "isearch" :action "/foo/bar" :method "get" :onsubmit
	  "initiateFormAction(\"abc123\", $(this), \"weblocks-session=1%3ATEST\"); return false;"
	  (:div :class "extra-top-1" "<!-- empty -->")
	  (:div :class "extra-top-2" "<!-- empty -->")
	  (:div :class "extra-top-3" "<!-- empty -->")
	  (:fieldset
	   (:input :type "text" :id "I2" :name "some-search" :class "search-bar" :value "test"
		   :maxlength "80")
	   (:input :id "I3" :name "submit" :type "submit" :class "submit" :value "Search")
	   (:input :name "action" :type "hidden" :value "abc123"))
	  (:div :class "extra-bottom-1" "<!-- empty -->")
	  (:div :class "extra-bottom-2" "<!-- empty -->")
	  (:div :class "extra-bottom-3" "<!-- empty -->"))
   (:script :type "text/javascript"
	    (fmt "~%// <![CDATA[~%")
	    (fmt "new Form.Element.DelayedObserver('I2', 0.4, function(elem, value) {initiateFormAction('abc123', $('I1'), 'weblocks-session=1%3ATEST');
});")
	    (fmt "~%// ]]>~%"))
   (:script :type "text/javascript"
	    (fmt "~%// <![CDATA[~%")
	    (fmt "$('I3').remove();")
	    (fmt "~%// ]]>~%"))))

(deftest-html render-isearch-2
    (with-request :get nil
      (make-request-ajax)
      (render-isearch "some-search" (lambda (&rest args) nil) :form-id 'i1 :input-id 'i2 :search-id 'i3
		      :value "test"))
  (htm
   (:form :id "I1" :class "isearch" :action "/foo/bar" :method "get" :onsubmit
	  "initiateFormAction(\"abc123\", $(this), \"weblocks-session=1%3ATEST\"); return false;"
	  (:div :class "extra-top-1" "<!-- empty -->")
	  (:div :class "extra-top-2" "<!-- empty -->")
	  (:div :class "extra-top-3" "<!-- empty -->")
	  (:fieldset
	   (:input :type "text" :id "I2" :name "some-search" :class "search-bar" :value "test"
		   :maxlength "80")
	   (:input :name "action" :type "hidden" :value "abc123"))
	  (:div :class "extra-bottom-1" "<!-- empty -->")
	  (:div :class "extra-bottom-2" "<!-- empty -->")
	  (:div :class "extra-bottom-3" "<!-- empty -->"))
   (:script :type "text/javascript"
	    (fmt "~%// <![CDATA[~%")
	    (fmt "new Form.Element.DelayedObserver('I2', 0.4, function(elem, value) {initiateFormAction('abc123', $('I1'), 'weblocks-session=1%3ATEST');
});")
	    (fmt "~%// ]]>~%"))))

;;; test isearch process in general
(deftest render-isearch-3
    (with-request :get nil
      (let (result
	    (*weblocks-output-stream* (make-string-output-stream)))
	(declare (special *weblocks-output-stream*))
	(render-isearch "some-search"
			(lambda (&key some-search &allow-other-keys)
			  (setf result some-search))
			:form-id 'i1 :input-id 'i2 :search-id 'i3)
	(do-request '(("action" . "abc123")
		      ("some-search" . "foo")))
	result))
  "foo")
