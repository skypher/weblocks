
(in-package :weblocks-test)

;;; test with-form-html macro
(deftest-html with-form-html-1
    (with-request :get nil
      (with-html-form (:get "abc123")
	(:div "test1")
	(:div "test2")))
  (:form
   :action ""
   :method "get"
   :onsubmit "initiateFormAction(\"abc123\", $(this), \"weblocks-session=1%3Atest\"); return false;"
    (:div :class "extra-top-1" "&nbsp;")
    (:div :class "extra-top-2" "&nbsp;")
    (:div :class "extra-top-3" "&nbsp;")
    (:fieldset
     (:div "test1")
     (:div "test2")
     (:input :name "action" :type "hidden" :value "abc123"))
    (:div :class "extra-bottom-1" "&nbsp;")
    (:div :class "extra-bottom-2" "&nbsp;")
    (:div :class "extra-bottom-3" "&nbsp;")))

(deftest-html with-form-html-2
    (with-request :get nil
      (with-html-form (:get "abc123" :id "some-id" :class "some-class")
	(:div "test")))
  (:form
   :id "some-id"
   :class "some-class"
   :action ""
   :method "get"
   :onsubmit "initiateFormAction(\"abc123\", $(this), \"weblocks-session=1%3Atest\"); return false;"
    (:div :class "extra-top-1" "&nbsp;")
    (:div :class "extra-top-2" "&nbsp;")
    (:div :class "extra-top-3" "&nbsp;")
    (:fieldset
     (:div "test")
     (:input :name "action" :type "hidden" :value "abc123"))
    (:div :class "extra-bottom-1" "&nbsp;")
    (:div :class "extra-bottom-2" "&nbsp;")
    (:div :class "extra-bottom-3" "&nbsp;")))
