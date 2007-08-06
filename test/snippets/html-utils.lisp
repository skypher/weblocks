
(in-package :weblocks-test)

;;; test with-form-html macro
(deftest-html with-form-html-1
    (with-request :get nil
      (with-html-form (:get "abc123")
	(:div "test1")
	(:div "test2")))
  (:form
   :action "/foo/bar"
   :method "get"
   :onsubmit "initiateFormAction(\"abc123\", $(this), \"weblocks-session=1%3Atest\"); return false;"
    (:div :class "extra-top-1" "<!-- empty -->")
    (:div :class "extra-top-2" "<!-- empty -->")
    (:div :class "extra-top-3" "<!-- empty -->")
    (:fieldset
     (:div "test1")
     (:div "test2")
     (:input :name "action" :type "hidden" :value "abc123"))
    (:div :class "extra-bottom-1" "<!-- empty -->")
    (:div :class "extra-bottom-2" "<!-- empty -->")
    (:div :class "extra-bottom-3" "<!-- empty -->")))

(deftest-html with-form-html-2
    (with-request :get nil
      (with-html-form (:get "abc123" :id "some-id" :class "some-class")
	(:div "test")))
  (:form
   :id "some-id"
   :class "some-class"
   :action "/foo/bar"
   :method "get"
   :onsubmit "initiateFormAction(\"abc123\", $(this), \"weblocks-session=1%3Atest\"); return false;"
    (:div :class "extra-top-1" "<!-- empty -->")
    (:div :class "extra-top-2" "<!-- empty -->")
    (:div :class "extra-top-3" "<!-- empty -->")
    (:fieldset
     (:div "test")
     (:input :name "action" :type "hidden" :value "abc123"))
    (:div :class "extra-bottom-1" "<!-- empty -->")
    (:div :class "extra-bottom-2" "<!-- empty -->")
    (:div :class "extra-bottom-3" "<!-- empty -->")))

;;; testing render-link
(deftest-html render-link-1
    (with-request :get nil
      (render-link "abc123" "some link"))
  #.(link-action-template "abc123" "some link"))

