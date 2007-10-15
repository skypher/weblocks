
(in-package :weblocks-test)

;;; test with-form-html macro
(deftest-html with-form-html-1
    (with-request :get nil
      (make-action #'identity "abc123")
      (with-html-form (:get "abc123")
	(:div "test1")
	(:div "test2")))
  (:form
   :action "/foo/bar"
   :method "get"
   :onsubmit "initiateFormAction(\"abc123\", $(this), \"weblocks-session=1%3ATEST\"); return false;"
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
      (make-action #'identity "abc123")
      (with-html-form (:get "abc123" :id "some-id" :class "some-class")
	(:div "test")))
  (:form
   :id "some-id"
   :class "some-class"
   :action "/foo/bar"
   :method "get"
   :onsubmit "initiateFormAction(\"abc123\", $(this), \"weblocks-session=1%3ATEST\"); return false;"
    (:div :class "extra-top-1" "<!-- empty -->")
    (:div :class "extra-top-2" "<!-- empty -->")
    (:div :class "extra-top-3" "<!-- empty -->")
    (:fieldset
     (:div "test")
     (:input :name "action" :type "hidden" :value "abc123"))
    (:div :class "extra-bottom-1" "<!-- empty -->")
    (:div :class "extra-bottom-2" "<!-- empty -->")
    (:div :class "extra-bottom-3" "<!-- empty -->")))

(deftest-html with-form-html-3
    (with-request :get nil
      (with-html-form (:get (lambda (&rest args) nil))
	(:div "test1")
	(:div "test2")))
  (:form
   :action "/foo/bar"
   :method "get"
   :onsubmit "initiateFormAction(\"abc123\", $(this), \"weblocks-session=1%3ATEST\"); return false;"
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

;;; testing render-link
(deftest-html render-link-1
    (with-request :get nil
      (make-action #'identity "abc123")
      (render-link "abc123" "some link"))
  #.(link-action-template "abc123" "some link"))

(deftest-html render-link-2
    (with-request :get nil
      (make-action #'identity "abc123")
      (render-link "abc123" "some link" :ajaxp nil))
  (:a :href "/foo/bar?action=abc123" "some link"))

(deftest-html render-link-3
    (with-request :get nil
      (render-link (lambda (&rest args)
		     nil) "some link"))
  #.(link-action-template "abc123" "some link"))

(deftest-html render-link-4
    (with-request :get nil
      (make-action #'identity "abc123")
      (render-link "abc123" "some link" :id "some-id" :class "some-class"))
  #.(link-action-template "abc123" "some link"
			  :id "some-id" :class "some-class"))

(deftest render-link-5
    (multiple-value-bind (res err)
	(ignore-errors
	  (with-request :get nil
	    (render-link "abc123" "some link")))
      (values res (format nil "~A" err)))
  nil "The value 'abc123' is not an existing action.")

;;; test render-button
(deftest-html render-button-1
    (render-button 'some-button)
  (:input :name "some-button" :type "submit" :class "submit" :value "Some Button"
	  :onclick "disableIrrelevantButtons(this);"))

(deftest-html render-button-2
    (render-button 'some-button :class "foo" :value "bar" :id "baz")
  (:input :name "some-button" :type "submit" :id "baz" :class "foo" :value "bar"
	  :onclick "disableIrrelevantButtons(this);"))

;;; test render-checkbox
(deftest-html render-checkbox-1
    (render-checkbox 'foo nil)
  (:input :name "foo" :type "checkbox" :class "checkbox" :value "t"))

(deftest-html render-checkbox-2
    (render-checkbox 'foo t :id "bar" :class "baz")
  (:input :name "foo" :type "checkbox" :id "bar" :class "baz" :value "t"
	  :checked "checked"))

;;; test render-dropdown
(deftest-html render-dropdown-1
    (render-dropdown 'some-name '("a" "b" "c"))
  (:select :name "some-name"
	   (:option "a")
	   (:option "b")
	   (:option "c")))

(deftest-html render-dropdown-2
    (render-dropdown 'some-name '("a") :id "some-id" :class "some-class")
  (:select :id "some-id" :class "some-class" :name "some-name"
	   (:option "a")))

(deftest-html render-dropdown-3
    (render-dropdown 'some-name '(("a" . "-a") ("b" . "-b")))
  (:select :name "some-name"
	   (:option :value "-a" "a")
	   (:option :value "-b" "b")))

(deftest-html render-dropdown-4
    (render-dropdown 'some-name '("a" "b" "c") :selected-value "b")
  (:select :name "some-name"
	   (:option "a")
	   (:option :selected "selected" "b")
	   (:option "c")))

(deftest-html render-dropdown-5
    (render-dropdown 'some-name '(("a" . "-a") ("b" . "-b") ("c" . "-c")) :selected-value "-b")
  (:select :name "some-name"
	   (:option :value "-a" "a")
	   (:option :value "-b" :selected "selected" "b")
	   (:option :value "-c" "c")))

(deftest-html render-dropdown-6
    (render-dropdown 'some-name '(("a" . "-a") ("b" . "-b") ("c" . "-c")) :selected-value "b")
  (:select :name "some-name"
	   (:option :value "-a" "a")
	   (:option :value "-b" "b")
	   (:option :value "-c" "c")))

(deftest-html render-dropdown-7
    (render-dropdown 'some-name '("a" "b" "c") :selected-value '("foo" "bar" "b"))
  (:select :name "some-name"
	   (:option "a")
	   (:option :selected "selected" "b")
	   (:option "c")))

(deftest-html render-dropdown-8
    (render-dropdown 'some-name '("a" "b" "c") :welcome-name "Value")
  (:select :name "some-name"
	   (:option :value "" "[Select Value]")
	   (:option "a")
	   (:option "b")
	   (:option "c")))

(deftest-html render-dropdown-9
    (render-dropdown 'some-name '("a" "b" "c") :welcome-name (cons "Value" "blah"))
  (:select :name "some-name"
	   (:option :value "blah" "[Select Value]")
	   (:option "a")
	   (:option "b")
	   (:option "c")))

(deftest-html render-dropdown-10
    (render-dropdown 'some-name '("a" "b" "c") :selected-value 'b)
  (:select :name "some-name"
	   (:option "a")
	   (:option :selected "selected" "b")
	   (:option "c")))

(deftest-html render-dropdown-11
    (render-dropdown 'some-name '(1 2 3) :selected-value "2")
  (:select :name "some-name"
	   (:option "1")
	   (:option :selected "selected" "2")
	   (:option "3")))

;;; test render-radio-buttons
(deftest-html render-radio-buttons-1
    (render-radio-buttons 'some-name '("a" "b" "c") :id "some-id" :class "some-class")
  (htm
   (:label :id "some-id" :class "some-class first"
	   (:input :name "some-name" :type "radio" :class "radio" :value "a" :checked "checked")
	   (:span "a&nbsp;"))
   (:label :id "some-id" :class "some-class"
	   (:input :name "some-name" :type "radio" :class "radio" :value "b")
	   (:span "b&nbsp;"))
   (:label :id "some-id" :class "some-class last"
	   (:input :name "some-name" :type "radio" :class "radio" :value "c")
	   (:span "c&nbsp;"))))

(deftest-html render-radio-buttons-2
    (render-radio-buttons 'some-name '("a" ("b" . "test") "c") :id "some-id"
			  :selected-value "test")
  (htm
   (:label :id "some-id" :class "radio first"
	   (:input :name "some-name" :type "radio" :class "radio" :value "a")
	   (:span "a&nbsp;"))
   (:label :id "some-id" :class "radio"
	   (:input :name "some-name" :type "radio" :class "radio" :value "test" :checked "checked")
	   (:span "b&nbsp;"))
   (:label :id "some-id" :class "radio last"
	   (:input :name "some-name" :type "radio" :class "radio" :value "c")
	   (:span "c&nbsp;"))))

;;; test render-close-button
(deftest-html render-close-button-1
    (with-request :get nil
      (make-action #'identity "abc123")
      (render-close-button "abc123"))
  (:span :class "close-button"
	 (:a :href "/foo/bar?action=abc123"
	     :onclick "initiateAction(\"abc123\", \"weblocks-session=1%3ATEST\"); return false;"
	     "(Close)")))

(deftest-html render-close-button-2
    (with-request :get nil
      (make-action #'identity "abc123")
      (render-close-button "abc123" "Foo"))
  (:span :class "close-button"
	 (:a :href "/foo/bar?action=abc123"
	     :onclick "initiateAction(\"abc123\", \"weblocks-session=1%3ATEST\"); return false;"
	     "Foo")))

