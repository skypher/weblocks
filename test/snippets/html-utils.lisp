
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

(deftest-html with-form-html-4
    (with-request :get nil
      (make-action #'identity "abc123")
      (with-html-form (:get "abc123" :use-ajax-p nil)
        (:div "test1")
        (:div "test2")))
  (:form
   :action "/foo/bar"
   :method "get"
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

(deftest-html with-form-html-5
    (with-request :get nil
      (make-action #'identity "abc123")
      (with-html-form (:get "abc123" :enctype "fooenc")
        (:div "test1")
        (:div "test2")))
  (:form
   :action "/foo/bar"
   :method "get"
   :enctype "fooenc"
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

(deftest-html with-form-html-6
    (with-request :get nil
      (make-action #'identity "abc123")
      (with-html-form (:get "abc123" :enctype "fooenc" :extra-submit-code "alert(123);")
        (:div "test1")
        (:div "test2")))
  (:form
   :action "/foo/bar"
   :method "get"
   :enctype "fooenc"
   :onsubmit "alert(123);initiateFormAction(\"abc123\", $(this), \"weblocks-session=1%3ATEST\"); return false;"
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
      (values res (not (null err))))
  nil t)

;;; test render-button
(deftest-html render-button-1
    (render-button 'some-button)
  (:input :name "some-button" :type "submit" :class "submit" :value "Some Button"
          :onclick "disableIrrelevantButtons(this);"))

(deftest-html render-button-2
    (render-button 'some-button :class "foo" :value "bar" :id "baz")
  (:input :name "some-button" :type "submit" :id "baz" :class "foo" :value "bar"
          :onclick "disableIrrelevantButtons(this);"))

;;; test render-form-and-button
(deftest-html render-form-and-button-1
    (with-request :get nil
      (render-form-and-button "some-button" (lambda ())))
  (:form
   :action "/foo/bar"
   :method "get"
   :onsubmit "initiateFormAction(\"abc123\", $(this), \"weblocks-session=1%3ATEST\"); return false;"
   (:div :class "extra-top-1" "<!-- empty -->")
   (:div :class "extra-top-2" "<!-- empty -->")
   (:div :class "extra-top-3" "<!-- empty -->")
   (:fieldset
    (:input :name "some-button" :type "submit" :class "submit" :value "Some Button"
          :onclick "disableIrrelevantButtons(this);")
    (:input :name "action" :type "hidden" :value "abc123"))
   (:div :class "extra-bottom-1" "<!-- empty -->")
   (:div :class "extra-bottom-2" "<!-- empty -->")
   (:div :class "extra-bottom-3" "<!-- empty -->")))

;;; test render-checkbox
(deftest-html render-checkbox-1
    (render-checkbox 'foo nil)
  (:input :name "foo" :type "checkbox" :class "checkbox" :value "f"))

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

(deftest-html render-dropdown-12
    (render-dropdown 'some-name '("a" "b" "c")
                     :autosubmitp t)
  (:select :name "some-name"
           :onchange "if(this.form.onsubmit) { this.form.onsubmit(); } else { this.form.submit(); }"
           (:option "a")
           (:option "b")
           (:option "c")))

;;; test render-autodropdown
(deftest-html render-autodropdown-1
    (with-request :get nil
      (render-autodropdown 'some-name '("a" "b" "c") (lambda ())))
  (:form :class "autodropdown" :action "/foo/bar" :method "get"
         :onsubmit "initiateFormAction(\"abc123\", $(this), \"weblocks-session=1%3ATEST\"); return false;"
         (:div :class "extra-top-1" "<!-- empty -->")
         (:div :class "extra-top-2" "<!-- empty -->")
         (:div :class "extra-top-3" "<!-- empty -->")
         (:fieldset
          (:select :name "some-name"
                   :onchange "if(this.form.onsubmit) { this.form.onsubmit(); } else { this.form.submit(); }"
                   (:option "a")
                   (:option "b")
                   (:option "c"))
          (:noscript
            (:input :name "submit" :type "submit" :value "Submit"
                    :onclick "disableIrrelevantButtons(this);"))
          (:input :name "action" :type "hidden" :value "abc123"))
         (:div :class "extra-bottom-1" "<!-- empty -->")
         (:div :class "extra-bottom-2" "<!-- empty -->")
         (:div :class "extra-bottom-3" "<!-- empty -->")))

;;; test render-radio-buttons
(deftest-html render-radio-buttons-1
    (render-radio-buttons 'some-name '("a" "b" "c") :id "some-id" :class "some-class")
  (htm
   (:label :id "some-id" :class "some-class first"
           (:input :name "some-name" :type "radio" :class "radio" :value "a")
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

;;; test render-password
(deftest-html render-password-1
    (render-password 'name1 "value1" :id "id1"  :class "class1")
  (:input :type "password" :name "name1"  :id "id1" :value "value1"
                           :class "class1"))

(deftest-html render-password-2
    (render-password 'name2 "value2" :id "id2" :class "class2" :maxlength 25)
  (:input :type "password" :name "name2"  :id "id2" :value "value2"
                           :maxlength 25 :class "class2"))

;; test render-textarea
(deftest-html render-textarea-1
    (render-textarea 'name1 "value1" 4 20 :id "id1" :class "class1")
  (:textarea :name "name1" :id "id1" :rows 4 :cols 20 :class "class1"
             "value1"))

(deftest-html render-textarea-2
    (render-textarea 'name2 "value2" 4 20)
  (:textarea :name "name2" :rows 4 :cols 20
             "value2")) 

;;; test render-list
(deftest-html render-list-1
    (with-request :get nil
      (render-list '("a" "b")))
  (:ul
   (:li (:div :class "widget string" (:p "a")))
   (:li (:div :class "widget string" (:p "b")))))

(deftest-html render-list-2
    (with-request :get nil
      (render-list '()))
  (:div :class "view"
        (:div :class "extra-top-1" "<!-- empty -->")
        (:div :class "extra-top-2" "<!-- empty -->")
        (:div :class "extra-top-3" "<!-- empty -->")
        (:div :class "empty"
              (:p :class "user-message" (:span :class "message" "There are no items in the list.")))
        (:div :class "extra-bottom-1" "<!-- empty -->")
        (:div :class "extra-bottom-2" "<!-- empty -->")
        (:div :class "extra-bottom-3" "<!-- empty -->")))

(deftest-html render-list-3
    (with-request :get nil
      (render-list '("a" "b") :orderedp t))
  (:ol
   (:li (:div :class "widget string" (:p "a")))
   (:li (:div :class "widget string" (:p "b")))))

(deftest-html render-list-4
    (with-request :get nil
      (render-list '("a" "b") :render-fn (lambda (item)
                                           (declare (ignore item))
                                           (with-html
                                             "foo"))))
  (:ul
   (:li "foo")
   (:li "foo")))

;;; test scriptonly
(deftest-html scriptonly-1
    (with-request :get nil
      (scriptonly "test"))
  #.(format nil
            "<script type='text/javascript'>~%// <![CDATA[~%document.write('test');~%// ]]>~%</script>"))

;;; test noscript
(deftest-html noscript-1
    (with-request :get nil
      (noscript "test"))
  (:noscript "test"))

(deftest-html noscript-2
    (with-request :get nil
      (make-request-ajax)
      (noscript "test"))
  "")

;;; test render-message
(deftest-html render-message-1
    (render-message "Some Message" "Some Caption")
  (:p :class "user-message"
      (:span :class "caption" "Some Caption:&nbsp;")
      (:span :class "message" "Some Message")))

(deftest-html render-message-2
    (render-message "Some Message")
  (:p :class "user-message"
      (:span :class "message" "Some Message")))

