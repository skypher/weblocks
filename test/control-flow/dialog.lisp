
(in-package :weblocks-test)

;;; test current-dialog
(deftest current-dialog-1
    (with-request :get nil
      (values (weblocks::current-dialog)
	      (progn
		(setf (weblocks::current-dialog) 1)
		(weblocks::current-dialog))))
  nil 1)

;;; test make-dialog-js
(deftest make-dialog-js-1
    (with-request :get nil
      (weblocks::make-dialog-js "Some Title"
				(lambda ()
				  (with-html (:p "]]>")))
				"some-class" nil))
  "showDialog(\"Some Title\", \"<div class='widget function'><p>]]></p></div>\", \"some-class\", null);")

(deftest make-dialog-js-2
    (with-request :get nil
      (weblocks::make-dialog-js "Some Title"
				(lambda ()
				  (with-html (:p "]]>")))
				"some-class" t))
  "showDialog(\"Some Title\", \"<div class='widget function'><p>]]></p></div>\", \"some-class\", \"<img src='/pub/images/dialog/close.gif' onclick='initiateAction(&quot;abc123&quot;, &quot;weblocks-session=1%3ATEST&quot;);' onmouseover='this.style.cursor = &quot;pointer&quot;;' style='cursor: expression(&quot;hand&quot;);' />\");")

(deftest make-dialog-js-3
    (with-request :get nil
      (weblocks::make-dialog-js "Some Title"
				(lambda ()
				  (with-html (:p "</script>")))
				"some-class" t))
  "showDialog(\"Some Title\", \"<div class='widget function'><p></script></p></div>\", \"some-class\", \"<img src='/pub/images/dialog/close.gif' onclick='initiateAction(&quot;abc123&quot;, &quot;weblocks-session=1%3ATEST&quot;);' onmouseover='this.style.cursor = &quot;pointer&quot;;' style='cursor: expression(&quot;hand&quot;);' />\");")

;;; test update-dialog-on-request
(deftest-html update-dialog-on-request-1
    (with-request :get nil
      (setf (weblocks::current-dialog)
	    (weblocks::make-dialog :title "foo" :widget (lambda (&rest args)
							  (with-html (:p "bar")))
				   :css-class "baz"))
      (weblocks::update-dialog-on-request))
  nil)

(deftest-html update-dialog-on-request-2
    (with-request :get nil
      (setf (session-value 'weblocks::last-request-uri) *uri-tokens*)
      (weblocks::update-dialog-on-request))
  nil)

(defjstest update-dialog-on-request-3
    (with-request :get nil
      (setf (webapp-session-value 'weblocks::last-request-uri) *uri-tokens*)
      (setf (weblocks::current-dialog)
	    (weblocks::make-dialog :title "foo" :widget (lambda (&rest args)
							  (with-html (:p "bar")))
				   :css-class "baz"))
      (with-output-to-string (*weblocks-output-stream*)
        (weblocks::update-dialog-on-request)))
    "Event.observe(window, 'load', function() {
showDialog(\"foo\", \"<div class='widget function'><p>bar</p></div>\", \"baz\", null);
});")

;;; test do-dialog
(deftest do-dialog-1
    (with-request :get nil
      (setf (weblocks::current-dialog) 1)
      (make-request-ajax)
      (multiple-value-bind (res err)
	  (ignore-errors (do-dialog "Some Title" (lambda (k)
						   (with-html (:p "some widget")))))
	(not (null err))))
  t)

(deftest-html do-dialog-2
    (with-request :get nil
      (setf (root-composite) (make-instance 'composite))
      (do-dialog "Some Title" (lambda (k)
				(with-html (:p "some widget"))))
      (render-widget (root-composite)))
  (:div :class "widget composite" :id "id-123"
	(:div :class "widget function"
	      (:div :class "modal"
		    (:h1 (:span "Some Title"))
		    (:div (:div :class "widget function"
				(:p "some widget")))))))

(defjstest do-dialog-3
    (with-request :get nil
      (make-request-ajax)
      (do-dialog "Some Title" (lambda (k)
				(with-html (:p "some widget"))))
      *on-ajax-complete-scripts*)
    ("showDialog(\"Some Title\", \"<div class='widget function'><p>some widget</p></div>\", \"\", null);"))

;;; test render-choices-get
(deftest-html render-choices-get-1
    (with-request :get nil
      (weblocks::render-choices-get "hello, world!" (list :a :b :c) nil))
  (htm
   (:p "hello, world!")
   #.(link-action-template "abc123" "A") "&nbsp;"
   #.(link-action-template "abc124" "B") "&nbsp;"
   #.(link-action-template "abc125" "C") "&nbsp;"))

;;; test render-choices-post
(deftest-html render-choices-post-1
    (with-request :get nil
      (weblocks::render-choices-post "hello, world!" (list :a :b :c) nil))
  (htm
   (:form
    :action "/foo/bar" :method "post"
    :onsubmit "initiateFormAction(\"abc123\", $(this), \"weblocks-session=1%3ATEST\"); return false;"
    (:div :class "extra-top-1" "<!-- empty -->")
    (:div :class "extra-top-2" "<!-- empty -->")
    (:div :class "extra-top-3" "<!-- empty -->")
    (:fieldset
     (:p "hello, world!")
     (:input :name "a" :type "submit" :class "submit" :value "A"
	  :onclick "disableIrrelevantButtons(this);")
     (:input :name "b" :type "submit" :class "submit" :value "B"
	  :onclick "disableIrrelevantButtons(this);")
     (:input :name "c" :type "submit" :class "submit" :value "C"
	  :onclick "disableIrrelevantButtons(this);")
     (:input :name "action" :type "hidden" :value "abc123"))
    (:div :class "extra-bottom-1" "<!-- empty -->")
    (:div :class "extra-bottom-2" "<!-- empty -->")
    (:div :class "extra-bottom-3" "<!-- empty -->"))))

;;; test do-choice
(defjstest do-choice-1
    (with-request :get nil
      (make-request-ajax)
      (do-choice "Please choose" (list :a :b))
      *on-ajax-complete-scripts*)
  ("showDialog(\"Select Option\", \"<div class='widget function'><form action='/foo/bar' method='post' onsubmit='initiateFormAction(&quot;abc123&quot;, $(this), &quot;weblocks-session=1%3ATEST&quot;); return false;'><div class='extra-top-1'><!-- empty --></div><div class='extra-top-2'><!-- empty --></div><div class='extra-top-3'><!-- empty --></div><fieldset><p>Please choose</p><input name='a' type='submit' class='submit' value='A' onclick='disableIrrelevantButtons(this);' /><input name='b' type='submit' class='submit' value='B' onclick='disableIrrelevantButtons(this);' /><input name='action' type='hidden' value='abc123' /></fieldset><div class='extra-bottom-1'><!-- empty --></div><div class='extra-bottom-2'><!-- empty --></div><div class='extra-bottom-3'><!-- empty --></div></form></div>\", \"choice \", null);"))

(deftest do-choice-2
    (let (res)
      (with-request :get nil
	(make-request-ajax)
	(with-call/cc (setf res (do-choice "Please choose" (list :a :b))))
	(do-request `(("b" . "B")
		      (,weblocks::*action-string* . "abc123"))))
      res)
  :b)

;;; test do-confirmation
(defjstest do-confirmation-1
    (with-request :get nil
      (make-request-ajax)
      (do-confirmation "Please confirm")
      *on-ajax-complete-scripts*)
  ("showDialog(\"Confirmation\", \"<div class='widget function'><form action='/foo/bar' method='post' onsubmit='initiateFormAction(&quot;abc123&quot;, $(this), &quot;weblocks-session=1%3ATEST&quot;); return false;'><div class='extra-top-1'><!-- empty --></div><div class='extra-top-2'><!-- empty --></div><div class='extra-top-3'><!-- empty --></div><fieldset><p>Please confirm</p><input name='ok' type='submit' class='submit' value='Ok' onclick='disableIrrelevantButtons(this);' /><input name='cancel' type='submit' class='submit' value='Cancel' onclick='disableIrrelevantButtons(this);' /><input name='action' type='hidden' value='abc123' /></fieldset><div class='extra-bottom-1'><!-- empty --></div><div class='extra-bottom-2'><!-- empty --></div><div class='extra-bottom-3'><!-- empty --></div></form></div>\", \"choice confirmation \", null);"))

(defjstest do-confirmation-2
    (with-request :get nil
      (make-request-ajax)
      (do-confirmation "Please confirm" :type :yes/no)
      *on-ajax-complete-scripts*)
  ("showDialog(\"Confirmation\", \"<div class='widget function'><form action='/foo/bar' method='post' onsubmit='initiateFormAction(&quot;abc123&quot;, $(this), &quot;weblocks-session=1%3ATEST&quot;); return false;'><div class='extra-top-1'><!-- empty --></div><div class='extra-top-2'><!-- empty --></div><div class='extra-top-3'><!-- empty --></div><fieldset><p>Please confirm</p><input name='yes' type='submit' class='submit' value='Yes' onclick='disableIrrelevantButtons(this);' /><input name='no' type='submit' class='submit' value='No' onclick='disableIrrelevantButtons(this);' /><input name='action' type='hidden' value='abc123' /></fieldset><div class='extra-bottom-1'><!-- empty --></div><div class='extra-bottom-2'><!-- empty --></div><div class='extra-bottom-3'><!-- empty --></div></form></div>\", \"choice confirmation \", null);"))

(deftest do-confirmation-3
    (let (res)
      (with-request :get nil
	(make-request-ajax)
	(with-call/cc (setf res (do-confirmation "Please confirm")))
	(do-request `(("ok" . "Ok")
		      (,weblocks::*action-string* . "abc123"))))
      res)
  :ok)

(defjstest do-information-1
    (with-request :get nil
      (make-request-ajax)
      (do-information "FYI")
      *on-ajax-complete-scripts*)
  ("showDialog(\"Information\", \"<div class='widget function'><form action='/foo/bar' method='post' onsubmit='initiateFormAction(&quot;abc123&quot;, $(this), &quot;weblocks-session=1%3ATEST&quot;); return false;'><div class='extra-top-1'><!-- empty --></div><div class='extra-top-2'><!-- empty --></div><div class='extra-top-3'><!-- empty --></div><fieldset><p>FYI</p><input name='ok' type='submit' class='submit' value='Ok' onclick='disableIrrelevantButtons(this);' /><input name='action' type='hidden' value='abc123' /></fieldset><div class='extra-bottom-1'><!-- empty --></div><div class='extra-bottom-2'><!-- empty --></div><div class='extra-bottom-3'><!-- empty --></div></form></div>\", \"choice information \", null);"))

