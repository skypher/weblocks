
(in-package :weblocks-test)

;;; test initialize-instance for navigation
(deftest initialize-navigation-1
    (slot-value
     (make-instance 'navigation :panes `(("Test One" . nil) ("Test Two" . nil)))
     'current-pane)
  "Test One")

(deftest initialize-navigation-2
    (slot-value
     (make-instance 'navigation
		    :panes `(("Test One" . nil) ("Test Two" . nil))
		    :current-pane "Test Two")
     'current-pane)
  "Test Two")

;;; test with-navigation-header
(deftest-html with-navigation-header-1
    (with-navigation-header (make-instance 'navigation) (lambda (x &rest args)
							  (with-html (:div "test"))))
  (:div :class "renderer navigation"
	(:div :class "extra-top-1" "&nbsp;")
	(:div :class "extra-top-2" "&nbsp;")
	(:div :class "extra-top-3" "&nbsp;")
	(:div :class "empty-navigation" "No navigation entries")
	(:div :class "extra-bottom-1" "&nbsp;")
	(:div :class "extra-bottom-2" "&nbsp;")
	(:div :class "extra-bottom-3" "&nbsp;")))

(deftest-html with-navigation-header-2
    (with-navigation-header (make-instance 'navigation :panes `(("Test One" . nil)))
      (lambda (x &rest args)
	(with-html (:div "test"))))
  (:div :class "renderer navigation"
	(:div :class "extra-top-1" "&nbsp;")
	(:div :class "extra-top-2" "&nbsp;")
	(:div :class "extra-top-3" "&nbsp;")
	(:h1 "Navigation")
	(:ul (:div "test"))
	(:div :class "extra-bottom-1" "&nbsp;")
	(:div :class "extra-bottom-2" "&nbsp;")
	(:div :class "extra-bottom-3" "&nbsp;")))

(deftest-html with-navigation-header-3
    (with-navigation-header (make-instance 'navigation :name "hello!" :panes `(("Test One" . nil)))
      (lambda (x &rest args)
	(with-html (:div "test"))))
  (:div :class "renderer navigation"
	(:div :class "extra-top-1" "&nbsp;")
	(:div :class "extra-top-2" "&nbsp;")
	(:div :class "extra-top-3" "&nbsp;")
	(:h1 "Hello!")
	(:ul (:div "test"))
	(:div :class "extra-bottom-1" "&nbsp;")
	(:div :class "extra-bottom-2" "&nbsp;")
	(:div :class "extra-bottom-3" "&nbsp;")))

;;; test render-navigation-body
(deftest-html render-navigation-body-1
    (with-request :post :nil
      (render-navigation-body
       (make-navigation "Test Navigation"
			"test1" (lambda (&rest args)
				  (with-html (:div "test")))
			"test2" nil)))
  (htm
   (:li :class "selected-item" (:span "Test1"))
   (:li (:a :href "test2" "Test2"))))

;;; test full navigation widget scenario
(deftest-html render-navigation-widget-1
    (with-request :get nil
      (let ((nav (make-navigation "Test Navigation"
				  "Test1" (make-instance 'dataform :data *joe*)
				  "Test2" (make-instance 'dataform :data *some-college*)))
	    (*current-navigation-url* "/"))
	(declare (special *current-navigation-url*))
	;; render widget
	(render-widget nav)
	;; switch to test2 and render
	(weblocks::apply-uri-to-navigation '("test2") nav)
	(render-widget nav)))
  (htm
   (:div :class "widget navigation test-navigation"
	 (:div :class "widget dataform"
	       #.(data-header-template
		  '((:li (:span :class "label" "Name:&nbsp;") (:span :class "value" "Joe"))
		    (:li (:span :class "label" "Manager:&nbsp;") (:span :class "value" "Jim")))
		  :postslots '((:div :class "submit" (:a :href "?action=abc123" "Modify")))))
	 (:div :class "renderer navigation"
	       (:div :class "extra-top-1" "&nbsp;")
	       (:div :class "extra-top-2" "&nbsp;")
	       (:div :class "extra-top-3" "&nbsp;")
	       (:h1 "Test Navigation")
	       (:ul (:li :class "selected-item" (:span "Test1"))
		    (:li (:a :href "/test2" "Test2")))
	       (:div :class "extra-bottom-1" "&nbsp;")
	       (:div :class "extra-bottom-2" "&nbsp;")
	       (:div :class "extra-bottom-3" "&nbsp;")))
   (:div :class "widget navigation test-navigation"
	 (:div :class "widget dataform"
	       (:div :class "renderer data education-history"
		     (:div :class "extra-top-1" "&nbsp;")
		     (:div :class "extra-top-2" "&nbsp;")
		     (:div :class "extra-top-3" "&nbsp;")
		     (:h1 (:span :class "action" "Viewing:&nbsp;")
			  (:span :class "object" "Education History"))
		     (:ul
		      (:li (:span :class "label" "University:&nbsp;") (:span :class "value"
									     "Bene Gesserit University"))
		      (:li (:span :class "label" "Graduation Year:&nbsp;") (:span :class "value" "2000")))
		     (:div :class "submit" (:a :href "?action=abc124" "Modify"))
		     (:div :class "extra-bottom-1" "&nbsp;")
		     (:div :class "extra-bottom-2" "&nbsp;")
		     (:div :class "extra-bottom-3" "&nbsp;")))
	 (:div :class "renderer navigation"
	       (:div :class "extra-top-1" "&nbsp;")
	       (:div :class "extra-top-2" "&nbsp;")
	       (:div :class "extra-top-3" "&nbsp;")
	       (:h1 "Test Navigation")
	       (:ul (:li (:a :href "/test1" "Test1"))
		    (:li :class "selected-item" (:span "Test2")))
	       (:div :class "extra-bottom-1" "&nbsp;")
	       (:div :class "extra-bottom-2" "&nbsp;")
	       (:div :class "extra-bottom-3" "&nbsp;")))))

;;; test current-pane-widget
(deftest current-pane-widget-1
    (current-pane-widget
     (make-navigation "Test Navigation"
		      "test1" "w1"
		      "test2" "w2"))
  "w1")

(deftest current-pane-widget-2
    (let ((nav (make-navigation "Test Navigation"
				"test1" "w1"
				"test2" "w2")))
      (setf (slot-value nav 'current-pane) "test2")
      (current-pane-widget nav))
  "w2")

;;; test init-navigation
(deftest init-navigation-1
    (navigation-panes
     (init-navigation (make-instance 'navigation)
		      'test1 "w1"
		      "Test-Two" "w2"))
  (("test1" . "w1") ("test-two" . "w2")))

;;; test make-navigation
(deftest make-navigation-1
    (navigation-panes
     (make-navigation "test navigation"
		      'test1 "w1"
		      "Test-Two" "w2"))
  (("test1" . "w1") ("test-two" . "w2")))


;;; test pane-exists-p
(deftest pane-exists-p-1
    (pane-exists-p (make-navigation "test navigation"
				    "test1" "w1"
				    "test2" "w2")
		   "helloworld")
  nil)

(deftest pane-exists-p-2
    (pane-exists-p (make-navigation "test navigation"
				    "test1" "w1"
				    "Test-Two" "w2")
		   "test-two")
  t)

(deftest pane-exists-p-3
    (pane-exists-p (make-navigation "test navigation"
				    'test1 "w1"
				    'test2 "w2")
		   "test1")
  t)

(deftest pane-exists-p-4
    (pane-exists-p (make-navigation "test navigation"
				    'test1 "w1"
				    'test2 "w2")
		   'test1)
  t)

;;; test reset-current-pane
(deftest reset-current-pane-1
    (let ((nav (make-navigation "test navigation"
				'test1 "w1"
				'test2 "w2")))
      (setf (slot-value nav 'current-pane) "test2")
      (reset-current-pane nav)
      (slot-value nav 'current-pane))
  "test1")
