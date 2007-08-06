
(in-package :weblocks-test)

;;; test navigation-default-pane
(deftest navigation-default-pane-1
    (navigation-default-pane (make-instance 'navigation :panes `(("Test One" . nil) ("Test Two" . nil))))
  "Test One")

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
  (:div :class "renderer menu"
	(:div :class "extra-top-1" "<!-- empty -->")
	(:div :class "extra-top-2" "<!-- empty -->")
	(:div :class "extra-top-3" "<!-- empty -->")
	(:div :class "empty-navigation" "No navigation entries")
	(:div :class "extra-bottom-1" "<!-- empty -->")
	(:div :class "extra-bottom-2" "<!-- empty -->")
	(:div :class "extra-bottom-3" "<!-- empty -->")))

(deftest-html with-navigation-header-2
    (with-navigation-header (make-instance 'navigation :panes `(("Test One" . nil)))
      (lambda (x &rest args)
	(with-html (:div "test"))))
  (:div :class "renderer menu"
	(:div :class "extra-top-1" "<!-- empty -->")
	(:div :class "extra-top-2" "<!-- empty -->")
	(:div :class "extra-top-3" "<!-- empty -->")
	(:h1 "Navigation")
	(:ul (:div "test"))
	(:div :class "extra-bottom-1" "<!-- empty -->")
	(:div :class "extra-bottom-2" "<!-- empty -->")
	(:div :class "extra-bottom-3" "<!-- empty -->")))

(deftest-html with-navigation-header-3
    (with-navigation-header (make-instance 'navigation :name "hello!" :panes `(("Test One" . nil)))
      (lambda (x &rest args)
	(with-html (:div "test"))))
  (:div :class "renderer menu"
	(:div :class "extra-top-1" "<!-- empty -->")
	(:div :class "extra-top-2" "<!-- empty -->")
	(:div :class "extra-top-3" "<!-- empty -->")
	(:h1 "Hello!")
	(:ul (:div "test"))
	(:div :class "extra-bottom-1" "<!-- empty -->")
	(:div :class "extra-bottom-2" "<!-- empty -->")
	(:div :class "extra-bottom-3" "<!-- empty -->")))

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
   (:div :class "widget navigation" :id "test-navigation"
	 (:div :class "widget dataform" :id "widget-123"
	       #.(data-header-template
		  "abc123"
		  '((:li :class "name" (:span :class "label" "Name:&nbsp;")
		     (:span :class "value" "Joe"))
		    (:li :class "manager"
		     (:span :class "label" "Manager:&nbsp;") (:span :class "value" "Jim")))))
	 (:div :class "renderer menu"
	       (:div :class "extra-top-1" "<!-- empty -->")
	       (:div :class "extra-top-2" "<!-- empty -->")
	       (:div :class "extra-top-3" "<!-- empty -->")
	       (:h1 "Test Navigation")
	       (:ul (:li :class "selected-item" (:span "Test1"))
		    (:li (:a :href "/test2" "Test2")))
	       (:div :class "extra-bottom-1" "<!-- empty -->")
	       (:div :class "extra-bottom-2" "<!-- empty -->")
	       (:div :class "extra-bottom-3" "<!-- empty -->")))
   (:div :class "widget navigation" :id "test-navigation"
	 (:div :class "widget dataform" :id "widget-123"
	       (:div :class "renderer data education-history"
		     (:div :class "extra-top-1" "<!-- empty -->")
		     (:div :class "extra-top-2" "<!-- empty -->")
		     (:div :class "extra-top-3" "<!-- empty -->")
		     (:h1 (:span :class "action" "Viewing:&nbsp;")
			  (:span :class "object" "Education History"))
		     (:ul
		      (:li :class "university"
			   (:span :class "label" "University:&nbsp;")
			   (:span :class "value" "Bene Gesserit University"))
		      (:li :class "graduation-year"
			   (:span :class "label" "Graduation Year:&nbsp;")
			   (:span :class "value" "2000")))
		     (:div :class "submit" #.(link-action-template "abc124" "Modify"))
		     (:div :class "extra-bottom-1" "<!-- empty -->")
		     (:div :class "extra-bottom-2" "<!-- empty -->")
		     (:div :class "extra-bottom-3" "<!-- empty -->")))
	 (:div :class "renderer menu"
	       (:div :class "extra-top-1" "<!-- empty -->")
	       (:div :class "extra-top-2" "<!-- empty -->")
	       (:div :class "extra-top-3" "<!-- empty -->")
	       (:h1 "Test Navigation")
	       (:ul (:li (:a :href "/" "Test1"))
		    (:li :class "selected-item" (:span "Test2")))
	       (:div :class "extra-bottom-1" "<!-- empty -->")
	       (:div :class "extra-bottom-2" "<!-- empty -->")
	       (:div :class "extra-bottom-3" "<!-- empty -->")))))

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

;;; test apply-uri-to-navigation
(deftest apply-uri-to-navigation-1
    (let ((site (create-site-layout)) nav1 nav2)
      (setf nav1 (weblocks::find-navigation-widget site))
      (weblocks::apply-uri-to-navigation '("test2" "test6") nav1)
      (setf nav2 (weblocks::find-navigation-widget (current-pane-widget nav1)))
      (values (slot-value nav1 'current-pane)
	      (slot-value nav2 'current-pane)))
  "test2"
  "test6")

(deftest apply-uri-to-navigation-2
    (let ((site (create-site-layout)) nav1)
      (with-request :get nil
	(setf nav1 (weblocks::find-navigation-widget site))
	(weblocks::apply-uri-to-navigation '("test2" "test69") nav1)
	(return-code)))
  404)

;;; test find-navigation-widget
(deftest find-navigation-widget-1
    (let ((site (create-site-layout))
	  nav1 nav2)
      (setf nav1 (weblocks::find-navigation-widget site))
      (setf nav2 (weblocks::find-navigation-widget (current-pane-widget nav1)))
      (values (widget-name nav1) (widget-name nav2)))
  "test-nav-1"
  "test-nav-2")

(deftest find-navigation-widget-2
    (weblocks::find-navigation-widget nil)
  nil)

;;; test reset-navigation-widgets
(deftest reset-navigation-widgets-1
    (let ((site (create-site-layout))
	  nav1 nav2)
      (setf nav1 (weblocks::find-navigation-widget site))
      (setf nav2 (weblocks::find-navigation-widget (current-pane-widget nav1)))
      (setf (slot-value nav1 'current-pane) "test2")
      (setf (slot-value nav2 'current-pane) "test4")
      (weblocks::reset-navigation-widgets nav1)
      (values (slot-value nav1 'current-pane)
	      (slot-value nav2 'current-pane)))
  "test1"
  "test3")

