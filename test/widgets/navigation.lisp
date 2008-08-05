
(in-package :weblocks-test)

;;; test pane-name
(deftest pane-name-1
    (pane-name (cons 1 2))
  1)

;;; test pane-widget
(deftest pane-widget-1
    (pane-widget (cons 1 2))
  2)

;;; test navigation-default-pane
(deftest navigation-default-pane-1
    (navigation-default-pane (make-instance 'navigation :panes `(("Test One" . nil) ("Test Two" . nil))))
  "Test One")

;;; test initialize-instance for navigation
(deftest initialize-navigation-1
    (weblocks::selector-mixin-current-pane-name
     (make-instance 'navigation :panes `(("Test One" . nil) ("Test Two" . nil))))
  "Test One")

(deftest initialize-navigation-2
    (weblocks::selector-mixin-current-pane-name
     (make-instance 'navigation
		    :panes `(("Test One" . nil) ("Test Two" . nil))
		    :current-pane-name "Test Two"))
  "Test Two")

;;; test with-navigation-header

(deftest-html with-navigation-header-1
    (with-navigation-header (make-instance 'navigation) (lambda (x &rest args)
							  (with-html (:div "test"))))
  (:div :class "view menu"
	(:div :class "extra-top-1" "<!-- empty -->")
	(:div :class "extra-top-2" "<!-- empty -->")
	(:div :class "extra-top-3" "<!-- empty -->")
	(:div :class "empty-navigation" "No navigation entries")
	(:div :class "extra-bottom-1" "<!-- empty -->")
	(:div :class "extra-bottom-2" "<!-- empty -->")
	(:div :class "extra-bottom-3" "<!-- empty -->")))

(deftest-html with-navigation-header-2
    (with-navigation-header (make-instance 'navigation :panes `(("Test One" . nil)) :dom-id nil)
      (lambda (x &rest args)
	(with-html (:div "test"))))
  (:div :class "view menu"
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
  (:div :class "view menu"
	(:div :class "extra-top-1" "<!-- empty -->")
	(:div :class "extra-top-2" "<!-- empty -->")
	(:div :class "extra-top-3" "<!-- empty -->")
	(:h1 "Hello!")
	(:ul (:div "test"))
	(:div :class "extra-bottom-1" "<!-- empty -->")
	(:div :class "extra-bottom-2" "<!-- empty -->")
	(:div :class "extra-bottom-3" "<!-- empty -->")))

(deftest-html with-navigation-header-4
    (with-navigation-header (make-instance 'navigation
					   :render-menu-p nil)
      (lambda (x &rest args)
	(with-html (:div "test"))))
  nil)

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
   (:li (:a :href "/foo/bar/test2" "Test2"))))

(deftest-html render-navigation-body-2
    (with-request :post :nil
      (render-navigation-body
       (make-navigation "Test Navigation"
			'foo nil
			(url-decode "%C3%A5%C3%A4%C3%B6") nil)))
  (htm
   (:li :class "selected-item" (:span "Foo"))
   (:li (:a :href "/foo/bar/%c3%a5%c3%a4%c3%b6" #.(humanize-name (url-decode "%C3%A5%C3%A4%C3%B6"))))))

;;; test full navigation widget scenario
(deftest-html render-navigation-widget-1
    (with-webapp ()
      (let ((nav (make-navigation "Test Navigation"
				  "Test1" (make-instance 'dataform :data *joe*)
				  "Test2" (make-instance 'dataform :data *some-college*))))
	(declare (special *current-navigation-url*))
	(with-request :get nil :uri "/"
	  (setf *current-navigation-url* "/")
	  (render-widget nav))
	(with-request :get nil :uri "/test2"
	  (setf *current-navigation-url* "/test2")
	  (render-widget nav))))
  (htm
   (:div :class "selector-mixin widget dispatcher selector navigation" :id "test-navigation"
	 (:div :class "navigation-body"
	       (:div :class "widget dataform" :id "id-123"
		     #.(data-header-template
			"abc123"
			'((:li :class "name" (:span :class "label text" "Name:&nbsp;")
			   (:span :class "value" "Joe"))
			  (:li :class "manager"
			   (:span :class "label text" "Manager:&nbsp;") (:span :class "value" "Jim")))
			:postslots `((:div :class "submit"
					   ,(link-action-template "abc123" "Modify"
								  :class "modify"
								  :uri "/"))))))
	 (:div :class "view menu"
	       (:div :class "extra-top-1" "<!-- empty -->")
	       (:div :class "extra-top-2" "<!-- empty -->")
	       (:div :class "extra-top-3" "<!-- empty -->")
	       (:h1 "Test Navigation")
	       (:ul (:li :class "selected-item" (:span "Test1"))
		    (:li (:a :href "/test2" "Test2")))
	       (:div :class "extra-bottom-1" "<!-- empty -->")
	       (:div :class "extra-bottom-2" "<!-- empty -->")
	       (:div :class "extra-bottom-3" "<!-- empty -->")))
   (:div :class "selector-mixin widget dispatcher selector navigation" :id "test-navigation"
	 (:div :class "navigation-body"
	       (:div :class "widget dataform" :id "id-123"
		     (:div :class "view data education-history"
			   (:div :class "extra-top-1" "<!-- empty -->")
			   (:div :class "extra-top-2" "<!-- empty -->")
			   (:div :class "extra-top-3" "<!-- empty -->")
			   (:h1 (:span :class "action" "Viewing:&nbsp;")
				(:span :class "object" "Education History"))
			   (:ul
			    (:li :class "university"
				 (:span :class "label text" "University:&nbsp;")
				 (:span :class "value" "Bene Gesserit University"))
			    (:li :class "graduation-year"
				 (:span :class "label text" "Graduation Year:&nbsp;")
				 (:span :class "value" "2000")))
			   (:div :class "submit" #.(link-action-template "abc123" "Modify"
									 :class "modify"
									 :uri "/test2"))
			   (:div :class "extra-bottom-1" "<!-- empty -->")
			   (:div :class "extra-bottom-2" "<!-- empty -->")
			   (:div :class "extra-bottom-3" "<!-- empty -->"))))
	 (:div :class "view menu"
	       (:div :class "extra-top-1" "<!-- empty -->")
	       (:div :class "extra-top-2" "<!-- empty -->")
	       (:div :class "extra-top-3" "<!-- empty -->")
	       (:h1 "Test Navigation")
	       (:ul (:li (:a :href "/" "Test1"))
		    (:li :class "selected-item" (:span "Test2")))
	       (:div :class "extra-bottom-1" "<!-- empty -->")
	       (:div :class "extra-bottom-2" "<!-- empty -->")
	       (:div :class "extra-bottom-3" "<!-- empty -->")))))

(deftest-html render-navigation-widget-2
    (with-request :get nil
      (let ((nav (make-navigation "Test Navigation"
				  "Test1" (make-instance 'dataform :data *joe*)))
	    (*current-navigation-url* "/"))
	(declare (special *current-navigation-url*))
	(setf (navigation-render-menu-p nav) nil)
	;; render widget
	(render-widget nav)))
  (htm
   (:div :class "widget navigation" :id "test-navigation"
	 (:div :class "widget dataform" :id "id-123"
	       #.(data-header-template
		  "abc123"
		  '((:li :class "name" (:span :class "label text" "Name:&nbsp;")
		     (:span :class "value" "Joe"))
		    (:li :class "manager"
		     (:span :class "label text" "Manager:&nbsp;") (:span :class "value" "Jim"))))))))

(deftest render-navigation-widget-3
    (with-request :get nil
      (let ((nav (make-navigation "Test Navigation"))
	    (*current-navigation-url* "/")
	    (*weblocks-output-stream* (make-string-output-stream)))
	(declare (special *current-navigation-url*
			  *weblocks-output-stream*))
	;; render widget
	(render-widget nav)
	(return-code)))
  404)

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


;;; test find-pane
(deftest find-pane-1
    (find-pane (make-navigation "test navigation"
				"test1" "w1"
				"test2" "w2")
	       "helloworld")
  nil)

(deftest find-pane-2
    (find-pane (make-navigation "test navigation"
				"test1" "w1"
				"Test-Two" "w2")
	       "test-two")
  ("test-two" . "w2"))

(deftest find-pane-3
    (find-pane (make-navigation "test navigation"
				'test1 "w1"
				'test2 "w2")
	       "test1")
  ("test1" . "w1"))

(deftest find-pane-4
    (find-pane (make-navigation "test navigation"
				'test1 "w1"
				'test2 "w2")
	       'test1)
  ("test1" . "w1"))

(deftest find-pane-5
    (let ((nav (make-navigation "test navigation"
				"test1" "w1"
				"test2" "w2")))
      (setf (navigation-on-find-pane nav)
	    (let ((c 0))
	      (lambda (nav name)
		(when (and (equalp name "test3")
			   (= c 0))
		  (incf c)
		  "w3"))))
      (values (find-pane nav "test3")
	      (find-pane nav "test3")
	      (find-pane nav "test4")))
  ("test3" . "w3") ("test3" . "w3") nil)

(deftest find-pane-6
    (let ((nav (make-navigation "test navigation"
				"test1" "w1"
				"test2" "w2")))
      (setf (navigation-on-find-pane nav)
	    (let ((c 0))
	      (lambda (nav name)
		(when (and (equalp name "test3")
			   (= c 0))
		  (incf c)
		  (values "w3" :no-cache)))))
      (values (find-pane nav "test3")
	      (find-pane nav "test3")
	      (find-pane nav "test4")))
  ("test3" . "w3") nil nil)

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
    (with-request :get nil
      (let ((site (create-site-layout)) nav1 nav2)
	(setf nav1 (weblocks::find-navigation-widget site))
	(weblocks::apply-uri-to-navigation '("test2" "test6") nav1)
	(setf nav2 (weblocks::find-navigation-widget (current-pane-widget nav1)))
	(values (slot-value nav1 'current-pane)
		(slot-value nav2 'current-pane))))
  "test2"
  "test6")

(deftest apply-uri-to-navigation-2
    (with-request :get nil
      (let ((site (create-site-layout)) nav1)
	(with-request :get nil
	  (setf nav1 (weblocks::find-navigation-widget site))
	  (weblocks::apply-uri-to-navigation '("test2" "test69") nav1)
	  (return-code))))
  404)

(deftest apply-uri-to-navigation-3
    (with-request :get nil
      (let* ((international-string (url-decode "%C3%A5%C3%A4%C3%B6"))
	     (nav (make-navigation "test-nav-1"
				   "test1" (make-instance 'composite)
				   international-string (make-instance 'composite))))
	(weblocks::apply-uri-to-navigation (list (url-encode international-string)) nav)
	(loop for i across (slot-value nav 'current-pane)
	   collect (char-code i))))
  (229 228 246))

(deftest apply-uri-to-navigation-4
    (with-request :get nil
      (let ((site (create-site-layout)) nav1 nav2)
	(setf nav1 (weblocks::find-navigation-widget site))
	(setf (navigation-on-find-pane nav1)
	      (lambda (nav name)
		(when (equalp name "test13")
		  "w3")))
	(weblocks::apply-uri-to-navigation '("test13") nav1)
	(slot-value nav1 'current-pane)))
  "test13")

;;; test obtain-uri-from-navigation
(deftest obtain-uri-from-navigation-1
    (with-request :get nil
      (let* ((site (create-site-layout))
	     (nav (weblocks::find-navigation-widget site)))
	(weblocks::apply-uri-to-navigation '("test2" "test6") nav)
	(weblocks::obtain-uri-from-navigation nav)))
  "/test2/test6/")

;;; test find-navigation-widget
(deftest find-navigation-widget-1
    (with-request :get nil
      (let ((site (create-site-layout))
	    nav1 nav2)
	(setf nav1 (weblocks::find-navigation-widget site))
	(setf nav2 (weblocks::find-navigation-widget (current-pane-widget nav1)))
	(values (widget-name nav1) (widget-name nav2))))
  "test-nav-1"
  "test-nav-2")

(deftest find-navigation-widget-2
    (weblocks::find-navigation-widget nil)
  nil)

;;; test reset-navigation-widgets
(deftest reset-navigation-widgets-1
    (with-request :get nil
      (let ((site (create-site-layout))
	    nav1 nav2)
	(setf nav1 (weblocks::find-navigation-widget site))
	(setf nav2 (weblocks::find-navigation-widget (current-pane-widget nav1)))
	(setf (slot-value nav1 'current-pane) "test2")
	(setf (slot-value nav2 'current-pane) "test4")
	(weblocks::reset-navigation-widgets nav1)
	(values (slot-value nav1 'current-pane)
		(slot-value nav2 'current-pane))))
  "test1"
  "test3")

