
(in-package :weblocks-test)

;;; test pane-name
(deftest pane-name-1
    (pane-info-name (make-pane-info :name 1 :label 2))
  1)

;;; test selector-mixin-default-pane
(deftest selector-mixin-default-pane-1
    (selector-mixin-default-pane
     (make-instance 'navigation
		    :panes `(("Test One" . nil) ("Test Two" . nil))))
  ("Test One" . nil))

;;; test lazy initialization for navigation
(deftest initialize-navigation-1
    (let (initial-current
	  (nav (make-instance 'navigation
		 :panes `(("Test One" . nil) ("Test Two" . nil)))))
      (setf initial-current
	    (selector-mixin-current-pane-name nav))
      (selector-on-dispatch nav '())
      (values initial-current
	      (selector-mixin-current-pane-name nav)))
  nil "Test One")

(deftest initialize-navigation-2
    (selector-mixin-current-pane-name
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

;;; test init-navigation
(deftest init-navigation-1
    (selector-mixin-panes
     (init-navigation (make-instance 'navigation)
		      'test1 "w1"
		      "Test-Two" "w2"))
  (("test1" . "w1") ("test-two" . "w2")))

;;; test make-navigation
(deftest make-navigation-1
    (selector-mixin-panes
     (make-navigation "test navigation"
		      'test1 "w1"
		      "Test-Two" "w2"))
  (("test1" . "w1") ("test-two" . "w2")))


;;; test selector-mixin-find-pane-by-name

(defun destructure-pane (pane)
  (let ((info (car pane)))
    (cons (list :name (pane-info-name info) :uri-tokens (pane-info-uri-tokens info)
		:label (pane-info-label info))
	  (cdr pane))))

(deftest find-pane-1
    (selector-mixin-find-pane-by-name
     (make-navigation "test navigation"
		      "test1" "w1"
		      "test2" "w2")
		"helloworld")
  nil)

(deftest find-pane-2
    (destructure-pane
     (selector-mixin-find-pane-by-name
      (make-navigation "test navigation"
		       "test1" "w1"
		       "Test-Two" "w2")
      "test-two"))
  ((:name "test-two" :uri-tokens ("test-two") :label "Test Two") . "w2"))

(deftest find-pane-3
    (destructure-pane
     (selector-mixin-find-pane-by-name
      (make-navigation "test navigation"
		       'test1 "w1"
		       'test2 "w2")
      "test1"))
  ((:name "test1" :uri-tokens ("test1") :label "Test1") . "w1"))

(deftest find-pane-4
    (destructure-pane
     (selector-mixin-find-pane-by-name
      (make-navigation "test navigation"
		       'test1 "w1"
		       'test2 "w2")
      "test1"))
  ((:name "test1" :uri-tokens ("test1") :label "Test1") . "w1"))

(deftest find-pane-5
    (let ((nav (make-navigation "test navigation"
				"test1" "w1"
				"test2" "w2")))
      (setf (dispatcher-on-dispatch nav)
	    (let ((c 0))
	      (lambda (nav name)
		(when (and (equalp name '("test3"))
			   (= c 0))
		  (incf c)
		  (values "w3" name '())))))
      (values-list
       (mapcar (curry #'weblocks::dispatcher-get-widget nav)
	       '(("test3") ("test3") ("test4")))))
  "w3" "w3" nil)

(deftest find-pane-6
    (let ((nav (make-navigation "test navigation"
				"test1" "w1"
				"test2" "w2")))
      (setf (dispatcher-on-dispatch nav)
	    (let ((c 0))
	      (lambda (nav name)
		(when (and (equalp name '("test3"))
			   (= c 0))
		  (incf c)
		  (values "w3" name '() :no-cache)))))
      (values-list
       (mapcar (curry #'weblocks::dispatcher-get-widget nav)
	       '(("test3") ("test3") ("test4")))))
  "w3" nil nil)

(defun find-navigation-widget (widgets)
  (setf widgets (ensure-list widgets))
  (flet ((navigation? (widget) (typep widget 'navigation)))
    (and widgets
	 (or (find-if #'navigation? widgets)
	     (find-navigation-widget
	      (mapappend #'composite-widgets widgets))))))

;;; test apply-uri-to-navigation
(deftest apply-uri-to-navigation-1
    (with-request :get nil :uri "/test2/test6"
      (setf (root-composite) (create-site-layout))
      (let* ((nav1 (find-navigation-widget (root-composite)))
	     (nav1-before-rendering (selector-mixin-current-pane-name nav1)))
	(handle-client-request (weblocks::current-webapp))
	(let ((nav2 (find-navigation-widget
		     (cdr (selector-mixin-find-pane-by-name
			   nav1
			   (selector-mixin-current-pane-name nav1))))))
	  (values nav1-before-rendering
		  (selector-mixin-current-pane-name nav1)
		  (selector-mixin-current-pane-name nav2)))))
  nil
  "test2"
  "test6")

(defwebapp autn-webapp
  :init-user-session autn-init-user-session)

(deftest apply-uri-to-navigation-2
    (with-webapp (:class-name 'autn-webapp)
      (with-request :get nil :uri "/test2/test69"
	(let ((site (create-site-layout)) nav1)
	  (defun autn-init-user-session (root) nil)
	  (unwind-protect
	       (progn
		 (catch 'handler-done
		   (handle-client-request (weblocks::current-webapp)))
		 (return-code))
	    (fmakunbound 'init-user-session)))))
  404)

(deftest apply-uri-to-navigation-3
    (with-request :get nil :uri (url-encode #0=(url-decode "%C3%A5%C3%A4%C3%B6"))
      (let* ((international-string #0#)
	     (nav (make-navigation "test-nav-1"
				   "test1" (make-instance 'composite)
				   international-string (make-instance 'composite))))
	(print *uri-tokens*)
	(setf (root-composite)
	      (make-instance 'composite :widgets (list nav)))
	(catch 'handler-done
	  (handle-client-request (weblocks::current-webapp)))
	(loop for i across (selector-mixin-current-pane-name nav)
	   collect (char-code i))))
  (229 228 246))

(deftest apply-uri-to-navigation-4
    (with-request :get nil :uri "/test13"
      (let* ((site (create-site-layout))
	     (nav1 (find-navigation-widget site)))
	(setf (root-composite) site)
	(setf (dispatcher-on-dispatch nav1)
	      (lambda (nav name)
		(when (equalp name '("test13"))
		  (values "w3" name '()))))
	(weblocks::dispatcher-get-widget nav1 '("test13"))
	(car (dispatcher-cache nav1))))
  ("test13"))

;;; test find-navigation-widget
(deftest find-navigation-widget-1
    (with-request :get nil
      (let ((site (create-site-layout))
	    nav1 nav2)
	(setf nav1 (find-navigation-widget site))
	(setf nav2 (find-navigation-widget
		    (cdr (selector-mixin-default-pane nav1))))
	(values (widget-name nav1) (widget-name nav2))))
  "test-nav-1"
  "test-nav-2")

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

