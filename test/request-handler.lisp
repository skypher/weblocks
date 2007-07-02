
(in-package :weblocks-test)

;;; testing handle-client-request
(deftest handle-client-request-0
    (with-request :get nil
      (setf (slot-value *request* 'hunchentoot::script-name) "/hello/world/bar.txt")
      (multiple-value-bind (res err)
	  (ignore-errors
	    (handle-client-request))
	res))
  nil)

(deftest handle-client-request-1
    (with-request :get nil
      (let (weblocks::*webapp-name* result1 result2 result3
				    (weblocks::*render-debug-toolbar* nil))
	;; set up our mini-application with one dataform widget
	(declare (special weblocks::*webapp-name*))
	(defwebapp 'hello)
	(defun init-user-session (comp)
	  (push (make-instance 'dataform :data *joe*) (composite-widgets comp)))
	;; handle the first request (make sure data is drawn)
	(setf result1 (handle-client-request))
	;; unbind init-user-session to make sure root-composite persists
	(fmakunbound 'init-user-session)
	;; fake user clicking on "modify"
	(setf (slot-value *request* 'get-parameters) `((,weblocks::*action-string* . "abc123")))
	;; handle another request, this time AJAX (make sure form is drawn)
	(setf (slot-value *request* 'hunchentoot::headers-in)
	    (cons '("X-Requested-With" . "blah") (slot-value *request* 'hunchentoot::headers-in)))
	(setf result2 (handle-client-request))
	(setf result3 (header-out "X-JSON"))
	(values (null (session-value "debug-reset-sessions")) result1 result2 result3)))
  t
  #.(with-request-template "~
<div class='widget dataform' id='widget-123'>~
<div class='renderer data employee'>~
<div class='extra-top-1'>&nbsp;</div>~
<div class='extra-top-2'>&nbsp;</div>~
<div class='extra-top-3'>&nbsp;</div>~
<h1><span class='action'>Viewing:&nbsp;</span><span class='object'>Employee</span></h1>~
<ul>~
<li class='name'><span class='label'>Name:&nbsp;</span><span class='value'>Joe</span></li>~
<li class='manager'><span class='label'>Manager:&nbsp;</span><span class='value'>Jim</span></li>~
</ul>~
<div class='submit'><a href='?action=abc123' ~
                       onclick='initiateAction(\"abc123\", \"weblocks-session=1%3Atest\"); ~
                       return false;'>Modify</a></div>~
<div class='extra-bottom-1'>&nbsp;</div>~
<div class='extra-bottom-2'>&nbsp;</div>~
<div class='extra-bottom-3'>&nbsp;</div>~
</div>~
</div>")
  " "
  #.(format nil "{\"widgets\":~
{\"widget-123\":~
\"<form class='renderer form employee' action='' method='post' ~
      onsubmit='initiateFormAction(\\\"abc124\\\", $(this), \\\"weblocks-session=1%3Atest\\\"); ~
                return false;'>~
<div class='extra-top-1'>&nbsp;</div>~
<div class='extra-top-2'>&nbsp;</div>~
<div class='extra-top-3'>&nbsp;</div>~
<fieldset><h1><span class='action'>Modifying:&nbsp;</span>~
<span class='object'>Employee</span>~
</h1>~
<h2 class='form-fields-title'>Form fields:</h2>~
<ul><li class='name'><label>~
<span class='slot-name'><span class='extra'>Name:&nbsp;~
<em class='required-slot'>(required)&nbsp;</em></span></span>~
<input type='text' name='name' value='Joe' />~
</label>~
</li>~
<li class='manager'><label>~
<span class='slot-name'><span class='extra'>Manager:&nbsp;</span></span>~
<input type='text' name='manager' value='Jim' />~
</label>~
</li>~
</ul>~
<div class='submit'>~
<input name='submit' type='submit' class='submit' value='Submit' onclick='disableIrrelevantButtons(this);' />~
<input name='cancel' type='submit' class='submit cancel' value='Cancel' ~
onclick='disableIrrelevantButtons(this);' />~
<input name='action' type='hidden' value='abc124' />~
</div>~
</fieldset>~
<div class='extra-bottom-1'>&nbsp;</div>~
<div class='extra-bottom-2'>&nbsp;</div>~
<div class='extra-bottom-3'>&nbsp;</div>~
</form>\"},~
\"on-load\":null}"))

;;; make sure debug toolbar is rendered when appropriate
(deftest handle-client-request-2
    (with-request :get nil
      (let (weblocks::*webapp-name* result1 result2
				    (weblocks::*render-debug-toolbar* t))
	;; set up our mini-application with one dataform widget
	(declare (special weblocks::*webapp-name*))
	(defwebapp 'hello)
	(defun init-user-session (comp)
	  (setf (composite-widgets comp) (list (lambda () nil))))
	;; handle the first request (make sure data is drawn)
	(setf result1 (handle-client-request))
	(fmakunbound 'init-user-session)
	(values result1 (not (null (session-value "debug-reset-sessions"))))))
  #.(with-request-template
	    "~
<div class='widget function'>~
</div>" :render-debug-toolbar-p t)
  t)

;;; make sure navigation controls are modified by request uri
(deftest handle-client-request-3
    (with-request :get nil
      (let (weblocks::*webapp-name* result
				    (weblocks::*render-debug-toolbar* nil))
	;; set up our mini-application with one navigation widget
	(declare (special weblocks::*webapp-name*))
	(defwebapp 'hello)
	(defun init-user-session (comp)
	  (setf (composite-widgets comp) (list (make-navigation "test-nav"
								"test1" (lambda (&rest args)
									  (with-html (:div "hi1")))
								"test2" (lambda (&rest args)
									  (with-html (:div "hi2")))))))
	;; set the URI
	(setf (slot-value *request* 'hunchentoot::uri) "/test2?action=blah")
	;; handle the request
	(setf result (handle-client-request))
	(fmakunbound 'init-user-session)
	result))
  #.(with-request-template
	    "~
<div class='widget navigation' id='test-nav'>~
<div class='widget function'>~
<div>hi2</div>~
</div>~
<div class='renderer menu'>~
<div class='extra-top-1'>&nbsp;</div>~
<div class='extra-top-2'>&nbsp;</div>~
<div class='extra-top-3'>&nbsp;</div>~
<h1>Test Nav</h1>~
<ul>~
<li><a href='/test1'>Test1</a></li>~
<li class='selected-item'><span>Test2</span></li>~
</ul>~
<div class='extra-bottom-1'>&nbsp;</div>~
<div class='extra-bottom-2'>&nbsp;</div>~
<div class='extra-bottom-3'>&nbsp;</div>~
</div>~
</div>"))

(deftest handle-client-request-4
    (with-request :get nil
      (let (weblocks::*webapp-name* result1 result2 result3
				    (weblocks::*render-debug-toolbar* nil))
	;; make sure we handle cookies
	(setf *session* nil)
	(catch 'hunchentoot::handler-done
	  (handle-client-request))
	(header-out "Location")))
  "http://NILNIL?weblocks-session=1%3Atest")

(deftest handle-client-request-5
    (with-request :get nil
      (let ((res 0)
	    *on-pre-request* *on-post-request* weblocks::*webapp-name*)
	;; set up our mini-application
	(declare (special weblocks::*webapp-name*))
	(defwebapp 'hello)
	(defun init-user-session (comp) nil)
	;; start the session
	(start-session)
	;; do the test
	(push (lambda ()
		(incf res))
	      (request-hook :application :pre-action))
	(push (lambda ()
		(incf res))
	      (request-hook :application :post-action))
	(handle-client-request)
	;; tear down the application
	(fmakunbound 'init-user-session)
	res))
  2)

(deftest handle-client-request-6
    (with-request :get nil
      (let ((res 0) weblocks::*webapp-name*)
	;; set up our mini-application
	(declare (special weblocks::*webapp-name* *request-hook*))
	(defwebapp 'hello)
	(defun init-user-session (comp) nil)
	;; start the session
	(start-session)
	;; do the test
	(push (lambda ()
		(incf res))
	      (request-hook :session :pre-render))
	(push (lambda ()
		(incf res))
	      (request-hook :session :post-render))
	(handle-client-request)
	;; tear down the application
	(fmakunbound 'init-user-session)
	res))
  2)

(deftest handle-client-request-7
    (with-request :get nil
      (let ((res 0) weblocks::*webapp-name*)
	(declare (special weblocks::*webapp-name* *request-hook*))
	;; start the session
	(start-session)
	;; set up our mini-application
	(defwebapp 'hello)
	(defun init-user-session (comp)
	  (declare (special *request-hook*))
	  (push (lambda ()
		  (incf res))
		(request-hook :request :pre-action))
	  (push (lambda ()
		  (incf res))
		(request-hook :request :post-render)))
	;; do the test
	(handle-client-request)
	;; tear down the application
	(fmakunbound 'init-user-session)
	res))
  2)

(deftest handle-client-request-8
    (with-request :get `(("pure" . true) (,weblocks::*action-string* . "abc123"))
      (let ((res 0) weblocks::*webapp-name*)
	(declare (special weblocks::*webapp-name* *request-hook*))
	;; start the session
	(start-session)
	;; action
	(make-action (lambda (&rest args)
		       (incf res)))
	;; set up our mini-application
	(defwebapp 'hello)
	(defun init-user-session (comp)
	  (declare (special *request-hook))
	  (push (lambda ()
		  (incf res))
		(request-hook :request :pre-action))
	  (push (lambda ()
		  (incf res))
		(request-hook :request :post-render)))
	;; do the test
	(ignore-errors
	  (handle-client-request))
	;; tear down the application
	res))
  1)

;;; test eval-action
(deftest eval-action-1
    (with-request :get `(("name" . "Bob")
			 ("cancel" . "Cancel")
			 (,weblocks::*action-string* . "abc123"))
      (make-action (lambda (&key name cancel &allow-other-keys)
		     (concatenate 'string name cancel)))
      (weblocks::eval-action))
  "BobCancel")

;;; test remove-session-from-uri
(deftest remove-session-from-uri-1
    (with-request :get nil
      (weblocks::remove-session-from-uri "/pub/test/blah"))
  "/pub/test/blah")

(deftest remove-session-from-uri-2
    (with-request :get '(("action" . "test"))
      (weblocks::remove-session-from-uri "/pub/test/blah"))
  "/pub/test/blah?action=test")

(deftest remove-session-from-uri-3
    (with-request :get '(("action" . "test") ("weblocks-session" "123"))
      (weblocks::remove-session-from-uri "/pub/test/blah"))
  "/pub/test/blah?action=test")

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

;;; test tokenize-uri
(deftest tokenize-uri-1
    (weblocks::tokenize-uri "///hello/world/blah\\test\\world?hello=5 ;blah=7")
  ("hello" "world" "blah" "test" "world"))

;;; test render-dirty-widgets
(deftest render-dirty-widgets-1
    (with-request :get nil
      (let ((weblocks::*dirty-widgets* (list (make-instance 'composite)
					     (lambda (&rest args)
					       (with-html (:p "test")))))
	    (*weblocks-output-stream* (make-string-output-stream))
	    (*on-ajax-complete-scripts* (list "testjs")))
	(declare (special weblocks::*dirty-widgets*
			  *weblocks-output-stream* *on-ajax-complete-scripts*))
	(weblocks::render-dirty-widgets)
	(values
	 (header-out "X-JSON")
	 (get-output-stream-string *weblocks-output-stream*))))
  #.(format nil "~
{~
\"widgets\":~
{~
\"widget-123\":\"\",~
null:\"<p>test</p>\"~
},~
\"on-load\":[\"testjs\"]~
}")
  ; this is necessary for Safari
  " ")

;;; test refresh-request-p
(deftest refresh-request-p-1
    (with-request :get nil
      (setf (session-value 'weblocks::last-request-uri) '("foo" "bar"))
      (refresh-request-p))
  t)

(deftest refresh-request-p-2
    (with-request :get nil
      (setf (session-value 'weblocks::last-request-uri) '("foo"))
      (refresh-request-p))
  nil)

(deftest refresh-request-p-3
    (with-request :get `((,weblocks::*action-string* . "abc123"))
      (make-action (lambda () nil))
      (setf (session-value 'weblocks::last-request-uri) '("foo" "bar"))
      (refresh-request-p))
  nil)
