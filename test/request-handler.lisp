
(in-package :weblocks-test)

;;; testing handle-client-request
(deftest handle-client-request-1
    (with-request :get nil
      (let (weblocks::*webapp-name* result1 result2
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
	;; handle another request (make sure form is drawn)
	(setf result2 (handle-client-request))
	(values (null (session-value "debug-reset-sessions")) result1 result2)))
  t
  #.(format nil
	    "~
<?xml version=\"1.0\" encoding=\"utf-8\" ?>~
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" ~
\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">~
<html>~
<head>~
<title>Hello!</title>~
<link rel='stylesheet' type='text/css' href='/pub/main.css' />~
<link rel='stylesheet' type='text/css' href='/pub/navigation.css' />~
<link rel='stylesheet' type='text/css' href='/pub/form.css' />~
<link rel='stylesheet' type='text/css' href='/pub/data.css' />~
<link rel='stylesheet' type='text/css' href='/pub/table.css' />~
<link rel='stylesheet' type='text/css' href='/pub/layout.css' />~
</head>~
<body>~
<div class='page-wrapper'>~
<div class='extra-top-1'>&nbsp;</div>~
<div class='extra-top-2'>&nbsp;</div>~
<div class='extra-top-3'>&nbsp;</div>~
<div class='widget composite' id='root'>~
<div class='widget dataform'>~
<div class='renderer data employee'>~
<div class='extra-top-1'>&nbsp;</div>~
<div class='extra-top-2'>&nbsp;</div>~
<div class='extra-top-3'>&nbsp;</div>~
<h1><span class='action'>Viewing:&nbsp;</span><span class='object'>Employee</span></h1>~
<ul>~
<li><span class='label'>Name:&nbsp;</span><span class='value'>Joe</span></li>~
<li><span class='label'>Manager:&nbsp;</span><span class='value'>Jim</span></li>~
</ul>~
<div class='submit'><a href='?action=abc123'>Modify</a></div>~
<div class='extra-bottom-1'>&nbsp;</div>~
<div class='extra-bottom-2'>&nbsp;</div>~
<div class='extra-bottom-3'>&nbsp;</div>~
</div>~
</div>~
</div>~
<div class='extra-bottom-1'>&nbsp;</div>~
<div class='extra-bottom-2'>&nbsp;</div>~
<div class='extra-bottom-3'>&nbsp;</div>~
</div>~
</body>~
</html>")
  #.(format nil
	    "~
<?xml version=\"1.0\" encoding=\"utf-8\" ?>~
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" ~
\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">~
<html>~
<head>~
<title>Hello!</title>~
<link rel='stylesheet' type='text/css' href='/pub/main.css' />~
<link rel='stylesheet' type='text/css' href='/pub/navigation.css' />~
<link rel='stylesheet' type='text/css' href='/pub/form.css' />~
<link rel='stylesheet' type='text/css' href='/pub/data.css' />~
<link rel='stylesheet' type='text/css' href='/pub/table.css' />~
<link rel='stylesheet' type='text/css' href='/pub/layout.css' />~
</head>~
<body>~
<div class='page-wrapper'>~
<div class='extra-top-1'>&nbsp;</div>~
<div class='extra-top-2'>&nbsp;</div>~
<div class='extra-top-3'>&nbsp;</div>~
<div class='widget composite' id='root'>~
<div class='widget dataform'>~
<form class='renderer form employee' action='' method='post'><div class='extra-top-1'>&nbsp;</div>~
<div class='extra-top-2'>&nbsp;</div>~
<div class='extra-top-3'>&nbsp;</div>~
<fieldset><h1><span class='action'>Modifying:&nbsp;</span>~
<span class='object'>Employee</span>~
</h1>~
<h2 class='form-fields-title'>Form fields:</h2>~
<ul><li><label><span>Name:&nbsp;</span>~
<input type='text' name='name' value='Joe' />~
</label>~
</li>~
<li><label><span>Manager:&nbsp;</span>~
<input type='text' name='manager' value='Jim' />~
</label>~
</li>~
</ul>~
<div class='submit'>~
<input name='submit' type='submit' value='Submit' />~
<input name='cancel' type='submit' value='Cancel' />~
<input name='action' type='hidden' value='abc124' />~
</div>~
</fieldset>~
<div class='extra-bottom-1'>&nbsp;</div>~
<div class='extra-bottom-2'>&nbsp;</div>~
<div class='extra-bottom-3'>&nbsp;</div>~
</form>~
</div>~
</div>~
<div class='extra-bottom-1'>&nbsp;</div>~
<div class='extra-bottom-2'>&nbsp;</div>~
<div class='extra-bottom-3'>&nbsp;</div>~
</div>~
</body>~
</html>"))

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
  #.(format nil
	    "~
<?xml version=\"1.0\" encoding=\"utf-8\" ?>~
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" ~
\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">~
<html>~
<head>~
<title>Hello!</title>~
<link rel='stylesheet' type='text/css' href='/pub/main.css' />~
<link rel='stylesheet' type='text/css' href='/pub/navigation.css' />~
<link rel='stylesheet' type='text/css' href='/pub/form.css' />~
<link rel='stylesheet' type='text/css' href='/pub/data.css' />~
<link rel='stylesheet' type='text/css' href='/pub/table.css' />~
<link rel='stylesheet' type='text/css' href='/pub/layout.css' />~
<link rel='stylesheet' type='text/css' href='/pub/debug-mode.css' />~
</head>~
<body>~
<div class='page-wrapper'>~
<div class='extra-top-1'>&nbsp;</div>~
<div class='extra-top-2'>&nbsp;</div>~
<div class='extra-top-3'>&nbsp;</div>~
<div class='widget composite' id='root'>~
<div class='widget function'>~
</div>~
</div>~
<div class='extra-bottom-1'>&nbsp;</div>~
<div class='extra-bottom-2'>&nbsp;</div>~
<div class='extra-bottom-3'>&nbsp;</div>~
</div>~
<div class='debug-toolbar'>~
<a href='?action=debug-reset-sessions' title='Reset Sessions'>~
<img src='/pub/images/reset.png' alt='Reset Sessions' /></a>~
</div>~
</body>~
</html>")
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
  #.(format nil
	    "~
<?xml version=\"1.0\" encoding=\"utf-8\" ?>~
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" ~
\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">~
<html>~
<head>~
<title>Hello!</title>~
<link rel='stylesheet' type='text/css' href='/pub/main.css' />~
<link rel='stylesheet' type='text/css' href='/pub/navigation.css' />~
<link rel='stylesheet' type='text/css' href='/pub/form.css' />~
<link rel='stylesheet' type='text/css' href='/pub/data.css' />~
<link rel='stylesheet' type='text/css' href='/pub/table.css' />~
<link rel='stylesheet' type='text/css' href='/pub/layout.css' />~
</head>~
<body>~
<div class='page-wrapper'>~
<div class='extra-top-1'>&nbsp;</div>~
<div class='extra-top-2'>&nbsp;</div>~
<div class='extra-top-3'>&nbsp;</div>~
<div class='widget composite' id='root'>~
<div class='widget navigation' id='test-nav'>~
<div class='widget function'><div>hi2</div></div>~
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
</div>~
</div>~
<div class='extra-bottom-1'>&nbsp;</div>~
<div class='extra-bottom-2'>&nbsp;</div>~
<div class='extra-bottom-3'>&nbsp;</div>~
</div>~
</body>~
</html>"))

;;; helper to create complex site layout
(defun create-site-layout ()
  (make-instance 'composite :widgets
		 (list
		  (make-instance
		   'composite
		   :widgets
		   (list
		    (make-instance 'composite)
		    (make-navigation "test-nav-1"
				     "test1" (make-instance
					      'composite
					      :widgets
					      (list
					       (make-instance 'composite)
					       (make-navigation "test-nav-2"
								"test3" (lambda (&rest args) nil)
								"test4" (lambda (&rest args) nil))))
				     "test2" (make-instance
					      'composite
					      :widgets
					      (list
					       (make-instance 'composite)
					       (make-navigation "test-nav-3"
								"test5" (lambda (&rest args) nil)
								"test6" (lambda (&rest args) nil))))))))))

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
