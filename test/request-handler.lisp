
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
<link rel='stylesheet' type='text/css' href='pub/main.css' />~
<link rel='stylesheet' type='text/css' href='pub/navigation.css' />~
<link rel='stylesheet' type='text/css' href='pub/form.css' />~
<link rel='stylesheet' type='text/css' href='pub/data.css' />~
<link rel='stylesheet' type='text/css' href='pub/table.css' />~
</head>~
<body>~
<div class='widget composite'>~
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
<link rel='stylesheet' type='text/css' href='pub/main.css' />~
<link rel='stylesheet' type='text/css' href='pub/navigation.css' />~
<link rel='stylesheet' type='text/css' href='pub/form.css' />~
<link rel='stylesheet' type='text/css' href='pub/data.css' />~
<link rel='stylesheet' type='text/css' href='pub/table.css' />~
</head>~
<body>~
<div class='widget composite'>~
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
<div class='submit'><input name='action' type='hidden' value='abc124' />~
<input name='submit' type='submit' value='Submit' />~
<input name='cancel' type='submit' value='Cancel' />~
</div>~
</fieldset>~
<div class='extra-bottom-1'>&nbsp;</div>~
<div class='extra-bottom-2'>&nbsp;</div>~
<div class='extra-bottom-3'>&nbsp;</div>~
</form>~
</div>~
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
<link rel='stylesheet' type='text/css' href='pub/main.css' />~
<link rel='stylesheet' type='text/css' href='pub/navigation.css' />~
<link rel='stylesheet' type='text/css' href='pub/form.css' />~
<link rel='stylesheet' type='text/css' href='pub/data.css' />~
<link rel='stylesheet' type='text/css' href='pub/table.css' />~
<link rel='stylesheet' type='text/css' href='pub/debug-mode.css' />~
</head>~
<body>~
<div class='widget composite'>~
<div class='widget function'>~
</div>~
</div>~
<div class='debug-toolbar'>~
<a href='?action=debug-reset-sessions' title='Reset Sessions'>~
<img src='pub/reset.png' alt='Reset Sessions' /></a>~
</div>~
</body>~
</html>")
  t)
