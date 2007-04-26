
(in-package :weblocks-test)

;;; testing make-action
(deftest make-action/get-request-action-1
    (with-request :get nil
      (let ((action-name (make-action (lambda () 123))))
	(do-request `(("action" . ,action-name)))))
  123)

(deftest make-action/get-request-action-2
    (with-request :post nil
      (let ((action-name (make-action (lambda () 123))))
	(do-request `(("action" . ,action-name)))))
  123)

;;; testing render-link
(deftest-html render-link-1
    (render-link "abc123" "some link")
  (:a :href "?action=abc123" "some link"))

;;; testing handle-client-request
(deftest handle-client-request-1
    (with-request :get nil
      (let (weblocks::*webapp-name* result1 result2)
	;; set up our mini-application with one dataform widget
	(declare (special weblocks::*webapp-name*))
	(defwebapp 'hello)
	(defun init-user-session (comp)
	  (push `("test" . ,(make-instance 'dataform :data *joe*)) (composite-widgets comp)))
	;; handle the first request (make sure data is drawn)
	(setf result1 (handle-client-request))
	;; unbind init-user-session to make root-composite persists
	(fmakunbound 'init-user-session)
	;; fake user clicking on "modify"
	(setf (slot-value *request* 'get-parameters) `((,weblocks::*action-string* . ,*dummy-action*)))
	;; handle another request (make sure form is drawn)
	(setf result2 (handle-client-request))
	(values result1 result2)))
  #.(format nil
	    "~
<?xml version=\"1.0\" encoding=\"utf-8\" ?>~
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" ~
\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">~
<html>~
<head>~
<title>Hello!</title>~
<link rel='stylesheet' type='text/css' href='pub/main.css' />~
<link rel='stylesheet' type='text/css' href='pub/form.css' />~
<link rel='stylesheet' type='text/css' href='pub/data.css' />~
<link rel='stylesheet' type='text/css' href='pub/table.css' />~
</head>~
<body>~
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
<link rel='stylesheet' type='text/css' href='pub/form.css' />~
<link rel='stylesheet' type='text/css' href='pub/data.css' />~
<link rel='stylesheet' type='text/css' href='pub/table.css' />~
</head>~
<body><form class='renderer form employee' action='' method='post'><div class='extra-top-1'>&nbsp;</div>~
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
<div class='submit'><input name='action' type='hidden' value='abc123' />~
<input name='submit' type='submit' value='Submit' />~
<input name='cancel' type='submit' value='Cancel' />~
</div>~
</fieldset>~
<div class='extra-bottom-1'>&nbsp;</div>~
<div class='extra-bottom-2'>&nbsp;</div>~
<div class='extra-bottom-3'>&nbsp;</div>~
</form>~
</body>~
</html>"))
