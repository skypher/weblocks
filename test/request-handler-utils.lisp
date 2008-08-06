
(in-package :weblocks-test)

(defun with-request-template (body &key
			      (title "Hello")
			      render-debug-toolbar-p widget-stylesheets)
  (format nil "~
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" ~
\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">~%~
<html xmlns='http://www.w3.org/1999/xhtml'>~
<head>~
<title>~A</title>~
<meta http-equiv='Content-type' content='text/html; charset=utf-8' />~
<link rel='stylesheet' type='text/css' href='/pub/stylesheets/layout.css' />~
<link rel='stylesheet' type='text/css' href='/pub/stylesheets/main.css' />~
<link rel='stylesheet' type='text/css' href='/pub/stylesheets/dialog.css' />~
~A~
~A~
<script src='/pub/scripts/prototype.js' type='text/javascript'></script>~
<script src='/pub/scripts/scriptaculous.js' type='text/javascript'></script>~
<script src='/pub/scripts/shortcut.js' type='text/javascript'></script>~
<script src='/pub/scripts/weblocks.js' type='text/javascript'></script>~
<script src='/pub/scripts/dialog.js' type='text/javascript'></script>~
~A~
</head>~
<body>~
<div class='page-wrapper'>~
<div class='page-extra-top-1'><!-- empty --></div>~
<div class='page-extra-top-2'><!-- empty --></div>~
<div class='page-extra-top-3'><!-- empty --></div>~
<div class='widget composite' id='root'>~
~A~
</div>~
<div class='page-extra-bottom-1'><!-- empty --></div>~
<div class='page-extra-bottom-2'><!-- empty --></div>~
<div class='page-extra-bottom-3'><!-- empty --></div>~
</div>~
~A~
<div id='ajax-progress'>&nbsp;</div>~
</body>~
</html>"
	  title
	  (apply #'concatenate
		   'string
		   (loop for i in widget-stylesheets
		      collect (format
			       nil
			       "<link rel='stylesheet' type='text/css' href='/pub/stylesheets/~A.css' />" i)))
	  (if render-debug-toolbar-p (format nil "~
	       <link rel='stylesheet' type='text/css' href='/pub/stylesheets/debug-toolbar.css' />")
	      "")
	  (if render-debug-toolbar-p (format nil "~
	       <script src='/pub/scripts/weblocks-debug.js' type='text/javascript'></script>")
	      "")
	  (format nil body)
	  (if render-debug-toolbar-p (format nil "~
	       <div class='debug-toolbar'>~
               <a href='/foo/bar?action=debug-reset-sessions' title='Reset Sessions'>~
               <img src='/pub/images/reset.png' alt='Reset Sessions' /></a>~
               </div>")
	      "")))
