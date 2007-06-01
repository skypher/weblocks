
(in-package :weblocks-test)

(defun with-request-template (body &key render-debug-toolbar-p)
  (format nil "~
<?xml version=\"1.0\" encoding=\"iso-8859-1\" ?>~
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" ~
\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">~
<html>~
<head>~
<title>Hello!</title>~
<link rel='stylesheet' type='text/css' href='/pub/main.css' />~
<link rel='stylesheet' type='text/css' href='/pub/flash.css' />~
<link rel='stylesheet' type='text/css' href='/pub/navigation.css' />~
<link rel='stylesheet' type='text/css' href='/pub/form.css' />~
<link rel='stylesheet' type='text/css' href='/pub/suggest.css' />~
<link rel='stylesheet' type='text/css' href='/pub/data.css' />~
<link rel='stylesheet' type='text/css' href='/pub/table.css' />~
<link rel='stylesheet' type='text/css' href='/pub/layout.css' />~
~A~
<script src='/pub/scripts/prototype.js' type='text/javascript'></script>~
<script src='/pub/scripts/weblocks.js' type='text/javascript'></script>~
<script src='/pub/scripts/scriptaculous.js?load=effects,controls' type='text/javascript'></script>~
</head>~
<body>~
<div class='page-wrapper'>~
<div class='extra-top-1'>&nbsp;</div>~
<div class='extra-top-2'>&nbsp;</div>~
<div class='extra-top-3'>&nbsp;</div>~
<div class='widget composite' id='root'>~
~A~
</div>~
<div class='extra-bottom-1'>&nbsp;</div>~
<div class='extra-bottom-2'>&nbsp;</div>~
<div class='extra-bottom-3'>&nbsp;</div>~
</div>~
~A~
<div id='ajax-progress'>&nbsp;</div>~
</body>~
</html>"
	  (if render-debug-toolbar-p (format nil "~
	       <link rel='stylesheet' type='text/css' href='/pub/debug-mode.css' />")
	      "")
	  (format nil body)
	  (if render-debug-toolbar-p (format nil "~
	       <div class='debug-toolbar'>~
               <a href='?action=debug-reset-sessions' title='Reset Sessions'>~
               <img src='/pub/images/reset.png' alt='Reset Sessions' /></a>~
               </div>")
	      "")))
