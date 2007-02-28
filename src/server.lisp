
(in-package :weblocks)

(defvar *weblocks-server* nil)

(defun start-weblocks ()
  (if (null *weblocks-server*)
      (setf *weblocks-server* (start-server :port 8080))))

(defun stop-weblocks ()
  (if (not (null *weblocks-server*))
      (stop-server *weblocks-server*)))

(defun hala ()
  (format *weblocks-output-stream* "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML Basic 1.0//EN\" \"http://www.w3.org/TR/xhtml-basic/xhtml-basic10.dtd\">
<html>
<head>
<link rel=\"stylesheet\" type=\"text/css\" href=\"pub/stylesheet.css\" />
</head>~%~%<body>~%")
  (render-data *joe-employee* :slot-names '((first-name . fn)))
  (format *weblocks-output-stream* "</body></html>")

  (get-output-stream-string *weblocks-output-stream*))

(setf *dispatch-table*
      (append (list (create-prefix-dispatcher "/test" 'hala)
		    (create-folder-dispatcher-and-handler "/pub/" "/home/coffeemug/projects/weblocks2/pub/"))
	      *dispatch-table*))
