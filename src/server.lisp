
(in-package :weblocks)

(defvar *weblocks-server* nil)

(defclass address ()
  ((street :reader street)
   (city)
   (state :reader state)))

(defclass person ()
  ((first-name :reader first-name)
   (last-name :reader last-name)
   (age :reader age)
;   (address-ref :reader address)
   (address :reader address)
   (id :initform 1)))

(defclass employee (person)
  ((department :reader department)))

(defparameter *joe-employee* (make-instance 'employee))

(setf (slot-value *joe-employee* 'first-name) "Slava")
(setf (slot-value *joe-employee* 'last-name) "Akhmechet")
(setf (slot-value *joe-employee* 'age) "23")

(let ((addr (make-instance 'address)))
  (setf (slot-value addr 'street) "1877 Ocean Ave.")
  (setf (slot-value addr 'city) "Brooklyn")
  (setf (slot-value addr 'state) "NY")
;  (setf (slot-value *joe-employee* 'address-ref) addr))
  (setf (slot-value *joe-employee* 'address) addr))


(setf (slot-value *joe-employee* 'department) "Technology")

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
  (render-data *joe-employee*)
  (format *weblocks-output-stream* "</body></html>")

  (get-output-stream-string *weblocks-output-stream*))

(setf *dispatch-table*
      (append (list (create-prefix-dispatcher "/test" 'hala)
		    (create-folder-dispatcher-and-handler "/pub/" "/home/coffeemug/projects/weblocks2/pub/"))
	      *dispatch-table*))
