
(in-package :weblocks)

(defvar *weblocks-server* nil)

(defclass address ()
  ((street :reader street)
   (city)
   (state :reader state)))

(defclass person ()
  ((first-name :accessor first-name)
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

(defparameter *dataform-widget* (make-instance 'dataform :data *joe-employee*))

(defun start-weblocks (&key debug)
  (when debug
    (setf *show-lisp-errors-p* t)
    (setf *show-lisp-backtraces-p* t))
  (when (null *weblocks-server*)
    (setf *session-cookie-name* "weblocks-session")
    (setf *weblocks-server* (start-server :port 8080))))

(defun stop-weblocks ()
  (if (not (null *weblocks-server*))
      (progn
	(stop-server *weblocks-server*)
	(setf *weblocks-server* nil))))

(defun hala ()
  (format *weblocks-output-stream* "<?xml version=\"1.0\" encoding=\"utf-8\" ?>")
  (with-html-output (*weblocks-output-stream* nil :prologue t)
    (:html
     (:head
      (:title "Hello!")
      (:link :rel "stylesheet" :type "text/css" :href "pub/main.css")
      (:link :rel "stylesheet" :type "text/css" :href "pub/form.css")
      (:link :rel "stylesheet" :type "text/css" :href "pub/data.css")
      (:link :rel "stylesheet" :type "text/css" :href "pub/table.css"))
     (:body
      (render *dataform-widget*))))
  (get-output-stream-string *weblocks-output-stream*))

(setf *dispatch-table*
      (append (list (create-folder-dispatcher-and-handler "/pub/" "/home/coffeemug/projects/weblocks2/pub/")
		    (create-prefix-dispatcher "/" 'handle-client-request))
	      *dispatch-table*))

(defvar *webapp-name* nil
  "The name of the currently running web application. See
  'defwebapp' for more details.")

(defun defwebapp (name)
  "Sets the application name (the *webapp-name* variable). 'name'
must be a symbol. This symbol will later be used to find a
package that defined 'init-user-session' - a function responsible
for the web application setup.

'init-user-session' must be defined by weblocks client in the
same package as 'name'. This function will accept a single
parameter - a composite widget at the root of the
application. 'init-user-session' is responsible for adding
initial widgets to this composite."
  (check-type name symbol)
  (setf *webapp-name* name))

(defwebapp 'weblocks-demo)

(defun init-user-session (comp)
  (push-end `("test" . ,(make-instance 'dataform :data *joe-employee*))
	    (composite-widgets comp)))
