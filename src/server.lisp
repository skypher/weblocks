
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

(defun start-weblocks ()
  (if (null *weblocks-server*)
      (setf *weblocks-server* (start-server :port 8080))))

(defun stop-weblocks ()
  (if (not (null *weblocks-server*))
      (progn
	(stop-server *weblocks-server*)
	(setf *weblocks-server* nil))))

(defun hala1 ()
  (let ((name (first-name *joe-employee*)))
    (if (eql name "Slava")
	(setf (first-name *joe-employee*) "Dima")
	(setf (first-name *joe-employee*) "Slava"))))

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
      (render-table (list *joe-employee* *joe-employee* *joe-employee*) :caption "Employees")
      (htm (:div (:p)))
      (render-table '() :caption "Employees")
      (htm (:div (:p)))
      (render *dataform-widget*))))
  (get-output-stream-string *weblocks-output-stream*))

(setf *dispatch-table*
      (append (list (create-folder-dispatcher-and-handler "/pub/" "/home/coffeemug/projects/weblocks2/pub/")
		    (create-prefix-dispatcher "/" 'handle-client-request))
	      *dispatch-table*))
