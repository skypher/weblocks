
(defpackage #:weblocks-test
  (:use :cl :weblocks :rt :c2mop :cl-who :hunchentoot)
  (:export #:test-weblocks))

(in-package :weblocks-test)

(defun test-weblocks ()
  "Call this function to run all unit tests defined in
weblocks-test package."
  (do-tests))

(defmacro deftest-html (name form value)
  "A helper macro for creating html test cases. The macro writes
code that temporarily binds the output stream to a string stream
and then compares the string to the expected result."
  (let ((expected-result (eval
			  `(with-html-output-to-string (s)
			     ,value))))
    `(deftest ,name
	 (let ((stream-bak *weblocks-output-stream*)
	       result)
	   (setf *weblocks-output-stream* (make-string-output-stream))
	   ,form
	   (setf result (get-output-stream-string *weblocks-output-stream*))
	   (setf *weblocks-output-stream* stream-bak)
	   result)
       ,expected-result)))

;;; faking hunchentoot's requests
(defclass unittest-request ()
  ((headers-in :initform nil)
   method uri server-protocol
   (content-stream :reader content-stream)
   (cookies-in :initform nil)
   (get-parameters :initform nil)
   (post-parameters :initform nil)
   (script-name :initform nil)
   (query-string :initform nil)
   (session :initform nil
            :accessor hunchentoot::session)
   (aux-data :initform nil
             :accessor hunchentoot::aux-data)
   (raw-post-data :initform nil))
  (:documentation "A class used to mock hunchentoot requests in
  order to be able to unit test across requests."))

;;; faking hunchentoot's server
(defclass unittest-server ()
  ((mod-lisp-p :initform nil
               :initarg :mod-lisp-p
               :reader server-mod-lisp-p))
  (:documentation "A class used to mock hunchentoot server in
  order to be able to unit test across requests."))

(defmethod hunchentoot::server-mod-lisp-p ((obj unittest-server))
  (slot-value obj 'mod-lisp-p))

(defmacro with-request (method parameters &body body)
  "A helper macro for test cases across web requests. The macro
sets up a request and executes code within its context.

'method' - A method with which the request was initiated (:get
or :post)
'parameters' - An association list of parameters sent along with
the request."
  (let ((parameters-slot (ecase method
			   (:get 'get-parameters)
			   (:post 'post-parameters))))
    `(let ((*server* (make-instance 'unittest-server))
	   (*request* (make-instance 'unittest-request)))
       (setf (slot-value *request* 'method) ,method)
       (setf (slot-value *request* ',parameters-slot) ,parameters)
       ,@body)))
