
(defpackage #:weblocks-test
  (:use :cl :weblocks :rtest :c2mop :cl-who :hunchentoot :metatilities :moptilities)
  (:shadowing-import-from :c2mop #:defclass #:ensure-generic-function
			  #:standard-generic-function #:defmethod #:defgeneric #:standard-class)
  (:shadow #:do-test #:do-tests #:continue-testing)
  (:export #:test-weblocks))

(in-package :weblocks-test)

(defmacro with-test-environment (&body body)
  "This macro takes steps to clear the environment for the unit
tests. For example, if an application is defined that may interfere
with the unit tests, this function removes the application. All
changes are rolled back after the tests are done. Note, the steps that
this macro takes may not be sufficient. If some tests fail, try to run
the test suite without loading an application."
  `(let ((app-name weblocks::*webapp-name*) interference-methods
	 result)
     ;; hide application
     (setf weblocks::*webapp-name* nil)
     ;; remove before/after methods from render-page-body
     (mapcar (lambda (m)
	       (let ((qualifiers (method-qualifiers m)))
		 (when (or (find :before qualifiers)
			   (find :after qualifiers))
		   (remove-method #'render-page-body m)
		   (push m interference-methods))))
	     (generic-function-methods #'render-page-body))
     ;; insert the body
     (setf result (progn ,@body))
     ;; reinstate the application
     (setf weblocks::*webapp-name* app-name)
     ;; reinstate render-page-body before/after methods
     (loop for m in interference-methods
	do (add-method #'render-page-body m))
     result))

(defun do-test ()
  "Shadows rt's 'do-test'. This function calls rt's do test in a clean
test environment. See 'with-test-environment'."
  (with-test-environment
      (rtest::do-test)))

(defun do-tests ()
  "Shadows rt's 'do-tests'. This function calls rt's do-tests in a
clean test environment. See 'with-test-environment'."
  (with-test-environment
      (rtest::do-tests)))

(defun test-weblocks ()
  "Call this function to run all unit tests defined in 'weblocks-test'
package. This function tests weblocks in a clean environment. See
'with-test-environment' for more details."
  (do-tests))

(defun continue-testing ()
  "Shadows rt's 'continue-testing'. This function calls rt's
continue-testing in a clean test environment. See
'with-test-environment'."
  (with-test-environment
      (rtest::continue-testing)))

(defparameter *test-widget-id* 0
  "Used to generate a unique ID for fixtures.")

(defun gen-object-id ()
  "Generates an object id that is guranteed to be unique and
consecutive per test, as long as the instance is created within
'deftest-html'."
  (declare (special *test-widget-id*))
  (incf *test-widget-id*))

(defmacro deftest-html (name form value)
  "A helper macro for creating html test cases. The macro writes
code that temporarily binds the output stream to a string stream
and then compares the string to the expected result."
  (let ((expected-result (eval
			  `(with-html-output-to-string (s)
			     ,value))))
    `(deftest ,name
	 (let ((stream-bak *weblocks-output-stream*)
	       (*test-widget-id* 1000)
	       result)
	   (declare (special *test-widget-id*))
	   (setf *weblocks-output-stream* (make-string-output-stream))
	   ,form
	   (setf result (get-output-stream-string *weblocks-output-stream*))
	   (setf *weblocks-output-stream* stream-bak)
	   result)
       ,expected-result)))

;;; faking hunchentoot's requests
(defclass unittest-request ()
  ((headers-in :initform nil)
   method server-protocol
   (hunchentoot::uri :initform nil)
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
               :reader hunchentoot::server-mod-lisp-p)
   (ssl-certificate-file :initarg :ssl-certificate-file
			 :initform nil
                         :reader hunchentoot::server-ssl-certificate-file))
  (:documentation "A class used to mock hunchentoot server in
  order to be able to unit test across requests."))

(defmethod hunchentoot::server-mod-lisp-p ((obj unittest-server))
  (slot-value obj 'mod-lisp-p))

(defparameter *dummy-action* "abc"
  "A dummy action code for unit tests.")

(defmacro with-request (method parameters &body body)
  "A helper macro for test cases across web requests. The macro
sets up a request and a session, and executes code within their
context.

'method' - A method with which the request was initiated (:get
or :post)
'parameters' - An association list of parameters sent along with
the request."
  (let ((parameters-slot (ecase method
			   (:get 'get-parameters)
			   (:post 'post-parameters))))
    `(let* ((*request* (make-instance 'unittest-request))
	    (*server* (make-instance 'unittest-server))
	    (hunchentoot::*remote-host* "localhost")
	    (hunchentoot::*session-secret* (hunchentoot::reset-session-secret))
	    (hunchentoot::*reply* (make-instance 'hunchentoot::reply))
	    (*session* (start-session))
	    (make-action-orig #'weblocks::make-action)
	    (generate-widget-id-orig #'weblocks::generate-widget-id)
	    (dummy-action-count 123)
	    (*session-cookie-name* "weblocks-session")
	    (*uri-tokens* '("foo" "bar"))
	    weblocks::*page-public-dependencies*)
       (declare (special *uri-tokens* weblocks::*page-public-dependencies*))
       (unwind-protect (progn
			 (setf (symbol-function 'weblocks::make-action)
			       (lambda (action-fn &optional action-code)
				 (if action-code
				   (funcall make-action-orig action-fn action-code)
				   (let ((result (funcall make-action-orig action-fn
							  (format nil "~A~D"
								  *dummy-action*
								  dummy-action-count))))
				     (incf dummy-action-count)
				     result))))
			 (setf (symbol-function 'weblocks::generate-widget-id)
			       (lambda ()
				 "widget-123"))
			 (setf (slot-value *request* 'method) ,method)
			 (setf (slot-value *request* ',parameters-slot) ,parameters)
			 (setf (slot-value *request* 'hunchentoot::script-name) "/foo/bar")
			 (setf (slot-value *session* 'hunchentoot::session-id) 1)
			 (setf (slot-value *session* 'hunchentoot::session-string) "test")
			 ,@body)
	 (setf (symbol-function 'weblocks::make-action) make-action-orig)
	 (setf (symbol-function 'weblocks::generate-widget-id) generate-widget-id-orig)))))

(defun do-request (parameters)
  "Mocks up a submitted request for unit tests."
  (setf (slot-value *request* (ecase (request-method)
				(:get 'get-parameters)
				(:post 'post-parameters))) parameters)
  (weblocks::eval-action))

(defun make-request-ajax ()
  "Adds appropriate headers to a request so it is considered to be an
AJAX request."
  (push '("X-Requested-With" . "test") (slot-value *request* 'headers-in)))
