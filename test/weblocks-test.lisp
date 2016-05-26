
(defpackage #:weblocks-test
  (:use :cl :weblocks :weblocks-stores :weblocks-util :lift :c2mop :cl-who :hunchentoot :metatilities :moptilities
        :anaphora :f-underscore)
  (:shadowing-import-from :c2mop #:defclass #:defgeneric #:defmethod
                          #:standard-generic-function #:ensure-generic-function #:standard-class
                          #:typep #:subtypep 
                          #:standard-method)
  (:shadowing-import-from :weblocks 
                          #:redirect #:reset-sessions 
                          #:create-folder-dispatcher-and-handler 
                          #:create-prefix-dispatcher 
                          #:create-regex-dispatcher 
                          #:create-static-file-dispatcher-and-handler)
  (:export #:test-weblocks))

(in-package :weblocks-test)

(declaim (special *recovery-strategies*))
(defvar *joe* nil)
(defvar generate-widget-id-orig nil)

(defun call-with-test-environment (thunk)
  "Helper for `with-test-environment'."
  (let ((weblocks::*current-webapp* nil) ;hide application
        (*print-case* :upcase)       ;needed by some comparison output
        interference-methods)
    (declare (special weblocks::*current-webapp*))
    ;; remove before/after methods from render-page-body
    (mapcar (lambda (m)
              (let ((qualifiers (method-qualifiers m)))
                (when (or (find :before qualifiers)
                          (find :after qualifiers))
                  (remove-method #'render-page-body m)
                  (push m interference-methods))))
            (generic-function-methods #'render-page-body))
    ;; do the body
    (unwind-protect (funcall thunk)
    ;; reinstate render-page-body before/after methods
      (loop for m in interference-methods
            do (add-method #'render-page-body m)))))

(defmacro with-test-environment (&body body)
  "This macro takes steps to clear the environment for the unit
tests. For example, if testing in the context of an application, it
may interfere with the unit tests, so we remove the application. All
changes are rolled back after the tests are done. Note, the steps that
this macro takes may not be sufficient. If some tests fail, try to run
the test suite without loading an application."
  `(call-with-test-environment (lambda () . ,body)))

(defmacro set-sensible-suite ()
  "Set up a sensible testsuite to use as the testsuite for addtest
forms that may not have a suite defined in-file, in the file in which
I am expanded.  Likely to work only at toplevel."
  (let ((inner-part
         `(let ((last-set-suite lift::*current-testsuite-name*))
            (unless (and file
                         (string-contains-p
                          (symbol-name last-set-suite) (pathname-name file)))
              (setf lift::*current-testsuite-name* 'weblocks-suite)))))
    `(progn
       (eval-when (:compile-toplevel)
         (let ((file *compile-file-pathname*))
           ,inner-part))
       (eval-when (:load-toplevel :execute)
         (let ((file *load-truename*))
           ,inner-part)))))

(defmacro deftest (name form &rest values)
  "Define a test in an appropriate testsuite called NAME, ensuring
that FORM's values and the literal VALUES are equal."
  `(progn
     (set-sensible-suite)
     (addtest ,name
       (ensure-same ,form ,(if (typep values '(cons t null))
                               `',(first values)
                               `(values . ,(mapcar (f_ `',_) values)))))))

(defmacro defjstest (name form value)
  "Define a test in an appropriate testsuite called NAME, ensuring
that FORM's values and the literal VALUES are equal."
  `(progn
     (set-sensible-suite)
     (addtest ,name
       (ensure-same ,form ,(if (atom value)
                             `(with-javascript-to-string ,value)
                             `(mapcar (lambda (x) (with-javascript-to-string x)) ',value))))))

(defun test-weblocks (&optional (verbose t))
  "Call this function to run all unit tests defined in 'weblocks-test'
package. This function tests weblocks in a clean environment. See
'with-test-environment' for more details.

Pass NIL as the optional arg to just return the results instead of
DESCRIBE-ing them."
  ;; XXX better results combination
  (setf weblocks::*dirty-widgets* nil)
  (let ((results (list (run-tests :suite 'weblocks-suite))))
    (when verbose
      (mapc #'describe results))
    (values-list results)))

(defparameter *test-widget-id* 0
  "Used to generate a unique ID for fixtures.")

;; see store/store-utils.lisp
(defwebapp app-with-not-searchable-store
  :js-backend :prototype
  :autostart nil)

; We'll use memory store for testing
(weblocks-stores:defstore *test-store* :memory)

(defmacro deftest-html (name form value)
  "A helper macro for creating html test cases. The macro writes
code that temporarily binds the output stream to a string stream
and then compares the string to the expected result."
  `(progn
     (set-sensible-suite)
     (addtest ,name
       (ensure-html-output ,form ,value))))

;;; faking hunchentoot's requests
(defclass unittest-request (request)
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
  (:default-initargs :remote-addr "localhost")
  (:documentation "A class used to mock hunchentoot requests in
  order to be able to unit test across requests."))

;;; faking hunchentoot's server
(defclass unittest-server (weblocks-acceptor)
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

(defmethod session-cookie-name ((obj unittest-server))
  "weblocks-session")

(defparameter *dummy-action* "abc"
  "A dummy action code for unit tests.")

(defun call-with-test-webapp (thunk &rest initargs &key full class-name &allow-other-keys)
  "Helper for WITH-TEST-WEBAPP's expansion."
  (remf initargs :full)
  (remf initargs :class-name)
  (let* ((app (apply #'make-instance (or class-name 'weblocks::weblocks-webapp)
                     `(,@initargs ,@(and (not class-name) '(:prefix ""))
                       :html-indent-p nil 
                       :js-backend :prototype)))
         (weblocks::*current-webapp* app))
     (declare (special weblocks::*current-webapp*))
     (if full
       (progn
         (assert class-name (class-name)
                 "A specific webapp must be defined for `with-webapp's :full parameter")
         (start-webapp class-name)
         (unwind-protect (funcall thunk)
           (stop-webapp class-name)))
       (funcall thunk))))

(defmacro with-test-webapp ((&rest initargs &key full class-name &allow-other-keys) &body body)
  "A helper macro (and marker) for test cases calling functions that
only work with a current webapp, in which BODY is evaluated in the
context of a temporary `weblocks-webapp'.  INITARGS are passed through
to `make-instance'.

If FULL is given, I will also start the webapp within BODY's context;
if CLASS-NAME is given, I will ignore other INITARGS and either find
or start a webapp with the class CLASS-NAME, setting it as the current
webapp in my context."
  ;; We just pass FULL and CLASS-NAME through as part of the
  ;; rest list. They are mentioned explicitly in the lambda list
  ;; for documentation purposes.
  (declare (ignore full class-name))
  `(call-with-test-webapp (lambda () ,@body) ,@initargs))

(defun call-with-request-in-webapp-context (thunk method parameters
                                            &key (uri nil uri?))
  "Helper for `call-with-request'."
  (let ((parameters-slot (ecase method
                           (:get 'get-parameters)
                           (:post 'post-parameters))))
    (let* ((*acceptor* (make-instance 'unittest-server))
           (*weblocks-server* *acceptor*)
           (*request* (make-instance 'unittest-request :acceptor *acceptor*))
           (hunchentoot::*session-secret* (hunchentoot::reset-session-secret))
           (hunchentoot::*reply* (make-instance 'hunchentoot::reply))
           (make-action-orig #'weblocks::make-action)
           (generate-widget-id-orig #'weblocks::gen-id)
           (dummy-action-count 123)
           (*uri-tokens* '("foo" "bar"))
           weblocks::*page-dependencies* *session*
           *on-ajax-complete-scripts*
           weblocks::*rendered-actions*)
      (declare (special *uri-tokens* weblocks::*page-dependencies* *session*
                        *on-ajax-complete-scripts* weblocks::*rendered-actions* 
                        weblocks-util:*parts-md5-hash*
                        weblocks-util:*parts-md5-context-hash*))
      (unwind-protect (progn
                        (setf weblocks-util:*parts-md5-hash* (make-hash-table :test 'equal))
                        (setf weblocks-util:*parts-md5-context-hash* (make-hash-table :test 'equal))

                        (weblocks::open-stores)
                        (start-session)
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
                        (setf (symbol-function 'weblocks::gen-id)
                              (lambda (&optional prefix)
                                (declare (ignore prefix))
                                "id-123"))
                        (setf (slot-value *request* 'method) method)
                        (setf (slot-value *request* parameters-slot) parameters)
                        (setf (slot-value *request* 'hunchentoot::script-name) "/foo/bar")
                        (setf (slot-value *session* 'hunchentoot::session-id) 1)
                        (setf (slot-value *session* 'hunchentoot::session-string) "test")
                        (when uri?
                          (setf *uri-tokens* (weblocks::tokenize-uri uri)))
                        (setf (slot-value *request* 'hunchentoot::uri)
                              (or uri (concatenate 'string "/"
                                                   (uri-tokens-to-string *uri-tokens*))))
                        (funcall thunk))
        (setf (symbol-function 'weblocks::make-action) make-action-orig)
        (setf (symbol-function 'weblocks::gen-id) generate-widget-id-orig)
        (weblocks-stores::close-stores)))))

(defun call-with-request (&rest args)
  "Helper for `with-request''s expansion."
  (if (and (boundp 'weblocks::*current-webapp*)
           (let ((app-type (class-name (class-of (weblocks::current-webapp)))))
             (or (eq 'weblocks-webapp app-type)
                 (equalp "WEBLOCKS-TEST"
                         (package-name (symbol-package app-type))))))
      (apply #'call-with-request-in-webapp-context args)
      (with-test-webapp ()
        (apply #'call-with-request-in-webapp-context args))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun span-keyword-params (implicit-progn)
    "Take the plist from the front of IMPLICIT-PROGN, and answer it and
the remaining forms.  Note that there must be at least one non-plist
form, because we want to always maintain \"returns value of last
form\" semantics to reduce any confusion caused by this syntax.

Anyway, if you want to force interpretation of a keyword in
IMPLICIT-PROGN as a prognable form, just quote it."
    (loop for forms on implicit-progn by #'cddr
       while (typep forms '(cons keyword (cons t cons)))
       append (subseq forms 0 2) into plist
       finally (return (values plist forms)))))

(defmacro with-request (method parameters &body body)
  "A helper macro for test cases across web requests. The macro
sets up a request and a session, and executes code within their
context.

'method' - A method with which the request was initiated (:get
or :post)
'parameters' - An association list of parameters sent along with
the request.

Other parameters given as keywords on the front of BODY:

URI - Set the Hunchentoot request URI to this."
  (multiple-value-bind (kwargs body) (span-keyword-params body)
    `(call-with-request (lambda () ,@body) ,method ,parameters ,@kwargs)))

(defun do-request (parameters)
  "Mocks up a submitted request for unit tests."
  (setf (slot-value *request* (ecase (request-method*)
                                (:get 'get-parameters)
                                (:post 'post-parameters))) parameters)
  (weblocks::eval-action))

(defun do-request-and-render-dirty (parameters)
  "Calls `do-request' and then renders dirty widgets."
  (do-request parameters)
  (weblocks::render-dirty-widgets))

(defun do-action (action-name &rest args)
  "A friendlier interface for do-request-and-render-dirty."
  (do-request-and-render-dirty
    (cons (cons weblocks::*action-string* action-name)
          (loop
             for i on args by #'cddr
             collect (cons (car i) (cadr i))))))

(defun make-request-ajax ()
  "Adds appropriate headers to a request so it is considered to be an
AJAX request."
  (push '("X-Requested-With" . "XMLHttpRequest") (slot-value *request* 'headers-in)))

(defvar *recovery-strategies*
  `((with-plain-webapp . ,(lambda (thunk)
                            (with-test-webapp () (funcall thunk))))
    (with-simple-request . ,(lambda (thunk)
                              (with-request :get nil (funcall thunk)))))
  "Alist of strategies to try in turn when a test fails in `do-test'.
  Each cdr is passed a thunk that will perform the test and determine
  whether it succeeded.  If one succeeds, the car is used as a label
  for the successful patched test when reporting it.")
