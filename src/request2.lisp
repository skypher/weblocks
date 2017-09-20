(defpackage #:weblocks.request
  (:use #:cl)
  (:export
   #:*request*
   #:request-parameters
   #:request-parameter
   #:request-header
   #:ajax-request-p
   #:request-server-name
   #:request-server-port
   #:request-uri
   #:request-method
   #:request-path-info
   #:refresh-request-p))
(in-package weblocks.request)


(defvar *request* nil
  "Holds current request from a browser.")


(defvar *latest-request* nil
  "For debugging")


(defun request-uri (&optional (request *request*))
  "For URL http://example.com/foo/bar?blah=minor returns
/foo/bar?blah=minor path of the request's URL."
  (lack.request:request-uri request))


(defun request-path-info (&optional (request *request*))
  "For URL http://example.com/foo/bar?blah=minor returns
/foo/bar path of the request's URL."
  (lack.request:request-path-info request))


(defun request-host (&optional (request *request*))
  "For URL http://example.com/foo/bar?blah=minor returns
/foo/bar path of the request's URL."
  (lack.request:request-path-info request))


(defun request-server-name (&optional (request *request*))
  (lack.request:request-server-name request))


(defun request-server-port (&optional (request *request*))
  (lack.request:request-server-port request))


(defun request-method (&key (request *request*))
  "Returns association list with GET or POST parameters for current request."
  (lack.request:request-method request))


(defun request-parameters (&key (request *request*))
  "Returns association list with GET or POST parameters for current request."
  (lack.request:request-parameters request))


(defun request-parameter (name &key (request *request*))
  "Returns GET or POST parameter by name."
  (declare (type string name))

  (let ((params (request-parameters :request request)))
    (alexandria:assoc-value params
                            name
                            :test #'equal)))


(defun request-header (name &key (request *request*))
  "Returns value of the HTTP header or nil. Name is case insensitive."
  (let ((headers (lack.request:request-headers request))
        (lowercased-name (string-downcase name)))
    (gethash lowercased-name
             headers)))


(defun ajax-request-p (&optional (request *request*))
  "Detects if the current request was initiated via AJAX by looking
for 'X-Requested-With' http header. This function expects to be called
in a dynamic hunchentoot environment."
  ;; Sometimes this function may be called not in the context of request,
  ;; to update an instance of the widgets in asyncrounous code and to send
  ;; it via websocket.
  (and request
       (equal (request-header "X-Requested-With" :request request)
              "XMLHttpRequest")))


(defun get-action-name-from-request ()
  "Returns called action name if any action was called"
  (request-parameter
   weblocks.variables:*action-string*))


(defun refresh-request-p ()
  "Determines if a request is a result of the user invoking a browser
refresh function. Note that a request will not be considered a refresh
if there is an action involved (even if the user hits refresh)."
  (let ((action-name (get-action-name-from-request)))
    (and
     (null (weblocks::get-request-action action-name))
     (equalp (weblocks::all-tokens weblocks::*uri-tokens*)
             (weblocks.session:get-value 'weblocks::last-request-uri)))))
