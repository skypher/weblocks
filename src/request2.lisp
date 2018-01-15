(defpackage #:weblocks.request
  (:use #:cl)
  (:export
   #:get-parameters
   #:get-parameter
   #:get-header
   #:ajax-request-p
   #:get-host
   #:get-port
   #:get-uri
   #:get-method
   #:refresh-request-p
   #:remove-header
   #:get-uri
   #:get-path
   #:with-request))
(in-package weblocks.request)


(defvar *request* nil
  "Holds current request from a browser.")


(defun get-uri (&key (request *request*))
  "For URL http://example.com/foo/bar?blah=minor returns it as is."
  (let ((host (get-host :request request))
        (port (get-port :request request))
        (scheme (lack.request:request-uri-scheme request))
        (path (get-path :request request))
        (query (lack.request:request-query-string request)))
    (quri:render-uri (quri:make-uri :scheme scheme
                                    :host host
                                    :port port
                                    :path path
                                    :query query))))


(defun get-path (&key (request *request*) with-params)
  "For URL http://example.com/foo/bar?blah=minor returns
/foo/bar path of the request's URL."
  (if with-params
      ;; request-uri returns path-info + GET params
      (lack.request:request-uri request)
      ;; Otherwice, return only a path
      (lack.request:request-path-info request)))

(defun get-path (&key (request *request*) with-params)
  "For URL http://example.com/foo/bar?blah=minor returns
/foo/bar path of the request's URL."
  (if with-params
      ;; request-uri returns path-info + GET params
      (lack.request:request-uri request)
      ;; Otherwice, return only a path
      (lack.request:request-path-info request)))


(defun get-host (&key (request *request*))
  (lack.request:request-server-name request))


(defun get-port (&key (request *request*))
  (lack.request:request-server-port request))


(defun get-method (&key (request *request*))
  "Returns association list with GET or POST parameters for current request."
  (lack.request:request-method request))


(defun get-parameters (&key (request *request*))
  "Returns association list with GET or POST parameters for current request."
  (lack.request:request-parameters request))


(defun get-parameter (name &key (request *request*))
  "Returns GET or POST parameter by name."
  (declare (type string name))

  (let ((params (get-parameters :request request)))
    (alexandria:assoc-value params
                            name
                            :test #'equal)))


(defun get-header (name &key (request *request*))
  "Returns value of the HTTP header or nil. Name is case insensitive."
  (let ((headers (lack.request:request-headers request))
        (lowercased-name (string-downcase name)))
    (gethash lowercased-name
             headers)))


(defun remove-header (name &key (request *request*))
  "Removes a HTTP header by name, returns new instance of request
without given header."
  
  (let ((lowercased-name (string-downcase name))
        ;; make shallow copy of the request
        (new-request (copy-structure request))
        (new-headers (metacopy:copy-thing (lack.request:request-headers request))))
    
    (remhash lowercased-name
             new-headers)
    
    (setf (lack.request:request-headers new-request)
          new-headers)

    new-request))


(defun ajax-request-p (&key (request *request*))
  "Detects if the current request was initiated via AJAX by looking
for 'X-Requested-With' http header. This function expects to be called
in a dynamic hunchentoot environment."
  ;; Sometimes this function may be called not in the context of request,
  ;; to update an instance of the widgets in asyncrounous code and to send
  ;; it via websocket.
  (and request
       (equal (get-header "X-Requested-With" :request request)
              "XMLHttpRequest")))


(defun get-action-name-from-request ()
  "Returns called action name if any action was called"
  (get-parameter weblocks.variables:*action-string*)) 


(defun refresh-request-p ()
  "Determines if a request is a result of the user invoking a browser
refresh function. Note that a request will not be considered a refresh
if there is an action involved (even if the user hits refresh)."
  (let ((action-name (get-action-name-from-request)))
    (and
     (null (weblocks::get-request-action action-name))
     (equalp (weblocks.request:get-path)
             (weblocks.session:get-value 'last-request-path)))))


(defun parse-location-hash ()
  (let ((raw-hash (weblocks.request:get-parameter "weblocks-internal-location-hash")))
    (when raw-hash
      (query-string->alist (cl-ppcre:regex-replace "^#" raw-hash "")))))


;; (defmacro with-path ((path) &body body)
;;   "This macro stores given uri in the session if requiest is not AJAX.

;;    Later, this value is used to determine if user refreshed the page."
;;   `(progn
;;      ,@body
;;      (unless (ajax-request-p)
;;        (setf (weblocks.session:get-value 'last-request-path)
;;              ,path))))


(defmacro with-request ((request) &body body)
  "This macro binds current request and stores request path in the session if requiest is not AJAX.

   Later, this value is used to determine if user refreshed the page."
  `(let ((*request* ,request))
     ,@body
     (unless (ajax-request-p)
       (setf (weblocks.session:get-value 'last-request-path)
             (get-path)))))



