(defpackage #:weblocks/request
  (:use #:cl)
  (:import-from #:weblocks/app)
  
  (:import-from #:metacopy
                #:copy-thing)
  (:import-from #:lack.request
                #:request-uri-scheme
                #:request-query-string
                #:request-uri
                #:request-path-info
                #:request-server-name
                #:request-server-port
                #:request-method
                #:request-parameters
                #:request-headers)
  (:import-from #:alexandria
                #:with-gensyms
                #:assoc-value)
  (:import-from #:weblocks/variables
                #:*action-string*
                #:*ignore-missing-actions*)
  (:import-from #:weblocks/utils/uri
                #:query-string->alist)
  ;; Just to add dependency
  (:import-from #:weblocks/session)
  (:import-from #:quri)
  (:import-from #:weblocks/actions
                #:get-session-action)
  
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
   #:with-request
   #:pure-request-p))
(in-package weblocks/request)


(defvar *request* nil
  "Holds current request from a browser.")


(defun get-uri (&key (request *request*))
  "For URL http://example.com/foo/bar?blah=minor returns it as is."
  (let ((host (get-host :request request))
        (port (get-port :request request))
        (scheme (request-uri-scheme request))
        (path (get-path :request request))
        (query (request-query-string request)))
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
      (request-uri request)
      ;; Otherwice, return only a path
      (request-path-info request)))


(defun get-host (&key (request *request*))
  (request-server-name request))


(defun get-port (&key (request *request*))
  (request-server-port request))


(defun get-method (&key (request *request*))
  "Returns association list with GET or POST parameters for current request."
  (request-method request))


(defun get-parameters (&key (request *request*))
  "Returns association list with GET or POST parameters for current request."
  (request-parameters request))


(defun get-parameter (name &key (request *request*))
  "Returns GET or POST parameter by name."
  (declare (type string name))

  (let ((params (get-parameters :request request)))
    (assoc-value params
                 name
                 :test #'equal)))


(defun get-header (name &key (request *request*))
  "Returns value of the HTTP header or nil. Name is case insensitive."
  (let ((headers (request-headers request))
        (lowercased-name (string-downcase name)))
    (gethash lowercased-name
             headers)))


(defun remove-header (name &key (request *request*))
  "Removes a HTTP header by name, returns new instance of request
without given header."
  
  (let ((lowercased-name (string-downcase name))
        ;; make shallow copy of the request
        (new-request (copy-structure request))
        (new-headers (copy-thing (request-headers request))))
    
    (remhash lowercased-name
             new-headers)
    
    (setf (request-headers new-request)
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
  (get-parameter *action-string*)) 


(defun refresh-request-p ()
  "Determines if a request is a result of the user invoking a browser
refresh function. Note that a request will not be considered a refresh
if there is an action involved (even if the user hits refresh)."
  (let ((action-name (get-action-name-from-request)))
    (and
     (null (get-request-action action-name))
     (equalp (get-path)
             (weblocks/session:get-value 'last-request-path)))))


(defun pure-request-p ()
  "Detects if the current request is declared as 'pure', i.e. affects
no widgets or internal application state, but merely is a request for
information. Such requests simply return the result of the function
that represents the action and are used by some AJAX operations to
retreive information (suggest block, etc). When such requests are
satisfied, the actions have access to the session, the widgets, and
all other parameters. However, none of the callbacks (see
*on-pre-request*) are executed, no widgets are sent to the client,
etc."
  (string-equal (get-parameter "pure") "true"))


(defun parse-location-hash ()
  (let ((raw-hash (get-parameter "weblocks-internal-location-hash")))
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
  (with-gensyms (result)
    `(let* ((*request* ,request)
            (,result (progn
                      ,@body)))
     
       (unless (ajax-request-p)
         (setf (weblocks/session:get-value 'last-request-path)
               (get-path)))

       ,result)))


(defun get-request-action (action-name)
  "Gets an action from the request. If the request contains
*action-string* parameter, the action is looked up in the session and
appropriate function is returned. If no action is in the parameter,
returns nil. If the action isn't in the session (somehow invalid),
raises an assertion."
  (when action-name
    (let* ((app-wide-action (weblocks/app:get-action action-name))
           (session-action (get-session-action action-name))
           (request-action (or app-wide-action session-action)))
      ;; TODO: rethink this form. May be throw a special condition instead of string
      (unless *ignore-missing-actions*
        (assert request-action (request-action)
                (concatenate 'string "Cannot find action: " action-name)))
      request-action)))
