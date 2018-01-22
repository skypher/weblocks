(defpackage #:weblocks/response
  (:use #:cl)
  (:import-from #:weblocks/request
                #:get-uri
                #:ajax-request-p
                #:get-header)
  (:import-from #:weblocks/js/base
                #:with-javascript-to-string
                #:with-javascript)
  (:import-from #:weblocks/actions
                #:on-missing-action)
  (:import-from #:weblocks/app
                #:get-prefix)
  (:import-from #:weblocks/commands
                #:add-command)
  (:import-from #:quri)
  (:export
   #:*code*
   #:*content-type*
   #:abort-processing
   #:add-header
   #:*headers*
   #:send-script
   #:make-uri
   #:redirect
   #:catch-possible-abort))
(in-package weblocks/response)


(defvar *code* nil
  "HTTP status code to return in response to request.

This variable is bound to 200 for each request. Set it to another
code if required to return something else.")


(defvar *content-type* nil
  "HTTP content type to return in response to request.

This variable is bound to text/html for each usual request and to application/json
for AJAX requests. Set it to another content type if you need to return something else.")


(defvar *headers* nil
  "HTTP headers to return in response to request.

This variable is bound to nil for each request. Set it to plist
with any headers you need or use (add-header ...) to add one header.

Additional header :content-type will be added to this list before
returning response. To change content type, set *content-type*.")

(defvar *abort-can-be-catched-p* nil
  "This variable will be set to 't automatically
   when you use `catch-possible-abort'.

   If it is not 't, then call to `abort-processing' will
   invoke the debugger, because it is abnormal situation.")


(defun add-header (name value)
  "Use this function to add a HTTP header:

\(add-header :x-request-id 100500\)"

  (declare (type symbol name)
           (type string value))
  (push value *headers*)
  (push name *headers*))


(defun make-uri (new-path)
  "Makes a new URL, based on the current request's URL.

   If new-path can be absolute, like /logout or relative,
   like ./stories.

   Also, it can contain a query params like /login?code=100500"
  (let* ((base (get-uri))
         (parsed-base (quri:uri base))
         (parsed-new-path (quri:uri new-path))
         (new-url (quri:merge-uris parsed-new-path
                                   parsed-base)))
    (quri:render-uri new-url)))


(defun abort-processing (content &key (content-type nil content-type-given)
                                      (code nil code-given)
                                      (headers nil headers-given))
  "Aborts request processing and return given value as response.

HTTP code and headers are taken from *code* and *content-type*."

  (log:debug "Aborting request processing"
             code
             content-type
             headers)

  (when content-type-given
    (setf *content-type* content-type))

  (when code-given
    (setf *code* code))

  (when headers-given
    (setf *headers* headers))

  (if *abort-can-be-catched-p*
      (throw 'abort-processing content)
      (error "Abort is not possible")))


(defmacro catch-possible-abort (&body body)
  "Catches throwed 'abort-processing and returns the value.

   Used in the server code and in tests."
  `(let ((*abort-can-be-catched-p* t))
     (catch 'abort-processing
       ,@body)))


(defun send-script (script &optional (place :after-load))
  "Send JavaScript to the browser. The way of sending depends
  on whether the current request is via AJAX or not.

  Script may be either a string or a list; if it is a list
  it will be compiled through Parenscript first.
  
  FIXME: is using PUSH or PUSHLAST correct?"
  (declare (ignorable place))
  (let ((script (etypecase script
                  (string script)
                  (list (ps:ps* script)))))
    (if (ajax-request-p)
        (let ((code (if (equalp (get-header "X-Weblocks-Client")
                                "JQuery")
                        script
                        (with-javascript-to-string script))))
          (add-command :execute-code
                       :code code)
          ;; TODO remove before-ajax-complete-scripts and on-ajax-complete-scripts completely
          ;; (ecase place
          ;;   (:before-load (push code weblocks.variables:*before-ajax-complete-scripts*))
          ;;   (:after-load (push code weblocks.variables:*on-ajax-complete-scripts*)))
          )
        (with-javascript
          script))))


(defun redirect (uri)
  "Redirects the client to a new URI."
  (if (ajax-request-p)
      (abort-processing
       (format nil "{\"redirect\":\"~A\"}" uri)
       :content-type "application/json")
      (abort-processing
       ""
       :headers (list :location uri)
       :code 302)))


(defmethod on-missing-action (app action-name)
  (declare (ignorable app action-name))
  (redirect
   (make-uri (get-prefix app))))


