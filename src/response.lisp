(defpackage #:weblocks.response
  (:use #:cl)
  (:export
   #:*code*
   #:*content-type*
   #:abort-processing
   #:add-header
   #:*headers*
   #:send-script))
(in-package weblocks.response)


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


(defun add-header (name value)
  "Use this function to add a HTTP header:

\(add-header :x-request-id 100500\)"

  (declare (type symbol name)
           (type string value))
  (push value *headers*)
  (push name *headers*))


(defun abort-processing (content &key (content-type nil content-type-given)
                                      (code nil code-given)
                                      (headers nil headers-given))
  "Aborts request processing and return given value as response.

HTTP code and headers are taken from *code* and *content-type*."

  (log:debug "Aborting request processing"
             content
             code
             content-type
             headers)
  
  (when content-type-given
    (setf *content-type* content-type))

  (when code-given
    (setf *code* code))

  (when headers-given
    (setf *headers* headers))

  (throw 'abort-processing content))


(defun send-script (script &optional (place :after-load))
  "Send JavaScript to the browser. The way of sending depends
  on whether the current request is via AJAX or not.

  Script may be either a string or a list; if it is a list
  it will be compiled through Parenscript first.
  
  FIXME: is using PUSH or PUSHLAST correct?"
  (let ((script (etypecase script
                  (string script)
                  (list (ps:ps* script)))))
    (if (weblocks.request:ajax-request-p)
        (let ((code (if (equalp (weblocks.request:request-header "X-Weblocks-Client")
                                "JQuery")
                        script
                        (weblocks:with-javascript-to-string script))))
          (ecase place
            (:before-load (push code weblocks.variables:*before-ajax-complete-scripts*))
            (:after-load (push code weblocks.variables:*on-ajax-complete-scripts*))))
        (weblocks:with-javascript
          script))))
