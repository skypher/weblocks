(defpackage #:weblocks.response
  (:use #:cl)
  (:export
   #:*code*
   #:*content-type*
   #:abort-processing
   #:*headers*))
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
with any headers you need.

Additional header :content-type will be added to this list before
returning response. To change content type, set *content-type*.")


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
