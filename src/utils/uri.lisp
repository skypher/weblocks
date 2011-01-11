
(in-package :weblocks)

(wexport '(request-uri-path
           add-get-param-to-url
	   remove-parameter-from-uri
           parse-location-hash)
	 '(t util))

;;; URI from pathname
(defmethod puri:uri ((thing pathname))
  (puri:uri
   (format nil "~A~{~A/~}~A~A"
	   (if (eql (car (pathname-directory thing)) :absolute) "/" "")
	   (cdr (pathname-directory thing))
	   (or (pathname-name thing) "")
	   (if (pathname-type thing)
	       (format nil ".~A" (pathname-type thing))
	       ""))))

(defun request-uri-path ()
  "Returns the path component of the request URI. The path component
does not include the domain name, and any query string parameters.
Ex (when URI is http://blah.com/foo/bar?x=1&y=2):
\(request-uri-path)
=> \"/foo/bar\""
  (identity (cl-ppcre:regex-replace "/?(?:\\?.*)$" (request-uri*) "")))


(defun add-get-param-to-url (url name value)
  "Based on Edi's code in URL-REWRITE but uses & instead of &amp;
which is more appropriate for our uses."
  (concatenate 'string
               url
               (if (find #\? url :test #'char=)
                 "&"
                 "?")
               name
               "="
               (url-rewrite:url-encode value)))

(defun remove-parameter-from-uri (uri parameter)
  "Removes the given parameter from a URI."
  (let ((path (puri:uri-path (puri:parse-uri uri))))
    (loop for x in (get-parameters*)
       when (not (string-equal (car x) parameter))
       do (setf path (add-get-param-to-url path (car x) (cdr x))))
    path))


(defun tokenize-uri (uri &optional (remove-app-prefix t) (app (when remove-app-prefix
                                                                (current-webapp))))
  "Tokenizes an URI into a list of elements.

ex:
\(tokenize-uri \"/hello/world/blah\\test\\hala/world?hello=5;blah=7\"
=> (\"hello\" \"world\" \"blah\" \"test\" \"hala\" \"world\")"
  (loop for token in (cl-ppcre:split "[/\\\\]" 
				     (cl-ppcre:regex-replace "\\?.*"
							     (if remove-app-prefix
								 (subseq uri
                                                                         (length
                                                                           (webapp-prefix
                                                                             app)))
								 uri)
							     ""))
     unless (string-equal "" token)
       collect (url-decode token)))

(defun query-string->alist (query-string)
  ;; stolen from cl-oauth -- does one of ours deps already offer this?
  ;; TODO: doesn't handle leading ? or #
  (check-type query-string string)
  (let* ((kv-pairs (remove "" (cl-ppcre:split "&" query-string) :test #'equal))
         (alist (mapcar (lambda (kv-pair)
                          (let ((kv (cl-ppcre:split "=" kv-pair)))
                            (cons (first kv) (second kv))))
                        kv-pairs)))
    alist))

(defun parse-location-hash ()
  (let ((raw-hash (hunchentoot:get-parameter "weblocks-internal-location-hash")))
    (when raw-hash
      (query-string->alist (cl-ppcre:regex-replace "^#" raw-hash "")))))

