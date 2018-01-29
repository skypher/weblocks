(defpackage #:weblocks/utils/uri
  (:use #:cl
        #:f-underscore)
  (:import-from #:puri
                #:uri
                #:parse-uri
                #:uri-query
                #:render-uri)
  (:import-from #:quri
                #:url-encode-params
                #:url-decode-params)
  (:export #:request-uri-path
           #:add-get-param-to-url
           #:remove-parameter-from-uri))
(in-package weblocks/utils/uri)


;;; URI from pathname
(defmethod uri ((thing pathname))
  (puri:uri
   (format nil "~A~{~A/~}~A~A"
           (if (eql (car (pathname-directory thing)) :absolute) "/" "")
           (cdr (pathname-directory thing))
           (or (pathname-name thing) "")
           (if (pathname-type thing)
               (format nil ".~A" (pathname-type thing))
               ""))))

(defun remove-parameter-from-uri (uri parameter)
  "Removes the given parameter from a URI."
  (let* ((parsed-uri (parse-uri uri))
         (query (or (uri-query parsed-uri)
                    ""))
         (params (url-decode-params query))
         (final-params (remove-if (m ((name . value))
                                    (declare (ignorable value))
                                    (equal name parameter))
                                  params)))

    (setf (uri-query parsed-uri)
          (url-encode-params final-params))
    (render-uri parsed-uri nil)))

(defun remove-url-query-string (str)
  (cl-ppcre:regex-replace "\\?.*" str ""))

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
