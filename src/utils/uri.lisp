
(in-package :weblocks)

;; (wexport '(request-uri-path
;;            add-get-param-to-url
;;            remove-parameter-from-uri
;;            parse-location-hash)
;;          '(t util))

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

;; (defun add-get-param-to-url (url name value)
;;   "Based on Edi's code in URL-REWRITE but uses & instead of &amp;
;; which is more appropriate for our uses."
;;   (concatenate 'string
;;                url
;;                (if (find #\? url :test #'char=)
;;                  "&"
;;                  "?")
;;                name
;;                "="
;;                (url-rewrite:url-encode value)))

(defun remove-parameter-from-uri (uri parameter)
  "Removes the given parameter from a URI."
  (let* ((parsed-uri (puri:parse-uri uri))
         (query (or (puri:uri-query parsed-uri)
                    ""))
         (params (quri:url-decode-params query))
         (final-params (remove-if (m ((name . value))
                                    (declare (ignorable value))
                                    (equal name parameter))
                                  params)))

    (setf (puri:uri-query parsed-uri)
          (quri:url-encode-params final-params))
    (puri:render-uri parsed-uri nil)))

(defun remove-url-query-string (str)
  (cl-ppcre:regex-replace "\\?.*" str ""))

(defun tokenize-uri (uri &optional (remove-app-prefix t) (app (when remove-app-prefix
                                                                (weblocks.app:get-current))))
  "Tokenizes an URI into a list of elements.

ex:
\(tokenize-uri \"/hello/world/blah\\test\\hala/world?hello=5;blah=7\"
=> (\"hello\" \"world\" \"blah\" \"test\" \"hala\" \"world\")"
  (flet ((remove-first-and-last-empty-strings (list)
           (when (string= "" (first list))
             (setf list (cdr list)))

           (when (string= "" (car (last list)))
             (setf list (butlast list)))
           list))

    (remove-first-and-last-empty-strings 
      (loop for token in (split-sequence:split-sequence 
                           #\/
                           (remove-url-query-string 
                             (if remove-app-prefix
                               (subseq uri
                                       (length
                                         (webapp-prefix
                                           app)))
                               uri)))
            collect (quri:url-decode token)))))

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

;; Moved to weblocks.request
;; (defun parse-location-hash ()
;;   (let ((raw-hash (weblocks.request:get-parameter "weblocks-internal-location-hash")))
;;     (when raw-hash
;;       (query-string->alist (cl-ppcre:regex-replace "^#" raw-hash "")))))

