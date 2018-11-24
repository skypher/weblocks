(defpackage #:weblocks/js/base
  (:use #:cl)
  (:import-from #:weblocks/html
                #:with-html
                #:with-html-string)
  (:import-from #:cl-ppcre
                #:regex-replace-all
                #:quote-meta-chars)
  (:export
   #:js-backend
   #:make-js-backend
   #:with-javascript-to-string
   #:with-javascript))
(in-package weblocks/js/base)


(defclass js-backend ()
  ())


(defgeneric make-js-backend (name)
  (:documentation "Creates a js backend instance of a given name.

Name should be a keyword like a :jquery or a :prototype."))


(defun escape-script-tags (source &key (delimiter ps:*js-string-delimiter*))
  "Escape script blocks inside scripts."
  (regex-replace-all
   (quote-meta-chars "</script>")
   (regex-replace-all (quote-meta-chars "]]>")
                      source
                      (format nil "]]~A + ~:*~A>" delimiter))
   (format nil "</scr~A + ~:*~Aipt>" delimiter)))


(defun %js (source &rest args)
  "Helper function for WITH-JAVASCRIPT macros."
  `(:script :type "text/javascript"
            (:raw #.(format nil "~%// <![CDATA[~%"))
            (:raw (escape-script-tags (format nil ,source ,@args)))
            (:raw #.(format nil "~%// ]]>~%"))
            ))


(defmacro with-javascript (source &rest args)
  "Places 'source' between script and CDATA elements. Used to avoid
having to worry about special characters in JavaScript code."
  `(with-html ,(apply #'%js source args)))


(defmacro with-javascript-to-string (source &rest args)
  "Places 'source' between script and CDATA elements. Used to avoid
having to worry about special characters in JavaScript code."
  `(with-html-string ,(apply #'%js source args)))

