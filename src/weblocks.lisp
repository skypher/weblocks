;;; Code shared accross the entire weblocks framework
(defpackage #:weblocks
  (:use :cl :c2mop :metabang.utilities :moptilities :hunchentoot :cl-who :json :fare-matcher :cont :parenscript)
  (:shadowing-import-from :c2mop #:defclass #:defgeneric #:defmethod
			  #:standard-generic-function #:ensure-generic-function
			  #:standard-class #:typep #:subtypep)
  (:shadowing-import-from :fare-matcher #:match)
  (:shadow #:redirect)
  (:documentation
   "Weblocks is a Common Lisp framework that eases the pain of web
application development. It achieves its goals by standardizing on
various libraries, providing flexible and extensible generic views,
and exposing a unique widget-based approach to maintaining UI
state."))

(in-package :weblocks)

; re-export external symbols from cl-cont
(do-external-symbols (s (find-package :cont))
  (export (list s)))

(export '(*weblocks-output-stream* with-html reset-sessions str
	  with-javascript root-composite))

(defparameter *weblocks-output-stream* nil
  "Output stream for Weblocks framework created for each request
and available to code executed within a request as a special
variable. All html should be rendered to this stream.")

(defparameter *dirty-widgets* nil
  "Contains a list of dirty widgets at the current point in rendering
  cycle. This is a special variable modified by the actions that
  change state of widgets.")

(defvar *autostarting-webapps* nil
  "A list of webapps to start when start-weblocks is called")

(defvar *active-webapps* nil
  "A list of running applications.  Applications are only available
   after they have been started.")

(defun cl-escape-string (maybe-string)
  "Force quoting of special characters by cl-who for with-html."
  ;(format t "cl-escape-string: ~s~%" (describe maybe-string))
  (cond
    ((stringp maybe-string)
     (cl-who:escape-string-minimal-plus-quotes maybe-string))
    (t maybe-string)))

(defmethod convert-tag-to-string-list (tag (attr-list list) body body-fn)
  "The method convert-tag-to-string-list is a hook into cl-who's
  output system; here we use it to automatically escape special
  characters."
  ;(format *standard-output* "non-cl-who tag: ~s, attr-list ~s.~%" tag attr-list)
  (call-next-method
    tag
    (loop for inner-attr-list in attr-list
        collect
        (progn
          (cons
            (car inner-attr-list)
            (cons 'cl-escape-string (list (cdr inner-attr-list))))))
    body
    body-fn))

(defmacro with-html (&body body)
  "A wrapper around cl-who with-html-output macro."
  `(with-html-output (*weblocks-output-stream* nil :indent nil)
     ,@body))

(defmacro with-javascript (source &rest args)
  "Places 'source' between script and CDATA elements. Used to avoid
having to worry about special characters in JavaScript code."
  `(with-html
     (:script :type "text/javascript"
	      (fmt "~%// <![CDATA[~%")
	      (fmt ,source ,@args)
	      (fmt "~%// ]]>~%"))))

(defmacro root-composite ()
  "Expands to code that can be used as a place to access to the root
composite."
  `(webapp-session-value 'root-composite))

;;; This turns off a regex optimization that eats A LOT of memory
(setq cl-ppcre:*use-bmh-matchers* nil)
