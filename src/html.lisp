(defpackage #:weblocks/html
  (:use #:cl)
  (:import-from #:spinneret
                #:*html*)
  (:export
   #:with-html
   #:*lang*
   #:with-html-string
   #:get-rendered-chunk))
(in-package weblocks/html)


(defvar *stream*
  (make-synonym-stream '*standard-output*)
  "Weblocks will write all output into this stream.

   This stream is created for each request and available to
   code executed within a request as a special
   variable. All html should be rendered to this stream,
   but don't worry, if you are using `with-html` or
   `with-html-string` macroses, they handle this for you.")


(defvar *lang* "en"
  "Language to add into the root html element.")


(defmacro with-html (&body body)
  `(let ((spinneret:*html-lang* *lang*)
         (spinneret:*html* *stream*))
     (spinneret:with-html
       ,@body)))


(defmacro with-html-string (&body body)
  "Like WITH-HTML, but capture the output as a string."
  `(with-output-to-string (*stream*)
     (with-html
       ,@body)))


(defun get-rendered-chunk ()
  "Returns a string containing HTML redered since last call to
   with-html, with-html-string or get-rendered-chunk.

   Call to this function clears internal stream, used for rendering!"
  (get-output-stream-string *stream*))
