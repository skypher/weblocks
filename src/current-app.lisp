(defpackage #:weblocks.current-app
  (:use #:cl)
  (:import-from #:weblocks.app
                #:*current-app*)
  (:export
   #:make-uri
   #:get-prefix))
(in-package weblocks.current-app)


(defun get-prefix ()
  "Returns the URL prefix of the application."
  (weblocks.app:get-prefix *current-app*))


(defun make-uri (uri)
  "Returns the URL prefix of the application."
  (weblocks.app:make-uri *current-app* uri))


