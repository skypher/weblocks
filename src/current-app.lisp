(defpackage #:weblocks/current-app
  (:use #:cl)
  (:import-from #:weblocks/app
                #:*current-app*
                #:get-prefix)
  (:export
   #:make-uri
   #:get-prefix))
(in-package weblocks/current-app)


(defun get-prefix ()
  "Returns the URL prefix of the application."
  (get-prefix *current-app*))



