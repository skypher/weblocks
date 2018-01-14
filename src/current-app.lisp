(defpackage #:weblocks.current-app
  (:use #:cl)
  (:import-from #:weblocks.app
                #:*current-app*))
(in-package weblocks.current-app)

(defun get-prefix (&optional (app *current-webapp*))
  "Returns the URL prefix of the application."
  (get-prefix app))


