(defpackage #:weblocks/error-handler
  (:use #:cl)
  (:import-from #:trivial-backtrace
                #:print-backtrace)
  (:import-from #:weblocks/response
                #:abort-processing)
  ;; Just dependencies
  (:import-from #:log)
  
  (:export #:on-error))
(in-package weblocks/error-handler)


(defgeneric on-error (app condition)
  (:documentation "This method is called when some unhandled error was raised by application.
                   It should call weblocks/response:abort-processing like this:

                       \(weblocks/response:abort-processing
                           \"Unhandled condition\"
                           :code 500
                           :content-type \"text/plain\"\)

"))


(defmethod on-error (app condition)
  "Default implementation returns a plain text page and 500 status code."
  (declare (ignorable app))
  
  (when condition
    (let ((traceback (print-backtrace
                      condition :output nil)))
      (log:error "Returning 500 error to user" traceback)))
  
  (abort-processing "Unhandled condition"
                    :code 500
                    :content-type "text/plain"))
