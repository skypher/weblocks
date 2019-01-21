(defpackage #:weblocks/error-handler
  (:use #:cl)
  (:import-from #:trivial-backtrace
                #:print-backtrace)
  (:import-from #:weblocks/response
                #:immediate-response)
  ;; Just dependencies
  (:import-from #:log4cl)
  
  (:export #:on-error))
(in-package weblocks/error-handler)


(defgeneric on-error (app condition)
  (:documentation "This method is called when some unhandled error was raised by application.
                   It should call weblocks/response:immediate-response like this:

                       \(weblocks/response:immediate-response
                           \"Unhandled condition\"
                           :code 500)

"))


(defmethod on-error (app condition)
  "Default implementation returns a plain text page and 500 status code."
  (declare (ignorable app))
  
  (when condition
    (let ((traceback (print-backtrace
                      condition :output nil)))
      (log:error "Returning 500 error to user" traceback)))
  
  (immediate-response "Unhandled condition"
                      :code 500))
