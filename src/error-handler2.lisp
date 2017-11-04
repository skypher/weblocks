(defpackage #:weblocks.error-handler
  (:use #:cl)
  (:export
   #:on-error))
(in-package weblocks.error-handler)


(defgeneric on-error (app)
  (:documentation "This method is called when some unhandled error was raised by application.
                   It should call weblocks.response:abort-processing like this:

                       \(weblocks.response:abort-processing
                           \"Unhandled condition\"
                           :code 500
                           :content-type \"text/plain\"\)

"))


(defmethod on-error (app)
  "Default implementation returns a plain text page and 500 status code."
  (declare (ignorable app))
  (weblocks.response:abort-processing "Unhandled condition"
                                      :code 500
                                      :content-type "text/plain"))
