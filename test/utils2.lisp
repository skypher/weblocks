(defpackage #:weblocks.t.utils
  (:use #:cl
        #:weblocks)
  (:export
   #:with-request
   #:with-session))
(in-package weblocks.t.utils)


(defmacro with-session (&body body)
  `(let ((weblocks.session::*session* (make-hash-table :test 'equal)))
     ,@body))


(defmacro with-request ((uri &key (method :get) data) &body body)
  "Argument 'data' should be an alist with POST parameters if method is :POST."
  `(let* ((env (lack.test:generate-env ,uri :method ,method :content ,data))
          (weblocks.request::*request* (lack.request:make-request env)))
     ,@body))



