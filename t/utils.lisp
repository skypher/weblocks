(defpackage #:weblocks.t.utils
  (:use #:cl
        #:weblocks)
  (:export
   #:with-request
   #:with-session
   #:is-html))
(in-package weblocks.t.utils)


(defmacro with-session (&body body)
  `(let ((weblocks.session::*session* (make-hash-table :test 'equal)))
     ,@body))


(defmacro with-request ((uri &key (method :get) data) &body body)
  "Argument 'data' should be an alist with POST parameters if method is :POST."
  `(weblocks.hooks:with-hooks
     (let* ((env (lack.test:generate-env ,uri :method ,method :content ,data))
            (weblocks.request::*request* (lack.request:make-request env)))
       ,@body)))


(defmacro is-html (form expected &optional message)
  `(let ((result (with-output-to-string
                     (weblocks:*weblocks-output-stream*)
                   ,form)))
     (prove:like result ,expected
               ,message)))

