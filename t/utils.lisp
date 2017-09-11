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


(defwebapp test-app
  :prefix "/")


(defmacro with-request ((uri &key (method :get) data) &body body)
  "Argument 'data' should be an alist with POST parameters if method is :POST."
  `(weblocks.hooks:prepare-hooks
     (let* ((env (lack.test:generate-env ,uri :method ,method :content ,data))
            (weblocks.request::*request* (lack.request:make-request env))
            ;; we need to setup a current webapp, because
            ;; uri tokenizer needs to know app's uri prefix
            (weblocks::*current-webapp* (make-instance 'test-app))
            (weblocks::*uri-tokens*
              (make-instance 'weblocks::uri-tokens
                             :tokens (weblocks::tokenize-uri (weblocks.request:request-uri)))))
       ,@body)))


(defmacro is-html (form expected &optional message)
  `(let ((result (with-output-to-string
                     (weblocks:*weblocks-output-stream*)
                   ,form)))
     (prove:like result ,expected
               ,message)))

