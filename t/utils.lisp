(in-package :cl-user)
(defpackage #:weblocks.t.utils
  (:use #:cl
        #:weblocks
        #:hamcrest.prove)
  (:export
   #:with-request
   #:with-session
   #:is-html
   #:catch-hooks
   #:assert-hooks-called))
(in-package weblocks.t.utils)


(defmacro with-session (&body body)
  `(let ((weblocks.session::*session* (make-hash-table :test 'equal)))
     ,@body))


(defwebapp test-app
  :prefix "/"
  :autostart nil)


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
                             :tokens (weblocks::tokenize-uri (weblocks.request:get-path)))))
       ,@body)))


(defmacro is-html (form expected &optional message)
  `(let ((result (with-output-to-string
                     (weblocks:*weblocks-output-stream*)
                   ,form)))
     (prove:like result ,expected
               ,message)))


(defmacro catch-hooks ((&rest hook-names) &body body)
  "Catches all hook calls and makes available function assert-hooks-contains
during the body execution. Hook names should be a list of keywords.

Call assert-hooks-contains inside the body, to check if proper hooks were called:

\(handle-hooks \(:fact-created :fact-removed\)
   \(do-something-to-add-a-fact\)
   \(assert-hooks-called
      \(contains :fact-created a-contact a-twitter-name\)
      \(contains :fact-removed a-contact a-twitter-name\)\)\)"
  (alexandria:with-gensyms (hook-calls)
    (let ((hook-handlers
            (loop for hook-name in hook-names
                  collect `(weblocks.hooks:add-session-hook
                               ,hook-name
                               ,(alexandria:symbolicate 'handle- hook-name)
                               (&rest args)
                             (push (cons ,hook-name args)
                                   ,hook-calls))
                  )))
      `(weblocks.hooks:prepare-hooks
         (let* ((,hook-calls nil)) 
           ,@hook-handlers

           (macrolet ((assert-hooks-called (&body fact-matchers)
                        "Returns a matcher which checks if all called hooks are match to given matchers.
                         Example:

                             \(assert-hooks-called
                                  (:fact-created contact email\)\)"
                        `(assert-that 
                          (reverse ,',hook-calls)
                          (contains
                           ,@(mapcar (lambda (matcher)
                                       (cons 'contains matcher))
                                     fact-matchers)))))
             ,@body))))))
