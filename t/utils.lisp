(defpackage #:weblocks/t/utils
  (:use #:cl)
  (:import-from #:alexandria
                #:with-gensyms
                #:symbolicate)
  (:import-from #:lack.test
                #:generate-env)
  (:import-from #:lack.request
                #:make-request)
  (:import-from #:cl-ppcre
                #:all-matches)
  (:import-from #:rove
                #:ok)
  (:import-from #:weblocks.session
                #:*session*)
  (:import-from #:weblocks.app
                #:*current-app*
                #:defapp)
  (:import-from #:weblocks.html
                #:with-html-string)
  (:import-from #:weblocks.hooks
                #:prepare-hooks
                #:add-session-hook)
  ;; Just to point to dependencies
  (:import-from #:weblocks.request)
  
  (:export
   #:with-request
   #:with-session
   #:is-html
   #:catch-hooks
   #:assert-hooks-called))
(in-package weblocks/t/utils)


(defmacro with-session (&body body)
  `(let ((*session* (make-hash-table :test 'equal)))
     ,@body))


(defapp empty-app
  :prefix "/"
  :autostart nil)


(defmacro with-request ((uri &key
                               data
                               (method :get)
                               (app 'empty-app)) &body body)
  "Argument 'data' should be an alist with POST parameters if method is :POST."
  `(prepare-hooks
     (let* ((env (generate-env ,uri :method ,method :content ,data))
            ;; we need to setup a current webapp, because
            ;; uri tokenizer needs to know app's uri prefix
            (*current-app* (make-instance ',app)))
       (weblocks.request:with-request ((make-request env))
        ,@body))))


(defmacro is-html (form expected &optional message)
  `(let ((result (with-html-string
                   ,form)))
     (ok (all-matches ,expected result)
         ;;(string= result ,expected)
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
  (with-gensyms (hook-calls)
    (let ((hook-handlers
            (loop for hook-name in hook-names
                  collect `(add-session-hook
                               ,hook-name
                               ,(symbolicate 'handle- hook-name)
                               (&rest args)
                             (push (cons ,hook-name args)
                                   ,hook-calls))
                  )))
      `(prepare-hooks
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
