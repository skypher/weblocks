(defpackage #:weblocks-test/request
  (:use #:cl
        #:rove
        #:weblocks-test/utils)
  (:import-from #:weblocks/request
                #:last-request-path
                #:refresh-request-p
                #:get-uri)
  (:import-from #:lack.test
                #:generate-env)
  (:import-from #:lack.request
                #:make-request
                #:request-uri))
(in-package weblocks-test/request)


(deftest refresh-request-p-1
  (with-session
    (with-request ("/foo/bar")
      (setf (weblocks/session:get-value 'last-request-path)
            "/foo/bar")
      (ok (refresh-request-p)
          "Refresh-request-p should return true, because current URI has same tokens as last-request-path."))))


(deftest get-full-url
  (testing "Getting full uri for request"
    ;; This test was made because old lack.test:generate-env
    ;; didn't supported full URIs and created environment
    ;; with full URI: http://localhost:80/some-path?with=params

    (let* ((uri "https://example.com:8443/some-path?with=params")
           (env (generate-env
                 uri :method :get))
           (request (make-request env)))
      (ok (equal (get-uri :request request)
                 "https://example.com:8443/some-path?with=params")
          "Weblock's get-uri should return full URL with correct scheme and port.")

      (ok (equal (request-uri request)
                 "/some-path?with=params")
          "However Lack's request-uri returns only the path with parameters."))))


(deftest with-request-returns-value-of-last-body-expression
  (with-session
    (let* ((env (generate-env "/" :method :get))
           (request (make-request env)))
      (ok (equal (weblocks/request:with-request (request)
                   100500
                   42)
                 42)
          "Macro with-request should return 42 because it was the last expression in the body."))))
