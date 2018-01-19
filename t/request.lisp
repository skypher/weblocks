(defpackage #:weblocks/t/request
  (:use #:cl
        #:rove
        #:weblocks/t/utils))
(in-package weblocks/t/request)


(deftest refresh-request-p-1
  (with-session
    (with-request ("/foo/bar")
      (setf (weblocks.session:get-value 'weblocks.request::last-request-path)
            "/foo/bar")
      (ok (weblocks.request:refresh-request-p)
          "Refresh-request-p should return true, because current URI has same tokens as last-request-path."))))


(deftest get-full-url
  (testing "Getting full uri for request"
    ;; This test was made because old lack.test:generate-env
    ;; didn't supported full URIs and created environment
    ;; with full URI: http://localhost:80/some-path?with=params

    (let* ((uri "https://example.com:8443/some-path?with=params")
           (env (lack.test:generate-env uri :method :get))
           (request (lack.request:make-request env)))
      (ok (equal (weblocks.request:get-uri :request request)
                 "https://example.com:8443/some-path?with=params")
          "Weblock's get-uri should return full URL with correct scheme and port.")

      (ok (equal (lack.request:request-uri request)
                 "/some-path?with=params")
          "However Lack's request-uri returns only the path with parameters."))))

