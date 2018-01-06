(defpackage #:weblocks.t.request
  (:use #:cl
        #:prove
        #:weblocks
        #:weblocks.t.utils))
(in-package weblocks.t.request)


(plan 1)

(subtest "refresh-request-p-1"
  (with-session
    (with-request ("/foo/bar")
      (setf (weblocks.session:get-value 'weblocks::last-request-uri)
            '("foo" "bar"))
      (is (weblocks.request:refresh-request-p)
          t
          "Refresh-request-p should return true, because current URI has same tokens as last-request-uri."))))


(subtest "Getting full uri for request"
  ;; This test was made because old lack.test:generate-env
  ;; didn't supported full URIs and created environment
  ;; with full URI: http://localhost:80/some-path?with=params

  (let* ((uri "https://example.com:8443/some-path?with=params")
         (env (lack.test:generate-env uri :method :get))
         (request (lack.request:make-request env)))
    (is (weblocks.request:get-uri :request request)
        "https://example.com:8443/some-path?with=params"
        "Weblock's get-uri should return full URL with correct scheme and port.")

    (is (lack.request:request-uri request)
        "/some-path?with=params"
        "However Lack's request-uri returns only the path with parameters.")))


(finalize)
