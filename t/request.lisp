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


(finalize)
